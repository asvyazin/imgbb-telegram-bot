{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where


import Control.Monad.IO.Class (MonadIO(liftIO))
import Network.HTTP.Client (newManager, Manager, parseRequest, httpLbs, responseBody, Request(..), RequestBody(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (methodPost)
import Options.Applicative.Simple
import RIO
import RIO.List (lastMaybe)
import qualified RIO.ByteString.Lazy as BL
import RIO.Text (unpack)
import Servant.Client.Core (ClientError)
import Web.Telegram.API.Bot


data Options = Options
               { optionsToken :: Text
               , optionsImgbbToken :: Text
               }


class HasToken a where
  tokenL :: Lens' a Token


class HasManager a where
  managerL :: Lens' a Manager


class HasImgbbToken a where
  imgbbTokenL :: Lens' a Text


data App = App
           { appToken :: Token
           , appManager :: Manager
           , appLogFunc :: LogFunc
           , appImgbbToken :: Text
           }


instance HasToken App where
  tokenL = lens appToken (\a b -> a { appToken = b })


instance HasManager App where
  managerL = lens appManager (\a b -> a { appManager = b })


instance HasLogFunc App where
  logFuncL = lens appLogFunc (\a b -> a { appLogFunc = b })


instance HasImgbbToken App where
  imgbbTokenL = lens appImgbbToken (\a b -> a { appImgbbToken = b })


data TGException = TGClient ClientError deriving Show


instance Exception TGException


liftTG :: (HasToken env, HasManager env) => TelegramClient a -> RIO env a
liftTG client = do
  token <- view tokenL
  manager <- view managerL
  res <- liftIO $ runTelegramClient token manager client
  case res of
    Left err -> throwM err
    Right r -> pure r


main :: IO ()
main = do
  let tokenOption =
        strOption (long "token" <> help "Telegram bot token")
      imgbbTokenOption =
        strOption (long "imgbb-token" <> help "Imgbb API token")
  (Options { optionsToken = token, optionsImgbbToken = imgbbToken }, ()) <- simpleOptions "0.1" "Telegram Bot" "My Telegram Bot" (Options <$> tokenOption <*> imgbbTokenOption) empty
  manager <- newManager tlsManagerSettings
  lo <- logOptionsHandle stderr True
  withLogFunc lo (\lf -> do
                    let app = App (Token token) manager lf imgbbToken
                    runRIO app $ mainLoop Nothing)


mainLoop :: (HasToken env, HasManager env, HasLogFunc env, HasImgbbToken env) => Maybe Int -> RIO env ()
mainLoop offset = do
  let request =
        GetUpdatesRequest offset Nothing Nothing Nothing
  Response updates _ <- liftTG $ getUpdatesM request
  results <- traverse doProcess updates
  let maybeLastProcessed =
        snd <$> lastMaybe (takeWhile fst results)
      newOffset =
        case maybeLastProcessed of
          Nothing -> offset
          Just (Update { update_id = lastUpdateId }) -> Just (lastUpdateId + 1)
  mainLoop newOffset
  where doProcess update = do
          res <- processUpdate update
          pure (res, update)


processUpdate :: (HasLogFunc env, HasToken env, HasManager env, HasImgbbToken env) => Update -> RIO env Bool
processUpdate update =
  fromMaybe True <$> traverse (fmap and . traverse processPhoto) (channel_post update >>= photo)


processPhoto :: (HasToken env, HasManager env, HasLogFunc env, HasImgbbToken env) => PhotoSize -> RIO env Bool
processPhoto PhotoSize { photo_file_id = fileId } = do
  Response { result = File { file_path = filePath } } <- liftTG $ getFileM fileId
  fromMaybe False <$> traverse processFilePath filePath


processFilePath :: (HasToken env, HasManager env, HasLogFunc env, HasImgbbToken env) => Text -> RIO env Bool
processFilePath filePath = do
  token <- view tokenL
  manager <- view managerL
  request <- liftIO $ parseRequest $ unpack $ fileUrl token filePath
  response <- liftIO $ httpLbs request manager
  let body =
        responseBody response
  url <- uploadToImgbb body
  logInfo $ displayShow url
  pure False


fileUrl :: Token -> Text -> Text
fileUrl (Token token) filePath =
  "https://api.telegram.org/file/" <> token <> "/" <> filePath


uploadToImgbb :: (HasImgbbToken env, HasManager env, HasLogFunc env) => BL.ByteString -> RIO env Text
uploadToImgbb body = do
  imgbbToken <- view imgbbTokenL
  manager <- view managerL
  request <- liftIO $ parseRequest "https://api.imgbb.com/1/upload"
  let qs =
        "key=" <> imgbbToken
      req =
        request { method = methodPost, queryString = encodeUtf8 qs, requestBody = RequestBodyLBS body }
  response <- liftIO $ httpLbs req manager
  logInfo $ displayShow response
  pure ""