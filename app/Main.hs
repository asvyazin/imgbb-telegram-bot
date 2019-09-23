{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where


import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject, eitherDecode)
import Network.HTTP.Client (newManager, Manager, parseRequest, httpLbs, responseBody, Request(..), RequestBody(..), responseStatus)
import Network.HTTP.Client.MultipartFormData (partFileRequestBody, formDataBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (methodPost, Status(..))
import Options.Applicative.Simple
import RIO
import RIO.List (lastMaybe, foldl')
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


data TGException = TGClient ClientError
                 | TGFileNotFound
                 | TGPhotoNotFound deriving Show


instance Exception TGException


data ImgbbException = ImgbbJsonException String
                    | ImgbbHttpException String deriving Show


instance Exception ImgbbException


data ImgbbResponse = ImgbbResponse
                     { imgbbResponseData :: ImgbbResponseData
                     , imgbbResponseSuccess :: Bool
                     , imgbbResponseStatus :: Int
                     } deriving (Show)


instance FromJSON ImgbbResponse where
  parseJSON = withObject "ImgbbResponse" $ \v -> ImgbbResponse <$> v .: "data" <*> v .: "success" <*> v .: "status"


data ImgbbResponseData = ImgbbResponseData
                         { imgbbResponseDataId :: Text
                         , imgbbResponseDataUrlViewer :: Text
                         , imgbbResponseDataUrl :: Text
                         , imgbbResponseDataDisplayUrl :: Text
                         , imgbbResponseDataTitle :: Text
                         , imgbbResponseDataTime :: Text
                         , imgbbResponseDataImage :: ImgbbResponseDataFile
                         , imgbbResponseDataThumb :: ImgbbResponseDataFile2
                         , imgbbResponseDataMedium :: Maybe ImgbbResponseDataFile2
                         , imgbbResponseDataDeleteUrl :: Text
                         } deriving (Show)


instance FromJSON ImgbbResponseData where
  parseJSON = withObject "ImgbbResponseData" $ \v -> ImgbbResponseData <$> v .: "id" <*> v .: "url_viewer" <*> v .: "url" <*> v .: "display_url" <*> v .: "title" <*> v .: "time" <*> v .: "image" <*> v .: "thumb" <*> v .:? "medium" <*> v .: "delete_url"


data ImgbbResponseDataFile = ImgbbResponseDataFile
                             { imgbbResponseDataFileFileName :: Text
                             , imgbbResponseDataFileName :: Text
                             , imgbbResponseDataFileMime :: Text
                             , imgbbResponseDataFileExtension :: Text
                             , imgbbResponseDataFileUrl :: Text
                             , imgbbResponseDataFileSize :: Int
                             } deriving (Show)


data ImgbbResponseDataFile2 = ImgbbResponseDataFile2
                              { imgbbResponseDataFile2FileName :: Text
                              , imgbbResponseDataFile2Name :: Text
                              , imgbbResponseDataFile2Mime :: Text
                              , imgbbResponseDataFile2Extension :: Text
                              , imgbbResponseDataFile2Url :: Text
                              , imgbbResponseDataFile2Size :: Text
                              } deriving (Show)


instance FromJSON ImgbbResponseDataFile where
  parseJSON =
    withObject "ImgbbResponseDataFile" $ \v -> ImgbbResponseDataFile <$> v .: "filename" <*> v .: "name" <*> v .: "mime" <*> v .: "extension" <*> v .: "url" <*> v .: "size"


instance FromJSON ImgbbResponseDataFile2 where
  parseJSON =
    withObject "ImgbbResponseDataFile2" $ \v -> ImgbbResponseDataFile2 <$> v .: "filename" <*> v .: "name" <*> v .: "mime" <*> v .: "extension" <*> v .: "url" <*> v .: "size"


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
processUpdate update = do
  post <- unwrap $ channel_post update
  phs <- unwrap $ photo post
  urls <- traverse processPhoto phs
  let chatId =
        ChatId $ chat_id $ chat post
      origMessageId =
        message_id post
      msg =
        foldl' (\x y -> x <> "\n" <> y) "uploaded: " urls
      req =
        SendMessageRequest chatId msg Nothing Nothing Nothing (Just origMessageId) Nothing
  messageResp <- liftTG $ sendMessageM req
  logInfo $ displayShow messageResp
  pure True
  where unwrap Nothing = throwM TGPhotoNotFound
        unwrap (Just p) = pure p


processPhoto :: (HasToken env, HasManager env, HasLogFunc env, HasImgbbToken env) => PhotoSize -> RIO env Text
processPhoto PhotoSize { photo_file_id = fileId } = do
  Response { result = File { file_path = maybeFilePath } } <- liftTG $ getFileM fileId
  case maybeFilePath of
    Nothing ->
      throwM TGFileNotFound
    Just filePath ->
      processFilePath filePath


processFilePath :: (HasToken env, HasManager env, HasLogFunc env, HasImgbbToken env) => Text -> RIO env Text
processFilePath filePath = do
  token <- view tokenL
  manager <- view managerL
  request <- liftIO $ parseRequest $ unpack $ fileUrl token filePath
  response <- liftIO $ httpLbs request manager
  let body =
        responseBody response
  url <- uploadToImgbb body $ unpack filePath
  logInfo $ displayShow url
  pure url


fileUrl :: Token -> Text -> Text
fileUrl (Token token) filePath =
  "https://api.telegram.org/file/" <> token <> "/" <> filePath


uploadToImgbb :: (HasImgbbToken env, HasManager env) => BL.ByteString -> FilePath -> RIO env Text
uploadToImgbb body fileName = do
  imgbbToken <- view imgbbTokenL
  manager <- view managerL
  request <- liftIO $ parseRequest "https://api.imgbb.com/1/upload"
  let qs =
        "key=" <> imgbbToken
      req =
        request { method = methodPost, queryString = encodeUtf8 qs, requestBody = RequestBodyLBS body }
      parts =
        [ partFileRequestBody "image" fileName (RequestBodyLBS body) ]
  req2 <- formDataBody parts req
  response <- liftIO $ httpLbs req2 manager
  checkStatus response
  let res =
        eitherDecode $ responseBody response
  case res of
    Left err ->
      throwM $ ImgbbJsonException err
    Right imgbbResp ->
      pure $ imgbbResponseDataUrl $ imgbbResponseData imgbbResp
  where checkStatus response = do
          let Status status _ =
                responseStatus response
          if 200 <= status && status < 300
             then pure ()
             else let err =
                        show response
                  in throwM $ ImgbbHttpException err
