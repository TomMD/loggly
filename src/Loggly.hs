{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
module Loggly
    ( Token, makeToken
    , Tag, makeTag
    , newLoggly, Loggly
    ) where

import           Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import           Data.Char (isAlphaNum)
import           Data.Coerce
import           Data.String
import           Data.Proxy
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Network.HTTP.Client.TLS as HTTP
import           Servant.API
import           Servant.Client

-- | A loggly authentication token (secret).
newtype Token = Token Text
    deriving stock (Eq,Ord)
    deriving newtype (Show)
    deriving newtype (ToHttpApiData)

-- | Construct an auth token from text.
makeToken :: Text -> Token
makeToken = Token

instance IsString Token where
    fromString = makeToken . fromString

-- | A loggly Tag starts with an alphanumeric character and remaining characters
-- are alphanum, hyphen, period, or underscores.  Max length 64.
--
-- See 'makeTag' for a dynamic check.
newtype Tag = Tag Text
    deriving stock (Eq,Ord)
    deriving newtype (Show)

-- | Safely creates a tag by replacing an invalid tag with "INVALID-TAG"
-- allowing loggly users to see the application has a tagging issue rather than
-- letting loggly simply drop the invalid tag.
makeTag :: Text -> Tag
makeTag t
  | T.all isAlphaNum (T.take 1 t) &&
    T.all isValidTagChar t &&
    T.length t <= 64                    = Tag t
  | otherwise                           = Tag "INVALID-TAG"
 where
     isValidTagChar :: Char -> Bool
     isValidTagChar x = isAlphaNum x || x `elem` ("-._"::String)

instance IsString Tag where
    fromString = makeTag . fromString

newtype Tags = Tags Text
    deriving stock (Eq,Ord)
    deriving newtype (Show)
    deriving newtype (ToHttpApiData)

base :: BaseUrl
base = either err id $ parseBaseUrl logglyLoggingURL
 where
 err = error $ "impossible: could not parse URL " <> (show logglyLoggingURL)
 logglyLoggingURL :: String
 logglyLoggingURL = "https://logs-01.loggly.com"

-- Create a new connection pool to loggly for use in logging.
newLoggly :: MonadIO m => Token -> m Loggly
newLoggly token =
    do mgr <- HTTP.newTlsManager
       let env = mkClientEnv mgr base
       let logglyIO payload tags =
                either Left (const (Right ())) <$>  
              runClientM (logglyCM payload token (makeTags tags)) env >>= \case
                Left (UnsupportedContentType _ _) -> pure $ Right ()
                e -> pure e
       pure (Loggly logglyIO)
  where
    logglyCM = client logglyAPIProxy

    makeTags :: [Tag] -> Tags
    makeTags = Tags . T.intercalate "," . coerce


-- | A structure allowing sending log messages to Loggly.  This value closes
-- over the HTTP manager and loggly token provided to 'newLoggly'.
newtype Loggly = Loggly { logToLoggly :: Aeson.Value -> [Tag] -> IO (Either ClientError ()) }

-- | Log a message to loggly with particular tags.
loggly :: (MonadIO m, Aeson.ToJSON a) => Loggly -> a -> [Tag] -> m (Either ClientError ())
loggly (Loggly f) a = liftIO . f (Aeson.toJSON a)

logglyAPIProxy :: Proxy LogglyAPI
logglyAPIProxy = Proxy

-- | The loggly API fragment provided is a single endpoint that accepts
-- arbitrary JSON, a token, and any number (?) of tags (probably <2k in
-- practice).
type LogglyAPI =
        "inputs" :> ReqBody '[JSON] Aeson.Value
                 :> Capture "token" Token
                 :> "tag"
                 :> Capture "tag" Tags
                 :> Post '[JSON] ()
