{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module      : Network.AWS.Request
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as LBS
import           Data.Data
import qualified Data.Digest.Pure.SHA   as SHA
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Time              (UTCTime, formatTime, getCurrentTime)
import qualified Network.HTTP.Types     as HTTP
import           Network.Http.Client
import           OpenSSL                (withOpenSSL)
import           System.Environment
import qualified System.IO.Streams      as Streams
import           System.Locale          (defaultTimeLocale, iso8601DateFormat)
import           Text.Hastache
import           Text.Hastache.Context

type Endpoint  = ByteString
type Action    = ByteString
type Path      = ByteString
type Version   = ByteString

data Credentials = Credentials
    { accessKey :: ByteString
    , secretKey :: ByteString
    } deriving (Show)

newtype AWS a = AWS { unWrap :: ReaderT Credentials IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadPlus, MonadReader Credentials)

runAWS :: Maybe Credentials -> AWS a -> IO a
runAWS mcreds aws = do
    creds <- maybe env return mcreds
    runReaderT (unWrap aws) creds
  where
    env :: IO Credentials
    env = do
        (acc, sec) <- (,)
            <$> lookupEnv "ACCESS_KEY_ID"
            <*> lookupEnv "SECRET_ACCESS_KEY"
        return . fromMaybe (error "Oh noes!") $
            Credentials <$> fmap BS.pack acc <*> fmap BS.pack sec

    -- metadataCredentials

class (Data a, Typeable a) => AWSRequest a where
    template :: a -> ByteString
    endpoint :: a -> ByteString
    request  :: a -> AWS Request

send :: AWSRequest a => a -> AWS ()
send rq = do
    r <- request rq
    liftIO . withOpenSSL $ do
        s <- baselineContextSSL
        c <- openConnectionSSL s (endpoint rq) 443

        bodyStream >>= sendRequest c r . inputStreamBody

        -- receiveResponse c (\p i -> do
        --     x <- Streams.read i
        --     BS.putStr $ fromMaybe "" x)

        closeConnection c
  where
    bodyStream = do
       b <- hastacheStr defaultConfig (template rq) $ mkGenericContext rq
       print b
       Streams.makeInputStream . return . Just $ LBS.toStrict b

apiVersion :: ByteString
apiVersion = "2012-12-01"

version2 :: Method
         -> Endpoint
         -> Action
         -> [(ByteString, ByteString)]
         -> AWS Request
version2 meth end action params = do
    creds <- ask
    time  <- liftIO getCurrentTime
    liftIO . buildRequest . http meth $ mconcat
        [ "/?"
        , query (accessKey creds) time
        , "&Signature="
        , signature creds time
        ]
  where
    signature creds time = HTTP.urlEncode True
        . Base64.encode
        . LBS.toStrict
        . SHA.bytestringDigest
        . SHA.hmacSha256 (LBS.fromStrict $ secretKey creds)
        . LBS.fromStrict
        $ BS.intercalate "\n"
            [packMethod meth
            , end
            , action
            , query (accessKey creds) time
            ]

    query access time = queryString $ params `union`
        [ ("Action", action)
        , ("Version", apiVersion)
        , ("SignatureVersion", "2")
        , ("SignatureMethod", "HmacSHA256")
        , ("Timestamp", timeFormat time)
        , ("AWSAccessKeyId", access)
        ]

version3 :: Method
         -> Path
         -> [(ByteString, ByteString)]
         -> AWS Request
version3 meth path params = do
    creds <- ask
    time  <- liftIO getCurrentTime
    liftIO . buildRequest $ do
        http meth $ "/" <> apiVersion <> "/" <> path <> "?" <> query (accessKey creds)
        setHeader "X-Amzn-Authorization" $ authorization creds time
  where
    query access = queryString $ ("AWSAccessKeyId", access) : params

    authorization creds time = mconcat
        [ "AWS3-HTTPS AWSAccessKeyId="
        , accessKey creds
        , ", Algorithm=HmacSHA256, Signature="
        , signature (secretKey creds) time
        ]

    signature secret = LBS.toStrict
        . SHA.bytestringDigest
        . SHA.hmacSha256 (LBS.fromStrict secret)
        . LBS.fromStrict
        . timeFormat

packMethod :: Method -> ByteString
packMethod = BS.pack . show

queryString :: [(ByteString, ByteString)] -> ByteString
queryString = BS.intercalate "&" . map concatEq . sort
  where
    concatEq (k, v) = mconcat [k, "=", HTTP.urlEncode True v]

timeFormat :: UTCTime -> ByteString
timeFormat = BS.pack . formatTime defaultTimeLocale fmt
  where
    fmt = iso8601DateFormat $ Just "%XZ"
