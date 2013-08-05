{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

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
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Base64   as Base64
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Digest.Pure.SHA     as SHA
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Time                (UTCTime, formatTime, getCurrentTime)
import           Network.AWS.EC2.Metadata
import           Network.AWS.Types
import           Network.HTTP.Types       (urlEncode)
import           Network.Http.Client
import           OpenSSL                  (withOpenSSL)
import           System.Environment
import qualified System.IO.Streams        as Streams
import           System.Locale            (defaultTimeLocale, iso8601DateFormat)

runAWS :: AWS a -> IO a
runAWS aws = withOpenSSL $ do
    auth <- discover
    putStrLn $ "Found: " ++ show auth
    runReaderT (unWrap aws) auth

global :: GlobalRequest a => a -> AWS ByteString
global = send signGlobal

region :: RegionRequest a => Region -> a -> AWS ByteString
region reg = send (signRegion reg)

sign :: SigningVersion -> RawRequest -> AWS SignedRequest
sign Version2 = version2
sign Version3 = version3

--
-- Internal
--

-- FIXME: Should I try to be smart about choosing the IAM role name
-- from the metadata, or require it to be specified?
discover :: MonadIO m => m Auth
discover = liftIO $ do
    me <- fromEnv
    case me of
        Just x  -> return x
        Nothing -> fromMaybe (error msg) <$> fromMetadata
  where
    fromEnv = do
        acc <- pack "ACCESS_KEY_ID"
        sec <- pack "SECRET_ACCESS_KEY"
        return $ Auth <$> acc <*> sec

    pack = fmap (fmap BS.pack) . lookupEnv

    fromMetadata = decode . LBS.fromStrict <$> metadata (SecurityCredentials "s3_ro")

    msg = "Failed to get authentication information from environment or EC2 metadata"

-- FIXME: XHT -> Aeson
send :: (a -> AWS SignedRequest) -> a -> AWS ByteString
send signer rq = do
    SignedRequest{..} <- signer rq
    liftIO . bracket (establishConnection rqUrl) closeConnection $ \conn -> do
        sendRequest conn rqRequest $ maybe emptyBody inputStreamBody rqStream
        receiveResponse conn $ \_ inp ->
            fromMaybe "" <$> Streams.read inp

version2 :: RawRequest -> AWS SignedRequest
version2 RawRequest{..} = do
    Auth{..} <- ask
    time     <- liftIO getCurrentTime

    let act = fromMaybe (error "Handle missing action") rqAction
        qry = query act accessKey time
        sig = signature secretKey qry
        url = "https://"
            <> validHost rqHost
            <> path
            <> "?"
            <> qry
            <> "&Signature="
            <> sig

    liftIO $ SignedRequest url rqBody <$> buildRequest (http rqMethod url)
  where
    path = validPath rqPath

    query action access time = fmtQueryString $ rqQuery `union`
        [ ("Action",           action)
        , ("Version",          toBS rqVersion)
        , ("SignatureVersion", "2")
        , ("SignatureMethod",  "HmacSHA256")
        , ("Timestamp",        awsTime time)
        , ("AWSAccessKeyId",   access)
        ]

    signature secret qry = urlEncode True
        . Base64.encode
        . LBS.toStrict
        . SHA.bytestringDigest
        . SHA.hmacSha256 (LBS.fromStrict secret)
        . LBS.fromStrict
        $ BS.intercalate "\n"
            [ packMethod rqMethod
            , rqHost
            , path
            , qry
            ]

version3 :: RawRequest -> AWS SignedRequest
version3 RawRequest{..} = do
    Auth{..} <- ask
    time     <- rfc822Time <$> liftIO getCurrentTime

    let sig  = signature secretKey time
        auth = authorization accessKey sig
        url  = "https://"
            <> validHost rqHost
            <> "/"
            <> toBS rqVersion
            <> validPath rqPath
            <> query

    liftIO $ SignedRequest url rqBody
        <$> buildRequest (do
                http rqMethod url
                setHeader "X-AMZ-Date" time
                setHeader "X-Amzn-Authorization" auth)
  where
    query | null rqQuery = ""
          | otherwise    = "?" <> fmtQueryString rqQuery

    authorization access sig = "AWS3-HTTPS AWSAccessKeyId="
        <> access
        <> ",Algorithm=HmacSHA256,Signature="
        <> sig

    signature secret = Base64.encode
        . LBS.toStrict
        . SHA.bytestringDigest
        . SHA.hmacSha256 (LBS.fromStrict secret)
        . LBS.fromStrict

packMethod :: Method -> ByteString
packMethod = BS.pack . show

fmtQueryString :: [(ByteString, ByteString)] -> ByteString
fmtQueryString = BS.intercalate "&" . map concatEq . sort
  where
    concatEq (k, v) = mconcat [k, "=", urlEncode True v]

rfc822Time :: UTCTime -> ByteString
rfc822Time = BS.pack . formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S GMT"

awsTime :: UTCTime -> ByteString
awsTime = BS.pack . formatTime defaultTimeLocale (iso8601DateFormat $ Just "%XZ")

validHost :: ByteString -> ByteString
validHost = strip '/'

validPath :: Maybe ByteString -> ByteString
validPath = maybe "/" (mappend "/" . strip '/')

strip :: Char -> ByteString -> ByteString
strip c bstr
    | BS.cons c "" == bstr = ""
    | otherwise = ($ bstr) $ case (BS.head bstr == c, BS.last bstr == c) of
        (True,  True)  -> BS.tail . BS.init
        (False, True)  -> BS.init
        (True,  False) -> BS.tail
        _              -> id
