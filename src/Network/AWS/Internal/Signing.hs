{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- |
-- Module      : Network.AWS.Internal.Signing
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Signing
    ( version2
    , version3
    ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Digest.Pure.SHA       as SHA
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Time                  (UTCTime, formatTime, getCurrentTime)
import           Network.AWS.Internal.Types
import           Network.HTTP.Types         (urlEncode)
import           Network.Http.Client
import           System.Locale              (defaultTimeLocale, iso8601DateFormat)

version2 :: AWSRegion a => RawRequest a -> AWS SignedRequest
version2 = signer version2Signer

version3 :: AWSRegion a => RawRequest a -> AWS SignedRequest
version3 = signer version3Signer

--
-- Internal
--

signer :: AWSRegion a
       => (Auth -> RawRequest a -> IO SignedRequest)
       -> RawRequest a
       -> AWS SignedRequest
signer f raw = do
    auth <- awsAuth <$> ask
    rq   <- maybe raw (`regionalise` raw) <$> (awsRegion <$> ask)
    liftIO $ f auth rq

version2Signer :: Auth -> RawRequest a -> IO SignedRequest
version2Signer Auth{..} RawRequest{..} = do
    time <- getCurrentTime

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

    SignedRequest url rqBody <$> buildRequest (http rqMethod url)
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

version3Signer :: Auth -> RawRequest a -> IO SignedRequest
version3Signer Auth{..} RawRequest{..} = do
    time <- rfc822Time <$> getCurrentTime

    let sig  = signature secretKey time
        auth = authorization accessKey sig
        url  = "https://"
            <> validHost rqHost
            <> "/"
            <> toBS rqVersion
            <> validPath rqPath
            <> query

    SignedRequest url rqBody <$>
        buildRequest (do
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
