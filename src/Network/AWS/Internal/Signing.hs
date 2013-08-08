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
    ( sign
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

sign :: RawRequest a b -> AWS (SignedRequest b)
sign rq = do
    svc  <- service rq
    auth <- awsAuth <$> ask
    liftIO . ($ svc auth) $ case svcVersion svc of
        Version2 -> version2
        Version3 -> version3
        Version4 -> version4

--
-- Internal
--

version2 :: Service -> Auth -> RawRequest a b -> IO (SignedRequest b)
version2 Service{..} Auth{..} RawRequest{..} = do
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
        . hmac secret
        $ BS.intercalate "\n"
            [ packMethod rqMethod
            , rqHost
            , path
            , qry
            ]

version3 :: Service -> Auth -> RawRequest a b -> IO (SignedRequest b)
version3 Serivce{..} Auth{..} RawRequest{..} = do
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

    signature = Base64.encode . hmac

version4 :: Service -> Auth -> RawRequest a b -> IO (SignedRequest b)
version4 Service{..} Auth{..} RawRequest{..} = do
    time <- getCurrentTime

    let act = fromMaybe (error "Handle missing action") rqAction
        qry = query act accessKey time
        url = "https://"
            <> validHost rqHost
            <> path
            <> "?"
            <> qry

    SignedRequest url rqBody <$> buildRequest (http rqMethod url)
  where
    path = validPath rqPath

    query action access time = fmtQueryString $ rqQuery `union`
        [ ("Action", action)
        , ("Version", toBS rqVersion)
        , ("X-Amz-Algorithm", "AWS4-HMAC-SHA256")
        , ("X-Amz-Credential", "AKIAIOSFODNN7EXAMPLE%2F20110909%2Fus-east-1%2Fiam%2Faws4_request")
        , ("X-Amz-Date", awsTime time)
        , ("X-Amz-SignedHeaders", signedHdrs)
        ]

    signature secret time qry = hmac signingKey stringToSign

    signingKey = hmac service "aws4_request"
      where
        date    = hmac ("AWS4" <> secret) time
        region  = hmac date reg
        service = hmac region svc

    stringToSign time = str <> canonicalRq qry
      where
        str = hmac . LBS.fromStrict $ BS.intercalate "\n"
            [ "AWS4-HMAC-SHA256"
            , awsTime time
            , credentialScope time
            ]

        credentialScope time = BS.intercalate "/"
            [ scopeTime time
            , toBS reg
            , svc
            , "aws4_request\n"
            ]

        canonicalRq qry = hmac . LBS.fromStrict $ BS.intercalate "\n"
            [ packMethod rqMethod    -- HTTPRequestMethod
            , path                   -- CanonicalURI
            , qry                    -- CanonicalQueryString
            , canonicalHdrs          -- CanonicalHeaders
            , signedHdrs             -- SignedHeaders
            , lBS.toStrict $ hmac "" -- HexEncode(Hash(Payload))
            ]

        canonicalHdrs = BS.intercalate ";"
            . map (\(k, v) -> BS.map toLower k <> ':' <> strip v)
            $ Map.toList rqHeaders

        signedHdrs = BS.intercalate ";"
            . map (BS.map toLower)
            $ Map.keys rqHeaders

strip :: ByteString -> ByteString
strip = f . f
  where
    f = BS.reverse . BS.dropWhile (== ' ')

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

scopeTime :: UTCTime -> ByteString
scopeTime = BS.pack . formatTime defaultTimeLocale "%Y%m%d"

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

sha256 :: ByteString -> ByteString
sha256 = LBS.toStrict
    . SHA.bytestringDigest
    . SHA.sha256
    . LBS.fromStrict

hmac :: ByteString -> ByteString -> ByteString
hmac key msg = LBS.toStrict
    . SHA.bytestringDigest
    . SHA.hmacSha256 (LBS.fromStrict key) (LBS.fromStrict msg)

