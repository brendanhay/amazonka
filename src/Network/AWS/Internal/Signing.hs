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
import           Data.Char                  (isSpace, toLower)
import qualified Data.Digest.Pure.SHA       as SHA
import           Data.List
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Time                  (UTCTime, formatTime, getCurrentTime)
import           Network.AWS.Internal.Types
import           Network.HTTP.Types         (urlEncode)
import           Network.Http.Client
import           System.Locale              (defaultTimeLocale, iso8601DateFormat)

sign :: AWSService a => RawRequest a b -> AWS SignedRequest
sign rq = do
    svc <- service rq
    let signer =
          case svcSigner svc of
              Version2 -> version2
              Version3 -> version3
              Version4 -> version4
    awsAuth <$> ask >>= liftIO . signer rq svc

--
-- Internal
--

version2 :: RawRequest a b -> Service -> Auth -> IO SignedRequest
version2 RawRequest{..} Service{..} Auth{..} = do
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
        , ("Version",          toBS svcVersion)
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

version3 :: RawRequest a b -> Service -> Auth -> IO SignedRequest
version3 RawRequest{..} Service{..} Auth{..} = do
    time <- rfc822Time <$> getCurrentTime

    let sig  = Base64.encode $ hmac secretKey time
        auth = authorization accessKey sig
        url  = "https://"
            <> validHost rqHost
            <> "/"
            <> toBS svcVersion
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

version4 :: RawRequest a b -> Service -> Auth -> IO SignedRequest
version4 RawRequest{..} Service{..} Auth{..} = do
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
        , ("Version", toBS svcVersion)
        , ("X-Amz-Algorithm", "AWS4-HMAC-SHA256")
        , ("X-Amz-Credential", "AKIAIOSFODNN7EXAMPLE%2F20110909%2Fus-east-1%2Fiam%2Faws4_request")
        , ("X-Amz-Date", awsTime time)
        , ("X-Amz-SignedHeaders", signedHdrs)
        ]

    signedHdrs = BS.intercalate ";"
        . map (BS.map toLower)
        $ Map.keys rqHeaders

    canonicalHdrs = BS.intercalate ";"
        . map (\(k, v) -> BS.map toLower k <> ":" <> strip ' ' v)
        $ Map.toList rqHeaders

    -- FIXME: Double check time formats

    -- FIXME: Use the tests from:
    -- https://awsiammedia.s3.amazonaws.com/public/sample/aws4_testsuite/aws4_testsuite.zip

    signature secret time qry = hmac signingKey $ stringToSign time qry
      where
        signingKey = hmac service "aws4_request"
          where
            date    = hmac ("AWS4" <> secret) $ awsTime time
            region  = hmac date $ toBS svcRegion
            service = hmac region svcName

        stringToSign time qry = str <> canonicalRq qry
          where
            str = sha256 $ BS.intercalate "\n"
                [ "AWS4-HMAC-SHA256"
                , awsTime time
                , credentialScope time
                ]

            credentialScope time = BS.intercalate "/"
                [ scopeTime time
                , toBS svcRegion
                , svcName
                , "aws4_request\n"
                ]

            canonicalRq qry = sha256 $ BS.intercalate "\n"
                [ packMethod rqMethod -- HTTPRequestMethod
                , path                -- CanonicalURI
                , qry                 -- CanonicalQueryString
                , canonicalHdrs       -- CanonicalHeaders
                , signedHdrs          -- SignedHeaders
                , sha256 ""           -- HexEncode(Hash(Payload))
                ]

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

stripSpaces :: ByteString -> ByteString
stripSpaces = f . f
  where
    f = BS.reverse . BS.dropWhile isSpace

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
    $ SHA.hmacSha256 (LBS.fromStrict key) (LBS.fromStrict msg)

