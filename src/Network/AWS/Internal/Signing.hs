{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

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
import           Control.Error
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Base64          as Base64
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy            as LBS
import           Data.Char                       (toLower)
import qualified Data.Digest.Pure.SHA            as SHA
import           Data.List
import qualified Data.Map                        as Map
import           Data.Monoid
import           Data.Time                       (UTCTime, formatTime, getCurrentTime)
import           Network.AWS.Internal.Monad
import           Network.AWS.Internal.String
import           Network.AWS.Internal.Types
import           Network.HTTP.QueryString.Pickle
import           Network.HTTP.Types              (urlEncode)
import           Network.Http.Client
import           System.Locale                   (defaultTimeLocale, iso8601DateFormat)

sign :: RawRequest -> AWS SignedRequest
sign rq = do
    auth <- currentAuth
    reg  <- currentRegion
    liftIO $ signer rq auth reg
  where
    signer = case svcSigner $ rqService rq of
        SigningVersion2 -> version2
        SigningVersion3 -> version3
        SigningVersion4 -> version4

--
-- Internal
--

version2 :: RawRequest -> Auth -> Region -> IO SignedRequest
version2 RawRequest{..} Auth{..} reg = do
    time <- getCurrentTime

    let qry = query time $ fromMaybe (error "Handle missing action") rqAction
        sig = signature qry
        url = "https://"
            <> svcEndpoint reg
            <> path
            <> "?"
            <> qry
            <> "&Signature="
            <> sig

    SignedRequest url rqBody <$> buildRequest (do
        http rqMethod url
        mapM_ (uncurry setHeader) $ Map.toList rqHeaders)
  where
    Service{..} = rqService

    path = validPath rqPath

    query time action = encodeQuery (urlEncode True) $ rqQuery `union`
        [ ("Action",           action)
        , ("Version",          toBS svcVersion)
        , ("SignatureVersion", "2")
        , ("SignatureMethod",  "HmacSHA256")
        , ("Timestamp",        awsTime time)
        , ("AWSAccessKeyId",   accessKey)
        ]

    signature qry = Base64.encode
        . hmac secretKey
        $ BS.intercalate "\n"
            [ packMethod rqMethod
            , svcEndpoint reg
            , path
            , qry
            ]

version3 :: RawRequest -> Auth -> Region -> IO SignedRequest
version3 RawRequest{..} Auth{..} reg = do
    time <- rfc822Time <$> getCurrentTime

    let url = "https://"
            <> svcEndpoint reg
            <> validPath rqPath
            <> query

    SignedRequest url rqBody <$> buildRequest (do
        http rqMethod url
        mapM_ (uncurry setHeader) $ Map.toList rqHeaders ++
            [ ("X-Amz-Date",           time)
            , ("X-Amzn-Authorization", authorization time)
            ])
  where
    Service{..} = rqService

    query | null rqQuery = ""
          | otherwise    = "?" <> encodeQuery (urlEncode True) rqQuery

    authorization time = "AWS3-HTTPS AWSAccessKeyId="
        <> accessKey
        <> ",Algorithm=HmacSHA256,Signature="
        <> Base64.encode (hmac secretKey time)

-- FIXME: need to investigate how to set the body/payload to x-url-formencoded
-- or in the querystring

version4 :: RawRequest -> Auth -> Region -> IO SignedRequest
version4 RawRequest{..} Auth{..} reg = do
    time <- getCurrentTime

    let hs  = headers time
        qry = query $ fromMaybe (error "Handle missing action") rqAction
        sig = signature time hs qry
        url = "https://"
            <> svcEndpoint reg
            <> path
            <> "?"
            <> qry

    SignedRequest url rqBody <$> buildRequest (do
        http rqMethod url
        mapM_ (uncurry setHeader) $
            ("Authorization", authorization time hs sig) : hs)
  where
    Service{..} = rqService

    path = validPath rqPath

    authorization time hdrs sig = BS.intercalate ","
        [ "AWS4-HMAC-SHA256 Credential=" <> accessKey <> "/" <> credentialScope time
        , "SignedHeaders=" <> signedHeaders hdrs
        , "Signature=" <> sig
        ]

    query action = encodeQuery (urlEncode True) $ rqQuery `union`
        [ ("Action", action)
        , ("Version", toBS svcVersion)
        ]

    signedHeaders    = BS.intercalate ";" . map (BS.map toLower . fst)
    canonicalHeaders = BS.intercalate ";"
        . map (\(k, v) -> BS.map toLower k <> ":" <> strip ' ' v)

    headers time = Map.toList rqHeaders `union`
        [ ("Host",         svcEndpoint reg)
        , ("X-Amz-Date",   awsTime time)
        , ("Content-Type", toBS rqContent)
        ]

    credentialScope time = BS.intercalate "/"
        [ scopeTime time
        , toBS reg
        , svcName
        , "aws4_request"
        ]

    signature time hs qry = hmac signingKey stringToSign
      where
        signingKey = foldl' hmac ("AWS4" <> secretKey)
            [ awsTime time
            , toBS reg
            , svcName
            , "aws4_request"
            ]

        stringToSign = BS.intercalate "\n"
            [ "AWS4-HMAC-SHA256"
            , awsTime time
            , credentialScope time
            , canonicalRq
            ]

        -- FIXME: must be lowercase base 16 encoded
        canonicalRq = sha256 $ BS.intercalate "\n"
            [ packMethod rqMethod
            , path
            , qry
            , canonicalHeaders hs
            , signedHeaders hs
            , sha256 ""
            ]

packMethod :: Method -> ByteString
packMethod = BS.pack . show

rfc822Time :: UTCTime -> ByteString
rfc822Time = BS.pack . formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S GMT"

awsTime :: UTCTime -> ByteString
awsTime = BS.pack . formatTime defaultTimeLocale (iso8601DateFormat $ Just "%XZ")

scopeTime :: UTCTime -> ByteString
scopeTime = BS.pack . formatTime defaultTimeLocale "%Y%m%d"

validPath :: Maybe ByteString -> ByteString
validPath = maybe "/" (mappend "/" . strip '/')

sha256 :: ByteString -> ByteString
sha256 = LBS.toStrict
    . SHA.bytestringDigest
    . SHA.sha256
    . LBS.fromStrict

hmac :: ByteString -> ByteString -> ByteString
hmac key msg = LBS.toStrict
    . SHA.bytestringDigest
    $ SHA.hmacSha256 (LBS.fromStrict key) (LBS.fromStrict msg)
