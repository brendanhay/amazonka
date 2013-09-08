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
    (
    -- * Metadata about the signing process
      SigningMetadata

    -- * Signing raw requests
    , sign
    , sign'
    ) where

import           Control.Applicative
import           Control.Error
import           Control.Monad.IO.Class
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Base64          as Base64
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy            as LBS
import           Data.Char                       (intToDigit, toLower, ord)
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

data SigningMetadata = SigningMetadata
    { smdReq   :: !ByteString
    , smdCReq  :: !ByteString
    , smdSReq  :: !ByteString
    , smdSTS   :: !ByteString
    , smdAuthz :: !ByteString
    } deriving (Show)

sign :: RawRequest -> AWS SignedRequest
sign = fmap fst . sign'

sign' :: RawRequest -> AWS (SignedRequest, SigningMetadata)
sign' rq = do
    auth <- currentAuth
    reg  <- currentRegion
    liftIO $ signer rq auth reg
  where
    signer = case svcSigner $ rqService rq of
        SigningVersion3 -> version3
        SigningVersion4 -> version4

--
-- Internal
--

version3 :: RawRequest -> Auth -> Region -> IO (SignedRequest, SigningMetadata)
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

version4 :: RawRequest -> Auth -> Region -> IO SignedRequest
version4 RawRequest{..} Auth{..} reg = do
    time <- getCurrentTime :: IO UTCTime

    let host = svcEndpoint reg
        url  = "https://" <> host <> path <> "?" <> query

    SignedRequest url rqBody <$> buildRequest (do
        http rqMethod $ path <> "?" <> query
        deleteHeader "Host"
        deleteHeader "User-Agent"
        setHeader "Authorization" $ authorizationHeader time
        setContentType $ toBS rqContent
        setHeader "Accept-Encoding" "gzip"
        deleteHeader "Host"
        setHeader "Host" host
        setHeader "Date" $ rfc822Time time)
  where
    Service{..} = rqService

    authorizationHeader time = mconcat
        [ algorithm
        , " Credential="
        , credentialScope time
        , ", SignedHeaders="
        , signedHeaders time
        , ", Signature="
        , signature time
        ]

    signature time = hex $ hmac (signingKey time) (stringToSign time)

    signingKey time =
          hmac "aws4_request"
        $ hmac svcName
        $ hmac (toBS reg)
        $ hmac ("AWS4" <> secretKey) (basicTime time)

    stringToSign time = BS.intercalate "\n"
        [ algorithm
        , awsTime time
        , credentialScope time
        , sha256 $ canonicalRequest time
        ]

    credentialScope time = BS.intercalate "/"
       [ accessKey
       , basicTime time
       , toBS reg
       , svcName
       , "aws4_request"
       ]

    algorithm = "AWS4-HMAC-SHA256"

    canonicalRequest time = BS.intercalate "/" $
        [ toBS rqMethod
        , path
        , query
        , canonicalHeaders time
        , signedHeaders time
        , sha256 $ fromMaybe "" rqBody
        ]

    path = validPath rqPath

    query = encodeQuery (urlEncode True) . sort $ rqQuery `union`
        [ ("Version", toBS svcVersion)
        , ("AWSAccessKeyId", accessKey)
        ]

    canonicalHeaders = mconcat . map f . headers
      where
        f (k, v) = mconcat $ [BS.map toLower k, ":", strip ' ' v, "\n"]

    signedHeaders = BS.intercalate ";" . map fst . headers

    headers time = map f
        . groupBy (\x y -> fst x == fst y)
        . sort
        $ union (Map.toList rqHeaders)
            [ ("Content-Type", toBS rqContent)
            , ("Host", svcEndpoint reg)
            , ("Date", rfc822Time time)
            , ("Accept-Encoding", "gzip")
            ]
      where
        f (h:hs) = (BS.map toLower $ fst h, BS.intercalate "," . map snd $ h:hs)
        f []     = ("", "")

rfc822Time :: UTCTime -> ByteString
rfc822Time = BS.pack . formatTime defaultTimeLocale "%a, %0d %b %Y %H:%M:%S GMT"

awsTime :: UTCTime -> ByteString
awsTime = BS.pack . formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ"

iso8601Time :: UTCTime -> ByteString
iso8601Time = BS.pack . formatTime defaultTimeLocale (iso8601DateFormat $ Just "%XZ")

basicTime :: UTCTime -> ByteString
basicTime = BS.pack . formatTime defaultTimeLocale "%Y%m%d"

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

hex :: ByteString -> ByteString
hex = BS.pack . foldr f "" . BS.unpack
  where
    f c t = intToDigit (n `div` 16) : intToDigit (n `mod` 16) : t
      where
        n = ord c
