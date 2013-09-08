{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

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
      SigningMetadata (..)

    -- * Signing raw requests
    , sign
    , sign'
    ) where

import           Control.Error
import           Control.Monad.IO.Class
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Base64          as Base64
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy            as LBS
import           Data.Char                       (intToDigit, toLower, ord)
import qualified Data.Digest.Pure.SHA            as SHA
import           Data.List
import           Data.Monoid
import           Data.Time                       (UTCTime, formatTime, getCurrentTime)
import           Network.AWS.Internal.Monad
import           Network.AWS.Internal.String
import           Network.AWS.Internal.Types
import           Network.HTTP.QueryString.Pickle
import           Network.HTTP.Types              (urlEncode)
import           Network.Http.Client
import           System.Locale                   (defaultTimeLocale,)

data SigningMetadata = SigningMetadata
    { smdCReq  :: !ByteString
    , smdSReq  :: !ByteString
    , smdSTS   :: !ByteString
    , smdAuthz :: !ByteString
    } deriving (Eq)

instance Show SigningMetadata where
    show SigningMetadata{..} = BS.unpack . BS.unlines $
          ("Canonical Request:" : BS.lines smdCReq)
       ++ ("Signed Request:"    : BS.lines smdSReq)
       ++ ("String To Sign:"    : BS.lines smdSTS)
       ++ ("Auth Header:"       : BS.lines smdAuthz)

sign :: RawRequest -> AWS SignedRequest
sign rq = do
    auth <- currentAuth
    reg  <- currentRegion
    time <- liftIO getCurrentTime
    liftIO . fmap fst $ sign' rq auth reg time

sign' :: RawRequest -> Auth -> Region -> UTCTime -> IO (SignedRequest, SigningMetadata)
sign' rq = signer rq
  where
    signer = case svcSigner $ rqService rq of
        SigningVersion3 -> version3
        SigningVersion4 -> version4

version3 :: RawRequest
         -> Auth
         -> Region
         -> UTCTime
         -> IO (SignedRequest, SigningMetadata)
version3 RawRequest{..} Auth{..} reg (rfc822Time -> time) = do
    let url = "https://"
            <> svcEndpoint reg
            <> validPath rqPath
            <> query

    build meta (SignedRequest url rqBody) $ do
        http rqMethod url
        mapM_ (uncurry setHeader) $ rqHeaders ++
            [ ("X-Amz-Date",           time)
            , ("X-Amzn-Authorization", authorization)
            ]
  where
    Service{..} = rqService

    query | null rqQuery = ""
          | otherwise    = "?" <> encodeQuery (urlEncode True) rqQuery

    authorization = "AWS3-HTTPS AWSAccessKeyId="
        <> accessKey
        <> ",Algorithm=HmacSHA256,Signature="
        <> Base64.encode (hmac secretKey time)

    meta = SigningMetadata "" "" "" ""

version4 :: RawRequest
         -> Auth
         -> Region
         -> UTCTime
         -> IO (SignedRequest, SigningMetadata)
version4 RawRequest{..} Auth{..} reg time = do
    let host = svcEndpoint reg
        url  = "https://" <> host <> path <> "?" <> query
        meta = SigningMetadata
            (canonicalRequest time)
            (signature time)
            (stringToSign time)
            (authorization time)

    build meta (SignedRequest url rqBody) $ do
        http rqMethod $ path <> "?" <> query
        deleteHeader "Host"
        deleteHeader "User-Agent"
        setHeader "Authorization" $ authorization time
        setContentType $ toBS rqContent
        setHeader "Accept-Encoding" "gzip"
        deleteHeader "Host"
        setHeader "Host" host
        setHeader "Date" $ rfc822Time time
  where
    Service{..} = rqService

    authorization ts = mconcat
        [ algorithm
        , " Credential="
        , credentialScope ts
        , ", SignedHeaders="
        , signedHeaders ts
        , ", Signature="
        , signature ts
        ]

    signature ts = hex $ hmac (signingKey ts) (stringToSign ts)

    signingKey ts =
          hmac "aws4_request"
        $ hmac svcName
        $ hmac (toBS reg)
        $ hmac ("AWS4" <> secretKey) (basicTime ts)

    stringToSign ts = BS.intercalate "\n"
        [ algorithm
        , awsTime ts
        , credentialScope ts
        , sha256 $ canonicalRequest ts
        ]

    credentialScope ts = BS.intercalate "/"
       [ accessKey
       , basicTime ts
       , toBS reg
       , svcName
       , "aws4_request"
       ]

    algorithm = "AWS4-HMAC-SHA256"

    canonicalRequest ts = BS.intercalate "/" $
        [ toBS rqMethod
        , path
        , query
        , canonicalHeaders ts
        , signedHeaders ts
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

    headers ts = map f
        . groupBy (\x y -> fst x == fst y)
        . sort
        $ union rqHeaders
            [ ("Content-Type", toBS rqContent)
            , ("Host", svcEndpoint reg)
            , ("Date", rfc822Time ts)
            , ("Accept-Encoding", "gzip")
            ]
      where
        f (h:hs) = (BS.map toLower $ fst h, BS.intercalate "," . map snd $ h:hs)
        f []     = ("", "")

build :: a -> (Request -> b) -> RequestBuilder () -> IO (b, a)
build meta rq = fmap (, meta) . fmap rq . buildRequest

rfc822Time :: UTCTime -> ByteString
rfc822Time = BS.pack . formatTime defaultTimeLocale "%a, %0d %b %Y %H:%M:%S GMT"

awsTime :: UTCTime -> ByteString
awsTime = BS.pack . formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ"

basicTime :: UTCTime -> ByteString
basicTime = BS.pack . formatTime defaultTimeLocale "%Y%m%d"

validPath :: ByteString -> ByteString
validPath = mappend "/" . strip '/'

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
