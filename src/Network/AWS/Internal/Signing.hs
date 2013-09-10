{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

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
    -- * Signing raw requests
      sign

    -- * Exposed for testing purposes
    , SigningMetadata (..)
    , version4
    ) where

import           Control.Arrow                   (first)
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Base64          as Base64
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy            as LBS
import           Data.Char                       (intToDigit, toLower, ord)
import qualified Data.Digest.Pure.SHA            as SHA
import           Data.Function                   (on)
import           Data.List
import           Data.Monoid
import           Data.Time                       (UTCTime)
import qualified Data.Time                       as Time
import           Network.AWS.Internal.Monad
import           Network.AWS.Internal.String
import           Network.AWS.Internal.Types
import           Network.HTTP.QueryString.Pickle
import           Network.HTTP.Types              (urlEncode)
import           Network.Http.Client
import           System.Locale

sign :: RawRequest -> AWS SignedRequest
sign r = do
    auth <- currentAuth
    reg  <- currentRegion
    time <- liftIO Time.getCurrentTime
    dbg  <- debugEnabled

    let hs = [("Date", rfc822Time time), ("Host", svcEndpoint reg)]
        rq = r { rqHeaders = rqHeaders r ++ hs }

    liftIO $ signer dbg rq auth reg time
  where
    Service{..} = rqService r

    signer dbg = case svcSigner of
        SigningVersion2 -> version2
        SigningVersion3 -> version3
        SigningVersion4 -> \a b c d -> fmap fst $ version4 a b c d dbg

--
-- Internal
--

version2 :: RawRequest
         -> Auth
         -> Region
         -> UTCTime
         -> IO SignedRequest
version2 RawRequest{..} Auth{..} reg time = do
    let url = "https://" <> host <> path

    fmap (SignedRequest url rqBody) . buildRequest $ do
        http rqMethod path
        setHeaders rqHeaders
  where
    Service{..} = rqService

    host = svcEndpoint reg
    path = rqPath <> "?" <> query <> "&Signature=" <> urlEncode True signature

    query = encodeQuery (urlEncode True) . sort $ rqQuery ++
        [ ("Version",          toBS svcVersion)
        , ("SignatureVersion", "2")
        , ("SignatureMethod",  "HmacSHA256")
        , ("Timestamp",        iso8601Time time)
        , ("AWSAccessKeyId",   accessKey)
        ]

    signature = Base64.encode
        . hmac secretKey
        $ BS.intercalate "\n"
            [ toBS rqMethod
            , host
            , validPath rqPath
            , query
            ]

version3 :: RawRequest
         -> Auth
         -> Region
         -> UTCTime
         -> IO SignedRequest
version3 RawRequest{..} Auth{..} reg time = do
    let url = "https://" <> svcEndpoint reg <> path

    fmap (SignedRequest url rqBody) . buildRequest $ do
        http rqMethod path
        setHeaders $ ("X-Amzn-Authorization", authorization) : rqHeaders
  where
    Service{..} = rqService

    path | null rqQuery = rqPath
         | otherwise    = rqPath <> "?" <> encodeQuery (urlEncode True) rqQuery

    authorization = "AWS3-HTTPS AWSAccessKeyId="
        <> accessKey
        <> ", Algorithm=HmacSHA256, Signature="
        <> Base64.encode (hmac secretKey $ rfc822Time time)

data SigningMetadata = SigningMetadata
    { smdCReq  :: !ByteString
    , smdSTS   :: !ByteString
    , smdAuthz :: !ByteString
    , smdSReq  :: !ByteString
    } deriving (Eq)

instance Show SigningMetadata where
    show SigningMetadata{..} = BS.unpack . BS.unlines
        $ BS.lines smdCReq
       ++ BS.lines smdSTS
       ++ BS.lines smdAuthz
       ++ BS.lines smdSReq

version4 :: RawRequest
         -> Auth
         -> Region
         -> UTCTime
         -> Bool
         -> IO (SignedRequest, SigningMetadata)
version4 RawRequest{..} Auth{..} reg time dbg = do
    let url = "https://" <> svcEndpoint reg <> path

        -- FIXME: Temporary, for testing purposes
    let a  = [("Authorization", " " <> authorization time)]
        hs = map (\(k,v) -> k <> ":" <> v) $ rqHeaders ++ a
        r  = BS.intercalate " " [toBS rqMethod, path, "http/1.1"]
        m  = SigningMetadata canonicalRequest (stringToSign time)
                 (authorization time) (BS.unlines $ r : hs)

    when dbg $ print m

    fmap (, m) . fmap (SignedRequest url rqBody) . buildRequest $ do
        http rqMethod path
        setHeaders $ ("Authorization", authorization time) : rqHeaders
  where
    Service{..} = rqService

    path | BS.null query = rqPath
         | otherwise     = rqPath <> "?" <> query

    authorization ts = mconcat
        [ algorithm
        , " Credential="
        , accessKey
        , "/"
        , credentialScope ts
        , ", SignedHeaders="
        , signedHeaders
        , ", Signature="
        , signature ts
        ]

    signature ts = hex . hmac signingKey $ stringToSign ts
      where
        signingKey = foldl1 hmac
            [ "AWS4" <> secretKey
            , basicTime ts
            , toBS reg
            , svcName
            , "aws4_request"
            ]

    stringToSign ts = BS.intercalate "\n"
        [ algorithm
        , awsTime ts
        , credentialScope ts
        , sha256 canonicalRequest
        ]

    credentialScope ts = BS.intercalate "/"
       [ basicTime ts
       , toBS reg
       , svcName
       , "aws4_request"
       ]

    algorithm = "AWS4-HMAC-SHA256"

    canonicalRequest = BS.intercalate "\n"
        [ toBS rqMethod
        , validPath rqPath
        , query
        , canonicalHeaders
        , signedHeaders
        , sha256 $ fromMaybe "" rqBody
        ]

    query = encodeQuery (urlEncode True) $ sort $ rqQuery ++
       [ ("AWSAccessKeyId", accessKey)
       ]

    canonicalHeaders = mconcat $ map f headers
      where
        f (k, v) = mconcat [BS.map toLower k, ":", strip ' ' v, "\n"]

    signedHeaders = BS.intercalate ";" . nub $ map fst headers

    headers = sort
        . map f
        . groupBy ((==) `on` fst)
        $ map (first (BS.map toLower)) rqHeaders
      where
        f (h:hs) = (BS.map toLower $ fst h, BS.intercalate "," . sort . map snd $ h:hs)
        f []     = ("", "")

setHeaders :: [(ByteString, ByteString)] -> RequestBuilder ()
setHeaders hs = do
    setHostname host 443
    deleteHeader "User-Agent"
    deleteHeader "Accept-Encoding"
    mapM_ (uncurry setHeader) $ filter ((/= "Host") . fst) hs
  where
    host = fromMaybe "unknown" $ "Host" `lookup` hs -- FIXME

rfc822Time, iso8601Time, awsTime, basicTime :: UTCTime -> ByteString
rfc822Time  = formatTime "%a, %d %b %Y %H:%M:%S GMT"
iso8601Time = formatTime (iso8601DateFormat $ Just "%XZ")
awsTime     = formatTime "%Y%m%dT%H%M%SZ"
basicTime   = formatTime "%Y%m%d"

formatTime :: String -> UTCTime -> ByteString
formatTime fmt = BS.pack . Time.formatTime defaultTimeLocale fmt

validPath :: ByteString -> ByteString
validPath = mappend "/" . strip '/'

hmac :: ByteString -> ByteString -> ByteString
hmac key msg = LBS.toStrict
    . SHA.bytestringDigest
    $ SHA.hmacSha256 (LBS.fromStrict key) (LBS.fromStrict msg)

sha256 :: ByteString -> ByteString
sha256 = hex
    . LBS.toStrict
    . SHA.bytestringDigest
    . SHA.sha256
    . LBS.fromStrict

hex :: ByteString -> ByteString
hex = BS.pack . foldr f "" . BS.unpack
  where
    f c t = intToDigit (n `div` 16) : intToDigit (n `mod` 16) : t
      where
        n = ord c
