{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

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
    , version2
    , version3
    , version4
    , versionS3
    ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Crypto.Hash.SHA1                as SHA1
import qualified Crypto.Hash.SHA256              as SHA256
import qualified Crypto.MAC.HMAC                 as HMAC
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Base16          as Base16
import qualified Data.ByteString.Base64          as Base64
import qualified Data.ByteString.Char8           as BS
import           Data.CaseInsensitive            (CI)
import qualified Data.CaseInsensitive            as Case
import           Data.Function                   (on)
import           Data.List                       (groupBy, nub, sort)
import           Data.Maybe
import           Data.Monoid
import           Data.Time                       (UTCTime, getCurrentTime)
import           Network.AWS.Headers
import           Network.AWS.Internal.Monadic
import           Network.AWS.Internal.String
import           Network.AWS.Internal.Time
import           Network.AWS.Internal.Types
import           Network.HTTP.QueryString.Pickle
import           Network.HTTP.Types              (urlEncode)
import           Network.Http.Client             (Method, Hostname)
import qualified Network.Http.Client             as Client
import qualified Network.Http.Internal           as Client

type Signer = Request -> Auth -> Region -> UTCTime -> Bool -> IO Signed

data Common = Common
    { service     :: !ByteString
    , version     :: !ByteString
    , host        :: !ByteString
    , fullPath    :: !ByteString
    , sortedQuery :: [(ByteString, ByteString)]
    }

sign :: Signer -> Request -> AWS Signed
sign f rq@Request{..} = do
    dbg  <- getDebug
    auth <- getAuth
    reg  <- region rqService
    time <- liftIO getCurrentTime

    let tok  = maybe [] ((:[]) . hdr) $ amzTokenHeader auth
        host = hdr . hostHeader $ endpoint rqService reg
        hs   = host : hdr acceptHeader : concat [rqHeaders, tok] --, hdr transferHeader]]

    liftIO $! f (rq { rqHeaders = hs }) auth reg time dbg

version2 :: Signer
version2 rq@Request{..} Auth{..} reg time dbg = signed rq host path headers
  where
    Common{..} = common rq reg

    path = joinPath rqPath $ query <> "&Signature=" <> urlEncode True signature

    headers = hdr (dateHeader $ formatISO8601 time) : rqHeaders

    signature = Base64.encode
        . hmacSHA256 secretAccessKey
        $ BS.intercalate "\n"
            [ BS.pack $ show rqMethod
            , host <> ":443"
            , rqPath
            , query
            ]

    query = encodeQuery (urlEncode True) $ sortedQuery ++
        [ ("Version",          svcVersion rqService)
        , ("SignatureVersion", "2")
        , ("SignatureMethod",  "HmacSHA256")
        , ("Timestamp",        formatISO8601 time)
        , ("AWSAccessKeyId",   accessKeyId)
        ]

version3 :: Signer
version3 rq@Request{..} Auth{..} reg time dbg = signed rq host fullPath headers
  where
    Common{..} = common rq reg

    headers = hdr (dateHeader $ formatRFC822 time) :
        hdr (amzAuthHeader authorisation) :
        rqHeaders

    authorisation = "AWS3-HTTPS AWSAccessKeyId="
        <> accessKeyId
        <> ", Algorithm=HmacSHA256, Signature="
        <> Base64.encode (hmacSHA256 secretAccessKey $ formatRFC822 time)

version4 :: Signer
version4 rq@Request{..} Auth{..} reg time dbg = do
    Signed{..} <- signed rq host fullPath (date : rqHeaders)

    let hs   = map hdr . Client.retrieveHeaders $ Client.getHeaders sRequest
        hs'  = hdr (hostHeader host) : hs
        auth = hdr . authHeader $ authorisation hs'

    signed rq host fullPath (auth : hs')
  where
    Common{..} = common rq reg

    date = hdr . amzDateHeader $ formatISO8601 time

    authorisation hs = mconcat
        [ algorithm
        , " Credential="
        , accessKeyId
        , "/"
        , credentialScope
        , ", SignedHeaders="
        , signedHeaders hs
        , ", Signature="
        , signature hs
        ]

    signature  = Base16.encode . hmacSHA256 signingKey . stringToSign
    signingKey = foldl1 hmacSHA256 $ ("AWS4" <> secretAccessKey) : scope

    stringToSign hs = BS.intercalate "\n"
        [ algorithm
        , formatAWS time
        , credentialScope
        , Base16.encode . SHA256.hash $ canonicalRequest hs
        ]

    credentialScope = BS.intercalate "/" scope

    algorithm = "AWS4-HMAC-SHA256"
    scope     = [formatBasic time, BS.pack $ show reg, service, "aws4_request"]

    canonicalRequest hs = BS.intercalate "\n"
        [ BS.pack $ show rqMethod
        , rqPath
        , encodeQuery (urlEncode True) sortedQuery
        , canonicalHeaders hs
        , signedHeaders hs
        , bodySHA256
        ]

    canonicalHeaders = mconcat . map flattenValues . groupHeaders

    signedHeaders = BS.intercalate ";"
        . nub
        . map (Case.foldedCase . fst)
        . groupHeaders

    bodySHA256 = Base16.encode . SHA256.hash $ case rqBody of
        (Strict   bs) -> bs
        (Streaming _) -> "" -- FIXME
        Empty         -> ""

versionS3 :: ByteString -> Signer
versionS3 bucket rq@Request{..} Auth{..} reg time dbg =
    signed rq host fullPath (authorisation : headers)
  where
    Common{..} = common rq reg

    authorisation = hdr . authHeader $
        BS.concat ["AWS ", accessKeyId, ":", signature]

    signature = Base64.encode $ hmacSHA1 secretAccessKey stringToSign

    stringToSign = BS.concat
        [ BS.pack $ show rqMethod
        , "\n"
        , optionalHeader "content-md5"
        , "\n"
        , optionalHeader "content-type"
        , "\n"
        , date
        , "\n"
        , canonicalHeaders
        , canonicalResource
        ]

    optionalHeader = fromMaybe "" . (`lookupHeader` headers)

    canonicalHeaders = BS.intercalate "\n"
        . map flattenValues
        . filter (BS.isPrefixOf "x-amz-" . Case.foldedCase . fst)
        $ groupHeaders headers

    headers = hdr (dateHeader date) : rqHeaders

    date = formatRFC822 time

    canonicalResource = '/' `wrap` bucket <> "/" `stripPrefix` rqPath

    -- relevantQueryKeys =
    --     [ "acl"
    --     , "cors"
    --     , "defaultObjectAcl"
    --     , "location"
    --     , "logging"
    --     , "partNumber"
    --     , "policy"
    --     , "requestPayment"
    --     , "torrent"
    --     , "versioning"
    --     , "versionId"
    --     , "versions"
    --     , "website"
    --     , "uploads"
    --     , "uploadId"
    --     , "response-content-type"
    --     , "response-content-language"
    --     , "response-expires"
    --     , "response-cache-control"
    --     , "response-content-disposition"
    --     , "response-content-encoding"
    --     , "delete"
    --     , "lifecycle"
    --     , "tagging"
    --     , "restore"
    --     , "storageClass"
    --     , "notification"
    --     ]

common :: Request -> Region -> Common
common Request{..} reg = Common
    { service     = svcName rqService
    , version     = svcVersion rqService
    , host        = endpoint rqService reg
    , fullPath    = path
    , sortedQuery = query
    }
  where
    path | null query = rqPath
         | otherwise  = mappend rqPath
             . addPrefix "?"
             $ encodeQuery (urlEncode True) query

    query = sort rqQuery

signed :: Request -> Hostname -> ByteString -> [AnyHeader] -> IO Signed
signed rq@Request{..} host path hs =
    Signed rq ("https://" <> host) rqBody <$> builder
  where
    builder = Client.buildRequest $ do
        Client.http rqMethod $ "/" `addPrefix` path
        Client.setHostname host 443
        maybe (return ()) Client.setContentLength contentLength
        mapM_ header $ filter exclude headers

    header (k, v) = Client.setHeader (Case.original k) v

    exclude (Case.foldedCase -> k, _) = "host" /= k && "content-length" /= k

    contentLength = read . BS.unpack . snd . (`encodeHeader` "") <$>
        lookupHeader "content-length" hs

    headers = flattenHeaders hs

hmacSHA1 :: ByteString -> ByteString -> ByteString
hmacSHA1 key msg = HMAC.hmac SHA1.hash 64 key msg

hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 key msg = HMAC.hmac SHA256.hash 64 key msg

hostHeader :: Hostname -> Header "Host" Hostname
hostHeader host = Header $ host <> ":443"

dateHeader :: ByteString -> Header "Date" ByteString
dateHeader = Header

amzDateHeader :: ByteString -> Header "X-Amz-Date" ByteString
amzDateHeader = Header

authHeader :: ByteString -> Header "Authorization" ByteString
authHeader = Header

amzAuthHeader :: ByteString -> Header "X-Amzn-Authorization" ByteString
amzAuthHeader = Header

amzTokenHeader :: Auth -> Maybe (Header "X-Amz-Security-Token" ByteString)
amzTokenHeader = fmap (\t -> Header t) . securityToken

acceptHeader :: Header "Accept-Encoding" ByteString
acceptHeader = Header "gzip"

groupHeaders :: [AnyHeader] -> [(CI ByteString, ByteString)]
groupHeaders = sort . map f . groupBy ((==) `on` fst) . flattenHeaders
  where
    f (h:hs) = (fst h, BS.intercalate "," . sort . map snd $ h : hs)
    f []     = ("", "")

flattenHeaders :: [AnyHeader] -> [(CI ByteString, ByteString)]
flattenHeaders = map (`encodeHeader` "")

lookupHeader :: ByteString -> [AnyHeader] -> Maybe ByteString
lookupHeader (Case.mk -> key) = lookup key . flattenHeaders

flattenValues :: IsByteString a => (CI ByteString, a) -> ByteString
flattenValues (k, v) = mconcat [Case.foldedCase k, ":", strip ' ' v, "\n"]

joinPath :: ByteString -> ByteString -> ByteString
joinPath path qry
    | BS.null qry = "/" `addPrefix` path
    | otherwise   = '/' `wrap` path <> "?" <> qry
