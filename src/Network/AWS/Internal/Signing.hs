{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
    ( signer
    , versionS3
    , version2
    , version3
    , version4
    ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Base64          as Base64
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy            as LBS
import           Data.Char                       (intToDigit, ord)
import qualified Data.Digest.Pure.SHA            as SHA
import           Data.Function                   (on)
import           Data.List
import           Data.Monoid
import           Data.String
import           Data.Strings
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

type Signer = Request -> Service -> Auth -> Region -> UTCTime -> IO Signed

signer :: Signer -> Request -> AWS Signed
signer f r@Request{..} = do
    let svc@Service{..} = rqService

    auth <- getAuth
    reg  <- svcRegion svc
    time <- liftIO getCurrentTime

    let tok  = maybe [] ((:[]) . hdr) $ tokenHeader auth
        host = hdr $ hostHeader rqHost
        hs   = host : concat [rqHeaders, tok, [hdr acceptHeader, hdr transferHeader]]
        rq   = r { rqHeaders = hs }

    liftIO $! f rq svc auth reg time

versionS3 :: ByteString -> Signer
versionS3 bucket Request{..} _ Auth{..} _ time =
    sign rqMethod rqHost path headers rqBody
  where
    Service{..} = rqService

    path = joinPath rqPath $ encodeQuery (urlEncode True) rqQuery

    headers = hdr (authHeader authorisation) : signingHeaders

    date = iso8601Time time

    authorisation = BS.concat ["AWS ", accessKeyId, ":", signature]
    signature     = Base64.encode $ hmac secretAccessKey stringToSign

    stringToSign = BS.concat
        [ BS.pack $ show rqMethod
        , "\n"
        , optionalHeader "content-md5"
        , optionalHeader "content-type"
        , date
        , "\n"
        , canonicalHeaders
        , "\n"
        , canonicalResource
        ]

    optionalHeader = maybe "" (<> "\n") . (`lookupHeader` signingHeaders)

    canonicalResource = "/" `sEnsurePrefix` BS.concat [bucket, "/", rqPath]
    -- [ subresource, if present. For example "?acl", "?location", "?logging", or "?torrent"]

    canonicalHeaders = BS.intercalate "\n"
        . map flattenValues
        $ groupHeaders signingHeaders

    signingHeaders = hdr (dateHeader date) : rqHeaders

version2 :: Signer
version2 Request{..} _ Auth{..} _ time =
    sign rqMethod rqHost path headers rqBody
  where
    Service{..} = rqService

    path = joinPath rqPath $ encodeQuery (urlEncode True) query

    headers = hdr (dateHeader $ iso8601Time time) : rqHeaders
    query   = ("Signature", signature) : qry

    signature = Base64.encode
        . hmac secretAccessKey
        $ BS.intercalate "\n"
            [ BS.pack $ show rqMethod
            , rqHost
            , rqPath
            , encodeQuery (urlEncode True) qry
            ]

    qry = sort $ rqQuery ++
        [ ("Version",          sPack svcVersion)
        , ("SignatureVersion", "2")
        , ("SignatureMethod",  "HmacSHA256")
        , ("Timestamp",        iso8601Time time)
        , ("AWSAccessKeyId",   accessKeyId)
        ]

version3 :: Signer
version3 Request{..} _ Auth{..} _ time =
    sign rqMethod rqHost path headers rqBody
  where
    Service{..} = rqService

    path = joinPath rqPath $ encodeQuery (urlEncode True) rqQuery

    headers = hdr (dateHeader $ rfc822Time time) :
        hdr (authHeader authorisation) :
        rqHeaders

    authorisation = "AWS3-HTTPS AWSAccessKeyId="
        <> accessKeyId
        <> ", Algorithm=HmacSHA256, Signature="
        <> Base64.encode (hmac secretAccessKey $ rfc822Time time)

version4 :: Signer
version4 Request{..} _ Auth{..} reg time = do
    Signed{..} <- sign rqMethod rqHost path (date : rqHeaders) rqBody

    let hs   = map hdr . Client.retrieveHeaders $ Client.getHeaders sRequest
        auth = hdr . authHeader $ authorisation hs

    sign rqMethod rqHost path (auth : hs) rqBody
  where
    Service{..} = rqService

    path = joinPath rqPath query
    date = hdr (dateHeader $ iso8601Time time)

    method = BS.pack $ show rqMethod
    region = BS.pack $ show reg

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

    signature = hex . hmac signingKey . stringToSign

    signingKey = foldl1 hmac $ ("AWS4" <> secretAccessKey) : scope

    stringToSign hs = BS.intercalate "\n"
        [ algorithm
        , awsTime time
        , credentialScope
        , strictSHA256 $ canonicalRequest hs
        ]

    credentialScope = BS.intercalate "/" scope

    scope = [basicTime time, region, svcName, "aws4_request"]

    algorithm = "AWS4-HMAC-SHA256"

    canonicalRequest hs = BS.intercalate "\n"
        [ method
        , rqPath
        , query
        , canonicalHeaders hs
        , signedHeaders hs
        , bodySHA256
        ]

    bodySHA256 = case rqBody of
        (Strict   bs) -> strictSHA256 bs
        (Streaming _) -> "" -- FIXME
        Empty         -> ""

    query = encodeQuery (urlEncode True) $ sort rqQuery

    signedHeaders = BS.intercalate ";" . nub . map fst . groupHeaders

    canonicalHeaders = mconcat . map flattenValues . groupHeaders

sign :: Method -> Hostname -> ByteString -> [AnyHeader] -> Body -> IO Signed
sign meth host path hs body = Signed ("https://" <> host) body <$> builder
  where
    builder = Client.buildRequest $ do
        Client.http meth $ "/" `sEnsurePrefix` path
        Client.setHostname host 443
        mapM_ (uncurry Client.setHeader)
            . filter ((/= "host") . fst)
            $ flattenHeaders hs

hmac :: ByteString -> ByteString -> ByteString
hmac key msg = LBS.toStrict
    . SHA.bytestringDigest
    $ SHA.hmacSha256 (LBS.fromStrict key) (LBS.fromStrict msg)

strictSHA256 :: ByteString -> ByteString
strictSHA256 = hex . LBS.toStrict . lazySHA256 . LBS.fromStrict

lazySHA256 :: LBS.ByteString -> LBS.ByteString
lazySHA256 = SHA.bytestringDigest . SHA.sha256 -- Need to hex encode the result

hex :: ByteString -> ByteString
hex = BS.pack . foldr f "" . BS.unpack
  where
    f c t = intToDigit (n `div` 16) : intToDigit (n `mod` 16) : t
      where
        n = ord c

tokenHeader :: Auth -> Maybe (Header "x-amz-security-token" ByteString)
tokenHeader = fmap (\t -> Header t) . securityToken

hostHeader :: Hostname -> Header "host" Hostname
hostHeader host = Header $ host <> ":443"

dateHeader :: ByteString -> Header "date" ByteString
dateHeader = Header

authHeader :: ByteString -> Header "authorization" ByteString
authHeader = Header

transferHeader :: Header "transfer-encoding" ByteString
transferHeader = Header "chunked"

acceptHeader :: Header "accept-encoding" ByteString
acceptHeader = Header "gzip"

groupHeaders :: [AnyHeader] -> [(ByteString, ByteString)]
groupHeaders = sort . map f . groupBy ((==) `on` fst) . flattenHeaders
  where
    f (h:hs) = (fst h, BS.intercalate "," . sort . map snd $ h : hs)
    f []     = ("", "")

flattenValues :: (Monoid a, IsString a, Strings a) => (a, a) -> a
flattenValues (k, v) = mconcat [k, ":", sStripChar ' ' v]

joinPath :: ByteString -> ByteString -> ByteString
joinPath path qry
    | BS.null qry = "/" `sEnsurePrefix` path
    | otherwise   = "/" `sWrap` path <> "?" <> qry
