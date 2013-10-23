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
    ( Signer (..)
    , signer
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

data Signer = Signer
    { sgService :: !Service
    , sgMethod  :: !Method
    , sgHost    :: !Hostname
    , sgPath    :: !ByteString
    , sgHeaders :: [AnyHeader]
    , sgQuery   :: [(ByteString, ByteString)]
    , sgBody    :: !Body
    , sgAuth    :: !Auth
    , sgRegion  :: !Region
    , sgTime    :: !UTCTime
    }

signer :: (Signer -> a)
       -> Service
       -> Method
       -> ByteString
       -> [AnyHeader]
       -> [(ByteString, ByteString)]
       -> Body
       -> AWS a
signer f svc@Service{..} meth path hs qry body = do
    auth <- getAuth
    reg  <- svcRegion svc

    let tok  = maybe [] ((:[]) . hdr) $ tokenHeader auth
        host = endpoint svc reg
        hs'  = hdr (hostHeader svc reg) : concat [hs, tok]

    f <$> Signer svc meth host (sWrap "/" path) hs' qry body auth reg
        <$> liftIO getCurrentTime

versionS3 :: Signer -> ByteString -> Request
versionS3 Signer{..} bucket =
    Request sgMethod sgHost sgPath headers sgQuery sgBody
  where
    Service{..} = sgService
    Auth{..}    = sgAuth

    headers = hdr (authHeader authorisation) : signingHeaders

    date = iso8601Time sgTime

    authorisation = BS.concat ["AWS ", accessKeyId, ":", signature]
    signature     = Base64.encode $ hmac secretAccessKey stringToSign

    stringToSign = BS.concat
        [ BS.pack $ show sgMethod
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

    canonicalResource = '/' `BS.cons` sJoin "/" [bucket, sgPath]
    -- [ subresource, if present. For example "?acl", "?location", "?logging", or "?torrent"]

    canonicalHeaders = BS.intercalate "\n"
        . map flattenValues
        $ groupHeaders signingHeaders

    signingHeaders = hdr (dateHeader date) : sgHeaders

version2 :: Signer -> Request
version2 Signer{..} = Request sgMethod sgHost sgPath headers query sgBody
  where
    Service{..} = sgService
    Auth{..}    = sgAuth

    headers = hdr (dateHeader $ iso8601Time sgTime) : sgHeaders
    query   = ("Signature", signature) : qry

    signature = Base64.encode
        . hmac secretAccessKey
        $ BS.intercalate "\n"
            [ BS.pack $ show sgMethod
            , sgHost <> ":443"
            , sgPath
            , encodeQuery (urlEncode True) qry
            ]

    qry = sort $ sgQuery ++
        [ ("Version",          sPack svcVersion)
        , ("SignatureVersion", "2")
        , ("SignatureMethod",  "HmacSHA256")
        , ("Timestamp",        iso8601Time sgTime)
        , ("AWSAccessKeyId",   accessKeyId)
        ]

version3 :: Signer -> Request
version3 Signer{..} = Request sgMethod sgHost sgPath headers sgQuery sgBody
  where
    Service{..} = sgService
    Auth{..}    = sgAuth

    headers = hdr (dateHeader $ rfc822Time sgTime) :
        hdr (authHeader authorisation) :
        sgHeaders

    authorisation = "AWS3-HTTPS AWSAccessKeyId="
        <> accessKeyId
        <> ", Algorithm=HmacSHA256, Signature="
        <> Base64.encode (hmac secretAccessKey $ rfc822Time sgTime)

version4 :: Signer -> Request
version4 Signer{..} = Request sgMethod sgHost sgPath headers sgQuery sgBody
  where
    Service{..} = sgService
    Auth{..}    = sgAuth

    headers = hdr (authHeader authorisation) : date : sgHeaders
    date    = hdr (dateHeader $ iso8601Time sgTime)

    method = BS.pack $ show sgMethod
    region = BS.pack $ show sgRegion

    authorisation = mconcat
        [ algorithm
        , " Credential="
        , accessKeyId
        , "/"
        , credentialScope
        , ", SignedHeaders="
        , signedHeaders
        , ", Signature="
        , signature
        ]

    signature = hex $ hmac signingKey stringToSign

    signingKey = foldl1 hmac $ ("AWS4" <> secretAccessKey) : scope

    stringToSign = BS.intercalate "\n"
        [ algorithm
        , awsTime sgTime
        , credentialScope
        , strictSHA256 canonicalRequest
        ]

    credentialScope = BS.intercalate "/" scope

    scope = [basicTime sgTime, region, svcName, "aws4_request"]

    algorithm = "AWS4-HMAC-SHA256"

    canonicalRequest = BS.intercalate "\n"
        [ method
        , sgPath
        , query
        , canonicalHeaders
        , signedHeaders
        , bodySHA256
        ]

    bodySHA256 = case sgBody of
        (Strict   bs) -> strictSHA256 bs
        (Streaming _) -> "" -- FIXME
        Empty         -> ""

    query = encodeQuery (urlEncode True) $ sort sgQuery

    signedHeaders    = BS.intercalate ";" . nub $ map fst groupedHeaders
    canonicalHeaders = mconcat $ map flattenValues groupedHeaders
    groupedHeaders   = groupHeaders $ date : sgHeaders

hmac :: ByteString -> ByteString -> ByteString
hmac key msg = LBS.toStrict
    . SHA.bytestringDigest
    $ SHA.hmacSha256 (LBS.fromStrict key) (LBS.fromStrict msg)

strictSHA256 :: ByteString -> ByteString
strictSHA256 = LBS.toStrict . lazySHA256 . LBS.fromStrict

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

hostHeader :: Service -> Region -> Header "host" ByteString
hostHeader svc reg = Header $ endpoint svc reg <> ":443"

dateHeader :: ByteString -> Header "date" ByteString
dateHeader = Header

authHeader :: ByteString -> Header "authorization" ByteString
authHeader = Header

groupHeaders :: [AnyHeader] -> [(ByteString, ByteString)]
groupHeaders = sort . map f . groupBy ((==) `on` fst) . flattenHeaders
  where
    f (h:hs) = (fst h, BS.intercalate "," . sort . map snd $ h : hs)
    f []     = ("", "")

flattenValues :: (Monoid a, IsString a, Strings a) => (a, a) -> a
flattenValues (k, v) = mconcat [k, ":", sStripChar ' ' v]
