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
    , presignS3
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
import           Data.Default
import           Data.Function                   (on)
import           Data.List                       (groupBy, nub, sort, sortBy, find)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Time                       (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX
import           Network.AWS.Headers
import           Network.AWS.Internal.String
import           Network.AWS.Internal.Time
import           Network.AWS.Internal.Types
import           Network.HTTP.Conduit
import           Network.HTTP.Types              (Header, StdMethod, urlEncode)

data Common = Common
    { _service :: !ByteString
    , _version :: !ByteString
    , _host    :: !ByteString
    , _query   :: [(ByteString, Maybe ByteString)]
    }

sign :: Raw -> AWS Request
sign raw@Raw{..} = do
    auth <- getAuth
    reg  <- region rqService
    time <- liftIO getCurrentTime

    let sig = svcSigner rqService
        hs  = hHost (endpoint rqService reg) : rqHeaders

    return $! sig (raw { rqHeaders = hs }) auth reg time

version2 :: Signer
version2 raw@Raw{..} auth reg time =
    signed rqMethod _host rqPath query headers rqBody
  where
    Common{..} = common raw reg

    query = encoded <> "&Signature=" <> urlEncode True signature

    signature = Base64.encode
        . hmacSHA256 (secretAccessKey auth)
        $ BS.intercalate "\n"
            [ BS.pack $ show rqMethod
            , _host
            , rqPath
            , encoded
            ]

    encoded = renderQuery $ _query
       ++ [ ("Version",          Just _version)
          , ("SignatureVersion", Just "2")
          , ("SignatureMethod",  Just "HmacSHA256")
          , ("Timestamp",        Just $ formatISO8601 time)
          , ("AWSAccessKeyId",   Just $ accessKeyId auth)
          ]
       ++ maybeToList ((\x -> ("SecurityToken", Just x)) <$> securityToken auth)

    headers = hDate (formatISO8601 time) : rqHeaders

version3 :: Signer
version3 raw@Raw{..} auth reg time =
    signed rqMethod _host rqPath query headers rqBody
  where
    Common{..} = common raw reg

    query   = renderQuery _query
    headers = hDate (formatRFC822 time)
        : hAMZAuth authorisation
        : maybeToList (hAMZToken <$> securityToken auth)
       ++ rqHeaders

    authorisation = "AWS3-HTTPS AWSAccessKeyId="
        <> accessKeyId auth
        <> ", Algorithm=HmacSHA256, Signature="
        <> Base64.encode (hmacSHA256 (secretAccessKey auth) $ formatRFC822 time)

version4 :: Signer
version4 raw@Raw{..} auth reg time =
    signed rqMethod _host rqPath query (hAuth authorisation : headers) rqBody
  where
    Common{..} = common raw reg

    query   = renderQuery . sort $ ("Version", Just _version) : _query
    headers = hAMZDate time
            : maybeToList (hAMZToken <$> securityToken auth)
           ++ rqHeaders

    authorisation = mconcat
        [ algorithm
        , " Credential="
        , accessKeyId auth
        , "/"
        , credentialScope
        , ", SignedHeaders="
        , signedHeaders
        , ", Signature="
        , signature
        ]

    signature  = Base16.encode $ hmacSHA256 signingKey stringToSign
    signingKey = foldl1 hmacSHA256 $ ("AWS4" <> secretAccessKey auth) : scope

    stringToSign = BS.intercalate "\n"
        [ algorithm
        , formatAWS time
        , credentialScope
        , Base16.encode $ SHA256.hash canonicalRequest
        ]

    credentialScope = BS.intercalate "/" scope

    algorithm = "AWS4-HMAC-SHA256"
    scope     = [formatBasic time, BS.pack $ show reg, _service, "aws4_request"]

    canonicalRequest = BS.intercalate "\n"
        [ BS.pack $ show rqMethod
        , rqPath
        , query
        , canonicalHeaders
        , signedHeaders
        , bodySHA256
        ]

    canonicalHeaders = mconcat $ map flattenValues grouped

    signedHeaders = BS.intercalate ";" . nub $
        map (Case.foldedCase . fst) grouped

    grouped = groupHeaders headers

    bodySHA256 = Base16.encode $ SHA256.hash ""
     -- sinkHash :: (Monad m, Hash ctx d) => Consumer ByteString m SHA256

versionS3 :: ByteString -> Signer
versionS3 bucket raw@Raw{..} auth reg time =
    signed rqMethod _host rqPath query (authorisation : headers) rqBody
  where
    Common{..} = common raw reg

    query = renderQuery _query

    authorisation = hAuth $ BS.concat ["AWS ", accessKeyId auth, ":", signature]

    signature = Base64.encode $ hmacSHA1 (secretAccessKey auth) stringToSign

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

    canonicalHeaders = BS.concat
        . map flattenValues
        . filter (BS.isPrefixOf "x-amz-" . Case.foldedCase . fst)
        $ groupHeaders headers

    headers = hDate date
        : maybeToList (hAMZToken <$> securityToken auth)
       ++ rqHeaders

    date = formatRFC822 time

    canonicalResource = '/' `wrap` bucket <> "/" `stripPrefix` rqPath
       <> subResource

    subResource = maybe "" ("?" <>) $ find (`elem` keys) (map fst _query)

    keys =
        [ "acl"
        , "cors"
        , "defaultObjectAcl"
        , "location"
        , "logging"
        , "partNumber"
        , "policy"
        , "requestPayment"
        , "torrent"
        , "versioning"
        , "versionId"
        , "versions"
        , "website"
        , "uploads"
        , "uploadId"
        , "response-content-type"
        , "response-content-language"
        , "response-expires"
        , "response-cache-control"
        , "response-content-disposition"
        , "response-content-encoding"
        , "delete"
        , "lifecycle"
        , "tagging"
        , "restore"
        , "storageClass"
        , "notification"
        ]

presignS3 :: StdMethod
          -> ByteString
          -> ByteString
          -> UTCTime
          -> AWS ByteString
presignS3 meth (strip '/' -> bucket) (strip '/' -> key) expires = do
    auth <- getAuth

    let access = accessKeyId auth
        secret = secretAccessKey auth

    return $! mconcat
        [ "https://"
        , bucket
        , ".s3.amazonaws.com/"
        , key
        , "?AWSAccessKeyId=" <> access
        , "&Expires="       <> expiry
        , "&Signature="     <> signature secret
        ]
  where
    signature = urlEncode True
        . Base64.encode
        . (`hmacSHA1` stringToSign)

    stringToSign = BS.intercalate "\n"
        [ BS.pack $ show meth
        , ""
        , ""
        , expiry
        , "/" <> bucket <> "/" <> key
        ]

    expiry = BS.pack $ show (truncate $ utcTimeToPOSIXSeconds expires :: Integer)

common :: Raw -> Region -> Common
common Raw{..} reg = Common
    { _service = svcName rqService
    , _version = svcVersion rqService
    , _host    = endpoint rqService reg
    , _query   = sort rqQuery
    }

signed :: StdMethod
       -> ByteString
       -> ByteString
       -> ByteString
       -> [Header]
       -> RequestBody
       -> Request
signed meth host path qs hs body = def
    { secure         = True
    , method         = BS.pack $ show meth
    , host           = host
    , port           = 443
    , path           = path
    , queryString    = qs
    , requestHeaders = hs
    , requestBody    = body
    , checkStatus    = \_ _ _ -> Nothing
    }

hmacSHA1 :: ByteString -> ByteString -> ByteString
hmacSHA1 key msg = HMAC.hmac SHA1.hash 64 key msg

hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 key msg = HMAC.hmac SHA256.hash 64 key msg

groupHeaders :: [Header] -> [Header]
groupHeaders = sort . map f . groupBy ((==) `on` fst)
  where
    f (h:hs) = (fst h, BS.intercalate "," . sort . map snd $ h : hs)
    f []     = ("", "")

lookupHeader :: ByteString -> [Header] -> Maybe ByteString
lookupHeader (Case.mk -> key) = lookup key

flattenValues :: IsByteString a => (CI ByteString, a) -> ByteString
flattenValues (k, v) = mconcat [Case.foldedCase k, ":", strip ' ' v, "\n"]

-- | Ensures the querystring is sorted - very important!
renderQuery :: [(ByteString, Maybe ByteString)] -> ByteString
renderQuery = BS.intercalate "&" . map f . sortBy (comparing fst)
  where
    f (k, Just v) = mconcat [k, "=", urlEncode True v]
    f (k, _)      = k
