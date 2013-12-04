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
import           Data.Default
import           Data.Function                   (on)
import           Data.List                       (groupBy, nub, sort)
import           Data.Maybe
import           Data.Monoid
import           Data.Time                       (getCurrentTime)
import           Network.AWS.Headers
import           Network.AWS.Internal.Monadic
import           Network.AWS.Internal.String
import           Network.AWS.Internal.Time
import           Network.AWS.Internal.Types
import           Network.HTTP.Conduit
import           Network.HTTP.QueryString.Pickle
import           Network.HTTP.Types              (Header, StdMethod, urlEncode)

data Common = Common
    { _service :: !ByteString
    , _version :: !ByteString
    , _host    :: !ByteString
    , _query   :: [(ByteString, ByteString)]
    }

sign :: Signer -> Raw -> AWS Request
sign s raw@Raw{..} = do
    auth <- getAuth
    reg  <- region rqService
    time <- liftIO getCurrentTime

    let tok = maybeToList $ hAMZToken <$> securityToken auth
        hs  = hHost (endpoint rqService reg) : concat [rqHeaders, tok]

    return $! s (raw { rqHeaders = hs }) auth reg time

version2 :: Signer
version2 raw@Raw{..} Auth{..} reg time =
    signed rqMethod _host path query headers rqBody
  where
    Common{..} = common raw reg

    path = joinPath rqPath $ query <> "&Signature=" <> urlEncode True signature

    signature = Base64.encode
        . hmacSHA256 secretAccessKey
        $ BS.intercalate "\n"
            [ BS.pack $ show rqMethod
            , _host <> ":443"
            , rqPath
            , query
            ]

    query = encodeQuery (urlEncode True) $ _query ++
        [ ("Version",          _version)
        , ("SignatureVersion", "2")
        , ("SignatureMethod",  "HmacSHA256")
        , ("Timestamp",        formatISO8601 time)
        , ("AWSAccessKeyId",   accessKeyId)
        ]

    headers = hDate (formatISO8601 time) : rqHeaders

version3 :: Signer
version3 raw@Raw{..} Auth{..} reg time =
    signed rqMethod _host rqPath query headers rqBody
  where
    Common{..} = common raw reg

    query   = encodeQuery (urlEncode True) _query
    headers = hDate (formatRFC822 time) : hAMZAuth authorisation : rqHeaders

    authorisation = "AWS3-HTTPS AWSAccessKeyId="
        <> accessKeyId
        <> ", Algorithm=HmacSHA256, Signature="
        <> Base64.encode (hmacSHA256 secretAccessKey $ formatRFC822 time)

version4 :: Signer
version4 raw@Raw{..} Auth{..} reg time =
    signed rqMethod _host rqPath query headers rqBody
  where
    Common{..} = common raw reg

    query   = encodeQuery (urlEncode True) _query
    headers = hAMZDate time : hAuth authorisation : rqHeaders

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

    signature  = Base16.encode $ hmacSHA256 signingKey stringToSign
    signingKey = foldl1 hmacSHA256 $ ("AWS4" <> secretAccessKey) : scope

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
        , encodeQuery (urlEncode True) _query
        , canonicalHeaders
        , signedHeaders
        , bodySHA256
        ]

    canonicalHeaders = mconcat $ map flattenValues grouped

    signedHeaders = BS.intercalate ";" . nub $
        map (Case.foldedCase . fst) grouped

    grouped = groupHeaders rqHeaders

    bodySHA256 = Base16.encode "" -- . SHA256.hash $ case rqBody of
        -- (Strict   bs) -> bs
        -- (Streaming _) -> "" -- FIXME
        -- Empty         -> ""

    -- main = do
    --     hash <- runResourceT $ sourceFile "my-file" $$ sinkHash
    --     print (hash :: SHA1)

versionS3 :: ByteString -> Signer
versionS3 bucket raw@Raw{..} Auth{..} reg time =
    signed rqMethod _host rqPath query (authorisation : headers) rqBody
  where
    Common{..} = common raw reg

    query = encodeQuery (urlEncode True) _query

    authorisation = hAuth $ BS.concat ["AWS ", accessKeyId, ":", signature]

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

    headers = hDate date : rqHeaders

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
--    , checkStatus = FIXME: 
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

joinPath :: ByteString -> ByteString -> ByteString
joinPath path qry
    | BS.null qry = "/" `addPrefix` path
    | otherwise   = '/' `wrap` path <> "?" <> qry
