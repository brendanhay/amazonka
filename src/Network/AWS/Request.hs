{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Request
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Digest.Pure.SHA   as SHA
import           Data.List
import           Data.Monoid
import           Data.Time              (UTCTime, formatTime, getCurrentTime)
import qualified Network.HTTP.Types     as HTTP
import           Network.Http.Client
import           System.Locale          (defaultTimeLocale, iso8601DateFormat)

type Endpoint  = ByteString
type Action    = ByteString
type Path      = ByteString
type Version   = ByteString

data Credentials = Credentials
    { accessKey :: ByteString
    , secretKey :: ByteString
    } deriving (Show)

apiVersion :: ByteString
apiVersion = "2012-12-01"

version2 :: Endpoint
         -> Action
         -> Method
         -> Credentials
         -> [(ByteString, ByteString)]
         -> IO Request
version2 end action meth creds params = do
    time <- getCurrentTime
    buildRequest . http meth
        $ "/?" <> query time <> "&Signature=" <> signature time
  where
    signature time = HTTP.urlEncode True
        . Base64.encode
        . LBS.toStrict
        . SHA.bytestringDigest
        . SHA.hmacSha256 (LBS.fromStrict $ secretKey creds)
        . LBS.fromStrict
        $ BS.intercalate "\n" [packMethod meth, end, action, query time]

    query time = queryString $ params `union`
        [ ("Action", action)
        , ("Version", apiVersion)
        , ("SignatureVersion", "2")
        , ("SignatureMethod", "HmacSHA256")
        , ("Timestamp", timeFormat time)
        , ("AWSAccessKeyId", accessKey creds)
        ]

version3 :: Endpoint
         -> Path
         -> Method
         -> Credentials
         -> [(ByteString, ByteString)]
         -> IO Request
version3 end path meth creds params = do
    time <- getCurrentTime
    buildRequest $ do
        http meth $ "/" <> apiVersion <> "/" <> path <> "?" <> query
        setHeader "X-Amzn-Authorization" $ authorization time
  where
    query = queryString $ ("AWSAccessKeyId", accessKey creds) : params

    authorization time = mconcat
        [ "AWS3-HTTPS AWSAccessKeyId="
        , accessKey creds
        , ", Algorithm=HmacSHA256, Signature="
        , signature time
        ]

    signature = LBS.toStrict
        . SHA.bytestringDigest
        . SHA.hmacSha256 (LBS.fromStrict $ secretKey creds)
        . LBS.fromStrict
        . timeFormat

packMethod :: Method -> ByteString
packMethod = BS.pack . show

queryString :: [(ByteString, ByteString)] -> ByteString
queryString = BS.intercalate "&" . map concatEq . sort
  where
    concatEq (k, v) = mconcat [k, "=", HTTP.urlEncode True v]

timeFormat :: UTCTime -> ByteString
timeFormat = BS.pack . formatTime defaultTimeLocale fmt
  where
    fmt = iso8601DateFormat $ Just "%XZ"
