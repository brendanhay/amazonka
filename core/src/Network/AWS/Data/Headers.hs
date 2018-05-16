{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- |
-- Module      : Network.AWS.Data.Headers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Headers
    ( module Network.AWS.Data.Headers
    , HeaderName
    , Header
    , hContentType
    ) where

import qualified Data.ByteString.Char8       as BS8
import qualified Data.CaseInsensitive        as CI
import           Data.Monoid
import qualified Data.Text.Encoding          as Text
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Text
import           Network.HTTP.Types

infixl 7 .#, .#?

-- FIXME: This whole toText/fromText shit is just stupid.
(.#) :: FromText a => ResponseHeaders -> HeaderName -> Either String a
hs .# k = hs .#? k >>= note
  where
    note Nothing  = Left (BS8.unpack $ "Unable to find header: " <> CI.original k)
    note (Just x) = Right x

(.#?) :: FromText a => ResponseHeaders -> HeaderName -> Either String (Maybe a)
hs .#? k =
    maybe (Right Nothing)
          (fmap Just . fromText . Text.decodeUtf8)
          (k `lookup` hs)

infixr 7 =#

(=#) :: ToHeader a => HeaderName -> a -> [Header]
(=#) = toHeader

hdr :: HeaderName -> ByteString -> [Header] -> [Header]
hdr k v hs = (k, v) : filter ((/= k) . fst) hs

class ToHeaders a where
    toHeaders :: a -> [Header]
    toHeaders = const mempty

class ToHeader a where
    toHeader :: HeaderName -> a -> [Header]

    default toHeader :: ToText a => HeaderName -> a -> [Header]
    toHeader k = toHeader k . toText

instance ToHeader Text where
    toHeader k v = [(k, Text.encodeUtf8 v)]

instance ToHeader ByteString where
    toHeader k v = [(k, v)]

instance ToText a => ToHeader (Maybe a) where
    toHeader k = maybe [] (toHeader k . toText)

hHost :: HeaderName
hHost = "Host"

hExpect :: HeaderName
hExpect = "Expect"

hAMZToken :: HeaderName
hAMZToken = "X-Amz-Security-Token"

hAMZTarget :: HeaderName
hAMZTarget = "X-Amz-Target"

hAMZAlgorithm :: HeaderName
hAMZAlgorithm = "X-Amz-Algorithm"

hAMZCredential :: HeaderName
hAMZCredential = "X-Amz-Credential"

hAMZExpires :: HeaderName
hAMZExpires = "X-Amz-Expires"

hAMZSignedHeaders :: HeaderName
hAMZSignedHeaders = "X-Amz-SignedHeaders"

hAMZContentSHA256 :: HeaderName
hAMZContentSHA256 = "X-Amz-Content-SHA256"

hAMZDate :: HeaderName
hAMZDate = "X-Amz-Date"

hMetaPrefix :: HeaderName
hMetaPrefix = "X-Amz-"

hAMZRequestId :: HeaderName
hAMZRequestId = "X-Amz-Request-Id"

hAMZNRequestId :: HeaderName
hAMZNRequestId = "X-Amzn-RequestId"

hAMZNErrorType :: HeaderName
hAMZNErrorType = "X-Amzn-ErrorType"

hAMZNAuth :: HeaderName
hAMZNAuth = "X-Amzn-Authorization"

hAMZDecodedContentLength :: HeaderName
hAMZDecodedContentLength = "X-Amz-Decoded-Content-Length"

hTransferEncoding :: HeaderName
hTransferEncoding = "Transfer-Encoding"

hFormEncoded :: ByteString
hFormEncoded = "application/x-www-form-urlencoded; charset=utf-8"
