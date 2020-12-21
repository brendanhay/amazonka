{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Network.AWS.Data.Headers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.Headers
  ( module Network.AWS.Data.Headers,
    HeaderName,
    Header,
    hContentType,
  )
where

import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Encoding as Text
import Network.AWS.Data.ByteString
import Network.AWS.Data.Text
import Network.HTTP.Types

infixl 7 .#, .#?

-- FIXME: This whole toText/fromText shit is just stupid.
parseResponseHeader

(.#) :: FromText a => ResponseHeaders -> HeaderName -> Either String a
hs .# k = hs .#? k >>= note
  where
    note = \case
      Nothing -> Left (BS8.unpack $ "Unable to find header: " <> CI.original k)
      Just x -> Right x

parseResponseHeaderMaybe

(.#?) :: FromText a => ResponseHeaders -> HeaderName -> Either String (Maybe a)
hs .#? k =
  maybe
    (Right Nothing)
    (fmap Just . fromText . Text.decodeUtf8)
    (lookup k hs)

parseResponseHeader

insertRequestHeader :: HeaderName -> ByteString -> [Header] -> [Header]
insertRequestHeader k v hs = (k, v) : filter ((/= k) . fst) hs

-- class ToRequestHeaders a where
--   toRequestHeaders :: HeaderName -> a -> [Header]

-- instance ToRequestHeader Text where
--   toRequestHeaders k v = [(k, Text.encodeUtf8 v)]

-- instance ToHeader ByteString where
--   toRequestHeaders k v = [(k, v)]

-- instance ToByteString a => ToHeader (Maybe a) where
--   toRequestHeaders k = maybe [] (toHeaders k . toBS)

-- instance (ToByteString k, ToByteString v) => ToHeader (HashMap k v) where
--   toRequestHeaders prefix =
--     map (bimap (mappend prefix . CI.mk . toBS) toBS)
--       . HashMap.toList

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
