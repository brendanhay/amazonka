-- |
-- Module      : Amazonka.Data.Headers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Data.Headers
  ( module Amazonka.Data.Headers,
    HeaderName,
    Header,
    HTTP.hContentType,
  )
where

import Amazonka.Data.ByteString
import Amazonka.Data.Text
import Amazonka.Prelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Encoding as Text
import Network.HTTP.Types (Header, HeaderName, ResponseHeaders)
import qualified Network.HTTP.Types as HTTP

infixl 7 .#, .#?

-- FIXME: This whole toText/fromText shit is just stupid.
(.#) :: FromText a => ResponseHeaders -> HeaderName -> Either String a
hs .# k = hs .#? k >>= note
  where
    note Nothing = Left (BS8.unpack $ "Unable to find header: " <> CI.original k)
    note (Just x) = Right x

(.#?) :: FromText a => ResponseHeaders -> HeaderName -> Either String (Maybe a)
hs .#? k =
  maybe
    (Right Nothing)
    (fmap Just . fromText . Text.decodeUtf8)
    (k `lookup` hs)

infixr 7 =#

(=#) :: ToHeader a => HeaderName -> a -> [Header]
(=#) = toHeader

hdr :: HeaderName -> ByteString -> [Header] -> [Header]
hdr k v hs = (k, v) : filter ((/= k) . fst) hs

class ToHeaders a where
  toHeaders :: a -> [Header]

instance (ToByteString k, ToByteString v) => ToHeaders (HashMap k v) where
  toHeaders = map (bimap (CI.mk . toBS) toBS) . HashMap.toList

class ToHeader a where
  toHeader :: HeaderName -> a -> [Header]
  default toHeader :: ToText a => HeaderName -> a -> [Header]
  toHeader k = toHeader k . toText

instance ToHeader Int

instance ToHeader Integer

instance ToHeader Natural

instance ToHeader Text where
  toHeader k v = [(k, Text.encodeUtf8 v)]

instance ToHeader ByteString where
  toHeader k v = [(k, v)]

instance ToText a => ToHeader (Maybe a) where
  toHeader k = maybe [] (toHeader k . toText)

instance (ToByteString k, ToByteString v) => ToHeader (HashMap k v) where
  toHeader p = map (bimap k v) . HashMap.toList
    where
      k = mappend p . CI.mk . toBS
      v = toBS

parseHeadersMap ::
  FromText a =>
  ByteString ->
  ResponseHeaders ->
  Either String (HashMap Text a)
parseHeadersMap p = fmap HashMap.fromList . traverse g . filter f
  where
    f = BS.isPrefixOf p . CI.foldedCase . fst

    g (k, v) =
      (Text.decodeUtf8 . BS.drop n $ CI.original k,)
        <$> fromText (Text.decodeUtf8 v)

    n = BS.length p

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
