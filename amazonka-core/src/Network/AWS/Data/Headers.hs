-- |
-- Module      : Network.AWS.Data.Headers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.Headers
  ( Headers,
    HeaderBuilder,
    buildHeaders,

    -- * Serialisation
    ToHeaders (..),

    -- * Deserialisation
    FromHeaders (..),
    parseHeadersMaybe,
    parseHeadersMap,

    -- * Header names
    hHost,
    hExpect,
    hAMZToken,
    hAMZTarget,
    hAMZAlgorithm,
    hAMZCredential,
    hAMZExpires,
    hAMZSignedHeaders,
    hAMZContentSHA256,
    hAMZDate,
    hMetaPrefix,
    hAMZRequestId,
    hAMZNRequestId,
    hAMZNErrorType,
    hAMZNAuth,
    hAMZDecodedContentLength,
    hTransferEncoding,

    -- * Re-exported
    HTTP.Types.HeaderName,
    HTTP.Types.Header,
    HTTP.Types.hContentType,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Network.AWS.Data.Text as AWS.Text
import Network.AWS.Prelude
import Network.HTTP.Types (HeaderName)
import qualified Network.HTTP.Types as HTTP.Types

-- Note: Chose to use Map rather than HashMap so we can utilise the
-- lexicographic ordering of keys for signing, without an additional sort pass.
--
-- This means when parsing a map of headers, we need to convert <-> HashMap.
type Headers = Map HeaderName ByteString

type HeaderPair = (HeaderName, ByteString)

newtype HeaderBuilder = HeaderBuilder ([HeaderPair] -> [HeaderPair])
  deriving (Semigroup, Monoid) via (Monoid.Endo [HeaderPair])

instance Show HeaderBuilder where
  showsPrec _ (HeaderBuilder builder) =
    showString "HeaderBuilder "
      . showsPrec 0 (builder [])
  {-# INLINEABLE showsPrec #-}

buildHeaders :: HeaderBuilder -> Headers
buildHeaders (HeaderBuilder builder) = Map.fromList (builder [])
{-# INLINEABLE buildHeaders #-}

class ToHeaders a where
  toHeaders :: HeaderName -> a -> HeaderBuilder
  default toHeaders :: AWS.Text.ToText a => HeaderName -> a -> HeaderBuilder
  toHeaders key = addPair key . AWS.Text.toUTF8
  {-# INLINEABLE toHeaders #-}

instance ToHeaders ByteString where
  toHeaders = addPair
  {-# INLINEABLE toHeaders #-}

-- instance AWS.Text.ToText a => ToHeaders (Maybe a) where
--   toHeaders key = \case
--     Nothing -> mempty
--     Just val -> addPair key (AWS.Text.toUTF8 val)
--   {-# INLINEABLE toHeaders #-}

instance ToHeaders Char

instance ToHeaders Text

instance ToHeaders Bool

instance ToHeaders Int

instance ToHeaders Integer

instance ToHeaders Natural

instance ToHeaders Double

instance ToHeaders UTCTime

instance ToHeaders NominalDiffTime

class FromHeaders a where
  parseHeaders :: HeaderName -> Headers -> Either Text a

parseHeadersMaybe ::
  FromHeaders a =>
  HeaderName ->
  Headers ->
  Either Text (Maybe a)
parseHeadersMaybe name headers
  | Map.member name headers = Right Nothing
  | otherwise = Just <$> parseHeaders name headers
{-# INLINEABLE parseHeadersMaybe #-}

-- Parsing a header map is inherently optional
parseHeadersMap ::
  (Eq k, Hashable k, AWS.Text.FromText k, AWS.Text.FromText v) =>
  -- | The prefix used to inclusively filter keys prior to parsing.
  HeaderName ->
  Headers ->
  Either Text (HashMap k v)
parseHeadersMap (CI.foldedCase -> prefix) =
  fmap HashMap.fromList
    . traverse (uncurry parseItem)
    . Map.toList
    . Map.filterWithKey (const . restrictKey)
  where
    restrictKey key =
      ByteString.isPrefixOf prefix (CI.foldedCase key)

    parseItem key value =
      (,) <$> AWS.Text.parseUTF8 (ByteString.drop prefixLength (CI.foldedCase key))
        <*> AWS.Text.parseUTF8 value

    prefixLength =
      ByteString.length prefix
{-# INLINEABLE parseHeadersMap #-}

-- infixl 7 .#, .#?

-- -- FIXME: This whole toText/fromText shit is just stupid.
-- parseResponseHeader

-- (.#) :: FromText a => ResponseHeaders -> HeaderName -> Either String a
-- hs .# k = hs .#? k >>= note
--   where
--     note = \case
--       Nothing -> Left (BS8.unpack $ "Unable to find header: " <> CI.original k)
--       Just x -> Right x

-- parseResponseHeaderMaybe

-- (.#?) :: FromText a => ResponseHeaders -> HeaderName -> Either String (Maybe a)
-- hs .#? k =
--   maybe
--     (Right Nothing)
--     (fmap Just . fromText . Text.decodeUtf8)
--     (lookup k hs)

-- parseResponseHeader

-- insertRequestHeader :: HeaderName -> ByteString -> [Header] -> [Header]
-- insertRequestHeader k v hs = (k, v) : filter ((/= k) . fst) hs

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

addPair :: HeaderName -> ByteString -> HeaderBuilder
addPair key val = HeaderBuilder ((key, val) :)

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

-- hFormEncoded :: ByteString
-- hFormEncoded = "application/x-www-form-urlencoded; charset=utf-8"
