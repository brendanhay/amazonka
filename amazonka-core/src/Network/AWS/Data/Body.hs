-- |
-- Module      : Network.AWS.Data.Body
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.Body where

import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Conduit (ConduitM)
import qualified Data.Conduit as Conduit
import qualified Data.Text.Encoding as Text.Encoding
import Network.AWS.Hash (Digest, SHA256)
import qualified Network.AWS.Hash as Hash
import qualified Network.AWS.Lens as Lens
import Network.AWS.Prelude
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Conduit as HTTP.Conduit

-- -- | A convenience alias encapsulating the common 'Response' body.
-- type ResponseBody = ConduitM () ByteString (ResourceT IO) ()

-- | A streaming, exception safe response body.
newtype ResponseBody = ResponseBody
  { _streamBody :: ConduitM () ByteString (ResourceT IO) ()
  } -- newtype for show/orhpan instance purposes.

instance Show ResponseBody where
  showsPrec _ _ =
    showString "ResponseBody { ConduitM () ByteString (ResourceT IO) () }"

fuseStream ::
  ResponseBody ->
  ConduitM ByteString ByteString (ResourceT IO) () ->
  ResponseBody
fuseStream body f =
  body { _streamBody = _streamBody body Conduit..| f
       }

-- | Specifies the transmitted size of the 'Transfer-Encoding' chunks.
--
-- /See:/ 'defaultChunk'.
newtype ChunkSize = ChunkSize Int
  deriving stock (Eq, Ord)
  deriving newtype (Show, Enum, Num, Real, Integral)

-- instance ToLog ChunkSize where
--   build = build . show

-- | The default chunk size of 128 KB. The minimum chunk size accepted by
-- AWS is 8 KB, unless the entirety of the request is below this threshold.
--
-- A chunk size of 64 KB or higher is recommended for performance reasons.
defaultChunkSize :: ChunkSize
defaultChunkSize = 128 * 1024

-- | An opaque request body which will be transmitted via
-- @Transfer-Encoding: chunked@.
--
-- /Invariant:/ Only services that support chunked encoding can
-- accept a 'ChunkedBody'. (Currently S3.) This is enforced by the type
-- signatures emitted by the generator.
data ChunkedBody = ChunkedBody
  { _chunkedSize :: ChunkSize,
    _chunkedLength :: Integer,
    _chunkedBody :: ConduitM () ByteString (ResourceT IO) ()
  }

chunkedLength :: Lens' ChunkedBody Integer
chunkedLength = Lens.lens _chunkedLength (\s a -> s {_chunkedLength = a})

-- Maybe revert to using Source's, and then enforce the chunk size
-- during conversion from HashedBody -> ChunkedBody

instance Show ChunkedBody where
  showsPrec _ chunked =
    showString "ChunkedBody { chunkSize = "
      . shows (_chunkedSize chunked)
      . showString ", originalLength = "
      . shows (_chunkedLength chunked)
      . showString ", fullChunks = "
      . shows (fullChunks chunked)
      . showString ", remainderBytes = "
      . shows (remainderBytes chunked)
      . showString "}"

fuseChunks ::
  ChunkedBody ->
  ConduitM ByteString ByteString (ResourceT IO) () ->
  ChunkedBody
fuseChunks c f = c {_chunkedBody = _chunkedBody c Conduit..| f}

fullChunks :: ChunkedBody -> Integer
fullChunks c = _chunkedLength c `div` fromIntegral (_chunkedSize c)

remainderBytes :: ChunkedBody -> Maybe Integer
remainderBytes c =
  case _chunkedLength c `mod` toInteger (_chunkedSize c) of
    0 -> Nothing
    n -> Just n

-- | An opaque request body containing a 'SHA256' hash.
data HashedBody
  = HashedStream (Digest SHA256) !Integer (ConduitM () ByteString (ResourceT IO) ())
  | HashedBytes (Digest SHA256) ByteString

instance Show HashedBody where
  showsPrec _ = \case
    HashedStream h n _ -> showHash "HashedStream" h n
    HashedBytes h x -> showHash "HashedBody" h (ByteString.length x)
    where
      showHash name hash len =
        showString name
          . showString " { sha256 = "
          . shows (Hash.digestToBase Hash.Base16 hash)
          . showString ", length = "
          . shows len

instance IsString HashedBody where
  fromString = toHashedBody . ByteString.Char8.pack
  {-# INLINEABLE fromString #-}

sha256Base16 :: HashedBody -> ByteString
sha256Base16 =
  Hash.digestToBase Hash.Base16 . \case
    HashedStream h _ _ -> h
    HashedBytes h _ -> h

-- | Invariant: only services that support _both_ standard and
-- chunked signing expose 'RequestBody' as a parameter.
data RequestBody
  = Chunked ChunkedBody
  | Hashed HashedBody
  deriving stock (Show)

instance IsString RequestBody where
  fromString = Hashed . fromString

md5Base64 :: RequestBody -> Maybe ByteString
md5Base64 = \case
  Hashed (HashedBytes _ x) ->
    Just . Hash.digestToBase Hash.Base64 $ Hash.hashMD5 x
  _ ->
    Nothing

isStreaming :: RequestBody -> Bool
isStreaming = \case
  Hashed (HashedStream {}) -> True
  _ -> False

toRequestBody :: RequestBody -> Client.RequestBody
toRequestBody = \case
  Chunked x -> HTTP.Conduit.requestBodySourceChunked (_chunkedBody x)
  Hashed x -> case x of
    HashedStream _ n f -> HTTP.Conduit.requestBodySource (fromIntegral n) f
    HashedBytes _ b -> Client.RequestBodyBS b

contentLength :: RequestBody -> Integer
contentLength = \case
  Chunked x -> _chunkedLength x
  Hashed x -> case x of
    HashedStream _ n _ -> n
    HashedBytes _ b -> fromIntegral (ByteString.length b)

-- | Anything that can be safely converted to a 'HashedBody'.
class ToHashedBody a where
  -- | Convert a value to a hashed request body.
  toHashedBody :: a -> HashedBody

instance ToHashedBody HashedBody where
  toHashedBody = id
  {-# INLINEABLE toHashedBody #-}

instance ToHashedBody ByteString where
  toHashedBody x = HashedBytes (Hash.hash x) x
  {-# INLINEABLE toHashedBody #-}

-- instance ToHashedBody String where
--   toHashedBody = toHashedBody . ByteString.Char8.pack
--   {-# INLINEABLE toHashedBody #-}

instance ToHashedBody Text where
  toHashedBody = toHashedBody . Text.Encoding.encodeUtf8
  {-# INLINEABLE toHashedBody #-}

-- instance ToHashedBody Value where
--   toHashedBody = toHashedBody . encode

-- instance ToHashedBody Element where
--   toHashedBody = toHashedBody . encodeXML

-- instance ToHashedBody QueryString where
--   toHashedBody = toHashedBody . toBS

-- instance ToHashedBody (HashMap Text Value) where
--   toHashedBody = toHashedBody . Object

-- | Anything that can be converted to a streaming request 'Body'.
class ToBody a where
  -- | Convert a value to a request body.
  toBody :: a -> RequestBody
  default toBody :: ToHashedBody a => a -> RequestBody
  toBody = Hashed . toHashedBody
  {-# INLINEABLE toBody #-}

instance ToBody RequestBody where
  toBody = id
  {-# INLINEABLE toBody #-}

instance ToBody HashedBody where
  toBody = Hashed
  {-# INLINEABLE toBody #-}

instance ToBody ChunkedBody where
  toBody = Chunked
  {-# INLINEABLE toBody #-}

-- instance ToHashedBody a => ToBody (Maybe a) where
--   toBody = Hashed . maybe (toHashedBody ByteString.empty) toHashedBody

instance ToBody ByteString

instance ToBody Text

-- instance ToBody LText.Text

-- instance ToBody (HashMap Text Value)

-- instance ToBody Value

-- instance ToBody Element

-- instance ToBody QueryString

-- toJSONBody :: ToJSON a => a -> RequestBody
-- toJSONBody = toBody . encode
-- {-# INLINEABLE toJSONBody #-}

-- toXMLBody :: ToElement a => a -> RequestBody
-- toXMLBody = toBody . encodeXML
-- {-# INLINEABLE toXMLBody #-}

-- toQueryBody :: QueryString -> RequestBody
-- toQueryBody = toBody . toBS
-- {-# INLINEABLE toQueryBody #-}
