{-# LANGUAGE CPP #-}

-- |
-- Module      : Amazonka.Data.Body
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Data.Body where

import qualified Amazonka.Bytes as Bytes
import Amazonka.Core.Lens.Internal (coerced)
import Amazonka.Crypto (Digest, SHA256)
import qualified Amazonka.Crypto as Crypto
import Amazonka.Data.ByteString
import Amazonka.Data.Log
import Amazonka.Data.Query (QueryString)
import Amazonka.Data.XML (encodeXML)
import Amazonka.Prelude hiding (length)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Conduit (ConduitM, (.|))
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Binary as Conduit.Binary
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.Encoding as LText
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Conduit as Client.Conduit
import qualified System.IO as IO
import qualified Text.XML as XML

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap (KeyMap)
#endif

-- | Convenience function for obtaining the size of a file.
getFileSize :: MonadIO m => FilePath -> m Integer
getFileSize path = liftIO (IO.withBinaryFile path IO.ReadMode IO.hFileSize)

-- | A streaming, exception safe response body.
--
-- @newtype@ for show/orhpan instance purposes.
newtype ResponseBody = ResponseBody
  {body :: ConduitM () ByteString (ResourceT IO) ()}
  deriving stock (Generic)

instance Show ResponseBody where
  show = const "ResponseBody { ConduitM () ByteString (ResourceT IO) () }"

{-# INLINE _ResponseBody #-}
_ResponseBody :: Iso' ResponseBody (ConduitM () ByteString (ResourceT IO) ())
_ResponseBody = coerced

fuseStream ::
  ResponseBody ->
  ConduitM ByteString ByteString (ResourceT IO) () ->
  ResponseBody
fuseStream b@ResponseBody {body} f = b {body = body .| f}

-- | Connect a 'Sink' to a response stream.
sinkBody :: MonadIO m => ResponseBody -> ConduitM ByteString Void (ResourceT IO) a -> m a
sinkBody (ResponseBody body) sink =
  liftIO $ Conduit.runConduitRes $ body .| sink

-- | Specifies the transmitted size of the 'Transfer-Encoding' chunks.
--
-- /See:/ 'defaultChunk'.
newtype ChunkSize = ChunkSize Int
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, Real, Integral)

instance ToLog ChunkSize where
  build = build . show

_ChunkSize :: Iso' ChunkSize Int
_ChunkSize = coerced

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
  { size :: ChunkSize,
    length :: Integer,
    body :: ConduitM () ByteString (ResourceT IO) ()
  }

{-# INLINE chunkedBody_size #-}
chunkedBody_size :: Lens' ChunkedBody ChunkSize
chunkedBody_size f b@ChunkedBody {size} = f size <&> \size' -> b {size = size'}

{-# INLINE chunkedBody_length #-}
chunkedBody_length :: Lens' ChunkedBody Integer
chunkedBody_length f b@ChunkedBody {length} = f length <&> \length' -> b {length = length'}

{-# INLINE chunkedBody_body #-}
chunkedBody_body :: Lens' ChunkedBody (ConduitM () ByteString (ResourceT IO) ())
chunkedBody_body f b@ChunkedBody {body} = f body <&> \body' -> (b :: ChunkedBody) {body = body'}

-- Maybe revert to using Source's, and then enforce the chunk size
-- during conversion from HashedBody -> ChunkedBody

instance Show ChunkedBody where
  show c =
    BS8.unpack . toBS $
      "ChunkedBody { chunkSize = "
        <> build (size c)
        <> "<> originalLength = "
        <> build (length c)
        <> "<> fullChunks = "
        <> build (fullChunks c)
        <> "<> remainderBytes = "
        <> build (remainderBytes c)
        <> "}"

fuseChunks ::
  ChunkedBody ->
  ConduitM ByteString ByteString (ResourceT IO) () ->
  ChunkedBody
fuseChunks c@ChunkedBody {body} f = c {body = body .| f}

fullChunks :: ChunkedBody -> Integer
fullChunks c = length c `div` fromIntegral (size c)

remainderBytes :: ChunkedBody -> Maybe Integer
remainderBytes ChunkedBody {length, size} =
  case length `mod` toInteger size of
    0 -> Nothing
    n -> Just n

-- | Construct a 'ChunkedBody' from a 'FilePath', where the contents will be
-- read and signed incrementally in chunks if the target service supports it.
--
-- Will intelligently revert to 'HashedBody' if the file is smaller than the
-- specified 'ChunkSize'.
--
-- /See:/ 'ToBody'.
chunkedFile :: MonadIO m => ChunkSize -> FilePath -> m RequestBody
chunkedFile chunk path = do
  size <- getFileSize path
  if size > toInteger chunk
    then return $ unsafeChunkedBody chunk size (sourceFileChunks chunk path)
    else Hashed <$> hashedFile path

-- | Construct a 'ChunkedBody' from a 'FilePath', specifying the range of bytes
-- to read. This can be useful for constructing multiple requests from a single
-- file, say for S3 multipart uploads.
--
-- /See:/ 'chunkedFile'.
chunkedFileRange ::
  MonadIO m =>
  -- | The idealized size of chunks that will be yielded downstream.
  ChunkSize ->
  -- | The file path to read.
  FilePath ->
  -- | The byte offset at which to start reading.
  Integer ->
  -- | The maximum number of bytes to read.
  Integer ->
  m RequestBody
chunkedFileRange chunk path offset len = do
  size <- getFileSize path
  let n = min (size - offset) len
  if n > toInteger chunk
    then return $ unsafeChunkedBody chunk n (sourceFileRangeChunks chunk path offset len)
    else Hashed <$> hashedFileRange path offset len

-- | Unsafely construct a 'ChunkedBody'.
--
-- This function is marked unsafe because it does nothing to enforce the chunk size.
-- Typically for conduit 'IO' functions, it's whatever ByteString's
-- 'defaultBufferSize' is, around 32 KB. If the chunk size is less than 8 KB,
-- the request will error. 64 KB or higher chunk size is recommended for
-- performance reasons.
--
-- Note that it will always create a chunked body even if the request
-- is too small.
--
-- /See:/ 'ToBody'.
unsafeChunkedBody ::
  -- | The idealized size of chunks that will be yielded downstream.
  ChunkSize ->
  -- | The size of the stream in bytes.
  Integer ->
  ConduitM () ByteString (ResourceT IO) () ->
  RequestBody
unsafeChunkedBody chunk size = Chunked . ChunkedBody chunk size

sourceFileChunks ::
  MonadResource m =>
  ChunkSize ->
  FilePath ->
  ConduitM () ByteString m ()
sourceFileChunks (ChunkSize chunk) path =
  Conduit.bracketP (IO.openBinaryFile path IO.ReadMode) IO.hClose go
  where
    -- Uses hGet with a specific buffer size, instead of hGetSome.
    go hd = do
      bs <- liftIO (BS.hGet hd chunk)
      unless (BS.null bs) $ do
        Conduit.yield bs
        go hd

sourceFileRangeChunks ::
  MonadResource m =>
  -- | The idealized size of chunks that will be yielded downstream.
  ChunkSize ->
  -- | The file path to read.
  FilePath ->
  -- | The byte offset at which to start reading.
  Integer ->
  -- | The maximum number of bytes to read.
  Integer ->
  ConduitM () ByteString m ()
sourceFileRangeChunks (ChunkSize chunk) path offset len =
  Conduit.bracketP acquire IO.hClose seek
  where
    acquire = IO.openBinaryFile path IO.ReadMode
    seek hd = do
      liftIO (IO.hSeek hd IO.AbsoluteSeek offset)
      go (fromIntegral len) hd

    go remainder hd
      | remainder <= chunk = do
          bs <- liftIO (BS.hGet hd remainder)
          unless (BS.null bs) $
            Conduit.yield bs
      --
      | otherwise = do
          bs <- liftIO (BS.hGet hd chunk)

          unless (BS.null bs) $ do
            Conduit.yield bs
            go (remainder - chunk) hd

-- | An opaque request body containing a 'SHA256' hash.
data HashedBody
  = HashedStream (Digest SHA256) !Integer (ConduitM () ByteString (ResourceT IO) ())
  | HashedBytes (Digest SHA256) ByteString

instance Show HashedBody where
  show = \case
    HashedStream h n _ -> str "HashedStream" h n
    HashedBytes h x -> str "HashedBody" h (BS.length x)
    where
      str c h n =
        BS8.unpack . toBS $
          c
            <> " { sha256 = "
            <> build (Bytes.encodeBase16 h)
            <> ", length = "
            <> build n

instance IsString HashedBody where
  fromString = toHashed

sha256Base16 :: HashedBody -> ByteString
sha256Base16 =
  Bytes.encodeBase16 . \case
    HashedStream h _ _ -> h
    HashedBytes h _ -> h

-- | Construct a 'HashedBody' from a 'FilePath', calculating the 'SHA256' hash
-- and file size.
--
-- /Note:/ While this function will perform in constant space, it will enumerate the
-- entirety of the file contents _twice_. Firstly to calculate the SHA256 and
-- lastly to stream the contents to the socket during sending.
--
-- /See:/ 'ToHashedBody'.
hashedFile ::
  MonadIO m =>
  -- | The file path to read.
  FilePath ->
  m HashedBody
hashedFile path =
  liftIO $
    HashedStream
      <$> runResourceT (Conduit.Binary.sourceFile path `Conduit.connect` Crypto.sinkSHA256)
      <*> getFileSize path
      <*> pure (Conduit.Binary.sourceFile path)

-- | Construct a 'HashedBody' from a 'FilePath', specifying the range of bytes
-- to read. This can be useful for constructing multiple requests from a single
-- file, say for S3 multipart uploads.
--
-- /See:/ 'hashedFile', 'Conduit.sourceFileRange'.
hashedFileRange ::
  MonadIO m =>
  -- | The file path to read.
  FilePath ->
  -- | The byte offset at which to start reading.
  Integer ->
  -- | The maximum number of bytes to read.
  Integer ->
  m HashedBody
hashedFileRange path offset len = do
  size <- getFileSize path
  let bytes = min len (size - offset)
      sourceFileRange =
        Conduit.Binary.sourceFileRange path (Just offset) (Just len)
  digest <-
    liftIO . runResourceT $ Conduit.connect sourceFileRange Crypto.sinkSHA256
  pure $ HashedStream digest bytes sourceFileRange

-- | Construct a 'HashedBody' from a 'Source', manually specifying the 'SHA256'
-- hash and file size. It's left up to the caller to calculate these correctly,
-- otherwise AWS will return signing errors.
--
-- /See:/ 'ToHashedBody'.
hashedBody ::
  -- | A SHA256 hash of the file contents.
  Crypto.Digest Crypto.SHA256 ->
  -- | The size of the stream in bytes.
  Integer ->
  ConduitM () ByteString (ResourceT IO) () ->
  HashedBody
hashedBody = HashedStream

-- | Invariant: only services that support _both_ standard and
-- chunked signing expose 'RequestBody' as a parameter.
data RequestBody
  = -- | Currently S3 only, see 'ChunkedBody' for details.
    Chunked ChunkedBody
  | Hashed HashedBody
  deriving stock (Show)

instance IsString RequestBody where
  fromString = Hashed . fromString

md5Base64 :: RequestBody -> Maybe ByteString
md5Base64 = \case
  Hashed (HashedBytes _ x) -> Just (Bytes.encodeBase64 (Crypto.hashMD5 x))
  _ -> Nothing

isStreaming :: RequestBody -> Bool
isStreaming = \case
  Hashed (HashedStream {}) -> True
  _ -> False

toRequestBody :: RequestBody -> Client.RequestBody
toRequestBody = \case
  Chunked ChunkedBody {body} -> Client.Conduit.requestBodySourceChunked body
  Hashed x -> case x of
    HashedStream _ n f -> Client.Conduit.requestBodySource (fromIntegral n) f
    HashedBytes _ b -> Client.RequestBodyBS b

contentLength :: RequestBody -> Integer
contentLength = \case
  Chunked ChunkedBody {length} -> length
  Hashed x -> case x of
    HashedStream _ n _ -> n
    HashedBytes _ b -> fromIntegral (BS.length b)

-- | Anything that can be safely converted to a 'HashedBody'.
class ToHashedBody a where
  -- | Convert a value to a hashed request body.
  toHashed :: a -> HashedBody

instance ToHashedBody ByteString where
  toHashed x = HashedBytes (Crypto.hash x) x

instance ToHashedBody HashedBody where
  toHashed = id

instance ToHashedBody String where
  toHashed = toHashed . LBS8.pack

instance ToHashedBody ByteStringLazy where
  toHashed = toHashed . toBS

instance ToHashedBody Text where
  toHashed = toHashed . Text.encodeUtf8

instance ToHashedBody TextLazy where
  toHashed = toHashed . LText.encodeUtf8

instance ToHashedBody Aeson.Value where
  toHashed = toHashed . Aeson.encode

instance ToHashedBody XML.Element where
  toHashed = toHashed . encodeXML

instance ToHashedBody QueryString where
  toHashed = toHashed . toBS

#if MIN_VERSION_aeson(2,0,0)
instance ToHashedBody (KeyMap Aeson.Value) where
  toHashed = toHashed . Aeson.Object
#else
instance ToHashedBody (HashMap Text Aeson.Value) where
  toHashed = toHashed . Aeson.Object
#endif

-- | Anything that can be converted to a streaming request 'Body'.
class ToBody a where
  -- | Convert a value to a request body.
  toBody :: a -> RequestBody
  default toBody :: ToHashedBody a => a -> RequestBody
  toBody = Hashed . toHashed

instance ToBody RequestBody where
  toBody = id

instance ToBody HashedBody where
  toBody = Hashed

instance ToBody ChunkedBody where
  toBody = Chunked

instance ToHashedBody a => ToBody (Maybe a) where
  toBody = Hashed . maybe (toHashed BS.empty) toHashed

instance ToBody String

instance ToBody ByteStringLazy

instance ToBody ByteString

instance ToBody Text

instance ToBody TextLazy

#if MIN_VERSION_aeson(2,0,0)
instance ToBody (KeyMap Aeson.Value)
#else
instance ToBody (HashMap Text Aeson.Value)
#endif

instance ToBody Aeson.Value

instance ToBody XML.Element

instance ToBody QueryString
