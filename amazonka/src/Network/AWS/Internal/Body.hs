-- |
-- Module      : Network.AWS.Internal.Body
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Internal.Body where

import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString as BS
import Data.Conduit (ConduitM)
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Binary as Conduit.Binary
import qualified Network.AWS.Internal.Crypto as Crypto
import Network.AWS.Data
import Network.AWS.Internal.Prelude
import qualified System.IO as IO

-- | Convenience function for obtaining the size of a file.
getFileSize :: MonadIO m => FilePath -> m Integer
getFileSize path = liftIO (IO.withBinaryFile path IO.ReadMode IO.hFileSize)

-- | Connect a 'Sink' to a response stream.
sinkBody :: MonadIO m => ResponseBody -> ConduitM ByteString Void (ResourceT IO) a -> m a
sinkBody (ResponseBody body) sink =
  liftIO $ Conduit.runConduitRes $ body Conduit..| sink

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
hashedFileRange path (Just -> offset) (Just -> len) =
  liftIO $
    HashedStream
      <$> runResourceT (Conduit.Binary.sourceFileRange path offset len `Conduit.connect` Crypto.sinkSHA256)
      <*> getFileSize path
      <*> pure (Conduit.Binary.sourceFileRange path offset len)

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
