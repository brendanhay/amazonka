{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Network.AWS.Internal.Body
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Internal.Body where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Network.AWS.Prelude
import           System.IO
import           Data.Void (Void) -- required for < conduit-1.3

-- | Convenience function for obtaining the size of a file.
getFileSize :: MonadIO m => FilePath -> m Integer
getFileSize path = liftIO (withBinaryFile path ReadMode hFileSize)

-- | Connect a 'Sink' to a response stream.
sinkBody :: MonadIO m => RsBody -> ConduitM ByteString Void (ResourceT IO) a -> m a
sinkBody (RsBody body) sink = liftIO $ runConduitRes $ body .| sink

-- | Construct a 'HashedBody' from a 'FilePath', calculating the 'SHA256' hash
-- and file size.
--
-- /Note:/ While this function will perform in constant space, it will enumerate the
-- entirety of the file contents _twice_. Firstly to calculate the SHA256 and
-- lastly to stream the contents to the socket during sending.
--
-- /See:/ 'ToHashedBody'.
hashedFile :: MonadIO m
           => FilePath -- ^ The file path to read.
           -> m HashedBody
hashedFile path =
    liftIO $ HashedStream
        <$> runResourceT (Conduit.sourceFile path `connect` sinkSHA256)
        <*> getFileSize path
        <*> pure (Conduit.sourceFile path)

-- | Construct a 'HashedBody' from a 'FilePath', specifying the range of bytes
-- to read. This can be useful for constructing multiple requests from a single
-- file, say for S3 multipart uploads.
--
-- /See:/ 'hashedFile', 'Conduit.sourceFileRange'.
hashedFileRange :: MonadIO m
                => FilePath -- ^ The file path to read.
                -> Integer  -- ^ The byte offset at which to start reading.
                -> Integer  -- ^ The maximum number of bytes to read.
                -> m HashedBody
hashedFileRange path (Just -> offset) (Just -> len) =
    liftIO $ HashedStream
        <$> runResourceT (Conduit.sourceFileRange path offset len `connect` sinkSHA256)
        <*> getFileSize path
        <*> pure (Conduit.sourceFileRange path offset len)

-- | Construct a 'HashedBody' from a 'Source', manually specifying the 'SHA256'
-- hash and file size. It's left up to the caller to calculate these correctly,
-- otherwise AWS will return signing errors.
--
-- /See:/ 'ToHashedBody'.
hashedBody :: Digest SHA256 -- ^ A SHA256 hash of the file contents.
           -> Integer       -- ^ The size of the stream in bytes.
           -> ConduitM () ByteString (ResourceT IO) ()
           -> HashedBody
hashedBody = HashedStream

-- | Construct a 'ChunkedBody' from a 'FilePath', where the contents will be
-- read and signed incrementally in chunks if the target service supports it.
--
-- Will intelligently revert to 'HashedBody' if the file is smaller than the
-- specified 'ChunkSize'.
--
-- /See:/ 'ToBody'.
chunkedFile :: MonadIO m => ChunkSize -> FilePath -> m RqBody
chunkedFile chunk path = do
    size <- getFileSize path
    if size > toInteger chunk
        then return $ unsafeChunkedBody chunk size (sourceFileChunks chunk path)
        else Hashed `liftM` hashedFile path

-- | Construct a 'ChunkedBody' from a 'FilePath', specifying the range of bytes
-- to read. This can be useful for constructing multiple requests from a single
-- file, say for S3 multipart uploads.
--
-- /See:/ 'chunkedFile'.
chunkedFileRange :: MonadIO m
                 => ChunkSize
                    -- ^ The idealized size of chunks that will be yielded downstream.
                 -> FilePath
                    -- ^ The file path to read.
                 -> Integer
                    -- ^ The byte offset at which to start reading.
                 -> Integer
                    -- ^ The maximum number of bytes to read.
                 -> m RqBody
chunkedFileRange chunk path offset len = do
    size <- getFileSize path
    let n = min (size - offset) len
    if  n > toInteger chunk
        then return $ unsafeChunkedBody chunk n (sourceFileRangeChunks chunk path offset len)
        else Hashed `liftM` hashedFileRange path offset len

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
unsafeChunkedBody :: ChunkSize
                     -- ^ The idealized size of chunks that will be yielded downstream.
                  -> Integer
                     -- ^ The size of the stream in bytes.
                  -> ConduitM () ByteString (ResourceT IO) ()
                  -> RqBody
unsafeChunkedBody chunk size = Chunked . ChunkedBody chunk size

sourceFileChunks :: MonadResource m
                 => ChunkSize
                 -> FilePath
                 -> ConduitM () ByteString m ()
sourceFileChunks (ChunkSize chunk) path =
    bracketP (openBinaryFile path ReadMode) hClose go
  where
    -- Uses hGet with a specific buffer size, instead of hGetSome.
    go hd = do
        bs <- liftIO (BS.hGet hd chunk)
        unless (BS.null bs) $ do
            yield bs
            go hd

sourceFileRangeChunks :: MonadResource m
                      => ChunkSize
                         -- ^ The idealized size of chunks that will be yielded downstream.
                      -> FilePath
                         -- ^ The file path to read.
                      -> Integer
                         -- ^ The byte offset at which to start reading.
                      -> Integer
                         -- ^ The maximum number of bytes to read.
                      -> ConduitM () ByteString m ()
sourceFileRangeChunks (ChunkSize chunk) path offset len =
    bracketP acquire hClose seek
  where
    acquire = openBinaryFile path ReadMode
    seek hd = liftIO (hSeek hd AbsoluteSeek offset) >> go (fromIntegral len) hd

    go remainder hd
        | remainder <= chunk = do
            bs <- liftIO (BS.hGet hd remainder)
            unless (BS.null bs) $
                yield bs

        | otherwise          = do
            bs <- liftIO (BS.hGet hd chunk)
            unless (BS.null bs) $ do
                yield bs
                go (remainder - chunk) hd

-- | Incrementally calculate a 'MD5' 'Digest'.
sinkMD5 :: Monad m => ConduitM ByteString o m (Digest MD5)
sinkMD5 = sinkHash

-- | Incrementally calculate a 'SHA256' 'Digest'.
sinkSHA256 :: Monad m => ConduitM ByteString o m (Digest SHA256)
sinkSHA256 = sinkHash

-- | A cryptonite compatible incremental hash sink.
sinkHash :: (Monad m, HashAlgorithm a) => ConduitM ByteString o m (Digest a)
sinkHash = sink hashInit
  where
    sink ctx = do
        mbs <- await
        case mbs of
            Nothing -> return $! hashFinalize ctx
            Just bs -> sink $! hashUpdate ctx bs
