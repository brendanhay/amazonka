{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Network.AWS.Internal.Body
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Internal.Body where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Network.AWS.Prelude
import           System.IO

import           Prelude

-- | Convenience function for obtaining the size of a file.
getFileSize :: MonadIO m => FilePath -> m Integer
getFileSize f = liftIO (withBinaryFile f ReadMode hFileSize)

-- | Connect a 'Sink' to a response stream.
sinkBody :: MonadResource m => RsBody -> Sink ByteString m a -> m a
sinkBody (RsBody s) sink = hoist liftResourceT s $$+- sink

-- | Construct a 'HashedBody' from a 'FilePath', calculating the 'SHA256' hash
-- and file size.
--
-- /Note:/ While this function will perform in constant space, it will enumerate the
-- entirety of the file contents _twice_. Firstly to calculate the SHA256 and
-- lastly to stream the contents to the socket during sending.
--
-- /See:/ 'ToHashedBody'.
hashedFile :: MonadIO m => FilePath -> m HashedBody
hashedFile f = liftIO $ HashedStream
    <$> runResourceT (Conduit.sourceFile f $$ sinkSHA256)
    <*> getFileSize f
    <*> pure (Conduit.sourceFile f)

-- | Construct a 'HashedBody' from a source, manually specifying the
-- 'SHA256' hash and file size.
--
-- /See:/ 'ToHashedBody'.
hashedBody :: Digest SHA256
           -> Integer
           -> Source (ResourceT IO) ByteString
           -> HashedBody
hashedBody h n = HashedStream h n

-- | Something something.
--
-- Will intelligently revert to 'HashedBody' if the file is smaller than the
-- specified 'ChunkSize'.
--
-- Add note about how it selects chunk size.
--
-- /See:/ 'ToBody'.
chunkedFile :: MonadIO m => ChunkSize -> FilePath -> m RqBody
chunkedFile c f = do
    n <- getFileSize f
    if n > toInteger c
        then return $ unsafeChunkedBody c n (sourceFileChunks c f)
        else Hashed `liftM` hashedFile f

-- | Something something.
--
-- Marked as unsafe because it does nothing to enforce the chunk size.
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
                  -> Integer
                  -> Source (ResourceT IO) ByteString
                  -> RqBody
unsafeChunkedBody c n = Chunked . ChunkedBody c n

-- Uses hGet with a specific buffer size, instead of hGetSome.
sourceFileChunks :: MonadResource m
                 => ChunkSize
                 -> FilePath
                 -> Source m ByteString
sourceFileChunks (ChunkSize sz) f =
    bracketP (openBinaryFile f ReadMode) hClose go
  where
    go h = do
        bs <- liftIO (BS.hGet h sz)
        unless (BS.null bs) $ do
            yield bs
            go h

-- | Incrementally calculate a 'MD5' 'Digest'.
sinkMD5 :: Monad m => Consumer ByteString m (Digest MD5)
sinkMD5 = sinkHash

-- | Incrementally calculate a 'SHA256' 'Digest'.
sinkSHA256 :: Monad m => Consumer ByteString m (Digest SHA256)
sinkSHA256 = sinkHash

-- | A cryptonite compatible incremental hash sink.
sinkHash :: (Monad m, HashAlgorithm a) => Consumer ByteString m (Digest a)
sinkHash = sink hashInit
  where
    sink ctx = do
        b <- await
        case b of
            Nothing -> return $! hashFinalize ctx
            Just bs -> sink $! hashUpdate ctx bs
