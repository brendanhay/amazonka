{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Network.AWS.S3.Encryption.Body
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption.Body where

import           Conduit
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.Int
import           Network.AWS.Data.Body

-- Resides here since it's unsafe without use of enforceChunks,
-- which incurs extra dependencies not desired in core.
class ToChunkedBody a where
    toChunked :: a -> ChunkedBody

instance ToChunkedBody ChunkedBody where
    toChunked = id

instance ToChunkedBody HashedBody where
    toChunked = \case
        HashedStream _ n s -> chk n s
        HashedBytes  _ b   -> chk (fromIntegral (BS.length b)) (mapM_ yield [b])
      where
        chk n = ChunkedBody defaultChunkSize n
            . flip fuse (enforceChunks sz)

        sz = fromIntegral defaultChunkSize

instance ToChunkedBody RqBody where
    toChunked = \case
        Chunked c -> c
        Hashed  h -> toChunked h

enforceChunks :: Monad m => Int64 -> Conduit ByteString m ByteString
enforceChunks n = awaitForever (\i -> leftover i >> sinkLazy >>= yield)
    =$= takeCE n
    =$= mapC LBS.toStrict
