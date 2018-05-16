{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Network.AWS.S3.Encryption.Body
-- Copyright   : (c) 2013-2018 Brendan Hay
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
import           Network.AWS.Data.Body

-- Resides here since it's unsafe without the use of enforceChunks,
-- which incurs extra dependencies not desired in core.
class ToChunkedBody a where
    toChunked :: a -> ChunkedBody

instance ToChunkedBody ChunkedBody where
    toChunked = id

instance ToChunkedBody HashedBody where
    toChunked = \case
        HashedStream _ n s -> enforceChunks n s
        HashedBytes  _ b   -> enforceChunks (BS.length b) (mapM_ yield [b])

instance ToChunkedBody RqBody where
    toChunked = \case
        Chunked c -> c
        Hashed  h -> toChunked h

enforceChunks :: Integral a
              => a
              -> ConduitM () ByteString (ResourceT IO) ()
              -> ChunkedBody
enforceChunks sz =
    ChunkedBody defaultChunkSize (fromIntegral sz) . flip fuse go
  where
    go = awaitForever (\i -> leftover i >> sinkLazy >>= yield)
      .| takeCE n
      .| mapC LBS.toStrict

    n = fromIntegral defaultChunkSize
