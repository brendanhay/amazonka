-- |
-- Module      : Network.AWS.S3.Encryption.Body
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Encryption.Body where

import qualified Conduit
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.AWS.Core
import Network.AWS.Prelude

-- Resides here since it's unsafe without the use of enforceChunks,
-- which incurs extra dependencies not desired in core.
class ToChunkedBody a where
  toChunked :: a -> ChunkedBody

instance ToChunkedBody ChunkedBody where
  toChunked = id

instance ToChunkedBody HashedBody where
  toChunked = \case
    HashedStream _ n s -> enforceChunks n s
    HashedBytes _ b -> enforceChunks (BS.length b) (mapM_ Conduit.yield [b])

instance ToChunkedBody RequestBody where
  toChunked = \case
    Chunked c -> c
    Hashed h -> toChunked h

enforceChunks ::
  Integral a =>
  a ->
  Conduit.ConduitM () ByteString (Conduit.ResourceT IO) () ->
  ChunkedBody
enforceChunks size =
  ChunkedBody defaultChunkSize (fromIntegral size) . flip Conduit.fuse loop
  where
    loop =
      Conduit.awaitForever yield
        Conduit..| Conduit.takeCE chunk
        Conduit..| Conduit.mapC LBS.toStrict

    yield input = do
      Conduit.leftover input
      bytes <- Conduit.sinkLazy
      Conduit.yield bytes

    chunk = fromIntegral defaultChunkSize
