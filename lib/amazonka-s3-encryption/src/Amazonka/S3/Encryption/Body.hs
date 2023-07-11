-- |
-- Module      : Amazonka.S3.Encryption.Body
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Encryption.Body where

import Amazonka.Core
import Amazonka.Prelude
import Conduit ((.|))
import qualified Conduit
import qualified Data.ByteString as BS

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
  Conduit.ConduitT () ByteString (Conduit.ResourceT IO) () ->
  ChunkedBody
enforceChunks size c =
  ChunkedBody defaultChunkSize (fromIntegral size) $
    c .| Conduit.chunksOfCE (fromIntegral defaultChunkSize)
