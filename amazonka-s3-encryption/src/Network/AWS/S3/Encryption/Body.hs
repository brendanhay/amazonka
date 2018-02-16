{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Network.AWS.S3.Encryption.Body
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption.Body where

import           Conduit
import           Control.Lens
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid ((<>))
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
              -> Source (ResourceT IO) ByteString
              -> ChunkedBody
enforceChunks sz =
  let n = fromIntegral defaultChunkSize
   in ChunkedBody defaultChunkSize (fromIntegral sz) . flip fuse (forceChunkSize n)


forceChunkSize :: Monad m
               => Int
               -> Conduit ByteString m ByteString
forceChunkSize bSize = alignChunksBy splitChunk
  where
    splitChunk b | l >  bSize = Just $ (_2 %~ Just) (BS.splitAt bSize b)
                 | l == bSize = Just (b, Nothing)
                 | otherwise  =  Nothing
                 where l = BS.length b


alignChunksBy :: Monad m
              => (ByteString -> Maybe (ByteString, Maybe ByteString))
              -> Conduit ByteString m ByteString
alignChunksBy cSplit = goChunk Nothing
  where
    goChunk bCarry = do
      nextB <- await
      case nextB
        of Nothing -> forM_ bCarry yield
           Just b' -> case bCarry
                        of Nothing -> goSplit b'
                           Just b  -> goSplit (b <> b')

    goSplit b = case cSplit b
                  of Nothing      -> (goChunk . Just) b
                     Just (c, c') -> do yield c
                                        goChunk c'
