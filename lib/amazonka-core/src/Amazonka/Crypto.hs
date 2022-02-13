-- |
-- Module      : Amazonka.Crypto
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Crypto
  ( -- * HMAC
    Key,
    hmacSHA1,
    hmacSHA256,

    -- * Hashing
    hashSHA1,
    hashSHA256,
    hashMD5,
    Hash.hash,
    --- * Incremental Hashing
    sinkSHA256,
    sinkMD5,

    -- * Re-exported
    Hash.HashAlgorithm,
    Hash.Digest,
    Hash.SHA256,
    Hash.MD5,
  )
where

import Amazonka.Prelude
import qualified Crypto.Hash as Hash
import qualified Crypto.MAC.HMAC as HMAC
import Data.ByteArray (ByteArrayAccess)
import Data.Conduit (ConduitM)
import qualified Data.Conduit as Conduit

type Key = ByteString

hmacSHA1 :: ByteArrayAccess a => Key -> a -> HMAC.HMAC Hash.SHA1
hmacSHA1 = HMAC.hmac

hmacSHA256 :: ByteArrayAccess a => Key -> a -> HMAC.HMAC Hash.SHA256
hmacSHA256 = HMAC.hmac

hashSHA1 :: ByteArrayAccess a => a -> Hash.Digest Hash.SHA1
hashSHA1 = Hash.hashWith Hash.SHA1

hashSHA256 :: ByteArrayAccess a => a -> Hash.Digest Hash.SHA256
hashSHA256 = Hash.hashWith Hash.SHA256

hashMD5 :: ByteArrayAccess a => a -> Hash.Digest Hash.MD5
hashMD5 = Hash.hashWith Hash.MD5

-- | Incrementally calculate a 'MD5' 'Digest'.
sinkMD5 :: Monad m => ConduitM ByteString o m (Hash.Digest Hash.MD5)
sinkMD5 = sinkHash

-- | Incrementally calculate a 'SHA256' 'Digest'.
sinkSHA256 :: Monad m => ConduitM ByteString o m (Hash.Digest Hash.SHA256)
sinkSHA256 = sinkHash

-- | A cryptonite compatible incremental hash sink.
sinkHash ::
  ( Monad m,
    Hash.HashAlgorithm a
  ) =>
  ConduitM ByteString o m (Hash.Digest a)
sinkHash = sink Hash.hashInit
  where
    sink ctx = do
      mbs <- Conduit.await

      case mbs of
        Nothing -> pure $! Hash.hashFinalize ctx
        Just bs -> sink $! Hash.hashUpdate ctx bs
