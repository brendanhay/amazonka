-- |
-- Module      : Network.AWS.Hash
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Crypt
  (-- * HMAC
    Key (..),
    hmacSHA1,
    hmacSHA256,

    -- * Hashing
    hashSHA256,
    hashMD5,

    -- * Incremental hashing 
    Hash.hash,
    Hash.hashlazy,
    Hash.hashInit,
    Hash.hashUpdate,
    Hash.hashFinalize,

    -- * Re-exported
    Hash.Digest,
    Hash.SHA256 ,
    Hash.MD5 ,
  )
where

import qualified Crypto.Hash as Hash
import qualified Crypto.MAC.HMAC as HMAC
import Data.ByteArray (ByteArrayAccess)
-- import Data.ByteArray.Encoding
import Network.AWS.Prelude

newtype Key = Key ByteString

hmacSHA1 :: ByteArrayAccess  a =>Key -> a -> HMAC.HMAC Hash.SHA1
hmacSHA1 (Key key) = HMAC.hmac key 

hmacSHA256 :: ByteArrayAccess  a =>Key -> a -> HMAC.HMAC Hash.SHA256
hmacSHA256 (Key key) = HMAC.hmac key

hashSHA256 :: ByteArrayAccess  a => a -> Hash.Digest Hash.SHA256
hashSHA256 = Hash.hashWith Hash.SHA256

hashMD5 ::  ByteArrayAccess a => a -> Hash.Digest Hash.MD5
hashMD5 = Hash.hashWith Hash.MD5
