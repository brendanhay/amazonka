{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}

-- |
-- Module      : Network.AWS.Data.Crypto
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Crypto
    (
    -- * Conversion
      digestToBS
    , digestToBase

    -- * Algorithms
    , hmacSHA1
    , hmacSHA256
    , hashSHA256
    , hashMD5

    -- * Contexts
    , hash
    , hashlazy
    , hashInit
    , hashUpdate
    , hashFinalize

    -- * Re-exported
    , HMAC
    , Digest
    , HashAlgorithm
    , SHA256 (..)
    , MD5    (..)
    , Base   (..)
    ) where

import           "cryptonite" Crypto.Hash
import           "cryptonite" Crypto.MAC.HMAC
import           Data.ByteArray
import           Data.ByteArray.Encoding
import           Data.ByteString         (ByteString)

digestToBS :: ByteArrayAccess a => a -> ByteString
digestToBS = convert

digestToBase :: ByteArrayAccess a => Base -> a -> ByteString
digestToBase = convertToBase


-- | Apply an HMAC sha1 with the given secret to the given value.
hmacSHA1 :: (ByteArrayAccess a, ByteArray b) => a -> b -> HMAC SHA1
hmacSHA1 = hmac

hmacSHA256 :: (ByteArrayAccess a, ByteArray b) => a -> b -> HMAC SHA256
hmacSHA256 = hmac

hashSHA256 :: ByteArrayAccess a => a -> Digest SHA256
hashSHA256 = hashWith SHA256

hashMD5 :: ByteArrayAccess a => a -> Digest MD5
hashMD5 = hashWith MD5
