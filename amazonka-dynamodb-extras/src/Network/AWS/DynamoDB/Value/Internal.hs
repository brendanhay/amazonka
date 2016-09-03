-- |
-- Module      : Network.AWS.DynamoDB.Value.Internal
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Value.Internal
    ( Value (..)
    ) where

import Data.Hashable

import Network.AWS.DynamoDB (AttributeValue)

-- | An opaque DynamoDB storable-value that ensures the invariant
-- that only a single field in an 'AttributeValue' may be set.
newtype Value = UnsafeValue { getValue :: AttributeValue }
    deriving (Eq, Show)

instance Hashable Value where
    hashWithSalt s (UnsafeValue v) =
        s `hashWithSalt` (0 :: Int) `hashWithSalt` v
