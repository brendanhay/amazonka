-- |
-- Module      : Network.AWS.DynamoDB.Value.Unsafe
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Value.Unsafe
    ( Value (..)
    ) where

import Network.AWS.DynamoDB (AttributeValue)

-- | An opaque DynamoDB storable-value that ensures the invariant
-- that only a single field in an 'AttributeValue' may be set.
newtype Value = Value { getValue :: AttributeValue }
    deriving (Eq, Show)
