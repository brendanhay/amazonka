{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module      : Network.AWS.DynamoDB.Schema.Types
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Type classes for determing attribute scalar types and definitions suitable
-- for use with "Network.AWS.DynamoDB" operations.
module Network.AWS.DynamoDB.Schema.Attribute
    (
    -- * Scalar Types
      DynamoScalarType (..)

    -- * Attribute Definitions
    , DynamoAttributes (..)
    ) where

import Data.ByteString    (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy         (Proxy (..))
import Data.Semigroup     ((<>))
import Data.Text          (Text)

import GHC.TypeLits

import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.Schema.Invariant
import Network.AWS.DynamoDB.Schema.Types

-- | Retrieve a type's corresponding DynamoDB scalar type.
--
-- Instances of this class are considered suitable for use as
-- a parameter to a table or index 'PartitionKey' and 'SortKey', but additional
-- considerations must be made about the value's uniformity, which affects
-- partition placement, data access, and provisioned throughput.
--
-- /See:/ <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GuidelinesForTables.html#GuidelinesForTables.UniformWorkload Table Guidelines - Uniform Workloads>.
class DynamoScalarType a where
    -- | Retrieve the 'ScalarAttributeType', which can be either a
    -- 'S' (string), 'N' (number), or 'B' (binary).
    getScalarType :: Proxy a -> ScalarAttributeType

instance DynamoScalarType Text       where getScalarType = const S
instance DynamoScalarType Integer    where getScalarType = const N
instance DynamoScalarType ByteString where getScalarType = const B

-- | Retrieve a type's non-empty list of DynamoDB attribute definitions.
--
-- Instances are expected to enforce the invariant that resulting definition's
-- attribute names are unique.
--
-- /See:/ 'UniqueAttributes'.
class DynamoAttributes a where
    -- | Retrieve the non-empty list of 'AttributeDefinition's.
    getAttributes :: Proxy a -> NonEmpty AttributeDefinition

instance ( UniqueAttributes a
         , DynamoAttributes a
         ) => DynamoAttributes (Table n a t s i) where
    getAttributes _ = getAttributes (Proxy :: Proxy a)

instance ( DynamoAttributes a
         , DynamoAttributes b
         ) => DynamoAttributes (a :# b) where
    getAttributes _ =
           getAttributes (Proxy :: Proxy a)
        <> getAttributes (Proxy :: Proxy b)

instance ( KnownSymbol      n
         , DynamoScalarType h
         ) => DynamoAttributes (PartitionKey n h) where
    getAttributes _ =
        pure $ attributeDefinition
            (symbolText    (Proxy :: Proxy n))
            (getScalarType (Proxy :: Proxy h))

instance ( KnownSymbol      n
         , DynamoScalarType r
         ) => DynamoAttributes (SortKey n r) where
    getAttributes _ =
        pure $ attributeDefinition
            (symbolText    (Proxy :: Proxy n))
            (getScalarType (Proxy :: Proxy r))

instance ( KnownSymbol      n
         , DynamoScalarType v
         ) => DynamoAttributes (Attribute n v) where
    getAttributes _ =
        pure $ attributeDefinition
            (symbolText    (Proxy :: Proxy n))
            (getScalarType (Proxy :: Proxy v))
