{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.AWS.DynamoDB.Schema.Types where

import Data.Text          (Text)
import Data.Type.Bool
import Data.Type.Equality

import GHC.TypeLits

import Network.AWS.DynamoDB (ProjectionType, StreamViewType)

import qualified Data.Text as Text

data Table name schema options

data PartitionKey (name :: Symbol) hash
data SortKey      (name :: Symbol) range
data Attribute    (name :: Symbol) value

data ReadCapacity  (read  :: Nat)
data WriteCapacity (write :: Nat)

data Throughput read write

data Stream (view :: StreamViewType)

data IndexPartitionKey key
data IndexSortKey      key

-- Every global secondary index must have a partition key, and can have an optional
-- sort key. The index key schema can be different from the table schema
data GlobalSecondaryIndex name options

-- Every local secondary index must meet the following conditions:
--
-- The partition key is the same as that of the source table.
-- The sort key consists of exactly one scalar attribute.
-- The sort key of the source table is projected into the index, where it acts as
-- a non-key attribute.
data LocalSecondaryIndex name options

-- Projections are not done.
data Project (proj :: ProjectionType)

infixr 8 :#, :*:

-- | A column specifier.
data a :# b

-- | A deserialized product.
data a :*: b = a :*: b
    deriving (Show)

-- | Does the element 'x' exist in the set 'xs'.
type family (∈) x xs :: Bool where
    (∈) x '[]       = 'False
    (∈) x (y ': xs) = x == y || x ∈ xs

-- | All elements of set 'ys' removed from set 'xs'.
type family Difference xs ys where
    Difference '[]       ys        = '[]
    Difference xs       '[]        = xs
    Difference (x ': xs) (x ': ys) = Difference xs ys
    Difference (x ': xs) ys        =
        If (x ∈ ys) (Difference xs ys)
           (x ': Difference xs ys)

-- | Element 'x' removed from set 'xs'.
type family Remove x xs where
    Remove x '[]       = '[]
    Remove x (x ': ys) = ys
    Remove x (y ': ys) = y ': Remove x ys

symbolText :: KnownSymbol n => proxy n -> Text
symbolText = Text.pack . symbolVal
