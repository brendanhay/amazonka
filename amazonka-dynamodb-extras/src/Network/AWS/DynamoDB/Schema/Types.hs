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

-- | A DynamoDB table schema.
--
-- The parameters are the table name, the attribute schema, and any additional
-- configuration options.
data Table (name :: Symbol) schema options

data PartitionKey (name :: Symbol) hash
data SortKey      (name :: Symbol) range
data Attribute    (name :: Symbol) value

-- | One read capacity unit represents one strongly consistent read per second, or
-- two eventually consistent reads per second, for items up to 4 KB in size.
--
-- If you need to read an item that is larger than 4 KB, DynamoDB will need to
-- consume additional read capacity units. The total number of read capacity units
-- required depends on the item size, and whether you want an eventually
-- consistent or strongly consistent read.
data ReadCapacity (read  :: Nat)

-- | One write capacity unit represents one write per second for items up
-- to 1 KB in size.
--
-- If you need to write an item that is larger than 1 KB, DynamoDB will need
-- to consume additional write capacity units. The total number of write capacity
-- units required depends on the item size.
data WriteCapacity (write :: Nat)

-- | The provisioned throughput capacity you want to reserve for reads and writes.
--
-- DynamoDB will reserve the necessary resources to meet your throughput needs
-- while ensuring consistent, low-latency performance. You can also change your
-- provisioned throughput settings, increasing or decreasing capacity as needed.
--
-- The parameters are 'ReadCapacity' and 'WriteCapacity', respectively.
data Throughput read write

data Stream (view :: StreamViewType)

data IndexPartitionKey (key :: Symbol)
data IndexSortKey      (key :: Symbol)

-- Every global secondary index must have a partition key and can also
-- have an optional sort key.
-- The index key schema can be different from the table schema
data GlobalSecondaryIndex (name :: Symbol) options

-- Every local secondary index must meet the following conditions:
--
-- The partition key is the same as that of the source table.
-- The sort key consists of exactly one scalar attribute.
-- The sort key of the source table is projected into the index, where it acts as
-- a non-key attribute.
data LocalSecondaryIndex (name :: Symbol) options

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
