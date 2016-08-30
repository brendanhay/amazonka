{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

-- |
-- Module      : Network.AWS.DynamoDB.Schema.Types
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Types for specifing DynamoDB tables via a type-level DSL.
module Network.AWS.DynamoDB.Schema.Types
    (
    -- * Usage
    -- $usage

    -- * Table Schema
      Table
    , Schema

    -- ** Attributes
    , AttributeKind      (..)

    , PartitionKey
    , SortKey
    , Attribute
    ,                    (:#)

    -- ** Provisioned Throughput
    , ThroughputKind     (..)
    , ReadCapacityKind   (..)
    , WriteCapacityKind  (..)

    , Throughput
    , ReadCapacity
    , WriteCapacity

    -- ** DynamoDB Streams
    , StreamingKind      (..)

    , StreamingDisabled
    , Streaming

    -- ** Indexes
    , SecondaryIndexKind (..)
    , GlobalSecondaryIndex
    , LocalSecondaryIndex

    -- *** Index Attributes
    , Project
    , IndexPartitionKey
    , IndexSortKey
    , IndexAttribute

    -- * Miscellaneous
    , KnownSymbols (..)
    , symbolText
    ) where

import Data.Proxy (Proxy (..))
import Data.Text  (Text)

import GHC.TypeLits

import Network.AWS.DynamoDB (StreamViewType)

import qualified Data.Text as Text

{- $usage

Something about type/constructor promotion and usage.

-}

-- | A DynamoDB table schema.
--
-- The kinds of the parameters are:
--
--    * The 'Symbol' table name.
--    * An attribute schema defined using the desired 'AttributeKind's. ':#' can be used for composition.
--    * The provisioned throughput specified by 'ThroughputKind'.
--    * The streaming specification settings specified by 'StreamingKind'.
--    * An optionally empty list of indexes defined using  'SecondaryIndexKind'.
--
data Table
    (name       :: Symbol)
    (attributes :: AttributeKind)
    (throughput :: ThroughputKind)
    (streaming  :: StreamingKind)
    (indexes    :: [SecondaryIndexKind])

-- | A type-level wrapper for passing the table's attribute schema
-- through type-class instance heads.
data Schema (schema :: AttributeKind) a

-- | A DynamoDB table key or attribute.
--
-- This kind specifies the set of available keys and attributes.
-- The constructors are promoted to the type-level and unticked aliases are
-- also exported.
data AttributeKind
    = forall hash  . PartitionKey Symbol hash
    | forall range . SortKey      Symbol range
    | forall value . Attribute    Symbol value
    | AttributeKind :# AttributeKind

-- NamingRules
-- http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.NamingRules

-- The following characters have special meanings in DynamoDB: # (hash) and : (colon)

-- Tables, attributes, and other objects in DynamoDB must have names. Names should be meaningful and concise—for example, names such as Products, Books, and Authors are self-explanatory.

-- The following are the naming rules for DynamoDB:

-- All names must be encoded using UTF-8, and are case-sensitive.
-- Table names and index names must be between 3 and 255 characters long, and can contain only the following characters:
-- a-z
-- A-Z
-- 0-9
-- _ (underscore)
-- - (dash)
-- . (dot)
-- Attribute names must be between 1 and 255 characters long.

-- http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html

-- Choosing a Partition Key
--
-- The following table compares some common partition key schemas for
-- provisioned throughput efficiency:

-- The primary key uniquely identifies each item in a table. The primary key can be simple (partition key) or composite (partition key and sort key).

-- http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.Partitions.html

-- Guidelines for tables:
-- http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GuidelinesForTables.html

type PartitionKey = 'PartitionKey
type SortKey      = 'SortKey
type Attribute    = 'Attribute

infixr 8 :#

type a :# b = a ':# b

-- -- | 'AttributeKind's must be specified as a type-level non-empty list.
-- --
-- -- For example a single attribute can be specified simply as:
-- --
-- -- @
-- -- PartitionKey "name" Text
-- -- @
-- --
-- -- But two or more attributes must composed:
-- --
-- -- @
-- --  ( PartitionKey "name"    Text
-- -- :# SortKey      "version" Integer
-- -- :# Attribute    "content" ByteString
-- --  )
-- -- @
-- --
-- data a :# b

data ReadCapacityKind = ReadCapacity Nat

-- | One read capacity unit represents one strongly consistent read per second, or
-- two eventually consistent reads per second, for items up to 4 KB in size.
--
-- If you need to read an item that is larger than 4 KB, DynamoDB will need to
-- consume additional read capacity units. The total number of read capacity units
-- required depends on the item size, and whether you want an eventually
-- consistent or strongly consistent read.
--
-- The type parameter is of kind 'Nat'.
type ReadCapacity = 'ReadCapacity

data WriteCapacityKind = WriteCapacity Nat

-- | One write capacity unit represents one write per second for items up
-- to 1 KB in size.
--
-- If you need to write an item that is larger than 1 KB, DynamoDB will need
-- to consume additional write capacity units. The total number of write capacity
-- units required depends on the item size.
--
-- The type parameter is of kind 'Nat'.
type WriteCapacity = 'WriteCapacity

-- | A promoted kind and types for specifying provisioned throughput capacity.
data ThroughputKind = Throughput ReadCapacityKind WriteCapacityKind

-- | The provisioned throughput capacity you want to reserve for reads and writes.
--
-- DynamoDB will reserve the necessary resources to meet your throughput needs
-- while ensuring consistent, low-latency performance. You can also change your
-- provisioned throughput settings, increasing or decreasing capacity as needed.
--
-- The types parameters are of kind 'ReadCapacity' and 'WriteCapacity', respectively.
--
-- Regarding 'PartitionKey's,
--
-- @
-- Total Provisioned Throughput / Partitions = Throughput Per Partition
-- @
--
-- /See:/ <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ProvisionedThroughput.html How it works - Provisioned Throughput>.
type Throughput = 'Throughput

-- |
--
-- http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Streams.html
data StreamingKind
    = StreamingDisabled
    | Streaming StreamViewType

type StreamingDisabled = 'StreamingDisabled
type Streaming         = 'Streaming

data SecondaryIndexKind
    = GlobalSecondaryIndex Symbol AttributeKind ThroughputKind
    | LocalSecondaryIndex  Symbol AttributeKind

-- | Every global secondary index must have a partition key and can also
-- have an optional sort key.
-- The index key schema can be different from the table schema
--
-- http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GSI.html
type GlobalSecondaryIndex = 'GlobalSecondaryIndex

-- <http://docs.aws.amazon.com/amazondynamodb/latt:DynamoAttributesest/APIReference/API_GlobalSecondaryIndex.html#DDB-Type-GlobalSecondaryIndex-KeySchema GlobalSecondaryIndex>
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_LocalSecondaryIndex.html#DDB-Type-LocalSecondaryIndex-KeySchema LocalSecondaryIndex>

-- | Every local secondary index must meet the following conditions:
--
-- The partition key is the same as that of the source table.
-- The sort key consists of exactly one scalar attribute.
-- The sort key of the source table is projected into the index, where it acts as
-- a non-key attribute.
--
-- http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LSI.html
type LocalSecondaryIndex = 'LocalSecondaryIndex

-- | Project the attribute into the index.
data Project

type IndexPartitionKey name = PartitionKey name Project
type IndexSortKey      name = SortKey      name Project
type IndexAttribute    name = Attribute    name Project

class KnownSymbols a where
    symbolTexts :: Proxy a -> [Text]

instance KnownSymbols '[] where
    symbolTexts = const []

instance (KnownSymbol a, KnownSymbols as) => KnownSymbols (a ': as) where
    symbolTexts _ =
        symbolText (Proxy :: Proxy a) : symbolTexts (Proxy :: Proxy as)

instance KnownSymbol n => KnownSymbols (PartitionKey n h) where
    symbolTexts _ = [symbolText (Proxy :: Proxy n)]

instance KnownSymbol n => KnownSymbols (SortKey n r) where
    symbolTexts _ = [symbolText (Proxy :: Proxy n)]

instance KnownSymbol n => KnownSymbols (Attribute n v) where
    symbolTexts _ = [symbolText (Proxy :: Proxy n)]

instance (KnownSymbols a, KnownSymbols b) => KnownSymbols (a :# b) where
    symbolTexts _ =
        symbolTexts (Proxy :: Proxy a) ++ symbolTexts (Proxy :: Proxy b)

symbolText :: KnownSymbol n => proxy n -> Text
symbolText = Text.pack . symbolVal
