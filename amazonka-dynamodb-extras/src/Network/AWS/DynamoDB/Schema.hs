{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Network.AWS.DynamoDB.Schema
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module allows you to define a DynamoDB type-level table schema,
-- that is separate from your domain's data types.
--
-- It provides type and value-level functions that take the schema as a
-- parameter to ensure your de/serialization logic, queries, and index
-- projections are checked at compile time.
module Network.AWS.DynamoDB.Schema
    (
    -- * Usage
    -- $usage

    -- * Defining a Table Schema
      Table
    , AttributeKind      (..)
    , ThroughputKind     (..)
    , StreamingKind      (..)
    , SecondaryIndexKind (..)

    -- * Working with Tables

    -- * Working with Indexes

    -- * Working with Serializers

    -- * Items
    , DynamoItem  (..)

    -- * Values
    , DynamoValue (..)
    , Value
    ) where

import Network.AWS.DynamoDB.Item
import Network.AWS.DynamoDB.Value

import Network.AWS.DynamoDB.Schema.Attribute
import Network.AWS.DynamoDB.Schema.Key
import Network.AWS.DynamoDB.Schema.Types

import Data.ByteString      (ByteString)
import Data.Proxy
import Data.Text            (Text)
import Network.AWS.DynamoDB hiding (GlobalSecondaryIndex, LocalSecondaryIndex)

import Network.AWS.DynamoDB.Schema.Index
import Network.AWS.DynamoDB.Schema.Key
import Network.AWS.DynamoDB.Schema.Serializer
import Network.AWS.DynamoDB.Schema.Stream
import Network.AWS.DynamoDB.Schema.Table
import Network.AWS.DynamoDB.Schema.Throughput

{- $usage
@
type Example =
    Table "credentials"
        ( PartitionKey "name"     Text
       :# SortKey      "version"  Integer
       :# Attribute    "revision" ByteString
       :# Attribute    "contents" Int
        )

        ( Throughput (ReadCapacity 1) (WriteCapacity 1) )
        ( Streaming 'SVTKeysOnly )

       '[ GlobalSecondaryIndex "revision"
             ( IndexPartitionKey "name"
            :# IndexSortKey      "revision"
             )
             ( Throughput (ReadCapacity 1) (WriteCapacity 1) )

        , LocalSecondaryIndex "version"
             ( IndexSortKey   "contents"
            :# IndexAttribute "name"
             )
        ]

example :: Proxy Example
example = Proxy
@
-}

type Example =
    Table "credentials"
        ( PartitionKey "name"     Text
       :# SortKey      "version"  Integer
       :# Attribute    "revision" ByteString
       :# Attribute    "contents" Integer
        )

        ( Throughput (ReadCapacity 1) (WriteCapacity 1) )
        ( Streaming 'SVTKeysOnly )

       '[ GlobalSecondaryIndex "revision"
             ( IndexPartitionKey "name"
            :# IndexSortKey      "revision"
             )
             ( Throughput (ReadCapacity 1) (WriteCapacity 1) )

        , LocalSecondaryIndex "version"
             ( IndexSortKey   "contents"
            :# IndexAttribute "name"
             )
        ]

example :: Proxy Example
example = Proxy
