{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Amazonka.DynamoDB.Schema
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
module Amazonka.DynamoDB.Schema
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
    ) where

import Amazonka.DynamoDB.Schema.Attribute
import Amazonka.DynamoDB.Schema.Expression
import Amazonka.DynamoDB.Schema.Index
import Amazonka.DynamoDB.Schema.Key
import Amazonka.DynamoDB.Schema.Stream
import Amazonka.DynamoDB.Schema.Table
import Amazonka.DynamoDB.Schema.Throughput

import Data.ByteString (ByteString)
import Data.Proxy
import Data.Text       (Text)

{- $usage
@
type Example =
    Table "credentials"
        ( PartitionKey "name"     ::: Text
       :# SortKey      "version"  ::: Integer
       :# Attribute    "revision" ::: ByteString
       :# Attribute    "contents" ::: Int
        )

        (Throughput (ReadCapacity 1) (WriteCapacity 1))
        (Streaming 'SVTKeysOnly)

       '[ GlobalSecondaryIndex "revision"
             ( PartitionKey "name"
            :# IndexSortKey      "revision"
             ) (Throughput (ReadCapacity 1) (WriteCapacity 1))

        , LocalSecondaryIndex "version"
             ( SortKey   "contents"
            :# Attribute "name"
             )
        ]

example :: Proxy Example
example = Proxy
@
-}

type Example =
    Table "credentials"
        ( PartitionKey "name"     ::: ByteString
       :# SortKey      "version"  ::: Text
       :# Attribute    "revision" ::: Integer
       :# Attribute    "contents" ::: Integer
        )

        (Throughput (ReadCapacity 1) (WriteCapacity 1))
        (Streaming 'SVTKeysOnly)

       '[ GlobalSecondaryIndex "revision"
             ( PartitionKey "name"
            :# SortKey      "revision"
             ) (Throughput (ReadCapacity 1) (WriteCapacity 1))

        , LocalSecondaryIndex "version"
             ( SortKey   "contents"
            :# Attribute "name"
             )
        ]

example :: Proxy Example
example = Proxy

-- scan :: Schema -> Maybe IndexName -> [AttributeNames] || Nothing (All) -> Scan
-- scanfilter
-- projectionexpression/expressionattributenames,expressionattributevalues
-- select
-- filter expressions
-- pagination
-- indexname
-- tablename

-- query :: Proxy a
-- projectionexpression
-- attributestoget
-- expressionattributenames
-- expressionattributevalues
-- filterexpression
-- select
-- keyconditionexpression
-- indexname
-- tablename
