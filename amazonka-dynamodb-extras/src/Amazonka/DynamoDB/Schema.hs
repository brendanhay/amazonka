{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

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

import Amazonka.DynamoDB.Item
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
newtype Name     = Name     Text       deriving (DynamoScalarType)
newtype Version  = Version  Integer    deriving (DynamoScalarType)
newtype Revision = Revision Text
newtype Contents = Contents ByteString

type instance DynamoAttributeName Name     = "name"
type instance DynamoAttributeName Version  = "version"
type instance DynamoAttributeName Revision = "revision"
type instance DynamoAttributeName Contents = "contents"

type Example =
    Table "credentials"
        ( PartitionKey Name
       :# SortKey      Version
       :# Attribute    Revision
       :# Attribute    Contents
        )

        (Throughput (ReadCapacity 1) (WriteCapacity 1))
        (Streaming 'SVTKeysOnly)

       '[ GlobalSecondaryIndex "revision"
             ( PartitionKey Name
            :# SortKey      Revision
             ) (Throughput (ReadCapacity 1) (WriteCapacity 1))

        , LocalSecondaryIndex "version"
             ( SortKey   Contents
            :# Attribute Name
             )
        ]

example :: Proxy Example
example = Proxy
@
-}

newtype Name     = Name     Text       deriving (DynamoScalarType)
newtype Version  = Version  Integer    deriving (DynamoScalarType)
newtype Revision = Revision Text
newtype Contents = Contents ByteString

type instance DynamoAttributeName Name     = "name"
type instance DynamoAttributeName Version  = "version"
type instance DynamoAttributeName Revision = "revision"
type instance DynamoAttributeName Contents = "contents"

type Example =
    Table "credentials"
        ( PartitionKey Name
       :# SortKey      Version
       :# Attribute    Revision
       :# Attribute    Contents
        )

        (Throughput (ReadCapacity 1) (WriteCapacity 1))
        (Streaming 'SVTKeysOnly)

       '[ GlobalSecondaryIndex "revision"
             ( PartitionKey Name
            :# SortKey      Revision
             ) (Throughput (ReadCapacity 1) (WriteCapacity 1))

        , LocalSecondaryIndex "version"
             ( SortKey   Contents
            :# Attribute Name
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
