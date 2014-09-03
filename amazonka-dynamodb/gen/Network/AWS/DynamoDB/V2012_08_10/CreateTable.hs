{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.V2012_08_10.CreateTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateTable operation adds a new table to your account. In an AWS
-- account, table names must be unique within each region. That is, you can
-- have two tables with same name if you create the tables in different
-- regions. CreateTable is an asynchronous operation. Upon receiving a
-- CreateTable request, DynamoDB immediately returns a response with a
-- TableStatus of CREATING. After the table is created, DynamoDB sets the
-- TableStatus to ACTIVE. You can perform read and write operations only on an
-- ACTIVE table. If you want to create multiple tables with secondary indexes
-- on them, you must create them sequentially. Only one table with secondary
-- indexes can be in the CREATING state at any given time. You can use the
-- DescribeTable API to check the table status. Create a Table This example
-- creates a table named Thread. The table primary key consists of ForumName
-- (hash) and Subject (range). A local secondary index is also created; its
-- key consists of ForumName (hash) and LastPostDateTime (range). {
-- "TableDescription": { "AttributeDefinitions": [ { "AttributeName":
-- "ForumName", "AttributeType": "S" }, { "AttributeName": "LastPostDateTime",
-- "AttributeType": "S" }, { "AttributeName": "Subject", "AttributeType": "S"
-- } ], "CreationDateTime": 1.36372808007E9, "ItemCount": 0, "KeySchema": [ {
-- "AttributeName": "ForumName", "KeyType": "HASH" }, { "AttributeName":
-- "Subject", "KeyType": "RANGE" } ], "LocalSecondaryIndexes": [ {
-- "IndexName": "LastPostIndex", "IndexSizeBytes": 0, "ItemCount": 0,
-- "KeySchema": [ { "AttributeName": "ForumName", "KeyType": "HASH" }, {
-- "AttributeName": "LastPostDateTime", "KeyType": "RANGE" } ], "Projection":
-- { "ProjectionType": "KEYS_ONLY" } } ], "ProvisionedThroughput": {
-- "NumberOfDecreasesToday": 0, "ReadCapacityUnits": 5, "WriteCapacityUnits":
-- 5 }, "TableName": "Thread", "TableSizeBytes": 0, "TableStatus": "CREATING"
-- } }.
module Network.AWS.DynamoDB.V2012_08_10.CreateTable
    (
    -- * Request
      CreateTable
    -- ** Request constructor
    , createTable
    -- ** Request lenses
    , ctiAttributeDefinitions
    , ctiKeySchema
    , ctiProvisionedThroughput
    , ctiTableName
    , ctiGlobalSecondaryIndexes
    , ctiLocalSecondaryIndexes

    -- * Response
    , CreateTableResponse
    -- ** Response lenses
    , ctoTableDescription
    ) where

import           Network.AWS.DynamoDB.V2012_08_10.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreateTable' request.
createTable :: [AttributeDefinition] -- ^ 'ctiAttributeDefinitions'
            -> [KeySchemaElement] -- ^ 'ctiKeySchema'
            -> ProvisionedThroughput -- ^ 'ctiProvisionedThroughput'
            -> Text -- ^ 'ctiTableName'
            -> CreateTable
createTable p1 p2 p3 p4 = CreateTable
    { _ctiAttributeDefinitions = p1
    , _ctiKeySchema = p2
    , _ctiProvisionedThroughput = p3
    , _ctiTableName = p4
    , _ctiGlobalSecondaryIndexes = mempty
    , _ctiLocalSecondaryIndexes = mempty
    }

data CreateTable = CreateTable
    { _ctiAttributeDefinitions :: [AttributeDefinition]
      -- ^ An array of attributes that describe the key schema for the table
      -- and indexes.
    , _ctiKeySchema :: [KeySchemaElement]
      -- ^ Specifies the attributes that make up the primary key for a table
      -- or an index. The attributes in KeySchema must also be defined in
      -- the AttributeDefinitions array. For more information, see Data
      -- Model in the Amazon DynamoDB Developer Guide. Each
      -- KeySchemaElement in the array is composed of: AttributeName - The
      -- name of this key attribute. KeyType - Determines whether the key
      -- attribute is HASH or RANGE. For a primary key that consists of a
      -- hash attribute, you must specify exactly one element with a
      -- KeyType of HASH. For a primary key that consists of hash and
      -- range attributes, you must specify exactly two elements, in this
      -- order: The first element must have a KeyType of HASH, and the
      -- second element must have a KeyType of RANGE. For more
      -- information, see Specifying the Primary Key in the Amazon
      -- DynamoDB Developer Guide.
    , _ctiProvisionedThroughput :: ProvisionedThroughput
      -- ^ Represents the provisioned throughput settings for a specified
      -- table or index. The settings can be modified using the
      -- UpdateTable operation. For current minimum and maximum
      -- provisioned throughput values, see Limits in the Amazon DynamoDB
      -- Developer Guide.
    , _ctiTableName :: Text
      -- ^ The name of the table to create.
    , _ctiGlobalSecondaryIndexes :: [GlobalSecondaryIndex]
      -- ^ One or more global secondary indexes (the maximum is five) to be
      -- created on the table. Each global secondary index in the array
      -- includes the following: IndexName - The name of the global
      -- secondary index. Must be unique only for this table. KeySchema -
      -- Specifies the key schema for the global secondary index.
      -- Projection - Specifies attributes that are copied (projected)
      -- from the table into the index. These are in addition to the
      -- primary key attributes and index key attributes, which are
      -- automatically projected. Each attribute specification is composed
      -- of: ProjectionType - One of the following: KEYS_ONLY - Only the
      -- index and primary keys are projected into the index. INCLUDE -
      -- Only the specified table attributes are projected into the index.
      -- The list of projected attributes are in NonKeyAttributes. ALL -
      -- All of the table attributes are projected into the index.
      -- NonKeyAttributes - A list of one or more non-key attribute names
      -- that are projected into the secondary index. The total count of
      -- attributes specified in NonKeyAttributes, summed across all of
      -- the secondary indexes, must not exceed 20. If you project the
      -- same attribute into two different indexes, this counts as two
      -- distinct attributes when determining the total.
      -- ProvisionedThroughput - The provisioned throughput settings for
      -- the global secondary index, consisting of read and write capacity
      -- units.
    , _ctiLocalSecondaryIndexes :: [LocalSecondaryIndex]
      -- ^ One or more local secondary indexes (the maximum is five) to be
      -- created on the table. Each index is scoped to a given hash key
      -- value. There is a 10 GB size limit per hash key; otherwise, the
      -- size of a local secondary index is unconstrained. Each local
      -- secondary index in the array includes the following: IndexName -
      -- The name of the local secondary index. Must be unique only for
      -- this table. KeySchema - Specifies the key schema for the local
      -- secondary index. The key schema must begin with the same hash key
      -- attribute as the table. Projection - Specifies attributes that
      -- are copied (projected) from the table into the index. These are
      -- in addition to the primary key attributes and index key
      -- attributes, which are automatically projected. Each attribute
      -- specification is composed of: ProjectionType - One of the
      -- following: KEYS_ONLY - Only the index and primary keys are
      -- projected into the index. INCLUDE - Only the specified table
      -- attributes are projected into the index. The list of projected
      -- attributes are in NonKeyAttributes. ALL - All of the table
      -- attributes are projected into the index. NonKeyAttributes - A
      -- list of one or more non-key attribute names that are projected
      -- into the secondary index. The total count of attributes specified
      -- in NonKeyAttributes, summed across all of the secondary indexes,
      -- must not exceed 20. If you project the same attribute into two
      -- different indexes, this counts as two distinct attributes when
      -- determining the total.
    } deriving (Show, Generic)

-- | An array of attributes that describe the key schema for the table and
-- indexes.
ctiAttributeDefinitions
    :: Functor f
    => ([AttributeDefinition]
    -> f ([AttributeDefinition]))
    -> CreateTable
    -> f CreateTable
ctiAttributeDefinitions f x =
    (\y -> x { _ctiAttributeDefinitions = y })
       <$> f (_ctiAttributeDefinitions x)
{-# INLINE ctiAttributeDefinitions #-}

-- | Specifies the attributes that make up the primary key for a table or an
-- index. The attributes in KeySchema must also be defined in the
-- AttributeDefinitions array. For more information, see Data Model in the
-- Amazon DynamoDB Developer Guide. Each KeySchemaElement in the array is
-- composed of: AttributeName - The name of this key attribute. KeyType -
-- Determines whether the key attribute is HASH or RANGE. For a primary key
-- that consists of a hash attribute, you must specify exactly one element
-- with a KeyType of HASH. For a primary key that consists of hash and range
-- attributes, you must specify exactly two elements, in this order: The first
-- element must have a KeyType of HASH, and the second element must have a
-- KeyType of RANGE. For more information, see Specifying the Primary Key in
-- the Amazon DynamoDB Developer Guide.
ctiKeySchema
    :: Functor f
    => ([KeySchemaElement]
    -> f ([KeySchemaElement]))
    -> CreateTable
    -> f CreateTable
ctiKeySchema f x =
    (\y -> x { _ctiKeySchema = y })
       <$> f (_ctiKeySchema x)
{-# INLINE ctiKeySchema #-}

-- | Represents the provisioned throughput settings for a specified table or
-- index. The settings can be modified using the UpdateTable operation. For
-- current minimum and maximum provisioned throughput values, see Limits in
-- the Amazon DynamoDB Developer Guide.
ctiProvisionedThroughput
    :: Functor f
    => (ProvisionedThroughput
    -> f (ProvisionedThroughput))
    -> CreateTable
    -> f CreateTable
ctiProvisionedThroughput f x =
    (\y -> x { _ctiProvisionedThroughput = y })
       <$> f (_ctiProvisionedThroughput x)
{-# INLINE ctiProvisionedThroughput #-}

-- | The name of the table to create.
ctiTableName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateTable
    -> f CreateTable
ctiTableName f x =
    (\y -> x { _ctiTableName = y })
       <$> f (_ctiTableName x)
{-# INLINE ctiTableName #-}

-- | One or more global secondary indexes (the maximum is five) to be created on
-- the table. Each global secondary index in the array includes the following:
-- IndexName - The name of the global secondary index. Must be unique only for
-- this table. KeySchema - Specifies the key schema for the global secondary
-- index. Projection - Specifies attributes that are copied (projected) from
-- the table into the index. These are in addition to the primary key
-- attributes and index key attributes, which are automatically projected.
-- Each attribute specification is composed of: ProjectionType - One of the
-- following: KEYS_ONLY - Only the index and primary keys are projected into
-- the index. INCLUDE - Only the specified table attributes are projected into
-- the index. The list of projected attributes are in NonKeyAttributes. ALL -
-- All of the table attributes are projected into the index. NonKeyAttributes
-- - A list of one or more non-key attribute names that are projected into the
-- secondary index. The total count of attributes specified in
-- NonKeyAttributes, summed across all of the secondary indexes, must not
-- exceed 20. If you project the same attribute into two different indexes,
-- this counts as two distinct attributes when determining the total.
-- ProvisionedThroughput - The provisioned throughput settings for the global
-- secondary index, consisting of read and write capacity units.
ctiGlobalSecondaryIndexes
    :: Functor f
    => ([GlobalSecondaryIndex]
    -> f ([GlobalSecondaryIndex]))
    -> CreateTable
    -> f CreateTable
ctiGlobalSecondaryIndexes f x =
    (\y -> x { _ctiGlobalSecondaryIndexes = y })
       <$> f (_ctiGlobalSecondaryIndexes x)
{-# INLINE ctiGlobalSecondaryIndexes #-}

-- | One or more local secondary indexes (the maximum is five) to be created on
-- the table. Each index is scoped to a given hash key value. There is a 10 GB
-- size limit per hash key; otherwise, the size of a local secondary index is
-- unconstrained. Each local secondary index in the array includes the
-- following: IndexName - The name of the local secondary index. Must be
-- unique only for this table. KeySchema - Specifies the key schema for the
-- local secondary index. The key schema must begin with the same hash key
-- attribute as the table. Projection - Specifies attributes that are copied
-- (projected) from the table into the index. These are in addition to the
-- primary key attributes and index key attributes, which are automatically
-- projected. Each attribute specification is composed of: ProjectionType -
-- One of the following: KEYS_ONLY - Only the index and primary keys are
-- projected into the index. INCLUDE - Only the specified table attributes are
-- projected into the index. The list of projected attributes are in
-- NonKeyAttributes. ALL - All of the table attributes are projected into the
-- index. NonKeyAttributes - A list of one or more non-key attribute names
-- that are projected into the secondary index. The total count of attributes
-- specified in NonKeyAttributes, summed across all of the secondary indexes,
-- must not exceed 20. If you project the same attribute into two different
-- indexes, this counts as two distinct attributes when determining the total.
ctiLocalSecondaryIndexes
    :: Functor f
    => ([LocalSecondaryIndex]
    -> f ([LocalSecondaryIndex]))
    -> CreateTable
    -> f CreateTable
ctiLocalSecondaryIndexes f x =
    (\y -> x { _ctiLocalSecondaryIndexes = y })
       <$> f (_ctiLocalSecondaryIndexes x)
{-# INLINE ctiLocalSecondaryIndexes #-}

instance ToPath CreateTable

instance ToQuery CreateTable

instance ToHeaders CreateTable

instance ToJSON CreateTable

data CreateTableResponse = CreateTableResponse
    { _ctoTableDescription :: Maybe TableDescription
      -- ^ Represents the properties of a table.
    } deriving (Show, Generic)

-- | Represents the properties of a table.
ctoTableDescription
    :: Functor f
    => (Maybe TableDescription
    -> f (Maybe TableDescription))
    -> CreateTableResponse
    -> f CreateTableResponse
ctoTableDescription f x =
    (\y -> x { _ctoTableDescription = y })
       <$> f (_ctoTableDescription x)
{-# INLINE ctoTableDescription #-}

instance FromJSON CreateTableResponse

instance AWSRequest CreateTable where
    type Sv CreateTable = DynamoDB
    type Rs CreateTable = CreateTableResponse

    request = get
    response _ = jsonResponse
