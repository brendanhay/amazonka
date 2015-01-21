{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.CreateTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /CreateTable/ operation adds a new table to your account. In an AWS
-- account, table names must be unique within each region. That is, you can have
-- two tables with same name if you create the tables in different regions.
--
-- /CreateTable/ is an asynchronous operation. Upon receiving a /CreateTable/
-- request, DynamoDB immediately returns a response with a /TableStatus/ of 'CREATING'. After the table is created, DynamoDB sets the /TableStatus/ to 'ACTIVE'. You
-- can perform read and write operations only on an 'ACTIVE' table.
--
-- If you want to create multiple tables with secondary indexes on them, you
-- must create them sequentially. Only one table with secondary indexes can be
-- in the 'CREATING' state at any given time.
--
-- You can use the /DescribeTable/ API to check the table status.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_CreateTable.html>
module Network.AWS.DynamoDB.CreateTable
    (
    -- * Request
      CreateTable
    -- ** Request constructor
    , createTable
    -- ** Request lenses
    , ctAttributeDefinitions
    , ctGlobalSecondaryIndexes
    , ctKeySchema
    , ctLocalSecondaryIndexes
    , ctProvisionedThroughput
    , ctTableName

    -- * Response
    , CreateTableResponse
    -- ** Response constructor
    , createTableResponse
    -- ** Response lenses
    , ctrTableDescription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DynamoDB.Types
import qualified GHC.Exts

data CreateTable = CreateTable
    { _ctAttributeDefinitions   :: List "AttributeDefinitions" AttributeDefinition
    , _ctGlobalSecondaryIndexes :: List "GlobalSecondaryIndexes" GlobalSecondaryIndex
    , _ctKeySchema              :: List1 "KeySchema" KeySchemaElement
    , _ctLocalSecondaryIndexes  :: List "LocalSecondaryIndexes" LocalSecondaryIndex
    , _ctProvisionedThroughput  :: ProvisionedThroughput
    , _ctTableName              :: Text
    } deriving (Eq, Read, Show)

-- | 'CreateTable' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctAttributeDefinitions' @::@ ['AttributeDefinition']
--
-- * 'ctGlobalSecondaryIndexes' @::@ ['GlobalSecondaryIndex']
--
-- * 'ctKeySchema' @::@ 'NonEmpty' 'KeySchemaElement'
--
-- * 'ctLocalSecondaryIndexes' @::@ ['LocalSecondaryIndex']
--
-- * 'ctProvisionedThroughput' @::@ 'ProvisionedThroughput'
--
-- * 'ctTableName' @::@ 'Text'
--
createTable :: Text -- ^ 'ctTableName'
            -> NonEmpty KeySchemaElement -- ^ 'ctKeySchema'
            -> ProvisionedThroughput -- ^ 'ctProvisionedThroughput'
            -> CreateTable
createTable p1 p2 p3 = CreateTable
    { _ctTableName              = p1
    , _ctKeySchema              = withIso _List1 (const id) p2
    , _ctProvisionedThroughput  = p3
    , _ctAttributeDefinitions   = mempty
    , _ctLocalSecondaryIndexes  = mempty
    , _ctGlobalSecondaryIndexes = mempty
    }

-- | An array of attributes that describe the key schema for the table and indexes.
ctAttributeDefinitions :: Lens' CreateTable [AttributeDefinition]
ctAttributeDefinitions =
    lens _ctAttributeDefinitions (\s a -> s { _ctAttributeDefinitions = a })
        . _List

-- | One or more global secondary indexes (the maximum is five) to be created on
-- the table. Each global secondary index in the array includes the following:
--
-- /IndexName/ - The name of the global secondary index. Must be unique only
-- for this table.
--
--
--
-- /KeySchema/ - Specifies the key schema for the global secondary index.
--
-- /Projection/ - Specifies attributes that are copied (projected) from the
-- table into the index. These are in addition to the primary key attributes and
-- index key attributes, which are automatically projected. Each attribute
-- specification is composed of:
--
-- /ProjectionType/ - One of the following:
--
-- 'KEYS_ONLY' - Only the index and primary keys are projected into the index.
--
-- 'INCLUDE' - Only the specified table attributes are projected into the
-- index. The list of projected attributes are in /NonKeyAttributes/.
--
-- 'ALL' - All of the table attributes are projected into the index.
--
-- /NonKeyAttributes/ - A list of one or more non-key attribute names that
-- are projected into the secondary index. The total count of attributes
-- specified in /NonKeyAttributes/, summed across all of the secondary indexes,
-- must not exceed 20. If you project the same attribute into two different
-- indexes, this counts as two distinct attributes when determining the total.
--
-- /ProvisionedThroughput/ - The provisioned throughput settings for the
-- global secondary index, consisting of read and write capacity units.
--
--
ctGlobalSecondaryIndexes :: Lens' CreateTable [GlobalSecondaryIndex]
ctGlobalSecondaryIndexes =
    lens _ctGlobalSecondaryIndexes
        (\s a -> s { _ctGlobalSecondaryIndexes = a })
            . _List

-- | Specifies the attributes that make up the primary key for a table or an
-- index. The attributes in /KeySchema/ must also be defined in the /AttributeDefinitions/ array. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html Data Model> in the /Amazon DynamoDB DeveloperGuide/.
--
-- Each /KeySchemaElement/ in the array is composed of:
--
-- /AttributeName/ - The name of this key attribute.
--
-- /KeyType/ - Determines whether the key attribute is 'HASH' or 'RANGE'.
--
-- For a primary key that consists of a hash attribute, you must specify
-- exactly one element with a /KeyType/ of 'HASH'.
--
-- For a primary key that consists of hash and range attributes, you must
-- specify exactly two elements, in this order: The first element must have a /KeyType/ of 'HASH', and the second element must have a /KeyType/ of 'RANGE'.
--
-- For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#WorkingWithTables.primary.key Specifying the Primary Key> in the /Amazon DynamoDBDeveloper Guide/.
ctKeySchema :: Lens' CreateTable (NonEmpty KeySchemaElement)
ctKeySchema = lens _ctKeySchema (\s a -> s { _ctKeySchema = a }) . _List1

-- | One or more local secondary indexes (the maximum is five) to be created on
-- the table. Each index is scoped to a given hash key value. There is a 10 GB
-- size limit per hash key; otherwise, the size of a local secondary index is
-- unconstrained.
--
-- Each local secondary index in the array includes the following:
--
-- /IndexName/ - The name of the local secondary index. Must be unique only for
-- this table.
--
--
--
-- /KeySchema/ - Specifies the key schema for the local secondary index. The
-- key schema must begin with the same hash key attribute as the table.
--
-- /Projection/ - Specifies attributes that are copied (projected) from the
-- table into the index. These are in addition to the primary key attributes and
-- index key attributes, which are automatically projected. Each attribute
-- specification is composed of:
--
-- /ProjectionType/ - One of the following:
--
-- 'KEYS_ONLY' - Only the index and primary keys are projected into the index.
--
-- 'INCLUDE' - Only the specified table attributes are projected into the
-- index. The list of projected attributes are in /NonKeyAttributes/.
--
-- 'ALL' - All of the table attributes are projected into the index.
--
-- /NonKeyAttributes/ - A list of one or more non-key attribute names that
-- are projected into the secondary index. The total count of attributes
-- specified in /NonKeyAttributes/, summed across all of the secondary indexes,
-- must not exceed 20. If you project the same attribute into two different
-- indexes, this counts as two distinct attributes when determining the total.
--
--
ctLocalSecondaryIndexes :: Lens' CreateTable [LocalSecondaryIndex]
ctLocalSecondaryIndexes =
    lens _ctLocalSecondaryIndexes (\s a -> s { _ctLocalSecondaryIndexes = a })
        . _List

ctProvisionedThroughput :: Lens' CreateTable ProvisionedThroughput
ctProvisionedThroughput =
    lens _ctProvisionedThroughput (\s a -> s { _ctProvisionedThroughput = a })

-- | The name of the table to create.
ctTableName :: Lens' CreateTable Text
ctTableName = lens _ctTableName (\s a -> s { _ctTableName = a })

newtype CreateTableResponse = CreateTableResponse
    { _ctrTableDescription :: Maybe TableDescription
    } deriving (Eq, Read, Show)

-- | 'CreateTableResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctrTableDescription' @::@ 'Maybe' 'TableDescription'
--
createTableResponse :: CreateTableResponse
createTableResponse = CreateTableResponse
    { _ctrTableDescription = Nothing
    }

ctrTableDescription :: Lens' CreateTableResponse (Maybe TableDescription)
ctrTableDescription =
    lens _ctrTableDescription (\s a -> s { _ctrTableDescription = a })

instance ToPath CreateTable where
    toPath = const "/"

instance ToQuery CreateTable where
    toQuery = const mempty

instance ToHeaders CreateTable

instance ToJSON CreateTable where
    toJSON CreateTable{..} = object
        [ "AttributeDefinitions"   .= _ctAttributeDefinitions
        , "TableName"              .= _ctTableName
        , "KeySchema"              .= _ctKeySchema
        , "LocalSecondaryIndexes"  .= _ctLocalSecondaryIndexes
        , "GlobalSecondaryIndexes" .= _ctGlobalSecondaryIndexes
        , "ProvisionedThroughput"  .= _ctProvisionedThroughput
        ]

instance AWSRequest CreateTable where
    type Sv CreateTable = DynamoDB
    type Rs CreateTable = CreateTableResponse

    request  = post "CreateTable"
    response = jsonResponse

instance FromJSON CreateTableResponse where
    parseJSON = withObject "CreateTableResponse" $ \o -> CreateTableResponse
        <$> o .:? "TableDescription"
