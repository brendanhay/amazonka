{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.V2012_08_10.DescribeTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the table, including the current status of the
-- table, when it was created, the primary key schema, and any indexes on the
-- table. Describe a Table This example describes the Thread table. { "Table":
-- { "AttributeDefinitions": [ { "AttributeName": "ForumName",
-- "AttributeType": "S" }, { "AttributeName": "LastPostDateTime",
-- "AttributeType": "S" }, { "AttributeName": "Subject", "AttributeType": "S"
-- } ], "CreationDateTime": 1.363729002358E9, "ItemCount": 0, "KeySchema": [ {
-- "AttributeName": "ForumName", "KeyType": "HASH" }, { "AttributeName":
-- "Subject", "KeyType": "RANGE" } ], "LocalSecondaryIndexes": [ {
-- "IndexName": "LastPostIndex", "IndexSizeBytes": 0, "ItemCount": 0,
-- "KeySchema": [ { "AttributeName": "ForumName", "KeyType": "HASH" }, {
-- "AttributeName": "LastPostDateTime", "KeyType": "RANGE" } ], "Projection":
-- { "ProjectionType": "KEYS_ONLY" } } ], "ProvisionedThroughput": {
-- "NumberOfDecreasesToday": 0, "ReadCapacityUnits": 5, "WriteCapacityUnits":
-- 5 }, "TableName": "Thread", "TableSizeBytes": 0, "TableStatus": "ACTIVE" }
-- }.
module Network.AWS.DynamoDB.V2012_08_10.DescribeTable
    (
    -- * Request
      DescribeTable
    -- ** Request constructor
    , mkDescribeTableInput
    -- ** Request lenses
    , dtjTableName

    -- * Response
    , DescribeTableResponse
    -- ** Response lenses
    , dtpTable
    ) where

import           Network.AWS.DynamoDB.V2012_08_10.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTable' request.
mkDescribeTableInput :: Text -- ^ 'dtjTableName'
                     -> DescribeTable
mkDescribeTableInput p1 = DescribeTable
    { _dtjTableName = p1
    }
{-# INLINE mkDescribeTableInput #-}

newtype DescribeTable = DescribeTable
    { _dtjTableName :: Text
      -- ^ The name of the table to describe.
    } deriving (Show, Generic)

-- | The name of the table to describe.
dtjTableName :: Lens' DescribeTable (Text)
dtjTableName = lens _dtjTableName (\s a -> s { _dtjTableName = a })
{-# INLINE dtjTableName #-}

instance ToPath DescribeTable

instance ToQuery DescribeTable

instance ToHeaders DescribeTable

instance ToJSON DescribeTable

newtype DescribeTableResponse = DescribeTableResponse
    { _dtpTable :: Maybe TableDescription
      -- ^ Represents the properties of a table.
    } deriving (Show, Generic)

-- | Represents the properties of a table.
dtpTable :: Lens' DescribeTableResponse (Maybe TableDescription)
dtpTable = lens _dtpTable (\s a -> s { _dtpTable = a })
{-# INLINE dtpTable #-}

instance FromJSON DescribeTableResponse

instance AWSRequest DescribeTable where
    type Sv DescribeTable = DynamoDB
    type Rs DescribeTable = DescribeTableResponse

    request = get
    response _ = jsonResponse
