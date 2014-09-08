{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.V2012_08_10.UpdateTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the provisioned throughput for the given table. Setting the
-- throughput for a table helps you manage performance and is part of the
-- provisioned throughput feature of DynamoDB. The provisioned throughput
-- values can be upgraded or downgraded based on the maximums and minimums
-- listed in the Limits section in the Amazon DynamoDB Developer Guide. The
-- table must be in the ACTIVE state for this operation to succeed.
-- UpdateTable is an asynchronous operation; while executing the operation,
-- the table is in the UPDATING state. While the table is in the UPDATING
-- state, the table still has the provisioned throughput from before the call.
-- The new provisioned throughput setting is in effect only when the table
-- returns to the ACTIVE state after the UpdateTable operation. You cannot
-- add, modify or delete indexes using UpdateTable. Indexes can only be
-- defined at table creation time. Modify Provisioned Write Throughput This
-- example changes both the provisioned read and write throughput of the
-- Thread table to 10 capacity units. { "TableDescription": {
-- "AttributeDefinitions": [ { "AttributeName": "ForumName", "AttributeType":
-- "S" }, { "AttributeName": "LastPostDateTime", "AttributeType": "S" }, {
-- "AttributeName": "Subject", "AttributeType": "S" } ], "CreationDateTime":
-- 1.363801528686E9, "ItemCount": 0, "KeySchema": [ { "AttributeName":
-- "ForumName", "KeyType": "HASH" }, { "AttributeName": "Subject", "KeyType":
-- "RANGE" } ], "LocalSecondaryIndexes": [ { "IndexName": "LastPostIndex",
-- "IndexSizeBytes": 0, "ItemCount": 0, "KeySchema": [ { "AttributeName":
-- "ForumName", "KeyType": "HASH" }, { "AttributeName": "LastPostDateTime",
-- "KeyType": "RANGE" } ], "Projection": { "ProjectionType": "KEYS_ONLY" } }
-- ], "ProvisionedThroughput": { "LastIncreaseDateTime": 1.363801701282E9,
-- "NumberOfDecreasesToday": 0, "ReadCapacityUnits": 5, "WriteCapacityUnits":
-- 5 }, "TableName": "Thread", "TableSizeBytes": 0, "TableStatus": "UPDATING"
-- } }.
module Network.AWS.DynamoDB.V2012_08_10.UpdateTable
    (
    -- * Request
      UpdateTable
    -- ** Request constructor
    , mkUpdateTable
    -- ** Request lenses
    , utTableName
    , utProvisionedThroughput
    , utGlobalSecondaryIndexUpdates

    -- * Response
    , UpdateTableResponse
    -- ** Response lenses
    , utrTableDescription
    ) where

import Network.AWS.DynamoDB.V2012_08_10.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input of an UpdateTable operation.
data UpdateTable = UpdateTable
    { _utTableName :: Text
    , _utProvisionedThroughput :: Maybe ProvisionedThroughput
    , _utGlobalSecondaryIndexUpdates :: [GlobalSecondaryIndexUpdate]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateTable' request.
mkUpdateTable :: Text -- ^ 'utTableName'
              -> UpdateTable
mkUpdateTable p1 = UpdateTable
    { _utTableName = p1
    , _utProvisionedThroughput = Nothing
    , _utGlobalSecondaryIndexUpdates = mempty
    }

-- | The name of the table to be updated.
utTableName :: Lens' UpdateTable Text
utTableName = lens _utTableName (\s a -> s { _utTableName = a })

-- | Represents the provisioned throughput settings for a specified table or
-- index. The settings can be modified using the UpdateTable operation. For
-- current minimum and maximum provisioned throughput values, see Limits in
-- the Amazon DynamoDB Developer Guide.
utProvisionedThroughput :: Lens' UpdateTable (Maybe ProvisionedThroughput)
utProvisionedThroughput =
    lens _utProvisionedThroughput
         (\s a -> s { _utProvisionedThroughput = a })

-- | An array of one or more global secondary indexes on the table, together
-- with provisioned throughput settings for each index.
utGlobalSecondaryIndexUpdates :: Lens' UpdateTable [GlobalSecondaryIndexUpdate]
utGlobalSecondaryIndexUpdates =
    lens _utGlobalSecondaryIndexUpdates
         (\s a -> s { _utGlobalSecondaryIndexUpdates = a })

instance ToPath UpdateTable

instance ToQuery UpdateTable

instance ToHeaders UpdateTable

instance ToJSON UpdateTable

-- | Represents the output of an UpdateTable operation.
newtype UpdateTableResponse = UpdateTableResponse
    { _utrTableDescription :: Maybe TableDescription
    } deriving (Show, Generic)

-- | Represents the properties of a table.
utrTableDescription :: Lens' UpdateTableResponse (Maybe TableDescription)
utrTableDescription =
    lens _utrTableDescription (\s a -> s { _utrTableDescription = a })

instance FromJSON UpdateTableResponse

instance AWSRequest UpdateTable where
    type Sv UpdateTable = DynamoDB
    type Rs UpdateTable = UpdateTableResponse

    request = get
    response _ = jsonResponse
