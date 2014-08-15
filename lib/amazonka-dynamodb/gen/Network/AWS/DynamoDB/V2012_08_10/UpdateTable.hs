{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.DynamoDB.V2012_08_10.UpdateTable where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.DynamoDB.V2012_08_10.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'UpdateTable' request.
updateTable :: Text -- ^ '_utiTableName'
            -> UpdateTable
updateTable p1 = UpdateTable
    { _utiTableName = p1
    , _utiGlobalSecondaryIndexUpdates = mempty
    , _utiProvisionedThroughput = Nothing
    }

data UpdateTable = UpdateTable
    { _utiTableName :: Text
      -- ^ The name of the table to be updated.
    , _utiGlobalSecondaryIndexUpdates :: [GlobalSecondaryIndexUpdate]
      -- ^ An array of one or more global secondary indexes on the table,
      -- together with provisioned throughput settings for each index.
    , _utiProvisionedThroughput :: Maybe ProvisionedThroughput
      -- ^ Represents the provisioned throughput settings for a specified
      -- table or index. The settings can be modified using the
      -- UpdateTable operation. For current minimum and maximum
      -- provisioned throughput values, see Limits in the Amazon DynamoDB
      -- Developer Guide.
    } deriving (Show, Generic)

makeLenses ''UpdateTable

instance ToPath UpdateTable

instance ToQuery UpdateTable

instance ToHeaders UpdateTable

instance ToJSON UpdateTable

data UpdateTableResponse = UpdateTableResponse
    { _utoTableDescription :: Maybe TableDescription
      -- ^ Represents the properties of a table.
    } deriving (Show, Generic)

makeLenses ''UpdateTableResponse

instance FromJSON UpdateTableResponse

instance AWSRequest UpdateTable where
    type Sv UpdateTable = DynamoDB
    type Rs UpdateTable = UpdateTableResponse

    request = get
    response _ = jsonResponse
