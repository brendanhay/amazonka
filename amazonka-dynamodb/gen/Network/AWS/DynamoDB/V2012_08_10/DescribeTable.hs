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
    , mkDescribeTable
    -- ** Request lenses
    , dt1TableName

    -- * Response
    , DescribeTableResponse
    -- ** Response lenses
    , dtrsrsTable
    ) where

import           Network.AWS.DynamoDB.V2012_08_10.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Represents the input of a DescribeTable operation.
newtype DescribeTable = DescribeTable
    { _dt1TableName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTable' request.
mkDescribeTable :: Text -- ^ 'dt1TableName'
                -> DescribeTable
mkDescribeTable p1 = DescribeTable
    { _dt1TableName = p1
    }
{-# INLINE mkDescribeTable #-}

-- | The name of the table to describe.
dt1TableName :: Lens' DescribeTable Text
dt1TableName = lens _dt1TableName (\s a -> s { _dt1TableName = a })
{-# INLINE dt1TableName #-}

instance ToPath DescribeTable

instance ToQuery DescribeTable

instance ToHeaders DescribeTable

instance ToJSON DescribeTable

-- | Represents the output of a DescribeTable operation.
newtype DescribeTableResponse = DescribeTableResponse
    { _dtrsrsTable :: Maybe TableDescription
    } deriving (Show, Generic)

-- | Represents the properties of a table.
dtrsrsTable :: Lens' DescribeTableResponse (Maybe TableDescription)
dtrsrsTable = lens _dtrsrsTable (\s a -> s { _dtrsrsTable = a })
{-# INLINE dtrsrsTable #-}

instance FromJSON DescribeTableResponse

instance AWSRequest DescribeTable where
    type Sv DescribeTable = DynamoDB
    type Rs DescribeTable = DescribeTableResponse

    request = get
    response _ = jsonResponse
