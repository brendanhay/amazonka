{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB
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
module Network.AWS.DynamoDB
    (
    -- * Request
      DescribeTable
    -- ** Request constructor
    , mkDescribeTable
    -- ** Request lenses
    , dt1TableName

    -- * Response
    , DescribeTableResponse
    -- ** Response constructor
    , mkDescribeTableResponse
    -- ** Response lenses
    , dtrrTable
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input of a DescribeTable operation.
newtype DescribeTable = DescribeTable
    { _dt1TableName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTable' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TableName ::@ @Text@
--
mkDescribeTable :: Text -- ^ 'dt1TableName'
                -> DescribeTable
mkDescribeTable p1 = DescribeTable
    { _dt1TableName = p1
    }

-- | The name of the table to describe.
dt1TableName :: Lens' DescribeTable Text
dt1TableName = lens _dt1TableName (\s a -> s { _dt1TableName = a })

instance ToPath DescribeTable

instance ToQuery DescribeTable

instance ToHeaders DescribeTable

instance ToJSON DescribeTable

-- | Represents the output of a DescribeTable operation.
newtype DescribeTableResponse = DescribeTableResponse
    { _dtrrTable :: Maybe TableDescription
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTableResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Table ::@ @Maybe TableDescription@
--
mkDescribeTableResponse :: DescribeTableResponse
mkDescribeTableResponse = DescribeTableResponse
    { _dtrrTable = Nothing
    }

-- | Represents the properties of a table.
dtrrTable :: Lens' DescribeTableResponse (Maybe TableDescription)
dtrrTable = lens _dtrrTable (\s a -> s { _dtrrTable = a })

instance FromJSON DescribeTableResponse

instance AWSRequest DescribeTable where
    type Sv DescribeTable = DynamoDB
    type Rs DescribeTable = DescribeTableResponse

    request = get
    response _ = jsonResponse
