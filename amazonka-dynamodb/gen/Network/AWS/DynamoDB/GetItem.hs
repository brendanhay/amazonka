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

-- | The GetItem operation returns a set of attributes for the item with the
-- given primary key. If there is no matching item, GetItem does not return
-- any data. GetItem provides an eventually consistent read by default. If
-- your application requires a strongly consistent read, set ConsistentRead to
-- true. Although a strongly consistent read might take more time than an
-- eventually consistent read, it always returns the last updated value.
-- Retrieve Item Attributes This example retrieves three attributes from the
-- Thread table. In the response, the ConsumedCapacityUnits value is 1,
-- because ConsistentRead is set to true. If ConsistentRead had been set to
-- false (or not specified) for the same request, an eventually consistent
-- read would have been used and ConsumedCapacityUnits would have been 0.5. {
-- "ConsumedCapacity": { "CapacityUnits": 1, "TableName": "Thread" }, "Item":
-- { "Tags": { "SS": ["Update","Multiple Items","HelpMe"] },
-- "LastPostDateTime": { "S": "201303190436" }, "Message": { "S": "I want to
-- update multiple items in a single API call. What's the best way to do
-- that?" } } }.
module Network.AWS.DynamoDB
    (
    -- * Request
      GetItem
    -- ** Request constructor
    , mkGetItem
    -- ** Request lenses
    , giTableName
    , giKey
    , giAttributesToGet
    , giConsistentRead
    , giReturnConsumedCapacity

    -- * Response
    , GetItemResponse
    -- ** Response constructor
    , mkGetItemResponse
    -- ** Response lenses
    , girItem
    , girConsumedCapacity
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input of a GetItem operation.
data GetItem = GetItem
    { _giTableName :: Text
    , _giKey :: Map Text AttributeValue
    , _giAttributesToGet :: Maybe (List1 Text)
    , _giConsistentRead :: Maybe Bool
    , _giReturnConsumedCapacity :: Maybe ReturnConsumedCapacity
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetItem' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TableName ::@ @Text@
--
-- * @Key ::@ @Map Text AttributeValue@
--
-- * @AttributesToGet ::@ @Maybe (List1 Text)@
--
-- * @ConsistentRead ::@ @Maybe Bool@
--
-- * @ReturnConsumedCapacity ::@ @Maybe ReturnConsumedCapacity@
--
mkGetItem :: Text -- ^ 'giTableName'
          -> Map Text AttributeValue -- ^ 'giKey'
          -> GetItem
mkGetItem p1 p2 = GetItem
    { _giTableName = p1
    , _giKey = p2
    , _giAttributesToGet = Nothing
    , _giConsistentRead = Nothing
    , _giReturnConsumedCapacity = Nothing
    }

-- | The name of the table containing the requested item.
giTableName :: Lens' GetItem Text
giTableName = lens _giTableName (\s a -> s { _giTableName = a })

-- | A map of attribute names to AttributeValue objects, representing the
-- primary key of the item to retrieve.
giKey :: Lens' GetItem (Map Text AttributeValue)
giKey = lens _giKey (\s a -> s { _giKey = a })

-- | The names of one or more attributes to retrieve. If no attribute names are
-- specified, then all attributes will be returned. If any of the requested
-- attributes are not found, they will not appear in the result.
giAttributesToGet :: Lens' GetItem (Maybe (List1 Text))
giAttributesToGet =
    lens _giAttributesToGet (\s a -> s { _giAttributesToGet = a })

-- | If set to true, then the operation uses strongly consistent reads;
-- otherwise, eventually consistent reads are used.
giConsistentRead :: Lens' GetItem (Maybe Bool)
giConsistentRead =
    lens _giConsistentRead (\s a -> s { _giConsistentRead = a })

-- | If set to TOTAL, the response includes ConsumedCapacity data for tables and
-- indexes. If set to INDEXES, the repsonse includes ConsumedCapacity for
-- indexes. If set to NONE (the default), ConsumedCapacity is not included in
-- the response.
giReturnConsumedCapacity :: Lens' GetItem (Maybe ReturnConsumedCapacity)
giReturnConsumedCapacity =
    lens _giReturnConsumedCapacity
         (\s a -> s { _giReturnConsumedCapacity = a })

instance ToPath GetItem

instance ToQuery GetItem

instance ToHeaders GetItem

instance ToJSON GetItem

-- | Represents the output of a GetItem operation.
data GetItemResponse = GetItemResponse
    { _girItem :: Map Text AttributeValue
    , _girConsumedCapacity :: Maybe ConsumedCapacity
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetItemResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Item ::@ @Map Text AttributeValue@
--
-- * @ConsumedCapacity ::@ @Maybe ConsumedCapacity@
--
mkGetItemResponse :: GetItemResponse
mkGetItemResponse = GetItemResponse
    { _girItem = mempty
    , _girConsumedCapacity = Nothing
    }

-- | A map of attribute names to AttributeValue objects, as specified by
-- AttributesToGet.
girItem :: Lens' GetItemResponse (Map Text AttributeValue)
girItem = lens _girItem (\s a -> s { _girItem = a })

-- | Represents the capacity units consumed by an operation. The data returned
-- includes the total provisioned throughput consumed, along with statistics
-- for the table and any indexes involved in the operation. ConsumedCapacity
-- is only returned if it was asked for in the request. For more information,
-- see Provisioned Throughput in the Amazon DynamoDB Developer Guide.
girConsumedCapacity :: Lens' GetItemResponse (Maybe ConsumedCapacity)
girConsumedCapacity =
    lens _girConsumedCapacity (\s a -> s { _girConsumedCapacity = a })

instance FromJSON GetItemResponse

instance AWSRequest GetItem where
    type Sv GetItem = DynamoDB
    type Rs GetItem = GetItemResponse

    request = get
    response _ = jsonResponse
