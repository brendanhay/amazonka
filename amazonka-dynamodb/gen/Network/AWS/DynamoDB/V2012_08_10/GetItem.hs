{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.V2012_08_10.GetItem
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
module Network.AWS.DynamoDB.V2012_08_10.GetItem
    (
    -- * Request
      GetItem
    -- ** Request constructor
    , mkGetItemInput
    -- ** Request lenses
    , giiTableName
    , giiKey
    , giiAttributesToGet
    , giiConsistentRead
    , giiReturnConsumedCapacity

    -- * Response
    , GetItemResponse
    -- ** Response lenses
    , gioItem
    , gioConsumedCapacity
    ) where

import           Network.AWS.DynamoDB.V2012_08_10.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetItem' request.
mkGetItemInput :: Text -- ^ 'giiTableName'
               -> Map Text AttributeValue -- ^ 'giiKey'
               -> GetItem
mkGetItemInput p1 p2 = GetItem
    { _giiTableName = p1
    , _giiKey = p2
    , _giiAttributesToGet = Nothing
    , _giiConsistentRead = Nothing
    , _giiReturnConsumedCapacity = Nothing
    }
{-# INLINE mkGetItemInput #-}

data GetItem = GetItem
    { _giiTableName :: Text
      -- ^ The name of the table containing the requested item.
    , _giiKey :: Map Text AttributeValue
      -- ^ A map of attribute names to AttributeValue objects, representing
      -- the primary key of the item to retrieve.
    , _giiAttributesToGet :: Maybe [Text]
      -- ^ The names of one or more attributes to retrieve. If no attribute
      -- names are specified, then all attributes will be returned. If any
      -- of the requested attributes are not found, they will not appear
      -- in the result.
    , _giiConsistentRead :: Maybe Bool
      -- ^ If set to true, then the operation uses strongly consistent
      -- reads; otherwise, eventually consistent reads are used.
    , _giiReturnConsumedCapacity :: Maybe ReturnConsumedCapacity
      -- ^ If set to TOTAL, the response includes ConsumedCapacity data for
      -- tables and indexes. If set to INDEXES, the repsonse includes
      -- ConsumedCapacity for indexes. If set to NONE (the default),
      -- ConsumedCapacity is not included in the response.
    } deriving (Show, Generic)

-- | The name of the table containing the requested item.
giiTableName :: Lens' GetItem (Text)
giiTableName = lens _giiTableName (\s a -> s { _giiTableName = a })
{-# INLINE giiTableName #-}

-- | A map of attribute names to AttributeValue objects, representing the
-- primary key of the item to retrieve.
giiKey :: Lens' GetItem (Map Text AttributeValue)
giiKey = lens _giiKey (\s a -> s { _giiKey = a })
{-# INLINE giiKey #-}

-- | The names of one or more attributes to retrieve. If no attribute names are
-- specified, then all attributes will be returned. If any of the requested
-- attributes are not found, they will not appear in the result.
giiAttributesToGet :: Lens' GetItem (Maybe [Text])
giiAttributesToGet = lens _giiAttributesToGet (\s a -> s { _giiAttributesToGet = a })
{-# INLINE giiAttributesToGet #-}

-- | If set to true, then the operation uses strongly consistent reads;
-- otherwise, eventually consistent reads are used.
giiConsistentRead :: Lens' GetItem (Maybe Bool)
giiConsistentRead = lens _giiConsistentRead (\s a -> s { _giiConsistentRead = a })
{-# INLINE giiConsistentRead #-}

-- | If set to TOTAL, the response includes ConsumedCapacity data for tables and
-- indexes. If set to INDEXES, the repsonse includes ConsumedCapacity for
-- indexes. If set to NONE (the default), ConsumedCapacity is not included in
-- the response.
giiReturnConsumedCapacity :: Lens' GetItem (Maybe ReturnConsumedCapacity)
giiReturnConsumedCapacity = lens _giiReturnConsumedCapacity (\s a -> s { _giiReturnConsumedCapacity = a })
{-# INLINE giiReturnConsumedCapacity #-}

instance ToPath GetItem

instance ToQuery GetItem

instance ToHeaders GetItem

instance ToJSON GetItem

data GetItemResponse = GetItemResponse
    { _gioItem :: Map Text AttributeValue
      -- ^ A map of attribute names to AttributeValue objects, as specified
      -- by AttributesToGet.
    , _gioConsumedCapacity :: Maybe ConsumedCapacity
      -- ^ Represents the capacity units consumed by an operation. The data
      -- returned includes the total provisioned throughput consumed,
      -- along with statistics for the table and any indexes involved in
      -- the operation. ConsumedCapacity is only returned if it was asked
      -- for in the request. For more information, see Provisioned
      -- Throughput in the Amazon DynamoDB Developer Guide.
    } deriving (Show, Generic)

-- | A map of attribute names to AttributeValue objects, as specified by
-- AttributesToGet.
gioItem :: Lens' GetItemResponse (Map Text AttributeValue)
gioItem = lens _gioItem (\s a -> s { _gioItem = a })
{-# INLINE gioItem #-}

-- | Represents the capacity units consumed by an operation. The data returned
-- includes the total provisioned throughput consumed, along with statistics
-- for the table and any indexes involved in the operation. ConsumedCapacity
-- is only returned if it was asked for in the request. For more information,
-- see Provisioned Throughput in the Amazon DynamoDB Developer Guide.
gioConsumedCapacity :: Lens' GetItemResponse (Maybe ConsumedCapacity)
gioConsumedCapacity = lens _gioConsumedCapacity (\s a -> s { _gioConsumedCapacity = a })
{-# INLINE gioConsumedCapacity #-}

instance FromJSON GetItemResponse

instance AWSRequest GetItem where
    type Sv GetItem = DynamoDB
    type Rs GetItem = GetItemResponse

    request = get
    response _ = jsonResponse
