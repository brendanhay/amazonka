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

-- Module      : Network.AWS.DynamoDB.BatchGetItem
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

-- | The /BatchGetItem/ operation returns the attributes of one or more items from
-- one or more tables. You identify requested items by primary key.
--
-- A single operation can retrieve up to 16 MB of data, which can contain as
-- many as 100 items. /BatchGetItem/ will return a partial result if the response
-- size limit is exceeded, the table's provisioned throughput is exceeded, or an
-- internal processing failure occurs. If a partial result is returned, the
-- operation returns a value for /UnprocessedKeys/. You can use this value to
-- retry the operation starting with the next item to get.
--
-- For example, if you ask to retrieve 100 items, but each individual item is
-- 300 KB in size, the system returns 52 items (so as not to exceed the 16 MB
-- limit). It also returns an appropriate /UnprocessedKeys/ value so you can get
-- the next page of results. If desired, your application can include its own
-- logic to assemble the pages of results into one data set.
--
-- If /none/ of the items can be processed due to insufficient provisioned
-- throughput on all of the tables in the request, then /BatchGetItem/ will return
-- a /ProvisionedThroughputExceededException/. If /at least one/ of the items is
-- successfully processed, then /BatchGetItem/ completes successfully, while
-- returning the keys of the unread items in /UnprocessedKeys/.
--
-- If DynamoDB returns any unprocessed items, you should retry the batch
-- operation on those items. However, /we strongly recommend that you use anexponential backoff algorithm/. If you retry the batch operation immediately,
-- the underlying read or write requests can still fail due to throttling on the
-- individual tables. If you delay the batch operation using exponential
-- backoff, the individual requests in the batch are much more likely to succeed.
--
-- For more information, go to <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#BatchOperations Batch Operations and Error Handling> in the /Amazon DynamoDB Developer Guide/.
--
-- By default, /BatchGetItem/ performs eventually consistent reads on every
-- table in the request. If you want strongly consistent reads instead, you can
-- set /ConsistentRead/ to 'true' for any or all tables.
--
-- In order to minimize response latency, /BatchGetItem/ retrieves items in
-- parallel.
--
-- When designing your application, keep in mind that DynamoDB does not return
-- attributes in any particular order. To help parse the response by item,
-- include the primary key values for the items in your request in the /AttributesToGet/ parameter.
--
-- If a requested item does not exist, it is not returned in the result.
-- Requests for nonexistent items consume the minimum read capacity units
-- according to the type of read. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#CapacityUnitCalculations Capacity UnitsCalculations> in the /Amazon DynamoDB Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_BatchGetItem.html>
module Network.AWS.DynamoDB.BatchGetItem
    (
    -- * Request
      BatchGetItem
    -- ** Request constructor
    , batchGetItem
    -- ** Request lenses
    , bgiRequestItems
    , bgiReturnConsumedCapacity

    -- * Response
    , BatchGetItemResponse
    -- ** Response constructor
    , batchGetItemResponse
    -- ** Response lenses
    , bgirConsumedCapacity
    , bgirResponses
    , bgirUnprocessedKeys
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DynamoDB.Types
import qualified GHC.Exts

data BatchGetItem = BatchGetItem
    { _bgiRequestItems           :: Map Text KeysAndAttributes
    , _bgiReturnConsumedCapacity :: Maybe ReturnConsumedCapacity
    } deriving (Eq, Read, Show)

-- | 'BatchGetItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgiRequestItems' @::@ 'HashMap' 'Text' 'KeysAndAttributes'
--
-- * 'bgiReturnConsumedCapacity' @::@ 'Maybe' 'ReturnConsumedCapacity'
--
batchGetItem :: BatchGetItem
batchGetItem = BatchGetItem
    { _bgiRequestItems           = mempty
    , _bgiReturnConsumedCapacity = Nothing
    }

-- | A map of one or more table names and, for each table, the corresponding
-- primary keys for the items to retrieve. Each table name can be invoked only
-- once.
--
-- Each element in the map consists of the following:
--
-- /Keys/ - An array of primary key attribute values that define specific items
-- in the table. For each primary key, you must provide /all/ of the key
-- attributes. For example, with a hash type primary key, you only need to
-- provide the hash attribute. For a hash-and-range type primary key, you must
-- provide /both/ the hash attribute and the range attribute.
--
-- /AttributesToGet/ - One or more attributes to be retrieved from the table.
-- By default, all attributes are returned. If a specified attribute is not
-- found, it does not appear in the result.
--
-- Note that /AttributesToGet/ has no effect on provisioned throughput
-- consumption. DynamoDB determines capacity units consumed based on item size,
-- not on the amount of data that is returned to an application.
--
-- /ConsistentRead/ - If 'true', a strongly consistent read is used; if 'false'
-- (the default), an eventually consistent read is used.
--
--
bgiRequestItems :: Lens' BatchGetItem (HashMap Text KeysAndAttributes)
bgiRequestItems = lens _bgiRequestItems (\s a -> s { _bgiRequestItems = a }) . _Map

bgiReturnConsumedCapacity :: Lens' BatchGetItem (Maybe ReturnConsumedCapacity)
bgiReturnConsumedCapacity =
    lens _bgiReturnConsumedCapacity
        (\s a -> s { _bgiReturnConsumedCapacity = a })

data BatchGetItemResponse = BatchGetItemResponse
    { _bgirConsumedCapacity :: List "ConsumedCapacity" ConsumedCapacity
    , _bgirResponses        :: Map Text (List "Responses" (Map Text AttributeValue))
    , _bgirUnprocessedKeys  :: Map Text KeysAndAttributes
    } deriving (Eq, Read, Show)

-- | 'BatchGetItemResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgirConsumedCapacity' @::@ ['ConsumedCapacity']
--
-- * 'bgirResponses' @::@ 'HashMap' 'Text' ['HashMap' 'Text' 'AttributeValue']
--
-- * 'bgirUnprocessedKeys' @::@ 'HashMap' 'Text' 'KeysAndAttributes'
--
batchGetItemResponse :: BatchGetItemResponse
batchGetItemResponse = BatchGetItemResponse
    { _bgirResponses        = mempty
    , _bgirUnprocessedKeys  = mempty
    , _bgirConsumedCapacity = mempty
    }

-- | The read capacity units consumed by the operation.
--
-- Each element consists of:
--
-- /TableName/ - The table that consumed the provisioned throughput.
--
-- /CapacityUnits/ - The total number of capacity units consumed.
--
--
bgirConsumedCapacity :: Lens' BatchGetItemResponse [ConsumedCapacity]
bgirConsumedCapacity =
    lens _bgirConsumedCapacity (\s a -> s { _bgirConsumedCapacity = a })
        . _List

-- | A map of table name to a list of items. Each object in /Responses/ consists of
-- a table name, along with a map of attribute data consisting of the data type
-- and attribute value.
bgirResponses :: Lens' BatchGetItemResponse (HashMap Text [HashMap Text AttributeValue])
bgirResponses = lens _bgirResponses (\s a -> s { _bgirResponses = a }) . _Map

-- | A map of tables and their respective keys that were not processed with the
-- current response. The /UnprocessedKeys/ value is in the same form as /RequestItems/, so the value can be provided directly to a subsequent /BatchGetItem/
-- operation. For more information, see /RequestItems/ in the Request Parameters
-- section.
--
-- Each element consists of:
--
-- /Keys/ - An array of primary key attribute values that define specific items
-- in the table.
--
-- /AttributesToGet/ - One or more attributes to be retrieved from the table or
-- index. By default, all attributes are returned. If a requested attribute is
-- not found, it does not appear in the result.
--
-- /ConsistentRead/ - The consistency of a read operation. If set to 'true', then
-- a strongly consistent read is used; otherwise, an eventually consistent read
-- is used.
--
-- If there are no unprocessed keys remaining, the response contains an empty /UnprocessedKeys/ map.
bgirUnprocessedKeys :: Lens' BatchGetItemResponse (HashMap Text KeysAndAttributes)
bgirUnprocessedKeys =
    lens _bgirUnprocessedKeys (\s a -> s { _bgirUnprocessedKeys = a })
        . _Map

instance ToPath BatchGetItem where
    toPath = const "/"

instance ToQuery BatchGetItem where
    toQuery = const mempty

instance ToHeaders BatchGetItem

instance ToJSON BatchGetItem where
    toJSON BatchGetItem{..} = object
        [ "RequestItems"           .= _bgiRequestItems
        , "ReturnConsumedCapacity" .= _bgiReturnConsumedCapacity
        ]

instance AWSRequest BatchGetItem where
    type Sv BatchGetItem = DynamoDB
    type Rs BatchGetItem = BatchGetItemResponse

    request  = post "BatchGetItem"
    response = jsonResponse

instance FromJSON BatchGetItemResponse where
    parseJSON = withObject "BatchGetItemResponse" $ \o -> BatchGetItemResponse
        <$> o .:? "ConsumedCapacity" .!= mempty
        <*> o .:? "Responses" .!= mempty
        <*> o .:? "UnprocessedKeys" .!= mempty
