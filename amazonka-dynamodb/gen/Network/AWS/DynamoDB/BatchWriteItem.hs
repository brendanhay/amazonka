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

-- Module      : Network.AWS.DynamoDB.BatchWriteItem
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The BatchWriteItem operation puts or deletes multiple items in one or more
-- tables. A single call to BatchWriteItem can write up to 16 MB of data,
-- which can comprise as many as 25 put or delete requests. Individual items
-- to be written can be as large as 400 KB. The individual PutItem and
-- DeleteItem operations specified in BatchWriteItem are atomic; however
-- BatchWriteItem as a whole is not. If any requested operations fail because
-- the table's provisioned throughput is exceeded or an internal processing
-- failure occurs, the failed operations are returned in the UnprocessedItems
-- response parameter. You can investigate and optionally resend the requests.
-- Typically, you would call BatchWriteItem in a loop. Each iteration would
-- check for unprocessed items and submit a new BatchWriteItem request with
-- those unprocessed items until all items have been processed. Note that if
-- none of the items can be processed due to insufficient provisioned
-- throughput on all of the tables in the request, then BatchWriteItem will
-- return a ProvisionedThroughputExceededException. If DynamoDB returns any
-- unprocessed items, you should retry the batch operation on those items.
-- However, we strongly recommend that you use an exponential backoff
-- algorithm. If you retry the batch operation immediately, the underlying
-- read or write requests can still fail due to throttling on the individual
-- tables. If you delay the batch operation using exponential backoff, the
-- individual requests in the batch are much more likely to succeed. For more
-- information, go to Batch Operations and Error Handling in the Amazon
-- DynamoDB Developer Guide. With BatchWriteItem, you can efficiently write or
-- delete large amounts of data, such as from Amazon Elastic MapReduce (EMR),
-- or copy data from another database into DynamoDB. In order to improve
-- performance with these large-scale operations, BatchWriteItem does not
-- behave in the same way as individual PutItem and DeleteItem calls would For
-- example, you cannot specify conditions on individual put and delete
-- requests, and BatchWriteItem does not return deleted items in the response.
-- If you use a programming language that supports concurrency, such as Java,
-- you can use threads to write items in parallel. Your application must
-- include the necessary logic to manage the threads. With languages that
-- don't support threading, such as PHP, you must update or delete the
-- specified items one at a time. In both situations, BatchWriteItem provides
-- an alternative where the API performs the specified put and delete
-- operations in parallel, giving you the power of the thread pool approach
-- without having to introduce complexity into your application. Parallel
-- processing reduces latency, but each specified put and delete request
-- consumes the same number of write capacity units whether it is processed in
-- parallel or not. Delete operations on nonexistent items consume one write
-- capacity unit. If one or more of the following is true, DynamoDB rejects
-- the entire batch write operation: One or more tables specified in the
-- BatchWriteItem request does not exist. Primary key attributes specified on
-- an item in the request do not match those in the corresponding table's
-- primary key schema. You try to perform multiple operations on the same item
-- in the same BatchWriteItem request. For example, you cannot put and delete
-- the same item in the same BatchWriteItem request. There are more than 25
-- requests in the batch. Any individual item in a batch exceeds 400 KB. The
-- total request size exceeds 16 MB.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_BatchWriteItem.html>
module Network.AWS.DynamoDB.BatchWriteItem
    (
    -- * Request
      BatchWriteItem
    -- ** Request constructor
    , batchWriteItem
    -- ** Request lenses
    , bwiRequestItems
    , bwiReturnConsumedCapacity
    , bwiReturnItemCollectionMetrics

    -- * Response
    , BatchWriteItemResponse
    -- ** Response constructor
    , batchWriteItemResponse
    -- ** Response lenses
    , bwirConsumedCapacity
    , bwirItemCollectionMetrics
    , bwirUnprocessedItems
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DynamoDB.Types
import qualified GHC.Exts

data BatchWriteItem = BatchWriteItem
    { _bwiRequestItems                :: Map Text (List1 "RequestItems" WriteRequest)
    , _bwiReturnConsumedCapacity      :: Maybe Text
    , _bwiReturnItemCollectionMetrics :: Maybe Text
    } deriving (Eq, Show)

-- | 'BatchWriteItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bwiRequestItems' @::@ 'HashMap' 'Text' ('NonEmpty' 'WriteRequest')
--
-- * 'bwiReturnConsumedCapacity' @::@ 'Maybe' 'Text'
--
-- * 'bwiReturnItemCollectionMetrics' @::@ 'Maybe' 'Text'
--
batchWriteItem :: BatchWriteItem
batchWriteItem = BatchWriteItem
    { _bwiRequestItems                = mempty
    , _bwiReturnConsumedCapacity      = Nothing
    , _bwiReturnItemCollectionMetrics = Nothing
    }

-- | A map of one or more table names and, for each table, a list of
-- operations to be performed (DeleteRequest or PutRequest). Each element in
-- the map consists of the following: DeleteRequest - Perform a DeleteItem
-- operation on the specified item. The item to be deleted is identified by
-- a Key subelement: Key - A map of primary key attribute values that
-- uniquely identify the ! item. Each entry in this map consists of an
-- attribute name and an attribute value. For each primary key, you must
-- provide all of the key attributes. For example, with a hash type primary
-- key, you only need to specify the hash attribute. For a hash-and-range
-- type primary key, you must specify both the hash attribute and the range
-- attribute. PutRequest - Perform a PutItem operation on the specified
-- item. The item to be put is identified by an Item subelement: Item - A
-- map of attributes and their values. Each entry in this map consists of an
-- attribute name and an attribute value. Attribute values must not be null;
-- string and binary type attributes must have lengths greater than zero;
-- and set type attributes must not be empty. Requests that contain empty
-- values will be rejected with a ValidationException exception. If you
-- specify any attributes that are part of an index key, then the data types
-- for those attributes must match those of the schema in the table's
-- attribute definition.
bwiRequestItems :: Lens' BatchWriteItem (HashMap Text (NonEmpty WriteRequest))
bwiRequestItems = lens _bwiRequestItems (\s a -> s { _bwiRequestItems = a }) . _Map

bwiReturnConsumedCapacity :: Lens' BatchWriteItem (Maybe Text)
bwiReturnConsumedCapacity =
    lens _bwiReturnConsumedCapacity
        (\s a -> s { _bwiReturnConsumedCapacity = a })

-- | A value that if set to SIZE, the response includes statistics about item
-- collections, if any, that were modified during the operation are returned
-- in the response. If set to NONE (the default), no statistics are
-- returned.
bwiReturnItemCollectionMetrics :: Lens' BatchWriteItem (Maybe Text)
bwiReturnItemCollectionMetrics =
    lens _bwiReturnItemCollectionMetrics
        (\s a -> s { _bwiReturnItemCollectionMetrics = a })

data BatchWriteItemResponse = BatchWriteItemResponse
    { _bwirConsumedCapacity      :: List "ConsumedCapacity" ConsumedCapacity
    , _bwirItemCollectionMetrics :: Map Text (List "ItemCollectionMetrics" ItemCollectionMetrics)
    , _bwirUnprocessedItems      :: Map Text (List1 "RequestItems" WriteRequest)
    } deriving (Eq, Show)

-- | 'BatchWriteItemResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bwirConsumedCapacity' @::@ ['ConsumedCapacity']
--
-- * 'bwirItemCollectionMetrics' @::@ 'HashMap' 'Text' ['ItemCollectionMetrics']
--
-- * 'bwirUnprocessedItems' @::@ 'HashMap' 'Text' ('NonEmpty' 'WriteRequest')
--
batchWriteItemResponse :: BatchWriteItemResponse
batchWriteItemResponse = BatchWriteItemResponse
    { _bwirUnprocessedItems      = mempty
    , _bwirItemCollectionMetrics = mempty
    , _bwirConsumedCapacity      = mempty
    }

-- | The capacity units consumed by the operation. Each element consists of:
-- TableName - The table that consumed the provisioned throughput.
-- CapacityUnits - The total number of capacity units consumed.
bwirConsumedCapacity :: Lens' BatchWriteItemResponse [ConsumedCapacity]
bwirConsumedCapacity =
    lens _bwirConsumedCapacity (\s a -> s { _bwirConsumedCapacity = a })
        . _List

-- | A list of tables that were processed by BatchWriteItem and, for each
-- table, information about any item collections that were affected by
-- individual DeleteItem or PutItem operations. Each entry consists of the
-- following subelements: ItemCollectionKey - The hash key value of the item
-- collection. This is the same as the hash key of the item.
-- SizeEstimateRange - An estimate of item collection size, expressed in GB.
-- This is a two-element array containing a lower bound and an upper bound
-- for the estimate. The estimate includes the size of all the items in the
-- table, plus the size of all attributes projected into all of the local
-- secondary indexes on the table. Use this estimate to measure whether a
-- local secondary index is approaching its size limit. The estimate is
-- subject to change over time; therefore, do not rely on the precision or
-- accuracy of the estimate.
bwirItemCollectionMetrics :: Lens' BatchWriteItemResponse (HashMap Text [ItemCollectionMetrics])
bwirItemCollectionMetrics =
    lens _bwirItemCollectionMetrics
        (\s a -> s { _bwirItemCollectionMetrics = a })
            . _Map

-- | A map of tables and requests against those tables that were not
-- processed. The UnprocessedItems value is in the same form as
-- RequestItems, so you can provide this value directly to a subsequent
-- BatchGetItem operation. For more information, see RequestItems in the
-- Request Parameters section. Each UnprocessedItems entry consists of a
-- table name and, for that table, a list of operations to perform
-- (DeleteRequest or PutRequest). DeleteRequest - Perform a DeleteItem
-- operation on the specified item. The item to be deleted is identified by
-- a Key subelement: Key - A map of primary key attribute values that
-- uniquely identify the item. Each entry in this map consists of an
-- attribute name and an attribute value. PutRequest - Perform a PutItem
-- operation on the specified item. The item to be put is identified by an
-- Item subelement: Item - A map of attributes and their values. Each entry
-- in this map consists of an attribute name and an attribute value.
-- Attribute values must not be null; string and binary type attributes must
-- have lengths greater than zero; and set type attributes must not be
-- empty. Requests that contain empty values will be rejected with a
-- ValidationException exception. If you specify any attributes that are
-- part of an index key, then the data types for those attributes must match
-- those of the schema in the table's attribute definition. If there are no
-- unprocessed items remaining, the response contains an empty
-- UnprocessedItems map.
bwirUnprocessedItems :: Lens' BatchWriteItemResponse (HashMap Text (NonEmpty WriteRequest))
bwirUnprocessedItems =
    lens _bwirUnprocessedItems (\s a -> s { _bwirUnprocessedItems = a })
        . _Map

instance ToPath BatchWriteItem where
    toPath = const "/"

instance ToQuery BatchWriteItem where
    toQuery = const mempty

instance ToHeaders BatchWriteItem

instance ToJSON BatchWriteItem where
    toJSON BatchWriteItem{..} = object
        [ "RequestItems"                .= _bwiRequestItems
        , "ReturnConsumedCapacity"      .= _bwiReturnConsumedCapacity
        , "ReturnItemCollectionMetrics" .= _bwiReturnItemCollectionMetrics
        ]

instance AWSRequest BatchWriteItem where
    type Sv BatchWriteItem = DynamoDB
    type Rs BatchWriteItem = BatchWriteItemResponse

    request  = post "BatchWriteItem"
    response = jsonResponse

instance FromJSON BatchWriteItemResponse where
    parseJSON = withObject "BatchWriteItemResponse" $ \o -> BatchWriteItemResponse
        <$> o .:? "ConsumedCapacity"
        <*> o .:? "ItemCollectionMetrics"
        <*> o .:? "UnprocessedItems"
