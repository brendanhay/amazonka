{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.BatchWriteItem
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /BatchWriteItem/ operation puts or deletes multiple items in one or
-- more tables. A single call to /BatchWriteItem/ can write up to 16 MB of
-- data, which can comprise as many as 25 put or delete requests.
-- Individual items to be written can be as large as 400 KB.
--
-- /BatchWriteItem/ cannot update items. To update items, use the
-- /UpdateItem/ API.
--
-- The individual /PutItem/ and /DeleteItem/ operations specified in
-- /BatchWriteItem/ are atomic; however /BatchWriteItem/ as a whole is not.
-- If any requested operations fail because the table\'s provisioned
-- throughput is exceeded or an internal processing failure occurs, the
-- failed operations are returned in the /UnprocessedItems/ response
-- parameter. You can investigate and optionally resend the requests.
-- Typically, you would call /BatchWriteItem/ in a loop. Each iteration
-- would check for unprocessed items and submit a new /BatchWriteItem/
-- request with those unprocessed items until all items have been
-- processed.
--
-- Note that if /none/ of the items can be processed due to insufficient
-- provisioned throughput on all of the tables in the request, then
-- /BatchWriteItem/ will return a /ProvisionedThroughputExceededException/.
--
-- If DynamoDB returns any unprocessed items, you should retry the batch
-- operation on those items. However, /we strongly recommend that you use
-- an exponential backoff algorithm/. If you retry the batch operation
-- immediately, the underlying read or write requests can still fail due to
-- throttling on the individual tables. If you delay the batch operation
-- using exponential backoff, the individual requests in the batch are much
-- more likely to succeed.
--
-- For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#BatchOperations Batch Operations and Error Handling>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- With /BatchWriteItem/, you can efficiently write or delete large amounts
-- of data, such as from Amazon Elastic MapReduce (EMR), or copy data from
-- another database into DynamoDB. In order to improve performance with
-- these large-scale operations, /BatchWriteItem/ does not behave in the
-- same way as individual /PutItem/ and /DeleteItem/ calls would. For
-- example, you cannot specify conditions on individual put and delete
-- requests, and /BatchWriteItem/ does not return deleted items in the
-- response.
--
-- If you use a programming language that supports concurrency, you can use
-- threads to write items in parallel. Your application must include the
-- necessary logic to manage the threads. With languages that don\'t
-- support threading, you must update or delete the specified items one at
-- a time. In both situations, /BatchWriteItem/ provides an alternative
-- where the API performs the specified put and delete operations in
-- parallel, giving you the power of the thread pool approach without
-- having to introduce complexity into your application.
--
-- Parallel processing reduces latency, but each specified put and delete
-- request consumes the same number of write capacity units whether it is
-- processed in parallel or not. Delete operations on nonexistent items
-- consume one write capacity unit.
--
-- If one or more of the following is true, DynamoDB rejects the entire
-- batch write operation:
--
-- -   One or more tables specified in the /BatchWriteItem/ request does
--     not exist.
--
-- -   Primary key attributes specified on an item in the request do not
--     match those in the corresponding table\'s primary key schema.
--
-- -   You try to perform multiple operations on the same item in the same
--     /BatchWriteItem/ request. For example, you cannot put and delete the
--     same item in the same /BatchWriteItem/ request.
--
-- -   There are more than 25 requests in the batch.
--
-- -   Any individual item in a batch exceeds 400 KB.
--
-- -   The total request size exceeds 16 MB.
--
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_BatchWriteItem.html>
module Network.AWS.DynamoDB.BatchWriteItem
    (
    -- * Request
      BatchWriteItem
    -- ** Request constructor
    , batchWriteItem
    -- ** Request lenses
    , bwiReturnConsumedCapacity
    , bwiReturnItemCollectionMetrics
    , bwiRequestItems

    -- * Response
    , BatchWriteItemResponse
    -- ** Response constructor
    , batchWriteItemResponse
    -- ** Response lenses
    , bwirsConsumedCapacity
    , bwirsItemCollectionMetrics
    , bwirsUnprocessedItems
    , bwirsStatus
    ) where

import           Network.AWS.DynamoDB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /BatchWriteItem/ operation.
--
-- /See:/ 'batchWriteItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bwiReturnConsumedCapacity'
--
-- * 'bwiReturnItemCollectionMetrics'
--
-- * 'bwiRequestItems'
data BatchWriteItem = BatchWriteItem'
    { _bwiReturnConsumedCapacity      :: !(Maybe ReturnConsumedCapacity)
    , _bwiReturnItemCollectionMetrics :: !(Maybe ReturnItemCollectionMetrics)
    , _bwiRequestItems                :: !(Map Text (List1 WriteRequest))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'BatchWriteItem' smart constructor.
batchWriteItem :: BatchWriteItem
batchWriteItem =
    BatchWriteItem'
    { _bwiReturnConsumedCapacity = Nothing
    , _bwiReturnItemCollectionMetrics = Nothing
    , _bwiRequestItems = mempty
    }

-- | FIXME: Undocumented member.
bwiReturnConsumedCapacity :: Lens' BatchWriteItem (Maybe ReturnConsumedCapacity)
bwiReturnConsumedCapacity = lens _bwiReturnConsumedCapacity (\ s a -> s{_bwiReturnConsumedCapacity = a});

-- | Determines whether item collection metrics are returned. If set to
-- @SIZE@, the response includes statistics about item collections, if any,
-- that were modified during the operation are returned in the response. If
-- set to @NONE@ (the default), no statistics are returned.
bwiReturnItemCollectionMetrics :: Lens' BatchWriteItem (Maybe ReturnItemCollectionMetrics)
bwiReturnItemCollectionMetrics = lens _bwiReturnItemCollectionMetrics (\ s a -> s{_bwiReturnItemCollectionMetrics = a});

-- | A map of one or more table names and, for each table, a list of
-- operations to be performed (/DeleteRequest/ or /PutRequest/). Each
-- element in the map consists of the following:
--
-- -   /DeleteRequest/ - Perform a /DeleteItem/ operation on the specified
--     item. The item to be deleted is identified by a /Key/ subelement:
--
--     -   /Key/ - A map of primary key attribute values that uniquely
--         identify the ! item. Each entry in this map consists of an
--         attribute name and an attribute value. For each primary key, you
--         must provide /all/ of the key attributes. For example, with a
--         hash type primary key, you only need to provide the hash
--         attribute. For a hash-and-range type primary key, you must
--         provide /both/ the hash attribute and the range attribute.
--
-- -   /PutRequest/ - Perform a /PutItem/ operation on the specified item.
--     The item to be put is identified by an /Item/ subelement:
--
--     -   /Item/ - A map of attributes and their values. Each entry in
--         this map consists of an attribute name and an attribute value.
--         Attribute values must not be null; string and binary type
--         attributes must have lengths greater than zero; and set type
--         attributes must not be empty. Requests that contain empty values
--         will be rejected with a /ValidationException/ exception.
--
--         If you specify any attributes that are part of an index key,
--         then the data types for those attributes must match those of the
--         schema in the table\'s attribute definition.
--
bwiRequestItems :: Lens' BatchWriteItem (HashMap Text (NonEmpty WriteRequest))
bwiRequestItems = lens _bwiRequestItems (\ s a -> s{_bwiRequestItems = a}) . _Map;

instance AWSRequest BatchWriteItem where
        type Sv BatchWriteItem = DynamoDB
        type Rs BatchWriteItem = BatchWriteItemResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 BatchWriteItemResponse' <$>
                   (x .?> "ConsumedCapacity" .!@ mempty) <*>
                     (x .?> "ItemCollectionMetrics" .!@ mempty)
                     <*> (x .?> "UnprocessedItems" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders BatchWriteItem where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.BatchWriteItem" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON BatchWriteItem where
        toJSON BatchWriteItem'{..}
          = object
              ["ReturnConsumedCapacity" .=
                 _bwiReturnConsumedCapacity,
               "ReturnItemCollectionMetrics" .=
                 _bwiReturnItemCollectionMetrics,
               "RequestItems" .= _bwiRequestItems]

instance ToPath BatchWriteItem where
        toPath = const "/"

instance ToQuery BatchWriteItem where
        toQuery = const mempty

-- | Represents the output of a /BatchWriteItem/ operation.
--
-- /See:/ 'batchWriteItemResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bwirsConsumedCapacity'
--
-- * 'bwirsItemCollectionMetrics'
--
-- * 'bwirsUnprocessedItems'
--
-- * 'bwirsStatus'
data BatchWriteItemResponse = BatchWriteItemResponse'
    { _bwirsConsumedCapacity      :: !(Maybe [ConsumedCapacity])
    , _bwirsItemCollectionMetrics :: !(Maybe (Map Text [ItemCollectionMetrics]))
    , _bwirsUnprocessedItems      :: !(Maybe (Map Text (List1 WriteRequest)))
    , _bwirsStatus                :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'BatchWriteItemResponse' smart constructor.
batchWriteItemResponse :: Int -> BatchWriteItemResponse
batchWriteItemResponse pStatus_ =
    BatchWriteItemResponse'
    { _bwirsConsumedCapacity = Nothing
    , _bwirsItemCollectionMetrics = Nothing
    , _bwirsUnprocessedItems = Nothing
    , _bwirsStatus = pStatus_
    }

-- | The capacity units consumed by the operation.
--
-- Each element consists of:
--
-- -   /TableName/ - The table that consumed the provisioned throughput.
--
-- -   /CapacityUnits/ - The total number of capacity units consumed.
--
bwirsConsumedCapacity :: Lens' BatchWriteItemResponse [ConsumedCapacity]
bwirsConsumedCapacity = lens _bwirsConsumedCapacity (\ s a -> s{_bwirsConsumedCapacity = a}) . _Default;

-- | A list of tables that were processed by /BatchWriteItem/ and, for each
-- table, information about any item collections that were affected by
-- individual /DeleteItem/ or /PutItem/ operations.
--
-- Each entry consists of the following subelements:
--
-- -   /ItemCollectionKey/ - The hash key value of the item collection.
--     This is the same as the hash key of the item.
--
-- -   /SizeEstimateRange/ - An estimate of item collection size, expressed
--     in GB. This is a two-element array containing a lower bound and an
--     upper bound for the estimate. The estimate includes the size of all
--     the items in the table, plus the size of all attributes projected
--     into all of the local secondary indexes on the table. Use this
--     estimate to measure whether a local secondary index is approaching
--     its size limit.
--
--     The estimate is subject to change over time; therefore, do not rely
--     on the precision or accuracy of the estimate.
--
bwirsItemCollectionMetrics :: Lens' BatchWriteItemResponse (HashMap Text [ItemCollectionMetrics])
bwirsItemCollectionMetrics = lens _bwirsItemCollectionMetrics (\ s a -> s{_bwirsItemCollectionMetrics = a}) . _Default . _Map;

-- | A map of tables and requests against those tables that were not
-- processed. The /UnprocessedItems/ value is in the same form as
-- /RequestItems/, so you can provide this value directly to a subsequent
-- /BatchGetItem/ operation. For more information, see /RequestItems/ in
-- the Request Parameters section.
--
-- Each /UnprocessedItems/ entry consists of a table name and, for that
-- table, a list of operations to perform (/DeleteRequest/ or
-- /PutRequest/).
--
-- -   /DeleteRequest/ - Perform a /DeleteItem/ operation on the specified
--     item. The item to be deleted is identified by a /Key/ subelement:
--
--     -   /Key/ - A map of primary key attribute values that uniquely
--         identify the item. Each entry in this map consists of an
--         attribute name and an attribute value.
--
-- -   /PutRequest/ - Perform a /PutItem/ operation on the specified item.
--     The item to be put is identified by an /Item/ subelement:
--
--     -   /Item/ - A map of attributes and their values. Each entry in
--         this map consists of an attribute name and an attribute value.
--         Attribute values must not be null; string and binary type
--         attributes must have lengths greater than zero; and set type
--         attributes must not be empty. Requests that contain empty values
--         will be rejected with a /ValidationException/ exception.
--
--         If you specify any attributes that are part of an index key,
--         then the data types for those attributes must match those of the
--         schema in the table\'s attribute definition.
--
-- If there are no unprocessed items remaining, the response contains an
-- empty /UnprocessedItems/ map.
bwirsUnprocessedItems :: Lens' BatchWriteItemResponse (HashMap Text (NonEmpty WriteRequest))
bwirsUnprocessedItems = lens _bwirsUnprocessedItems (\ s a -> s{_bwirsUnprocessedItems = a}) . _Default . _Map;

-- | FIXME: Undocumented member.
bwirsStatus :: Lens' BatchWriteItemResponse Int
bwirsStatus = lens _bwirsStatus (\ s a -> s{_bwirsStatus = a});
