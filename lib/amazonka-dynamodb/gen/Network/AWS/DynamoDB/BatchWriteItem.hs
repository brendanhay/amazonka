{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.BatchWriteItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @BatchWriteItem@ operation puts or deletes multiple items in one or more tables. A single call to @BatchWriteItem@ can write up to 16 MB of data, which can comprise as many as 25 put or delete requests. Individual items to be written can be as large as 400 KB.
--
-- The individual @PutItem@ and @DeleteItem@ operations specified in @BatchWriteItem@ are atomic; however @BatchWriteItem@ as a whole is not. If any requested operations fail because the table's provisioned throughput is exceeded or an internal processing failure occurs, the failed operations are returned in the @UnprocessedItems@ response parameter. You can investigate and optionally resend the requests. Typically, you would call @BatchWriteItem@ in a loop. Each iteration would check for unprocessed items and submit a new @BatchWriteItem@ request with those unprocessed items until all items have been processed.
-- If /none/ of the items can be processed due to insufficient provisioned throughput on all of the tables in the request, then @BatchWriteItem@ returns a @ProvisionedThroughputExceededException@ .
-- /Important:/ If DynamoDB returns any unprocessed items, you should retry the batch operation on those items. However, /we strongly recommend that you use an exponential backoff algorithm/ . If you retry the batch operation immediately, the underlying read or write requests can still fail due to throttling on the individual tables. If you delay the batch operation using exponential backoff, the individual requests in the batch are much more likely to succeed.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#Programming.Errors.BatchOperations Batch Operations and Error Handling> in the /Amazon DynamoDB Developer Guide/ .
-- With @BatchWriteItem@ , you can efficiently write or delete large amounts of data, such as from Amazon EMR, or copy data from another database into DynamoDB. In order to improve performance with these large-scale operations, @BatchWriteItem@ does not behave in the same way as individual @PutItem@ and @DeleteItem@ calls would. For example, you cannot specify conditions on individual put and delete requests, and @BatchWriteItem@ does not return deleted items in the response.
-- If you use a programming language that supports concurrency, you can use threads to write items in parallel. Your application must include the necessary logic to manage the threads. With languages that don't support threading, you must update or delete the specified items one at a time. In both situations, @BatchWriteItem@ performs the specified put and delete operations in parallel, giving you the power of the thread pool approach without having to introduce complexity into your application.
-- Parallel processing reduces latency, but each specified put and delete request consumes the same number of write capacity units whether it is processed in parallel or not. Delete operations on nonexistent items consume one write capacity unit.
-- If one or more of the following is true, DynamoDB rejects the entire batch write operation:
--
--     * One or more tables specified in the @BatchWriteItem@ request does not exist.
--
--
--     * Primary key attributes specified on an item in the request do not match those in the corresponding table's primary key schema.
--
--
--     * You try to perform multiple operations on the same item in the same @BatchWriteItem@ request. For example, you cannot put and delete the same item in the same @BatchWriteItem@ request.
--
--
--     * Your request contains at least two items with identical hash and range keys (which essentially is two put operations).
--
--
--     * There are more than 25 requests in the batch.
--
--
--     * Any individual item in a batch exceeds 400 KB.
--
--
--     * The total request size exceeds 16 MB.
module Network.AWS.DynamoDB.BatchWriteItem
  ( -- * Creating a request
    BatchWriteItem (..),
    mkBatchWriteItem,

    -- ** Request lenses
    bwiReturnConsumedCapacity,
    bwiReturnItemCollectionMetrics,
    bwiRequestItems,

    -- * Destructuring the response
    BatchWriteItemResponse (..),
    mkBatchWriteItemResponse,

    -- ** Response lenses
    bwirsItemCollectionMetrics,
    bwirsConsumedCapacity,
    bwirsUnprocessedItems,
    bwirsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @BatchWriteItem@ operation.
--
-- /See:/ 'mkBatchWriteItem' smart constructor.
data BatchWriteItem = BatchWriteItem'
  { returnConsumedCapacity ::
      Lude.Maybe ReturnConsumedCapacity,
    returnItemCollectionMetrics ::
      Lude.Maybe ReturnItemCollectionMetrics,
    requestItems ::
      Lude.HashMap Lude.Text (Lude.NonEmpty WriteRequest)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchWriteItem' with the minimum fields required to make a request.
--
-- * 'requestItems' - A map of one or more table names and, for each table, a list of operations to be performed (@DeleteRequest@ or @PutRequest@ ). Each element in the map consists of the following:
--
--
--     * @DeleteRequest@ - Perform a @DeleteItem@ operation on the specified item. The item to be deleted is identified by a @Key@ subelement:
--
--     * @Key@ - A map of primary key attribute values that uniquely identify the item. Each entry in this map consists of an attribute name and an attribute value. For each primary key, you must provide /all/ of the key attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for /both/ the partition key and the sort key.
--
--
--
--
--     * @PutRequest@ - Perform a @PutItem@ operation on the specified item. The item to be put is identified by an @Item@ subelement:
--
--     * @Item@ - A map of attributes and their values. Each entry in this map consists of an attribute name and an attribute value. Attribute values must not be null; string and binary type attributes must have lengths greater than zero; and set type attributes must not be empty. Requests that contain empty values are rejected with a @ValidationException@ exception.
-- If you specify any attributes that are part of an index key, then the data types for those attributes must match those of the schema in the table's attribute definition.
--
--
--
--
-- * 'returnConsumedCapacity' - Undocumented field.
-- * 'returnItemCollectionMetrics' - Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
mkBatchWriteItem ::
  BatchWriteItem
mkBatchWriteItem =
  BatchWriteItem'
    { returnConsumedCapacity = Lude.Nothing,
      returnItemCollectionMetrics = Lude.Nothing,
      requestItems = Lude.mempty
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwiReturnConsumedCapacity :: Lens.Lens' BatchWriteItem (Lude.Maybe ReturnConsumedCapacity)
bwiReturnConsumedCapacity = Lens.lens (returnConsumedCapacity :: BatchWriteItem -> Lude.Maybe ReturnConsumedCapacity) (\s a -> s {returnConsumedCapacity = a} :: BatchWriteItem)
{-# DEPRECATED bwiReturnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead." #-}

-- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
--
-- /Note:/ Consider using 'returnItemCollectionMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwiReturnItemCollectionMetrics :: Lens.Lens' BatchWriteItem (Lude.Maybe ReturnItemCollectionMetrics)
bwiReturnItemCollectionMetrics = Lens.lens (returnItemCollectionMetrics :: BatchWriteItem -> Lude.Maybe ReturnItemCollectionMetrics) (\s a -> s {returnItemCollectionMetrics = a} :: BatchWriteItem)
{-# DEPRECATED bwiReturnItemCollectionMetrics "Use generic-lens or generic-optics with 'returnItemCollectionMetrics' instead." #-}

-- | A map of one or more table names and, for each table, a list of operations to be performed (@DeleteRequest@ or @PutRequest@ ). Each element in the map consists of the following:
--
--
--     * @DeleteRequest@ - Perform a @DeleteItem@ operation on the specified item. The item to be deleted is identified by a @Key@ subelement:
--
--     * @Key@ - A map of primary key attribute values that uniquely identify the item. Each entry in this map consists of an attribute name and an attribute value. For each primary key, you must provide /all/ of the key attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for /both/ the partition key and the sort key.
--
--
--
--
--     * @PutRequest@ - Perform a @PutItem@ operation on the specified item. The item to be put is identified by an @Item@ subelement:
--
--     * @Item@ - A map of attributes and their values. Each entry in this map consists of an attribute name and an attribute value. Attribute values must not be null; string and binary type attributes must have lengths greater than zero; and set type attributes must not be empty. Requests that contain empty values are rejected with a @ValidationException@ exception.
-- If you specify any attributes that are part of an index key, then the data types for those attributes must match those of the schema in the table's attribute definition.
--
--
--
--
--
-- /Note:/ Consider using 'requestItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwiRequestItems :: Lens.Lens' BatchWriteItem (Lude.HashMap Lude.Text (Lude.NonEmpty WriteRequest))
bwiRequestItems = Lens.lens (requestItems :: BatchWriteItem -> Lude.HashMap Lude.Text (Lude.NonEmpty WriteRequest)) (\s a -> s {requestItems = a} :: BatchWriteItem)
{-# DEPRECATED bwiRequestItems "Use generic-lens or generic-optics with 'requestItems' instead." #-}

instance Lude.AWSRequest BatchWriteItem where
  type Rs BatchWriteItem = BatchWriteItemResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchWriteItemResponse'
            Lude.<$> (x Lude..?> "ItemCollectionMetrics" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ConsumedCapacity" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "UnprocessedItems" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchWriteItem where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.BatchWriteItem" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchWriteItem where
  toJSON BatchWriteItem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ReturnConsumedCapacity" Lude..=)
              Lude.<$> returnConsumedCapacity,
            ("ReturnItemCollectionMetrics" Lude..=)
              Lude.<$> returnItemCollectionMetrics,
            Lude.Just ("RequestItems" Lude..= requestItems)
          ]
      )

instance Lude.ToPath BatchWriteItem where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchWriteItem where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @BatchWriteItem@ operation.
--
-- /See:/ 'mkBatchWriteItemResponse' smart constructor.
data BatchWriteItemResponse = BatchWriteItemResponse'
  { itemCollectionMetrics ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            ([ItemCollectionMetrics])
        ),
    consumedCapacity ::
      Lude.Maybe [ConsumedCapacity],
    unprocessedItems ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.NonEmpty WriteRequest)
        ),
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchWriteItemResponse' with the minimum fields required to make a request.
--
-- * 'consumedCapacity' - The capacity units consumed by the entire @BatchWriteItem@ operation.
--
-- Each element consists of:
--
--     * @TableName@ - The table that consumed the provisioned throughput.
--
--
--     * @CapacityUnits@ - The total number of capacity units consumed.
--
--
-- * 'itemCollectionMetrics' - A list of tables that were processed by @BatchWriteItem@ and, for each table, information about any item collections that were affected by individual @DeleteItem@ or @PutItem@ operations.
--
-- Each entry consists of the following subelements:
--
--     * @ItemCollectionKey@ - The partition key value of the item collection. This is the same as the partition key value of the item.
--
--
--     * @SizeEstimateRangeGB@ - An estimate of item collection size, expressed in GB. This is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on the table. Use this estimate to measure whether a local secondary index is approaching its size limit.
-- The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
--
--
-- * 'responseStatus' - The response status code.
-- * 'unprocessedItems' - A map of tables and requests against those tables that were not processed. The @UnprocessedItems@ value is in the same form as @RequestItems@ , so you can provide this value directly to a subsequent @BatchGetItem@ operation. For more information, see @RequestItems@ in the Request Parameters section.
--
-- Each @UnprocessedItems@ entry consists of a table name and, for that table, a list of operations to perform (@DeleteRequest@ or @PutRequest@ ).
--
--     * @DeleteRequest@ - Perform a @DeleteItem@ operation on the specified item. The item to be deleted is identified by a @Key@ subelement:
--
--     * @Key@ - A map of primary key attribute values that uniquely identify the item. Each entry in this map consists of an attribute name and an attribute value.
--
--
--
--
--     * @PutRequest@ - Perform a @PutItem@ operation on the specified item. The item to be put is identified by an @Item@ subelement:
--
--     * @Item@ - A map of attributes and their values. Each entry in this map consists of an attribute name and an attribute value. Attribute values must not be null; string and binary type attributes must have lengths greater than zero; and set type attributes must not be empty. Requests that contain empty values will be rejected with a @ValidationException@ exception.
-- If you specify any attributes that are part of an index key, then the data types for those attributes must match those of the schema in the table's attribute definition.
--
--
--
--
-- If there are no unprocessed items remaining, the response contains an empty @UnprocessedItems@ map.
mkBatchWriteItemResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchWriteItemResponse
mkBatchWriteItemResponse pResponseStatus_ =
  BatchWriteItemResponse'
    { itemCollectionMetrics = Lude.Nothing,
      consumedCapacity = Lude.Nothing,
      unprocessedItems = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of tables that were processed by @BatchWriteItem@ and, for each table, information about any item collections that were affected by individual @DeleteItem@ or @PutItem@ operations.
--
-- Each entry consists of the following subelements:
--
--     * @ItemCollectionKey@ - The partition key value of the item collection. This is the same as the partition key value of the item.
--
--
--     * @SizeEstimateRangeGB@ - An estimate of item collection size, expressed in GB. This is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on the table. Use this estimate to measure whether a local secondary index is approaching its size limit.
-- The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
--
--
--
-- /Note:/ Consider using 'itemCollectionMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwirsItemCollectionMetrics :: Lens.Lens' BatchWriteItemResponse (Lude.Maybe (Lude.HashMap Lude.Text ([ItemCollectionMetrics])))
bwirsItemCollectionMetrics = Lens.lens (itemCollectionMetrics :: BatchWriteItemResponse -> Lude.Maybe (Lude.HashMap Lude.Text ([ItemCollectionMetrics]))) (\s a -> s {itemCollectionMetrics = a} :: BatchWriteItemResponse)
{-# DEPRECATED bwirsItemCollectionMetrics "Use generic-lens or generic-optics with 'itemCollectionMetrics' instead." #-}

-- | The capacity units consumed by the entire @BatchWriteItem@ operation.
--
-- Each element consists of:
--
--     * @TableName@ - The table that consumed the provisioned throughput.
--
--
--     * @CapacityUnits@ - The total number of capacity units consumed.
--
--
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwirsConsumedCapacity :: Lens.Lens' BatchWriteItemResponse (Lude.Maybe [ConsumedCapacity])
bwirsConsumedCapacity = Lens.lens (consumedCapacity :: BatchWriteItemResponse -> Lude.Maybe [ConsumedCapacity]) (\s a -> s {consumedCapacity = a} :: BatchWriteItemResponse)
{-# DEPRECATED bwirsConsumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead." #-}

-- | A map of tables and requests against those tables that were not processed. The @UnprocessedItems@ value is in the same form as @RequestItems@ , so you can provide this value directly to a subsequent @BatchGetItem@ operation. For more information, see @RequestItems@ in the Request Parameters section.
--
-- Each @UnprocessedItems@ entry consists of a table name and, for that table, a list of operations to perform (@DeleteRequest@ or @PutRequest@ ).
--
--     * @DeleteRequest@ - Perform a @DeleteItem@ operation on the specified item. The item to be deleted is identified by a @Key@ subelement:
--
--     * @Key@ - A map of primary key attribute values that uniquely identify the item. Each entry in this map consists of an attribute name and an attribute value.
--
--
--
--
--     * @PutRequest@ - Perform a @PutItem@ operation on the specified item. The item to be put is identified by an @Item@ subelement:
--
--     * @Item@ - A map of attributes and their values. Each entry in this map consists of an attribute name and an attribute value. Attribute values must not be null; string and binary type attributes must have lengths greater than zero; and set type attributes must not be empty. Requests that contain empty values will be rejected with a @ValidationException@ exception.
-- If you specify any attributes that are part of an index key, then the data types for those attributes must match those of the schema in the table's attribute definition.
--
--
--
--
-- If there are no unprocessed items remaining, the response contains an empty @UnprocessedItems@ map.
--
-- /Note:/ Consider using 'unprocessedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwirsUnprocessedItems :: Lens.Lens' BatchWriteItemResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.NonEmpty WriteRequest)))
bwirsUnprocessedItems = Lens.lens (unprocessedItems :: BatchWriteItemResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.NonEmpty WriteRequest))) (\s a -> s {unprocessedItems = a} :: BatchWriteItemResponse)
{-# DEPRECATED bwirsUnprocessedItems "Use generic-lens or generic-optics with 'unprocessedItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwirsResponseStatus :: Lens.Lens' BatchWriteItemResponse Lude.Int
bwirsResponseStatus = Lens.lens (responseStatus :: BatchWriteItemResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchWriteItemResponse)
{-# DEPRECATED bwirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
