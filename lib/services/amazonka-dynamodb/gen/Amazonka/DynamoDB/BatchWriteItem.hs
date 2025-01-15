{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DynamoDB.BatchWriteItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @BatchWriteItem@ operation puts or deletes multiple items in one or
-- more tables. A single call to @BatchWriteItem@ can transmit up to 16MB
-- of data over the network, consisting of up to 25 item put or delete
-- operations. While individual items can be up to 400 KB once stored,
-- it\'s important to note that an item\'s representation might be greater
-- than 400KB while being sent in DynamoDB\'s JSON format for the API call.
-- For more details on this distinction, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html Naming Rules and Data Types>.
--
-- @BatchWriteItem@ cannot update items. If you perform a @BatchWriteItem@
-- operation on an existing item, that item\'s values will be overwritten
-- by the operation and it will appear like it was updated. To update
-- items, we recommend you use the @UpdateItem@ action.
--
-- The individual @PutItem@ and @DeleteItem@ operations specified in
-- @BatchWriteItem@ are atomic; however @BatchWriteItem@ as a whole is not.
-- If any requested operations fail because the table\'s provisioned
-- throughput is exceeded or an internal processing failure occurs, the
-- failed operations are returned in the @UnprocessedItems@ response
-- parameter. You can investigate and optionally resend the requests.
-- Typically, you would call @BatchWriteItem@ in a loop. Each iteration
-- would check for unprocessed items and submit a new @BatchWriteItem@
-- request with those unprocessed items until all items have been
-- processed.
--
-- If /none/ of the items can be processed due to insufficient provisioned
-- throughput on all of the tables in the request, then @BatchWriteItem@
-- returns a @ProvisionedThroughputExceededException@.
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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#Programming.Errors.BatchOperations Batch Operations and Error Handling>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- With @BatchWriteItem@, you can efficiently write or delete large amounts
-- of data, such as from Amazon EMR, or copy data from another database
-- into DynamoDB. In order to improve performance with these large-scale
-- operations, @BatchWriteItem@ does not behave in the same way as
-- individual @PutItem@ and @DeleteItem@ calls would. For example, you
-- cannot specify conditions on individual put and delete requests, and
-- @BatchWriteItem@ does not return deleted items in the response.
--
-- If you use a programming language that supports concurrency, you can use
-- threads to write items in parallel. Your application must include the
-- necessary logic to manage the threads. With languages that don\'t
-- support threading, you must update or delete the specified items one at
-- a time. In both situations, @BatchWriteItem@ performs the specified put
-- and delete operations in parallel, giving you the power of the thread
-- pool approach without having to introduce complexity into your
-- application.
--
-- Parallel processing reduces latency, but each specified put and delete
-- request consumes the same number of write capacity units whether it is
-- processed in parallel or not. Delete operations on nonexistent items
-- consume one write capacity unit.
--
-- If one or more of the following is true, DynamoDB rejects the entire
-- batch write operation:
--
-- -   One or more tables specified in the @BatchWriteItem@ request does
--     not exist.
--
-- -   Primary key attributes specified on an item in the request do not
--     match those in the corresponding table\'s primary key schema.
--
-- -   You try to perform multiple operations on the same item in the same
--     @BatchWriteItem@ request. For example, you cannot put and delete the
--     same item in the same @BatchWriteItem@ request.
--
-- -   Your request contains at least two items with identical hash and
--     range keys (which essentially is two put operations).
--
-- -   There are more than 25 requests in the batch.
--
-- -   Any individual item in a batch exceeds 400 KB.
--
-- -   The total request size exceeds 16 MB.
module Amazonka.DynamoDB.BatchWriteItem
  ( -- * Creating a Request
    BatchWriteItem (..),
    newBatchWriteItem,

    -- * Request Lenses
    batchWriteItem_returnConsumedCapacity,
    batchWriteItem_returnItemCollectionMetrics,
    batchWriteItem_requestItems,

    -- * Destructuring the Response
    BatchWriteItemResponse (..),
    newBatchWriteItemResponse,

    -- * Response Lenses
    batchWriteItemResponse_consumedCapacity,
    batchWriteItemResponse_itemCollectionMetrics,
    batchWriteItemResponse_httpStatus,
    batchWriteItemResponse_unprocessedItems,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @BatchWriteItem@ operation.
--
-- /See:/ 'newBatchWriteItem' smart constructor.
data BatchWriteItem = BatchWriteItem'
  { returnConsumedCapacity :: Prelude.Maybe ReturnConsumedCapacity,
    -- | Determines whether item collection metrics are returned. If set to
    -- @SIZE@, the response includes statistics about item collections, if any,
    -- that were modified during the operation are returned in the response. If
    -- set to @NONE@ (the default), no statistics are returned.
    returnItemCollectionMetrics :: Prelude.Maybe ReturnItemCollectionMetrics,
    -- | A map of one or more table names and, for each table, a list of
    -- operations to be performed (@DeleteRequest@ or @PutRequest@). Each
    -- element in the map consists of the following:
    --
    -- -   @DeleteRequest@ - Perform a @DeleteItem@ operation on the specified
    --     item. The item to be deleted is identified by a @Key@ subelement:
    --
    --     -   @Key@ - A map of primary key attribute values that uniquely
    --         identify the item. Each entry in this map consists of an
    --         attribute name and an attribute value. For each primary key, you
    --         must provide /all/ of the key attributes. For example, with a
    --         simple primary key, you only need to provide a value for the
    --         partition key. For a composite primary key, you must provide
    --         values for /both/ the partition key and the sort key.
    --
    -- -   @PutRequest@ - Perform a @PutItem@ operation on the specified item.
    --     The item to be put is identified by an @Item@ subelement:
    --
    --     -   @Item@ - A map of attributes and their values. Each entry in
    --         this map consists of an attribute name and an attribute value.
    --         Attribute values must not be null; string and binary type
    --         attributes must have lengths greater than zero; and set type
    --         attributes must not be empty. Requests that contain empty values
    --         are rejected with a @ValidationException@ exception.
    --
    --         If you specify any attributes that are part of an index key,
    --         then the data types for those attributes must match those of the
    --         schema in the table\'s attribute definition.
    requestItems :: Prelude.HashMap Prelude.Text (Prelude.NonEmpty WriteRequest)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchWriteItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnConsumedCapacity', 'batchWriteItem_returnConsumedCapacity' - Undocumented member.
--
-- 'returnItemCollectionMetrics', 'batchWriteItem_returnItemCollectionMetrics' - Determines whether item collection metrics are returned. If set to
-- @SIZE@, the response includes statistics about item collections, if any,
-- that were modified during the operation are returned in the response. If
-- set to @NONE@ (the default), no statistics are returned.
--
-- 'requestItems', 'batchWriteItem_requestItems' - A map of one or more table names and, for each table, a list of
-- operations to be performed (@DeleteRequest@ or @PutRequest@). Each
-- element in the map consists of the following:
--
-- -   @DeleteRequest@ - Perform a @DeleteItem@ operation on the specified
--     item. The item to be deleted is identified by a @Key@ subelement:
--
--     -   @Key@ - A map of primary key attribute values that uniquely
--         identify the item. Each entry in this map consists of an
--         attribute name and an attribute value. For each primary key, you
--         must provide /all/ of the key attributes. For example, with a
--         simple primary key, you only need to provide a value for the
--         partition key. For a composite primary key, you must provide
--         values for /both/ the partition key and the sort key.
--
-- -   @PutRequest@ - Perform a @PutItem@ operation on the specified item.
--     The item to be put is identified by an @Item@ subelement:
--
--     -   @Item@ - A map of attributes and their values. Each entry in
--         this map consists of an attribute name and an attribute value.
--         Attribute values must not be null; string and binary type
--         attributes must have lengths greater than zero; and set type
--         attributes must not be empty. Requests that contain empty values
--         are rejected with a @ValidationException@ exception.
--
--         If you specify any attributes that are part of an index key,
--         then the data types for those attributes must match those of the
--         schema in the table\'s attribute definition.
newBatchWriteItem ::
  BatchWriteItem
newBatchWriteItem =
  BatchWriteItem'
    { returnConsumedCapacity =
        Prelude.Nothing,
      returnItemCollectionMetrics = Prelude.Nothing,
      requestItems = Prelude.mempty
    }

-- | Undocumented member.
batchWriteItem_returnConsumedCapacity :: Lens.Lens' BatchWriteItem (Prelude.Maybe ReturnConsumedCapacity)
batchWriteItem_returnConsumedCapacity = Lens.lens (\BatchWriteItem' {returnConsumedCapacity} -> returnConsumedCapacity) (\s@BatchWriteItem' {} a -> s {returnConsumedCapacity = a} :: BatchWriteItem)

-- | Determines whether item collection metrics are returned. If set to
-- @SIZE@, the response includes statistics about item collections, if any,
-- that were modified during the operation are returned in the response. If
-- set to @NONE@ (the default), no statistics are returned.
batchWriteItem_returnItemCollectionMetrics :: Lens.Lens' BatchWriteItem (Prelude.Maybe ReturnItemCollectionMetrics)
batchWriteItem_returnItemCollectionMetrics = Lens.lens (\BatchWriteItem' {returnItemCollectionMetrics} -> returnItemCollectionMetrics) (\s@BatchWriteItem' {} a -> s {returnItemCollectionMetrics = a} :: BatchWriteItem)

-- | A map of one or more table names and, for each table, a list of
-- operations to be performed (@DeleteRequest@ or @PutRequest@). Each
-- element in the map consists of the following:
--
-- -   @DeleteRequest@ - Perform a @DeleteItem@ operation on the specified
--     item. The item to be deleted is identified by a @Key@ subelement:
--
--     -   @Key@ - A map of primary key attribute values that uniquely
--         identify the item. Each entry in this map consists of an
--         attribute name and an attribute value. For each primary key, you
--         must provide /all/ of the key attributes. For example, with a
--         simple primary key, you only need to provide a value for the
--         partition key. For a composite primary key, you must provide
--         values for /both/ the partition key and the sort key.
--
-- -   @PutRequest@ - Perform a @PutItem@ operation on the specified item.
--     The item to be put is identified by an @Item@ subelement:
--
--     -   @Item@ - A map of attributes and their values. Each entry in
--         this map consists of an attribute name and an attribute value.
--         Attribute values must not be null; string and binary type
--         attributes must have lengths greater than zero; and set type
--         attributes must not be empty. Requests that contain empty values
--         are rejected with a @ValidationException@ exception.
--
--         If you specify any attributes that are part of an index key,
--         then the data types for those attributes must match those of the
--         schema in the table\'s attribute definition.
batchWriteItem_requestItems :: Lens.Lens' BatchWriteItem (Prelude.HashMap Prelude.Text (Prelude.NonEmpty WriteRequest))
batchWriteItem_requestItems = Lens.lens (\BatchWriteItem' {requestItems} -> requestItems) (\s@BatchWriteItem' {} a -> s {requestItems = a} :: BatchWriteItem) Prelude.. Lens.coerced

instance Core.AWSRequest BatchWriteItem where
  type
    AWSResponse BatchWriteItem =
      BatchWriteItemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchWriteItemResponse'
            Prelude.<$> ( x
                            Data..?> "ConsumedCapacity"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "ItemCollectionMetrics"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "UnprocessedItems"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable BatchWriteItem where
  hashWithSalt _salt BatchWriteItem' {..} =
    _salt
      `Prelude.hashWithSalt` returnConsumedCapacity
      `Prelude.hashWithSalt` returnItemCollectionMetrics
      `Prelude.hashWithSalt` requestItems

instance Prelude.NFData BatchWriteItem where
  rnf BatchWriteItem' {..} =
    Prelude.rnf returnConsumedCapacity `Prelude.seq`
      Prelude.rnf returnItemCollectionMetrics `Prelude.seq`
        Prelude.rnf requestItems

instance Data.ToHeaders BatchWriteItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.BatchWriteItem" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchWriteItem where
  toJSON BatchWriteItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ReturnConsumedCapacity" Data..=)
              Prelude.<$> returnConsumedCapacity,
            ("ReturnItemCollectionMetrics" Data..=)
              Prelude.<$> returnItemCollectionMetrics,
            Prelude.Just ("RequestItems" Data..= requestItems)
          ]
      )

instance Data.ToPath BatchWriteItem where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchWriteItem where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @BatchWriteItem@ operation.
--
-- /See:/ 'newBatchWriteItemResponse' smart constructor.
data BatchWriteItemResponse = BatchWriteItemResponse'
  { -- | The capacity units consumed by the entire @BatchWriteItem@ operation.
    --
    -- Each element consists of:
    --
    -- -   @TableName@ - The table that consumed the provisioned throughput.
    --
    -- -   @CapacityUnits@ - The total number of capacity units consumed.
    consumedCapacity :: Prelude.Maybe [ConsumedCapacity],
    -- | A list of tables that were processed by @BatchWriteItem@ and, for each
    -- table, information about any item collections that were affected by
    -- individual @DeleteItem@ or @PutItem@ operations.
    --
    -- Each entry consists of the following subelements:
    --
    -- -   @ItemCollectionKey@ - The partition key value of the item
    --     collection. This is the same as the partition key value of the item.
    --
    -- -   @SizeEstimateRangeGB@ - An estimate of item collection size,
    --     expressed in GB. This is a two-element array containing a lower
    --     bound and an upper bound for the estimate. The estimate includes the
    --     size of all the items in the table, plus the size of all attributes
    --     projected into all of the local secondary indexes on the table. Use
    --     this estimate to measure whether a local secondary index is
    --     approaching its size limit.
    --
    --     The estimate is subject to change over time; therefore, do not rely
    --     on the precision or accuracy of the estimate.
    itemCollectionMetrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text [ItemCollectionMetrics]),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A map of tables and requests against those tables that were not
    -- processed. The @UnprocessedItems@ value is in the same form as
    -- @RequestItems@, so you can provide this value directly to a subsequent
    -- @BatchWriteItem@ operation. For more information, see @RequestItems@ in
    -- the Request Parameters section.
    --
    -- Each @UnprocessedItems@ entry consists of a table name and, for that
    -- table, a list of operations to perform (@DeleteRequest@ or
    -- @PutRequest@).
    --
    -- -   @DeleteRequest@ - Perform a @DeleteItem@ operation on the specified
    --     item. The item to be deleted is identified by a @Key@ subelement:
    --
    --     -   @Key@ - A map of primary key attribute values that uniquely
    --         identify the item. Each entry in this map consists of an
    --         attribute name and an attribute value.
    --
    -- -   @PutRequest@ - Perform a @PutItem@ operation on the specified item.
    --     The item to be put is identified by an @Item@ subelement:
    --
    --     -   @Item@ - A map of attributes and their values. Each entry in
    --         this map consists of an attribute name and an attribute value.
    --         Attribute values must not be null; string and binary type
    --         attributes must have lengths greater than zero; and set type
    --         attributes must not be empty. Requests that contain empty values
    --         will be rejected with a @ValidationException@ exception.
    --
    --         If you specify any attributes that are part of an index key,
    --         then the data types for those attributes must match those of the
    --         schema in the table\'s attribute definition.
    --
    -- If there are no unprocessed items remaining, the response contains an
    -- empty @UnprocessedItems@ map.
    unprocessedItems :: Prelude.HashMap Prelude.Text (Prelude.NonEmpty WriteRequest)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchWriteItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumedCapacity', 'batchWriteItemResponse_consumedCapacity' - The capacity units consumed by the entire @BatchWriteItem@ operation.
--
-- Each element consists of:
--
-- -   @TableName@ - The table that consumed the provisioned throughput.
--
-- -   @CapacityUnits@ - The total number of capacity units consumed.
--
-- 'itemCollectionMetrics', 'batchWriteItemResponse_itemCollectionMetrics' - A list of tables that were processed by @BatchWriteItem@ and, for each
-- table, information about any item collections that were affected by
-- individual @DeleteItem@ or @PutItem@ operations.
--
-- Each entry consists of the following subelements:
--
-- -   @ItemCollectionKey@ - The partition key value of the item
--     collection. This is the same as the partition key value of the item.
--
-- -   @SizeEstimateRangeGB@ - An estimate of item collection size,
--     expressed in GB. This is a two-element array containing a lower
--     bound and an upper bound for the estimate. The estimate includes the
--     size of all the items in the table, plus the size of all attributes
--     projected into all of the local secondary indexes on the table. Use
--     this estimate to measure whether a local secondary index is
--     approaching its size limit.
--
--     The estimate is subject to change over time; therefore, do not rely
--     on the precision or accuracy of the estimate.
--
-- 'httpStatus', 'batchWriteItemResponse_httpStatus' - The response's http status code.
--
-- 'unprocessedItems', 'batchWriteItemResponse_unprocessedItems' - A map of tables and requests against those tables that were not
-- processed. The @UnprocessedItems@ value is in the same form as
-- @RequestItems@, so you can provide this value directly to a subsequent
-- @BatchWriteItem@ operation. For more information, see @RequestItems@ in
-- the Request Parameters section.
--
-- Each @UnprocessedItems@ entry consists of a table name and, for that
-- table, a list of operations to perform (@DeleteRequest@ or
-- @PutRequest@).
--
-- -   @DeleteRequest@ - Perform a @DeleteItem@ operation on the specified
--     item. The item to be deleted is identified by a @Key@ subelement:
--
--     -   @Key@ - A map of primary key attribute values that uniquely
--         identify the item. Each entry in this map consists of an
--         attribute name and an attribute value.
--
-- -   @PutRequest@ - Perform a @PutItem@ operation on the specified item.
--     The item to be put is identified by an @Item@ subelement:
--
--     -   @Item@ - A map of attributes and their values. Each entry in
--         this map consists of an attribute name and an attribute value.
--         Attribute values must not be null; string and binary type
--         attributes must have lengths greater than zero; and set type
--         attributes must not be empty. Requests that contain empty values
--         will be rejected with a @ValidationException@ exception.
--
--         If you specify any attributes that are part of an index key,
--         then the data types for those attributes must match those of the
--         schema in the table\'s attribute definition.
--
-- If there are no unprocessed items remaining, the response contains an
-- empty @UnprocessedItems@ map.
newBatchWriteItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchWriteItemResponse
newBatchWriteItemResponse pHttpStatus_ =
  BatchWriteItemResponse'
    { consumedCapacity =
        Prelude.Nothing,
      itemCollectionMetrics = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      unprocessedItems = Prelude.mempty
    }

-- | The capacity units consumed by the entire @BatchWriteItem@ operation.
--
-- Each element consists of:
--
-- -   @TableName@ - The table that consumed the provisioned throughput.
--
-- -   @CapacityUnits@ - The total number of capacity units consumed.
batchWriteItemResponse_consumedCapacity :: Lens.Lens' BatchWriteItemResponse (Prelude.Maybe [ConsumedCapacity])
batchWriteItemResponse_consumedCapacity = Lens.lens (\BatchWriteItemResponse' {consumedCapacity} -> consumedCapacity) (\s@BatchWriteItemResponse' {} a -> s {consumedCapacity = a} :: BatchWriteItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of tables that were processed by @BatchWriteItem@ and, for each
-- table, information about any item collections that were affected by
-- individual @DeleteItem@ or @PutItem@ operations.
--
-- Each entry consists of the following subelements:
--
-- -   @ItemCollectionKey@ - The partition key value of the item
--     collection. This is the same as the partition key value of the item.
--
-- -   @SizeEstimateRangeGB@ - An estimate of item collection size,
--     expressed in GB. This is a two-element array containing a lower
--     bound and an upper bound for the estimate. The estimate includes the
--     size of all the items in the table, plus the size of all attributes
--     projected into all of the local secondary indexes on the table. Use
--     this estimate to measure whether a local secondary index is
--     approaching its size limit.
--
--     The estimate is subject to change over time; therefore, do not rely
--     on the precision or accuracy of the estimate.
batchWriteItemResponse_itemCollectionMetrics :: Lens.Lens' BatchWriteItemResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [ItemCollectionMetrics]))
batchWriteItemResponse_itemCollectionMetrics = Lens.lens (\BatchWriteItemResponse' {itemCollectionMetrics} -> itemCollectionMetrics) (\s@BatchWriteItemResponse' {} a -> s {itemCollectionMetrics = a} :: BatchWriteItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchWriteItemResponse_httpStatus :: Lens.Lens' BatchWriteItemResponse Prelude.Int
batchWriteItemResponse_httpStatus = Lens.lens (\BatchWriteItemResponse' {httpStatus} -> httpStatus) (\s@BatchWriteItemResponse' {} a -> s {httpStatus = a} :: BatchWriteItemResponse)

-- | A map of tables and requests against those tables that were not
-- processed. The @UnprocessedItems@ value is in the same form as
-- @RequestItems@, so you can provide this value directly to a subsequent
-- @BatchWriteItem@ operation. For more information, see @RequestItems@ in
-- the Request Parameters section.
--
-- Each @UnprocessedItems@ entry consists of a table name and, for that
-- table, a list of operations to perform (@DeleteRequest@ or
-- @PutRequest@).
--
-- -   @DeleteRequest@ - Perform a @DeleteItem@ operation on the specified
--     item. The item to be deleted is identified by a @Key@ subelement:
--
--     -   @Key@ - A map of primary key attribute values that uniquely
--         identify the item. Each entry in this map consists of an
--         attribute name and an attribute value.
--
-- -   @PutRequest@ - Perform a @PutItem@ operation on the specified item.
--     The item to be put is identified by an @Item@ subelement:
--
--     -   @Item@ - A map of attributes and their values. Each entry in
--         this map consists of an attribute name and an attribute value.
--         Attribute values must not be null; string and binary type
--         attributes must have lengths greater than zero; and set type
--         attributes must not be empty. Requests that contain empty values
--         will be rejected with a @ValidationException@ exception.
--
--         If you specify any attributes that are part of an index key,
--         then the data types for those attributes must match those of the
--         schema in the table\'s attribute definition.
--
-- If there are no unprocessed items remaining, the response contains an
-- empty @UnprocessedItems@ map.
batchWriteItemResponse_unprocessedItems :: Lens.Lens' BatchWriteItemResponse (Prelude.HashMap Prelude.Text (Prelude.NonEmpty WriteRequest))
batchWriteItemResponse_unprocessedItems = Lens.lens (\BatchWriteItemResponse' {unprocessedItems} -> unprocessedItems) (\s@BatchWriteItemResponse' {} a -> s {unprocessedItems = a} :: BatchWriteItemResponse) Prelude.. Lens.coerced

instance Prelude.NFData BatchWriteItemResponse where
  rnf BatchWriteItemResponse' {..} =
    Prelude.rnf consumedCapacity `Prelude.seq`
      Prelude.rnf itemCollectionMetrics `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf unprocessedItems
