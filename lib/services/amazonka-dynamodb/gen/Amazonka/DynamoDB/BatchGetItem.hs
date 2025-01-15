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
-- Module      : Amazonka.DynamoDB.BatchGetItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @BatchGetItem@ operation returns the attributes of one or more items
-- from one or more tables. You identify requested items by primary key.
--
-- A single operation can retrieve up to 16 MB of data, which can contain
-- as many as 100 items. @BatchGetItem@ returns a partial result if the
-- response size limit is exceeded, the table\'s provisioned throughput is
-- exceeded, or an internal processing failure occurs. If a partial result
-- is returned, the operation returns a value for @UnprocessedKeys@. You
-- can use this value to retry the operation starting with the next item to
-- get.
--
-- If you request more than 100 items, @BatchGetItem@ returns a
-- @ValidationException@ with the message \"Too many items requested for
-- the BatchGetItem call.\"
--
-- For example, if you ask to retrieve 100 items, but each individual item
-- is 300 KB in size, the system returns 52 items (so as not to exceed the
-- 16 MB limit). It also returns an appropriate @UnprocessedKeys@ value so
-- you can get the next page of results. If desired, your application can
-- include its own logic to assemble the pages of results into one dataset.
--
-- If /none/ of the items can be processed due to insufficient provisioned
-- throughput on all of the tables in the request, then @BatchGetItem@
-- returns a @ProvisionedThroughputExceededException@. If /at least one/ of
-- the items is successfully processed, then @BatchGetItem@ completes
-- successfully, while returning the keys of the unread items in
-- @UnprocessedKeys@.
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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#BatchOperations Batch Operations and Error Handling>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- By default, @BatchGetItem@ performs eventually consistent reads on every
-- table in the request. If you want strongly consistent reads instead, you
-- can set @ConsistentRead@ to @true@ for any or all tables.
--
-- In order to minimize response latency, @BatchGetItem@ retrieves items in
-- parallel.
--
-- When designing your application, keep in mind that DynamoDB does not
-- return items in any particular order. To help parse the response by
-- item, include the primary key values for the items in your request in
-- the @ProjectionExpression@ parameter.
--
-- If a requested item does not exist, it is not returned in the result.
-- Requests for nonexistent items consume the minimum read capacity units
-- according to the type of read. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#CapacityUnitCalculations Working with Tables>
-- in the /Amazon DynamoDB Developer Guide/.
module Amazonka.DynamoDB.BatchGetItem
  ( -- * Creating a Request
    BatchGetItem (..),
    newBatchGetItem,

    -- * Request Lenses
    batchGetItem_returnConsumedCapacity,
    batchGetItem_requestItems,

    -- * Destructuring the Response
    BatchGetItemResponse (..),
    newBatchGetItemResponse,

    -- * Response Lenses
    batchGetItemResponse_consumedCapacity,
    batchGetItemResponse_httpStatus,
    batchGetItemResponse_responses,
    batchGetItemResponse_unprocessedKeys,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @BatchGetItem@ operation.
--
-- /See:/ 'newBatchGetItem' smart constructor.
data BatchGetItem = BatchGetItem'
  { returnConsumedCapacity :: Prelude.Maybe ReturnConsumedCapacity,
    -- | A map of one or more table names and, for each table, a map that
    -- describes one or more items to retrieve from that table. Each table name
    -- can be used only once per @BatchGetItem@ request.
    --
    -- Each element in the map of items to retrieve consists of the following:
    --
    -- -   @ConsistentRead@ - If @true@, a strongly consistent read is used; if
    --     @false@ (the default), an eventually consistent read is used.
    --
    -- -   @ExpressionAttributeNames@ - One or more substitution tokens for
    --     attribute names in the @ProjectionExpression@ parameter. The
    --     following are some use cases for using @ExpressionAttributeNames@:
    --
    --     -   To access an attribute whose name conflicts with a DynamoDB
    --         reserved word.
    --
    --     -   To create a placeholder for repeating occurrences of an
    --         attribute name in an expression.
    --
    --     -   To prevent special characters in an attribute name from being
    --         misinterpreted in an expression.
    --
    --     Use the __#__ character in an expression to dereference an attribute
    --     name. For example, consider the following attribute name:
    --
    --     -   @Percentile@
    --
    --     The name of this attribute conflicts with a reserved word, so it
    --     cannot be used directly in an expression. (For the complete list of
    --     reserved words, see
    --     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
    --     in the /Amazon DynamoDB Developer Guide/). To work around this, you
    --     could specify the following for @ExpressionAttributeNames@:
    --
    --     -   @{\"#P\":\"Percentile\"}@
    --
    --     You could then use this substitution in an expression, as in this
    --     example:
    --
    --     -   @#P = :val@
    --
    --     Tokens that begin with the __:__ character are /expression attribute
    --     values/, which are placeholders for the actual value at runtime.
    --
    --     For more information about expression attribute names, see
    --     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
    --     in the /Amazon DynamoDB Developer Guide/.
    --
    -- -   @Keys@ - An array of primary key attribute values that define
    --     specific items in the table. For each primary key, you must provide
    --     /all/ of the key attributes. For example, with a simple primary key,
    --     you only need to provide the partition key value. For a composite
    --     key, you must provide /both/ the partition key value and the sort
    --     key value.
    --
    -- -   @ProjectionExpression@ - A string that identifies one or more
    --     attributes to retrieve from the table. These attributes can include
    --     scalars, sets, or elements of a JSON document. The attributes in the
    --     expression must be separated by commas.
    --
    --     If no attribute names are specified, then all attributes are
    --     returned. If any of the requested attributes are not found, they do
    --     not appear in the result.
    --
    --     For more information, see
    --     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
    --     in the /Amazon DynamoDB Developer Guide/.
    --
    -- -   @AttributesToGet@ - This is a legacy parameter. Use
    --     @ProjectionExpression@ instead. For more information, see
    --     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet>
    --     in the /Amazon DynamoDB Developer Guide/.
    requestItems :: Prelude.HashMap Prelude.Text KeysAndAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnConsumedCapacity', 'batchGetItem_returnConsumedCapacity' - Undocumented member.
--
-- 'requestItems', 'batchGetItem_requestItems' - A map of one or more table names and, for each table, a map that
-- describes one or more items to retrieve from that table. Each table name
-- can be used only once per @BatchGetItem@ request.
--
-- Each element in the map of items to retrieve consists of the following:
--
-- -   @ConsistentRead@ - If @true@, a strongly consistent read is used; if
--     @false@ (the default), an eventually consistent read is used.
--
-- -   @ExpressionAttributeNames@ - One or more substitution tokens for
--     attribute names in the @ProjectionExpression@ parameter. The
--     following are some use cases for using @ExpressionAttributeNames@:
--
--     -   To access an attribute whose name conflicts with a DynamoDB
--         reserved word.
--
--     -   To create a placeholder for repeating occurrences of an
--         attribute name in an expression.
--
--     -   To prevent special characters in an attribute name from being
--         misinterpreted in an expression.
--
--     Use the __#__ character in an expression to dereference an attribute
--     name. For example, consider the following attribute name:
--
--     -   @Percentile@
--
--     The name of this attribute conflicts with a reserved word, so it
--     cannot be used directly in an expression. (For the complete list of
--     reserved words, see
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
--     in the /Amazon DynamoDB Developer Guide/). To work around this, you
--     could specify the following for @ExpressionAttributeNames@:
--
--     -   @{\"#P\":\"Percentile\"}@
--
--     You could then use this substitution in an expression, as in this
--     example:
--
--     -   @#P = :val@
--
--     Tokens that begin with the __:__ character are /expression attribute
--     values/, which are placeholders for the actual value at runtime.
--
--     For more information about expression attribute names, see
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
--     in the /Amazon DynamoDB Developer Guide/.
--
-- -   @Keys@ - An array of primary key attribute values that define
--     specific items in the table. For each primary key, you must provide
--     /all/ of the key attributes. For example, with a simple primary key,
--     you only need to provide the partition key value. For a composite
--     key, you must provide /both/ the partition key value and the sort
--     key value.
--
-- -   @ProjectionExpression@ - A string that identifies one or more
--     attributes to retrieve from the table. These attributes can include
--     scalars, sets, or elements of a JSON document. The attributes in the
--     expression must be separated by commas.
--
--     If no attribute names are specified, then all attributes are
--     returned. If any of the requested attributes are not found, they do
--     not appear in the result.
--
--     For more information, see
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
--     in the /Amazon DynamoDB Developer Guide/.
--
-- -   @AttributesToGet@ - This is a legacy parameter. Use
--     @ProjectionExpression@ instead. For more information, see
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet>
--     in the /Amazon DynamoDB Developer Guide/.
newBatchGetItem ::
  BatchGetItem
newBatchGetItem =
  BatchGetItem'
    { returnConsumedCapacity =
        Prelude.Nothing,
      requestItems = Prelude.mempty
    }

-- | Undocumented member.
batchGetItem_returnConsumedCapacity :: Lens.Lens' BatchGetItem (Prelude.Maybe ReturnConsumedCapacity)
batchGetItem_returnConsumedCapacity = Lens.lens (\BatchGetItem' {returnConsumedCapacity} -> returnConsumedCapacity) (\s@BatchGetItem' {} a -> s {returnConsumedCapacity = a} :: BatchGetItem)

-- | A map of one or more table names and, for each table, a map that
-- describes one or more items to retrieve from that table. Each table name
-- can be used only once per @BatchGetItem@ request.
--
-- Each element in the map of items to retrieve consists of the following:
--
-- -   @ConsistentRead@ - If @true@, a strongly consistent read is used; if
--     @false@ (the default), an eventually consistent read is used.
--
-- -   @ExpressionAttributeNames@ - One or more substitution tokens for
--     attribute names in the @ProjectionExpression@ parameter. The
--     following are some use cases for using @ExpressionAttributeNames@:
--
--     -   To access an attribute whose name conflicts with a DynamoDB
--         reserved word.
--
--     -   To create a placeholder for repeating occurrences of an
--         attribute name in an expression.
--
--     -   To prevent special characters in an attribute name from being
--         misinterpreted in an expression.
--
--     Use the __#__ character in an expression to dereference an attribute
--     name. For example, consider the following attribute name:
--
--     -   @Percentile@
--
--     The name of this attribute conflicts with a reserved word, so it
--     cannot be used directly in an expression. (For the complete list of
--     reserved words, see
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
--     in the /Amazon DynamoDB Developer Guide/). To work around this, you
--     could specify the following for @ExpressionAttributeNames@:
--
--     -   @{\"#P\":\"Percentile\"}@
--
--     You could then use this substitution in an expression, as in this
--     example:
--
--     -   @#P = :val@
--
--     Tokens that begin with the __:__ character are /expression attribute
--     values/, which are placeholders for the actual value at runtime.
--
--     For more information about expression attribute names, see
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
--     in the /Amazon DynamoDB Developer Guide/.
--
-- -   @Keys@ - An array of primary key attribute values that define
--     specific items in the table. For each primary key, you must provide
--     /all/ of the key attributes. For example, with a simple primary key,
--     you only need to provide the partition key value. For a composite
--     key, you must provide /both/ the partition key value and the sort
--     key value.
--
-- -   @ProjectionExpression@ - A string that identifies one or more
--     attributes to retrieve from the table. These attributes can include
--     scalars, sets, or elements of a JSON document. The attributes in the
--     expression must be separated by commas.
--
--     If no attribute names are specified, then all attributes are
--     returned. If any of the requested attributes are not found, they do
--     not appear in the result.
--
--     For more information, see
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
--     in the /Amazon DynamoDB Developer Guide/.
--
-- -   @AttributesToGet@ - This is a legacy parameter. Use
--     @ProjectionExpression@ instead. For more information, see
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet>
--     in the /Amazon DynamoDB Developer Guide/.
batchGetItem_requestItems :: Lens.Lens' BatchGetItem (Prelude.HashMap Prelude.Text KeysAndAttributes)
batchGetItem_requestItems = Lens.lens (\BatchGetItem' {requestItems} -> requestItems) (\s@BatchGetItem' {} a -> s {requestItems = a} :: BatchGetItem) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetItem where
  type AWSResponse BatchGetItem = BatchGetItemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetItemResponse'
            Prelude.<$> ( x
                            Data..?> "ConsumedCapacity"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Responses" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "UnprocessedKeys"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable BatchGetItem where
  hashWithSalt _salt BatchGetItem' {..} =
    _salt
      `Prelude.hashWithSalt` returnConsumedCapacity
      `Prelude.hashWithSalt` requestItems

instance Prelude.NFData BatchGetItem where
  rnf BatchGetItem' {..} =
    Prelude.rnf returnConsumedCapacity `Prelude.seq`
      Prelude.rnf requestItems

instance Data.ToHeaders BatchGetItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.BatchGetItem" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetItem where
  toJSON BatchGetItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ReturnConsumedCapacity" Data..=)
              Prelude.<$> returnConsumedCapacity,
            Prelude.Just ("RequestItems" Data..= requestItems)
          ]
      )

instance Data.ToPath BatchGetItem where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetItem where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @BatchGetItem@ operation.
--
-- /See:/ 'newBatchGetItemResponse' smart constructor.
data BatchGetItemResponse = BatchGetItemResponse'
  { -- | The read capacity units consumed by the entire @BatchGetItem@ operation.
    --
    -- Each element consists of:
    --
    -- -   @TableName@ - The table that consumed the provisioned throughput.
    --
    -- -   @CapacityUnits@ - The total number of capacity units consumed.
    consumedCapacity :: Prelude.Maybe [ConsumedCapacity],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A map of table name to a list of items. Each object in @Responses@
    -- consists of a table name, along with a map of attribute data consisting
    -- of the data type and attribute value.
    responses :: Prelude.HashMap Prelude.Text [Prelude.HashMap Prelude.Text AttributeValue],
    -- | A map of tables and their respective keys that were not processed with
    -- the current response. The @UnprocessedKeys@ value is in the same form as
    -- @RequestItems@, so the value can be provided directly to a subsequent
    -- @BatchGetItem@ operation. For more information, see @RequestItems@ in
    -- the Request Parameters section.
    --
    -- Each element consists of:
    --
    -- -   @Keys@ - An array of primary key attribute values that define
    --     specific items in the table.
    --
    -- -   @ProjectionExpression@ - One or more attributes to be retrieved from
    --     the table or index. By default, all attributes are returned. If a
    --     requested attribute is not found, it does not appear in the result.
    --
    -- -   @ConsistentRead@ - The consistency of a read operation. If set to
    --     @true@, then a strongly consistent read is used; otherwise, an
    --     eventually consistent read is used.
    --
    -- If there are no unprocessed keys remaining, the response contains an
    -- empty @UnprocessedKeys@ map.
    unprocessedKeys :: Prelude.HashMap Prelude.Text KeysAndAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumedCapacity', 'batchGetItemResponse_consumedCapacity' - The read capacity units consumed by the entire @BatchGetItem@ operation.
--
-- Each element consists of:
--
-- -   @TableName@ - The table that consumed the provisioned throughput.
--
-- -   @CapacityUnits@ - The total number of capacity units consumed.
--
-- 'httpStatus', 'batchGetItemResponse_httpStatus' - The response's http status code.
--
-- 'responses', 'batchGetItemResponse_responses' - A map of table name to a list of items. Each object in @Responses@
-- consists of a table name, along with a map of attribute data consisting
-- of the data type and attribute value.
--
-- 'unprocessedKeys', 'batchGetItemResponse_unprocessedKeys' - A map of tables and their respective keys that were not processed with
-- the current response. The @UnprocessedKeys@ value is in the same form as
-- @RequestItems@, so the value can be provided directly to a subsequent
-- @BatchGetItem@ operation. For more information, see @RequestItems@ in
-- the Request Parameters section.
--
-- Each element consists of:
--
-- -   @Keys@ - An array of primary key attribute values that define
--     specific items in the table.
--
-- -   @ProjectionExpression@ - One or more attributes to be retrieved from
--     the table or index. By default, all attributes are returned. If a
--     requested attribute is not found, it does not appear in the result.
--
-- -   @ConsistentRead@ - The consistency of a read operation. If set to
--     @true@, then a strongly consistent read is used; otherwise, an
--     eventually consistent read is used.
--
-- If there are no unprocessed keys remaining, the response contains an
-- empty @UnprocessedKeys@ map.
newBatchGetItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetItemResponse
newBatchGetItemResponse pHttpStatus_ =
  BatchGetItemResponse'
    { consumedCapacity =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      responses = Prelude.mempty,
      unprocessedKeys = Prelude.mempty
    }

-- | The read capacity units consumed by the entire @BatchGetItem@ operation.
--
-- Each element consists of:
--
-- -   @TableName@ - The table that consumed the provisioned throughput.
--
-- -   @CapacityUnits@ - The total number of capacity units consumed.
batchGetItemResponse_consumedCapacity :: Lens.Lens' BatchGetItemResponse (Prelude.Maybe [ConsumedCapacity])
batchGetItemResponse_consumedCapacity = Lens.lens (\BatchGetItemResponse' {consumedCapacity} -> consumedCapacity) (\s@BatchGetItemResponse' {} a -> s {consumedCapacity = a} :: BatchGetItemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetItemResponse_httpStatus :: Lens.Lens' BatchGetItemResponse Prelude.Int
batchGetItemResponse_httpStatus = Lens.lens (\BatchGetItemResponse' {httpStatus} -> httpStatus) (\s@BatchGetItemResponse' {} a -> s {httpStatus = a} :: BatchGetItemResponse)

-- | A map of table name to a list of items. Each object in @Responses@
-- consists of a table name, along with a map of attribute data consisting
-- of the data type and attribute value.
batchGetItemResponse_responses :: Lens.Lens' BatchGetItemResponse (Prelude.HashMap Prelude.Text [Prelude.HashMap Prelude.Text AttributeValue])
batchGetItemResponse_responses = Lens.lens (\BatchGetItemResponse' {responses} -> responses) (\s@BatchGetItemResponse' {} a -> s {responses = a} :: BatchGetItemResponse) Prelude.. Lens.coerced

-- | A map of tables and their respective keys that were not processed with
-- the current response. The @UnprocessedKeys@ value is in the same form as
-- @RequestItems@, so the value can be provided directly to a subsequent
-- @BatchGetItem@ operation. For more information, see @RequestItems@ in
-- the Request Parameters section.
--
-- Each element consists of:
--
-- -   @Keys@ - An array of primary key attribute values that define
--     specific items in the table.
--
-- -   @ProjectionExpression@ - One or more attributes to be retrieved from
--     the table or index. By default, all attributes are returned. If a
--     requested attribute is not found, it does not appear in the result.
--
-- -   @ConsistentRead@ - The consistency of a read operation. If set to
--     @true@, then a strongly consistent read is used; otherwise, an
--     eventually consistent read is used.
--
-- If there are no unprocessed keys remaining, the response contains an
-- empty @UnprocessedKeys@ map.
batchGetItemResponse_unprocessedKeys :: Lens.Lens' BatchGetItemResponse (Prelude.HashMap Prelude.Text KeysAndAttributes)
batchGetItemResponse_unprocessedKeys = Lens.lens (\BatchGetItemResponse' {unprocessedKeys} -> unprocessedKeys) (\s@BatchGetItemResponse' {} a -> s {unprocessedKeys = a} :: BatchGetItemResponse) Prelude.. Lens.coerced

instance Prelude.NFData BatchGetItemResponse where
  rnf BatchGetItemResponse' {..} =
    Prelude.rnf consumedCapacity `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf responses `Prelude.seq`
          Prelude.rnf unprocessedKeys
