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
-- Module      : Network.AWS.DynamoDB.DeleteItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a single item in a table by primary key. You can perform a
-- conditional delete operation that deletes the item if it exists, or if
-- it has an expected attribute value.
--
-- In addition to deleting an item, you can also return the item\'s
-- attribute values in the same operation, using the @ReturnValues@
-- parameter.
--
-- Unless you specify conditions, the @DeleteItem@ is an idempotent
-- operation; running it multiple times on the same item or attribute does
-- /not/ result in an error response.
--
-- Conditional deletes are useful for deleting items only if specific
-- conditions are met. If those conditions are met, DynamoDB performs the
-- delete. Otherwise, the item is not deleted.
module Network.AWS.DynamoDB.DeleteItem
  ( -- * Creating a Request
    DeleteItem (..),
    newDeleteItem,

    -- * Request Lenses
    deleteItem_expected,
    deleteItem_expressionAttributeValues,
    deleteItem_returnItemCollectionMetrics,
    deleteItem_expressionAttributeNames,
    deleteItem_returnValues,
    deleteItem_conditionExpression,
    deleteItem_returnConsumedCapacity,
    deleteItem_conditionalOperator,
    deleteItem_tableName,
    deleteItem_key,

    -- * Destructuring the Response
    DeleteItemResponse (..),
    newDeleteItemResponse,

    -- * Response Lenses
    deleteItemResponse_itemCollectionMetrics,
    deleteItemResponse_attributes,
    deleteItemResponse_consumedCapacity,
    deleteItemResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteItem@ operation.
--
-- /See:/ 'newDeleteItem' smart constructor.
data DeleteItem = DeleteItem'
  { -- | This is a legacy parameter. Use @ConditionExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected>
    -- in the /Amazon DynamoDB Developer Guide/.
    expected :: Prelude.Maybe (Prelude.HashMap Prelude.Text ExpectedAttributeValue),
    -- | One or more values that can be substituted in an expression.
    --
    -- Use the __:__ (colon) character in an expression to dereference an
    -- attribute value. For example, suppose that you wanted to check whether
    -- the value of the /ProductStatus/ attribute was one of the following:
    --
    -- @Available | Backordered | Discontinued@
    --
    -- You would first need to specify @ExpressionAttributeValues@ as follows:
    --
    -- @{ \":avail\":{\"S\":\"Available\"}, \":back\":{\"S\":\"Backordered\"}, \":disc\":{\"S\":\"Discontinued\"} }@
    --
    -- You could then use these values in an expression, such as this:
    --
    -- @ProductStatus IN (:avail, :back, :disc)@
    --
    -- For more information on expression attribute values, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
    -- in the /Amazon DynamoDB Developer Guide/.
    expressionAttributeValues :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | Determines whether item collection metrics are returned. If set to
    -- @SIZE@, the response includes statistics about item collections, if any,
    -- that were modified during the operation are returned in the response. If
    -- set to @NONE@ (the default), no statistics are returned.
    returnItemCollectionMetrics :: Prelude.Maybe ReturnItemCollectionMetrics,
    -- | One or more substitution tokens for attribute names in an expression.
    -- The following are some use cases for using @ExpressionAttributeNames@:
    --
    -- -   To access an attribute whose name conflicts with a DynamoDB reserved
    --     word.
    --
    -- -   To create a placeholder for repeating occurrences of an attribute
    --     name in an expression.
    --
    -- -   To prevent special characters in an attribute name from being
    --     misinterpreted in an expression.
    --
    -- Use the __#__ character in an expression to dereference an attribute
    -- name. For example, consider the following attribute name:
    --
    -- -   @Percentile@
    --
    -- The name of this attribute conflicts with a reserved word, so it cannot
    -- be used directly in an expression. (For the complete list of reserved
    -- words, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
    -- in the /Amazon DynamoDB Developer Guide/). To work around this, you
    -- could specify the following for @ExpressionAttributeNames@:
    --
    -- -   @{\"#P\":\"Percentile\"}@
    --
    -- You could then use this substitution in an expression, as in this
    -- example:
    --
    -- -   @#P = :val@
    --
    -- Tokens that begin with the __:__ character are /expression attribute
    -- values/, which are placeholders for the actual value at runtime.
    --
    -- For more information on expression attribute names, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
    -- in the /Amazon DynamoDB Developer Guide/.
    expressionAttributeNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Use @ReturnValues@ if you want to get the item attributes as they
    -- appeared before they were deleted. For @DeleteItem@, the valid values
    -- are:
    --
    -- -   @NONE@ - If @ReturnValues@ is not specified, or if its value is
    --     @NONE@, then nothing is returned. (This setting is the default for
    --     @ReturnValues@.)
    --
    -- -   @ALL_OLD@ - The content of the old item is returned.
    --
    -- The @ReturnValues@ parameter is used by several DynamoDB operations;
    -- however, @DeleteItem@ does not recognize any values other than @NONE@ or
    -- @ALL_OLD@.
    returnValues :: Prelude.Maybe ReturnValue,
    -- | A condition that must be satisfied in order for a conditional
    -- @DeleteItem@ to succeed.
    --
    -- An expression can contain any of the following:
    --
    -- -   Functions:
    --     @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@
    --
    --     These function names are case-sensitive.
    --
    -- -   Comparison operators: @= | \<> | \< | > | \<= | >= | BETWEEN | IN @
    --
    -- -   Logical operators: @AND | OR | NOT@
    --
    -- For more information about condition expressions, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
    -- in the /Amazon DynamoDB Developer Guide/.
    conditionExpression :: Prelude.Maybe Prelude.Text,
    returnConsumedCapacity :: Prelude.Maybe ReturnConsumedCapacity,
    -- | This is a legacy parameter. Use @ConditionExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
    -- in the /Amazon DynamoDB Developer Guide/.
    conditionalOperator :: Prelude.Maybe ConditionalOperator,
    -- | The name of the table from which to delete the item.
    tableName :: Prelude.Text,
    -- | A map of attribute names to @AttributeValue@ objects, representing the
    -- primary key of the item to delete.
    --
    -- For the primary key, you must provide all of the attributes. For
    -- example, with a simple primary key, you only need to provide a value for
    -- the partition key. For a composite primary key, you must provide values
    -- for both the partition key and the sort key.
    key :: Prelude.HashMap Prelude.Text AttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expected', 'deleteItem_expected' - This is a legacy parameter. Use @ConditionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'expressionAttributeValues', 'deleteItem_expressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an
-- attribute value. For example, suppose that you wanted to check whether
-- the value of the /ProductStatus/ attribute was one of the following:
--
-- @Available | Backordered | Discontinued@
--
-- You would first need to specify @ExpressionAttributeValues@ as follows:
--
-- @{ \":avail\":{\"S\":\"Available\"}, \":back\":{\"S\":\"Backordered\"}, \":disc\":{\"S\":\"Discontinued\"} }@
--
-- You could then use these values in an expression, such as this:
--
-- @ProductStatus IN (:avail, :back, :disc)@
--
-- For more information on expression attribute values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'returnItemCollectionMetrics', 'deleteItem_returnItemCollectionMetrics' - Determines whether item collection metrics are returned. If set to
-- @SIZE@, the response includes statistics about item collections, if any,
-- that were modified during the operation are returned in the response. If
-- set to @NONE@ (the default), no statistics are returned.
--
-- 'expressionAttributeNames', 'deleteItem_expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
-- The following are some use cases for using @ExpressionAttributeNames@:
--
-- -   To access an attribute whose name conflicts with a DynamoDB reserved
--     word.
--
-- -   To create a placeholder for repeating occurrences of an attribute
--     name in an expression.
--
-- -   To prevent special characters in an attribute name from being
--     misinterpreted in an expression.
--
-- Use the __#__ character in an expression to dereference an attribute
-- name. For example, consider the following attribute name:
--
-- -   @Percentile@
--
-- The name of this attribute conflicts with a reserved word, so it cannot
-- be used directly in an expression. (For the complete list of reserved
-- words, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/). To work around this, you
-- could specify the following for @ExpressionAttributeNames@:
--
-- -   @{\"#P\":\"Percentile\"}@
--
-- You could then use this substitution in an expression, as in this
-- example:
--
-- -   @#P = :val@
--
-- Tokens that begin with the __:__ character are /expression attribute
-- values/, which are placeholders for the actual value at runtime.
--
-- For more information on expression attribute names, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'returnValues', 'deleteItem_returnValues' - Use @ReturnValues@ if you want to get the item attributes as they
-- appeared before they were deleted. For @DeleteItem@, the valid values
-- are:
--
-- -   @NONE@ - If @ReturnValues@ is not specified, or if its value is
--     @NONE@, then nothing is returned. (This setting is the default for
--     @ReturnValues@.)
--
-- -   @ALL_OLD@ - The content of the old item is returned.
--
-- The @ReturnValues@ parameter is used by several DynamoDB operations;
-- however, @DeleteItem@ does not recognize any values other than @NONE@ or
-- @ALL_OLD@.
--
-- 'conditionExpression', 'deleteItem_conditionExpression' - A condition that must be satisfied in order for a conditional
-- @DeleteItem@ to succeed.
--
-- An expression can contain any of the following:
--
-- -   Functions:
--     @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@
--
--     These function names are case-sensitive.
--
-- -   Comparison operators: @= | \<> | \< | > | \<= | >= | BETWEEN | IN @
--
-- -   Logical operators: @AND | OR | NOT@
--
-- For more information about condition expressions, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'returnConsumedCapacity', 'deleteItem_returnConsumedCapacity' - Undocumented member.
--
-- 'conditionalOperator', 'deleteItem_conditionalOperator' - This is a legacy parameter. Use @ConditionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'tableName', 'deleteItem_tableName' - The name of the table from which to delete the item.
--
-- 'key', 'deleteItem_key' - A map of attribute names to @AttributeValue@ objects, representing the
-- primary key of the item to delete.
--
-- For the primary key, you must provide all of the attributes. For
-- example, with a simple primary key, you only need to provide a value for
-- the partition key. For a composite primary key, you must provide values
-- for both the partition key and the sort key.
newDeleteItem ::
  -- | 'tableName'
  Prelude.Text ->
  DeleteItem
newDeleteItem pTableName_ =
  DeleteItem'
    { expected = Prelude.Nothing,
      expressionAttributeValues = Prelude.Nothing,
      returnItemCollectionMetrics = Prelude.Nothing,
      expressionAttributeNames = Prelude.Nothing,
      returnValues = Prelude.Nothing,
      conditionExpression = Prelude.Nothing,
      returnConsumedCapacity = Prelude.Nothing,
      conditionalOperator = Prelude.Nothing,
      tableName = pTableName_,
      key = Prelude.mempty
    }

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected>
-- in the /Amazon DynamoDB Developer Guide/.
deleteItem_expected :: Lens.Lens' DeleteItem (Prelude.Maybe (Prelude.HashMap Prelude.Text ExpectedAttributeValue))
deleteItem_expected = Lens.lens (\DeleteItem' {expected} -> expected) (\s@DeleteItem' {} a -> s {expected = a} :: DeleteItem) Prelude.. Lens.mapping Lens._Coerce

-- | One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an
-- attribute value. For example, suppose that you wanted to check whether
-- the value of the /ProductStatus/ attribute was one of the following:
--
-- @Available | Backordered | Discontinued@
--
-- You would first need to specify @ExpressionAttributeValues@ as follows:
--
-- @{ \":avail\":{\"S\":\"Available\"}, \":back\":{\"S\":\"Backordered\"}, \":disc\":{\"S\":\"Discontinued\"} }@
--
-- You could then use these values in an expression, such as this:
--
-- @ProductStatus IN (:avail, :back, :disc)@
--
-- For more information on expression attribute values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
deleteItem_expressionAttributeValues :: Lens.Lens' DeleteItem (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
deleteItem_expressionAttributeValues = Lens.lens (\DeleteItem' {expressionAttributeValues} -> expressionAttributeValues) (\s@DeleteItem' {} a -> s {expressionAttributeValues = a} :: DeleteItem) Prelude.. Lens.mapping Lens._Coerce

-- | Determines whether item collection metrics are returned. If set to
-- @SIZE@, the response includes statistics about item collections, if any,
-- that were modified during the operation are returned in the response. If
-- set to @NONE@ (the default), no statistics are returned.
deleteItem_returnItemCollectionMetrics :: Lens.Lens' DeleteItem (Prelude.Maybe ReturnItemCollectionMetrics)
deleteItem_returnItemCollectionMetrics = Lens.lens (\DeleteItem' {returnItemCollectionMetrics} -> returnItemCollectionMetrics) (\s@DeleteItem' {} a -> s {returnItemCollectionMetrics = a} :: DeleteItem)

-- | One or more substitution tokens for attribute names in an expression.
-- The following are some use cases for using @ExpressionAttributeNames@:
--
-- -   To access an attribute whose name conflicts with a DynamoDB reserved
--     word.
--
-- -   To create a placeholder for repeating occurrences of an attribute
--     name in an expression.
--
-- -   To prevent special characters in an attribute name from being
--     misinterpreted in an expression.
--
-- Use the __#__ character in an expression to dereference an attribute
-- name. For example, consider the following attribute name:
--
-- -   @Percentile@
--
-- The name of this attribute conflicts with a reserved word, so it cannot
-- be used directly in an expression. (For the complete list of reserved
-- words, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/). To work around this, you
-- could specify the following for @ExpressionAttributeNames@:
--
-- -   @{\"#P\":\"Percentile\"}@
--
-- You could then use this substitution in an expression, as in this
-- example:
--
-- -   @#P = :val@
--
-- Tokens that begin with the __:__ character are /expression attribute
-- values/, which are placeholders for the actual value at runtime.
--
-- For more information on expression attribute names, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
deleteItem_expressionAttributeNames :: Lens.Lens' DeleteItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
deleteItem_expressionAttributeNames = Lens.lens (\DeleteItem' {expressionAttributeNames} -> expressionAttributeNames) (\s@DeleteItem' {} a -> s {expressionAttributeNames = a} :: DeleteItem) Prelude.. Lens.mapping Lens._Coerce

-- | Use @ReturnValues@ if you want to get the item attributes as they
-- appeared before they were deleted. For @DeleteItem@, the valid values
-- are:
--
-- -   @NONE@ - If @ReturnValues@ is not specified, or if its value is
--     @NONE@, then nothing is returned. (This setting is the default for
--     @ReturnValues@.)
--
-- -   @ALL_OLD@ - The content of the old item is returned.
--
-- The @ReturnValues@ parameter is used by several DynamoDB operations;
-- however, @DeleteItem@ does not recognize any values other than @NONE@ or
-- @ALL_OLD@.
deleteItem_returnValues :: Lens.Lens' DeleteItem (Prelude.Maybe ReturnValue)
deleteItem_returnValues = Lens.lens (\DeleteItem' {returnValues} -> returnValues) (\s@DeleteItem' {} a -> s {returnValues = a} :: DeleteItem)

-- | A condition that must be satisfied in order for a conditional
-- @DeleteItem@ to succeed.
--
-- An expression can contain any of the following:
--
-- -   Functions:
--     @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@
--
--     These function names are case-sensitive.
--
-- -   Comparison operators: @= | \<> | \< | > | \<= | >= | BETWEEN | IN @
--
-- -   Logical operators: @AND | OR | NOT@
--
-- For more information about condition expressions, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
deleteItem_conditionExpression :: Lens.Lens' DeleteItem (Prelude.Maybe Prelude.Text)
deleteItem_conditionExpression = Lens.lens (\DeleteItem' {conditionExpression} -> conditionExpression) (\s@DeleteItem' {} a -> s {conditionExpression = a} :: DeleteItem)

-- | Undocumented member.
deleteItem_returnConsumedCapacity :: Lens.Lens' DeleteItem (Prelude.Maybe ReturnConsumedCapacity)
deleteItem_returnConsumedCapacity = Lens.lens (\DeleteItem' {returnConsumedCapacity} -> returnConsumedCapacity) (\s@DeleteItem' {} a -> s {returnConsumedCapacity = a} :: DeleteItem)

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
-- in the /Amazon DynamoDB Developer Guide/.
deleteItem_conditionalOperator :: Lens.Lens' DeleteItem (Prelude.Maybe ConditionalOperator)
deleteItem_conditionalOperator = Lens.lens (\DeleteItem' {conditionalOperator} -> conditionalOperator) (\s@DeleteItem' {} a -> s {conditionalOperator = a} :: DeleteItem)

-- | The name of the table from which to delete the item.
deleteItem_tableName :: Lens.Lens' DeleteItem Prelude.Text
deleteItem_tableName = Lens.lens (\DeleteItem' {tableName} -> tableName) (\s@DeleteItem' {} a -> s {tableName = a} :: DeleteItem)

-- | A map of attribute names to @AttributeValue@ objects, representing the
-- primary key of the item to delete.
--
-- For the primary key, you must provide all of the attributes. For
-- example, with a simple primary key, you only need to provide a value for
-- the partition key. For a composite primary key, you must provide values
-- for both the partition key and the sort key.
deleteItem_key :: Lens.Lens' DeleteItem (Prelude.HashMap Prelude.Text AttributeValue)
deleteItem_key = Lens.lens (\DeleteItem' {key} -> key) (\s@DeleteItem' {} a -> s {key = a} :: DeleteItem) Prelude.. Lens._Coerce

instance Core.AWSRequest DeleteItem where
  type AWSResponse DeleteItem = DeleteItemResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteItemResponse'
            Prelude.<$> (x Core..?> "ItemCollectionMetrics")
            Prelude.<*> (x Core..?> "Attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "ConsumedCapacity")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteItem

instance Prelude.NFData DeleteItem

instance Core.ToHeaders DeleteItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.DeleteItem" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteItem where
  toJSON DeleteItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Expected" Core..=) Prelude.<$> expected,
            ("ExpressionAttributeValues" Core..=)
              Prelude.<$> expressionAttributeValues,
            ("ReturnItemCollectionMetrics" Core..=)
              Prelude.<$> returnItemCollectionMetrics,
            ("ExpressionAttributeNames" Core..=)
              Prelude.<$> expressionAttributeNames,
            ("ReturnValues" Core..=) Prelude.<$> returnValues,
            ("ConditionExpression" Core..=)
              Prelude.<$> conditionExpression,
            ("ReturnConsumedCapacity" Core..=)
              Prelude.<$> returnConsumedCapacity,
            ("ConditionalOperator" Core..=)
              Prelude.<$> conditionalOperator,
            Prelude.Just ("TableName" Core..= tableName),
            Prelude.Just ("Key" Core..= key)
          ]
      )

instance Core.ToPath DeleteItem where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteItem where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @DeleteItem@ operation.
--
-- /See:/ 'newDeleteItemResponse' smart constructor.
data DeleteItemResponse = DeleteItemResponse'
  { -- | Information about item collections, if any, that were affected by the
    -- @DeleteItem@ operation. @ItemCollectionMetrics@ is only returned if the
    -- @ReturnItemCollectionMetrics@ parameter was specified. If the table does
    -- not have any local secondary indexes, this information is not returned
    -- in the response.
    --
    -- Each @ItemCollectionMetrics@ element consists of:
    --
    -- -   @ItemCollectionKey@ - The partition key value of the item
    --     collection. This is the same as the partition key value of the item
    --     itself.
    --
    -- -   @SizeEstimateRangeGB@ - An estimate of item collection size, in
    --     gigabytes. This value is a two-element array containing a lower
    --     bound and an upper bound for the estimate. The estimate includes the
    --     size of all the items in the table, plus the size of all attributes
    --     projected into all of the local secondary indexes on that table. Use
    --     this estimate to measure whether a local secondary index is
    --     approaching its size limit.
    --
    --     The estimate is subject to change over time; therefore, do not rely
    --     on the precision or accuracy of the estimate.
    itemCollectionMetrics :: Prelude.Maybe ItemCollectionMetrics,
    -- | A map of attribute names to @AttributeValue@ objects, representing the
    -- item as it appeared before the @DeleteItem@ operation. This map appears
    -- in the response only if @ReturnValues@ was specified as @ALL_OLD@ in the
    -- request.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | The capacity units consumed by the @DeleteItem@ operation. The data
    -- returned includes the total provisioned throughput consumed, along with
    -- statistics for the table and any indexes involved in the operation.
    -- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
    -- parameter was specified. For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Mode>
    -- in the /Amazon DynamoDB Developer Guide/.
    consumedCapacity :: Prelude.Maybe ConsumedCapacity,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'itemCollectionMetrics', 'deleteItemResponse_itemCollectionMetrics' - Information about item collections, if any, that were affected by the
-- @DeleteItem@ operation. @ItemCollectionMetrics@ is only returned if the
-- @ReturnItemCollectionMetrics@ parameter was specified. If the table does
-- not have any local secondary indexes, this information is not returned
-- in the response.
--
-- Each @ItemCollectionMetrics@ element consists of:
--
-- -   @ItemCollectionKey@ - The partition key value of the item
--     collection. This is the same as the partition key value of the item
--     itself.
--
-- -   @SizeEstimateRangeGB@ - An estimate of item collection size, in
--     gigabytes. This value is a two-element array containing a lower
--     bound and an upper bound for the estimate. The estimate includes the
--     size of all the items in the table, plus the size of all attributes
--     projected into all of the local secondary indexes on that table. Use
--     this estimate to measure whether a local secondary index is
--     approaching its size limit.
--
--     The estimate is subject to change over time; therefore, do not rely
--     on the precision or accuracy of the estimate.
--
-- 'attributes', 'deleteItemResponse_attributes' - A map of attribute names to @AttributeValue@ objects, representing the
-- item as it appeared before the @DeleteItem@ operation. This map appears
-- in the response only if @ReturnValues@ was specified as @ALL_OLD@ in the
-- request.
--
-- 'consumedCapacity', 'deleteItemResponse_consumedCapacity' - The capacity units consumed by the @DeleteItem@ operation. The data
-- returned includes the total provisioned throughput consumed, along with
-- statistics for the table and any indexes involved in the operation.
-- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
-- parameter was specified. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Mode>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'httpStatus', 'deleteItemResponse_httpStatus' - The response's http status code.
newDeleteItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteItemResponse
newDeleteItemResponse pHttpStatus_ =
  DeleteItemResponse'
    { itemCollectionMetrics =
        Prelude.Nothing,
      attributes = Prelude.Nothing,
      consumedCapacity = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about item collections, if any, that were affected by the
-- @DeleteItem@ operation. @ItemCollectionMetrics@ is only returned if the
-- @ReturnItemCollectionMetrics@ parameter was specified. If the table does
-- not have any local secondary indexes, this information is not returned
-- in the response.
--
-- Each @ItemCollectionMetrics@ element consists of:
--
-- -   @ItemCollectionKey@ - The partition key value of the item
--     collection. This is the same as the partition key value of the item
--     itself.
--
-- -   @SizeEstimateRangeGB@ - An estimate of item collection size, in
--     gigabytes. This value is a two-element array containing a lower
--     bound and an upper bound for the estimate. The estimate includes the
--     size of all the items in the table, plus the size of all attributes
--     projected into all of the local secondary indexes on that table. Use
--     this estimate to measure whether a local secondary index is
--     approaching its size limit.
--
--     The estimate is subject to change over time; therefore, do not rely
--     on the precision or accuracy of the estimate.
deleteItemResponse_itemCollectionMetrics :: Lens.Lens' DeleteItemResponse (Prelude.Maybe ItemCollectionMetrics)
deleteItemResponse_itemCollectionMetrics = Lens.lens (\DeleteItemResponse' {itemCollectionMetrics} -> itemCollectionMetrics) (\s@DeleteItemResponse' {} a -> s {itemCollectionMetrics = a} :: DeleteItemResponse)

-- | A map of attribute names to @AttributeValue@ objects, representing the
-- item as it appeared before the @DeleteItem@ operation. This map appears
-- in the response only if @ReturnValues@ was specified as @ALL_OLD@ in the
-- request.
deleteItemResponse_attributes :: Lens.Lens' DeleteItemResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
deleteItemResponse_attributes = Lens.lens (\DeleteItemResponse' {attributes} -> attributes) (\s@DeleteItemResponse' {} a -> s {attributes = a} :: DeleteItemResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The capacity units consumed by the @DeleteItem@ operation. The data
-- returned includes the total provisioned throughput consumed, along with
-- statistics for the table and any indexes involved in the operation.
-- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
-- parameter was specified. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Mode>
-- in the /Amazon DynamoDB Developer Guide/.
deleteItemResponse_consumedCapacity :: Lens.Lens' DeleteItemResponse (Prelude.Maybe ConsumedCapacity)
deleteItemResponse_consumedCapacity = Lens.lens (\DeleteItemResponse' {consumedCapacity} -> consumedCapacity) (\s@DeleteItemResponse' {} a -> s {consumedCapacity = a} :: DeleteItemResponse)

-- | The response's http status code.
deleteItemResponse_httpStatus :: Lens.Lens' DeleteItemResponse Prelude.Int
deleteItemResponse_httpStatus = Lens.lens (\DeleteItemResponse' {httpStatus} -> httpStatus) (\s@DeleteItemResponse' {} a -> s {httpStatus = a} :: DeleteItemResponse)

instance Prelude.NFData DeleteItemResponse
