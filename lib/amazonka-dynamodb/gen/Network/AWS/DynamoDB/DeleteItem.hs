{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DeleteItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a single item in a table by primary key. You can perform a conditional delete operation that deletes the item if it exists, or if it has an expected attribute value.
--
-- In addition to deleting an item, you can also return the item's attribute values in the same operation, using the @ReturnValues@ parameter.
-- Unless you specify conditions, the @DeleteItem@ is an idempotent operation; running it multiple times on the same item or attribute does /not/ result in an error response.
-- Conditional deletes are useful for deleting items only if specific conditions are met. If those conditions are met, DynamoDB performs the delete. Otherwise, the item is not deleted.
module Network.AWS.DynamoDB.DeleteItem
  ( -- * Creating a request
    DeleteItem (..),
    mkDeleteItem,

    -- ** Request lenses
    diExpressionAttributeNames,
    diReturnValues,
    diExpressionAttributeValues,
    diReturnConsumedCapacity,
    diReturnItemCollectionMetrics,
    diConditionExpression,
    diKey,
    diConditionalOperator,
    diExpected,
    diTableName,

    -- * Destructuring the response
    DeleteItemResponse (..),
    mkDeleteItemResponse,

    -- ** Response lenses
    dirsItemCollectionMetrics,
    dirsConsumedCapacity,
    dirsAttributes,
    dirsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeleteItem@ operation.
--
-- /See:/ 'mkDeleteItem' smart constructor.
data DeleteItem = DeleteItem'
  { -- | One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :
    --
    --
    --     * To access an attribute whose name conflicts with a DynamoDB reserved word.
    --
    --
    --     * To create a placeholder for repeating occurrences of an attribute name in an expression.
    --
    --
    --     * To prevent special characters in an attribute name from being misinterpreted in an expression.
    --
    --
    -- Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:
    --
    --     * @Percentile@
    --
    --
    -- The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :
    --
    --     * @{"#P":"Percentile"}@
    --
    --
    -- You could then use this substitution in an expression, as in this example:
    --
    --     * @#P = :val@
    --
    --
    -- For more information on expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
    expressionAttributeNames :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Use @ReturnValues@ if you want to get the item attributes as they appeared before they were deleted. For @DeleteItem@ , the valid values are:
    --
    --
    --     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)
    --
    --
    --     * @ALL_OLD@ - The content of the old item is returned.
    returnValues :: Lude.Maybe ReturnValue,
    -- | One or more values that can be substituted in an expression.
    --
    -- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following:
    -- @Available | Backordered | Discontinued@
    -- You would first need to specify @ExpressionAttributeValues@ as follows:
    -- @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@
    -- You could then use these values in an expression, such as this:
    -- @ProductStatus IN (:avail, :back, :disc)@
    -- For more information on expression attribute values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
    expressionAttributeValues :: Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    returnConsumedCapacity :: Lude.Maybe ReturnConsumedCapacity,
    -- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
    returnItemCollectionMetrics :: Lude.Maybe ReturnItemCollectionMetrics,
    -- | A condition that must be satisfied in order for a conditional @DeleteItem@ to succeed.
    --
    -- An expression can contain any of the following:
    --
    --     * Functions: @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@
    -- These function names are case-sensitive.
    --
    --
    --     * Comparison operators: @= | <> | < | > | <= | >= | BETWEEN | IN @
    --
    --
    --     * Logical operators: @AND | OR | NOT@
    --
    --
    -- For more information about condition expressions, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
    conditionExpression :: Lude.Maybe Lude.Text,
    -- | A map of attribute names to @AttributeValue@ objects, representing the primary key of the item to delete.
    --
    -- For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
    key :: Lude.HashMap Lude.Text (AttributeValue),
    -- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
    conditionalOperator :: Lude.Maybe ConditionalOperator,
    -- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
    expected :: Lude.Maybe (Lude.HashMap Lude.Text (ExpectedAttributeValue)),
    -- | The name of the table from which to delete the item.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteItem' with the minimum fields required to make a request.
--
-- * 'expressionAttributeNames' - One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :
--
--
--     * To access an attribute whose name conflicts with a DynamoDB reserved word.
--
--
--     * To create a placeholder for repeating occurrences of an attribute name in an expression.
--
--
--     * To prevent special characters in an attribute name from being misinterpreted in an expression.
--
--
-- Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:
--
--     * @Percentile@
--
--
-- The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :
--
--     * @{"#P":"Percentile"}@
--
--
-- You could then use this substitution in an expression, as in this example:
--
--     * @#P = :val@
--
--
-- For more information on expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
-- * 'returnValues' - Use @ReturnValues@ if you want to get the item attributes as they appeared before they were deleted. For @DeleteItem@ , the valid values are:
--
--
--     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)
--
--
--     * @ALL_OLD@ - The content of the old item is returned.
--
--
-- * 'expressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following:
-- @Available | Backordered | Discontinued@
-- You would first need to specify @ExpressionAttributeValues@ as follows:
-- @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@
-- You could then use these values in an expression, such as this:
-- @ProductStatus IN (:avail, :back, :disc)@
-- For more information on expression attribute values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
-- * 'returnConsumedCapacity' -
-- * 'returnItemCollectionMetrics' - Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
-- * 'conditionExpression' - A condition that must be satisfied in order for a conditional @DeleteItem@ to succeed.
--
-- An expression can contain any of the following:
--
--     * Functions: @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@
-- These function names are case-sensitive.
--
--
--     * Comparison operators: @= | <> | < | > | <= | >= | BETWEEN | IN @
--
--
--     * Logical operators: @AND | OR | NOT@
--
--
-- For more information about condition expressions, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
-- * 'key' - A map of attribute names to @AttributeValue@ objects, representing the primary key of the item to delete.
--
-- For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
-- * 'conditionalOperator' - This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
-- * 'expected' - This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
-- * 'tableName' - The name of the table from which to delete the item.
mkDeleteItem ::
  -- | 'tableName'
  Lude.Text ->
  DeleteItem
mkDeleteItem pTableName_ =
  DeleteItem'
    { expressionAttributeNames = Lude.Nothing,
      returnValues = Lude.Nothing,
      expressionAttributeValues = Lude.Nothing,
      returnConsumedCapacity = Lude.Nothing,
      returnItemCollectionMetrics = Lude.Nothing,
      conditionExpression = Lude.Nothing,
      key = Lude.mempty,
      conditionalOperator = Lude.Nothing,
      expected = Lude.Nothing,
      tableName = pTableName_
    }

-- | One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :
--
--
--     * To access an attribute whose name conflicts with a DynamoDB reserved word.
--
--
--     * To create a placeholder for repeating occurrences of an attribute name in an expression.
--
--
--     * To prevent special characters in an attribute name from being misinterpreted in an expression.
--
--
-- Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:
--
--     * @Percentile@
--
--
-- The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :
--
--     * @{"#P":"Percentile"}@
--
--
-- You could then use this substitution in an expression, as in this example:
--
--     * @#P = :val@
--
--
-- For more information on expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diExpressionAttributeNames :: Lens.Lens' DeleteItem (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
diExpressionAttributeNames = Lens.lens (expressionAttributeNames :: DeleteItem -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {expressionAttributeNames = a} :: DeleteItem)
{-# DEPRECATED diExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | Use @ReturnValues@ if you want to get the item attributes as they appeared before they were deleted. For @DeleteItem@ , the valid values are:
--
--
--     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)
--
--
--     * @ALL_OLD@ - The content of the old item is returned.
--
--
--
-- /Note:/ Consider using 'returnValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diReturnValues :: Lens.Lens' DeleteItem (Lude.Maybe ReturnValue)
diReturnValues = Lens.lens (returnValues :: DeleteItem -> Lude.Maybe ReturnValue) (\s a -> s {returnValues = a} :: DeleteItem)
{-# DEPRECATED diReturnValues "Use generic-lens or generic-optics with 'returnValues' instead." #-}

-- | One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following:
-- @Available | Backordered | Discontinued@
-- You would first need to specify @ExpressionAttributeValues@ as follows:
-- @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@
-- You could then use these values in an expression, such as this:
-- @ProductStatus IN (:avail, :back, :disc)@
-- For more information on expression attribute values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expressionAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diExpressionAttributeValues :: Lens.Lens' DeleteItem (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
diExpressionAttributeValues = Lens.lens (expressionAttributeValues :: DeleteItem -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {expressionAttributeValues = a} :: DeleteItem)
{-# DEPRECATED diExpressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diReturnConsumedCapacity :: Lens.Lens' DeleteItem (Lude.Maybe ReturnConsumedCapacity)
diReturnConsumedCapacity = Lens.lens (returnConsumedCapacity :: DeleteItem -> Lude.Maybe ReturnConsumedCapacity) (\s a -> s {returnConsumedCapacity = a} :: DeleteItem)
{-# DEPRECATED diReturnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead." #-}

-- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
--
-- /Note:/ Consider using 'returnItemCollectionMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diReturnItemCollectionMetrics :: Lens.Lens' DeleteItem (Lude.Maybe ReturnItemCollectionMetrics)
diReturnItemCollectionMetrics = Lens.lens (returnItemCollectionMetrics :: DeleteItem -> Lude.Maybe ReturnItemCollectionMetrics) (\s a -> s {returnItemCollectionMetrics = a} :: DeleteItem)
{-# DEPRECATED diReturnItemCollectionMetrics "Use generic-lens or generic-optics with 'returnItemCollectionMetrics' instead." #-}

-- | A condition that must be satisfied in order for a conditional @DeleteItem@ to succeed.
--
-- An expression can contain any of the following:
--
--     * Functions: @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@
-- These function names are case-sensitive.
--
--
--     * Comparison operators: @= | <> | < | > | <= | >= | BETWEEN | IN @
--
--
--     * Logical operators: @AND | OR | NOT@
--
--
-- For more information about condition expressions, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'conditionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diConditionExpression :: Lens.Lens' DeleteItem (Lude.Maybe Lude.Text)
diConditionExpression = Lens.lens (conditionExpression :: DeleteItem -> Lude.Maybe Lude.Text) (\s a -> s {conditionExpression = a} :: DeleteItem)
{-# DEPRECATED diConditionExpression "Use generic-lens or generic-optics with 'conditionExpression' instead." #-}

-- | A map of attribute names to @AttributeValue@ objects, representing the primary key of the item to delete.
--
-- For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diKey :: Lens.Lens' DeleteItem (Lude.HashMap Lude.Text (AttributeValue))
diKey = Lens.lens (key :: DeleteItem -> Lude.HashMap Lude.Text (AttributeValue)) (\s a -> s {key = a} :: DeleteItem)
{-# DEPRECATED diKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'conditionalOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diConditionalOperator :: Lens.Lens' DeleteItem (Lude.Maybe ConditionalOperator)
diConditionalOperator = Lens.lens (conditionalOperator :: DeleteItem -> Lude.Maybe ConditionalOperator) (\s a -> s {conditionalOperator = a} :: DeleteItem)
{-# DEPRECATED diConditionalOperator "Use generic-lens or generic-optics with 'conditionalOperator' instead." #-}

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diExpected :: Lens.Lens' DeleteItem (Lude.Maybe (Lude.HashMap Lude.Text (ExpectedAttributeValue)))
diExpected = Lens.lens (expected :: DeleteItem -> Lude.Maybe (Lude.HashMap Lude.Text (ExpectedAttributeValue))) (\s a -> s {expected = a} :: DeleteItem)
{-# DEPRECATED diExpected "Use generic-lens or generic-optics with 'expected' instead." #-}

-- | The name of the table from which to delete the item.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTableName :: Lens.Lens' DeleteItem Lude.Text
diTableName = Lens.lens (tableName :: DeleteItem -> Lude.Text) (\s a -> s {tableName = a} :: DeleteItem)
{-# DEPRECATED diTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest DeleteItem where
  type Rs DeleteItem = DeleteItemResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteItemResponse'
            Lude.<$> (x Lude..?> "ItemCollectionMetrics")
            Lude.<*> (x Lude..?> "ConsumedCapacity")
            Lude.<*> (x Lude..?> "Attributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteItem where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.DeleteItem" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteItem where
  toJSON DeleteItem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExpressionAttributeNames" Lude..=)
              Lude.<$> expressionAttributeNames,
            ("ReturnValues" Lude..=) Lude.<$> returnValues,
            ("ExpressionAttributeValues" Lude..=)
              Lude.<$> expressionAttributeValues,
            ("ReturnConsumedCapacity" Lude..=) Lude.<$> returnConsumedCapacity,
            ("ReturnItemCollectionMetrics" Lude..=)
              Lude.<$> returnItemCollectionMetrics,
            ("ConditionExpression" Lude..=) Lude.<$> conditionExpression,
            Lude.Just ("Key" Lude..= key),
            ("ConditionalOperator" Lude..=) Lude.<$> conditionalOperator,
            ("Expected" Lude..=) Lude.<$> expected,
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath DeleteItem where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteItem where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @DeleteItem@ operation.
--
-- /See:/ 'mkDeleteItemResponse' smart constructor.
data DeleteItemResponse = DeleteItemResponse'
  { -- | Information about item collections, if any, that were affected by the @DeleteItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response.
    --
    -- Each @ItemCollectionMetrics@ element consists of:
    --
    --     * @ItemCollectionKey@ - The partition key value of the item collection. This is the same as the partition key value of the item itself.
    --
    --
    --     * @SizeEstimateRangeGB@ - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit.
    -- The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
    itemCollectionMetrics :: Lude.Maybe ItemCollectionMetrics,
    -- | The capacity units consumed by the @DeleteItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Mode> in the /Amazon DynamoDB Developer Guide/ .
    consumedCapacity :: Lude.Maybe ConsumedCapacity,
    -- | A map of attribute names to @AttributeValue@ objects, representing the item as it appeared before the @DeleteItem@ operation. This map appears in the response only if @ReturnValues@ was specified as @ALL_OLD@ in the request.
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteItemResponse' with the minimum fields required to make a request.
--
-- * 'itemCollectionMetrics' - Information about item collections, if any, that were affected by the @DeleteItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response.
--
-- Each @ItemCollectionMetrics@ element consists of:
--
--     * @ItemCollectionKey@ - The partition key value of the item collection. This is the same as the partition key value of the item itself.
--
--
--     * @SizeEstimateRangeGB@ - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit.
-- The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
--
--
-- * 'consumedCapacity' - The capacity units consumed by the @DeleteItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Mode> in the /Amazon DynamoDB Developer Guide/ .
-- * 'attributes' - A map of attribute names to @AttributeValue@ objects, representing the item as it appeared before the @DeleteItem@ operation. This map appears in the response only if @ReturnValues@ was specified as @ALL_OLD@ in the request.
-- * 'responseStatus' - The response status code.
mkDeleteItemResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteItemResponse
mkDeleteItemResponse pResponseStatus_ =
  DeleteItemResponse'
    { itemCollectionMetrics = Lude.Nothing,
      consumedCapacity = Lude.Nothing,
      attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about item collections, if any, that were affected by the @DeleteItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response.
--
-- Each @ItemCollectionMetrics@ element consists of:
--
--     * @ItemCollectionKey@ - The partition key value of the item collection. This is the same as the partition key value of the item itself.
--
--
--     * @SizeEstimateRangeGB@ - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit.
-- The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
--
--
--
-- /Note:/ Consider using 'itemCollectionMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsItemCollectionMetrics :: Lens.Lens' DeleteItemResponse (Lude.Maybe ItemCollectionMetrics)
dirsItemCollectionMetrics = Lens.lens (itemCollectionMetrics :: DeleteItemResponse -> Lude.Maybe ItemCollectionMetrics) (\s a -> s {itemCollectionMetrics = a} :: DeleteItemResponse)
{-# DEPRECATED dirsItemCollectionMetrics "Use generic-lens or generic-optics with 'itemCollectionMetrics' instead." #-}

-- | The capacity units consumed by the @DeleteItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Mode> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsConsumedCapacity :: Lens.Lens' DeleteItemResponse (Lude.Maybe ConsumedCapacity)
dirsConsumedCapacity = Lens.lens (consumedCapacity :: DeleteItemResponse -> Lude.Maybe ConsumedCapacity) (\s a -> s {consumedCapacity = a} :: DeleteItemResponse)
{-# DEPRECATED dirsConsumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead." #-}

-- | A map of attribute names to @AttributeValue@ objects, representing the item as it appeared before the @DeleteItem@ operation. This map appears in the response only if @ReturnValues@ was specified as @ALL_OLD@ in the request.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsAttributes :: Lens.Lens' DeleteItemResponse (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
dirsAttributes = Lens.lens (attributes :: DeleteItemResponse -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {attributes = a} :: DeleteItemResponse)
{-# DEPRECATED dirsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DeleteItemResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DeleteItemResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteItemResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
