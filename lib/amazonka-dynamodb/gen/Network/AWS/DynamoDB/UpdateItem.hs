{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Edits an existing item's attributes, or adds a new item to the table if it does not already exist. You can put, delete, or add attribute values. You can also perform a conditional update on an existing item (insert a new attribute name-value pair if it doesn't exist, or replace an existing name-value pair if it has certain expected attribute values).
--
-- You can also return the item's attribute values in the same @UpdateItem@ operation using the @ReturnValues@ parameter.
module Network.AWS.DynamoDB.UpdateItem
  ( -- * Creating a request
    UpdateItem (..),
    mkUpdateItem,

    -- ** Request lenses
    uiTableName,
    uiKey,
    uiAttributeUpdates,
    uiConditionExpression,
    uiConditionalOperator,
    uiExpected,
    uiExpressionAttributeNames,
    uiExpressionAttributeValues,
    uiReturnConsumedCapacity,
    uiReturnItemCollectionMetrics,
    uiReturnValues,
    uiUpdateExpression,

    -- * Destructuring the response
    UpdateItemResponse (..),
    mkUpdateItemResponse,

    -- ** Response lenses
    uirrsAttributes,
    uirrsConsumedCapacity,
    uirrsItemCollectionMetrics,
    uirrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an @UpdateItem@ operation.
--
-- /See:/ 'mkUpdateItem' smart constructor.
data UpdateItem = UpdateItem'
  { -- | The name of the table containing the item to update.
    tableName :: Types.TableName,
    -- | The primary key of the item to be updated. Each element consists of an attribute name and a value for that attribute.
    --
    -- For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
    key :: Core.HashMap Types.AttributeName Types.AttributeValue,
    -- | This is a legacy parameter. Use @UpdateExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributeUpdates.html AttributeUpdates> in the /Amazon DynamoDB Developer Guide/ .
    attributeUpdates :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValueUpdate),
    -- | A condition that must be satisfied in order for a conditional update to succeed.
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
    -- For more information about condition expressions, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions> in the /Amazon DynamoDB Developer Guide/ .
    conditionExpression :: Core.Maybe Types.ConditionExpression,
    -- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
    conditionalOperator :: Core.Maybe Types.ConditionalOperator,
    -- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
    expected :: Core.Maybe (Core.HashMap Types.AttributeName Types.ExpectedAttributeValue),
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
    -- The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ .) To work around this, you could specify the following for @ExpressionAttributeNames@ :
    --
    --     * @{"#P":"Percentile"}@
    --
    --
    -- You could then use this substitution in an expression, as in this example:
    --
    --     * @#P = :val@
    --
    --
    -- For more information about expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
    expressionAttributeNames :: Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName),
    -- | One or more values that can be substituted in an expression.
    --
    -- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the @ProductStatus@ attribute was one of the following:
    -- @Available | Backordered | Discontinued@
    -- You would first need to specify @ExpressionAttributeValues@ as follows:
    -- @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@
    -- You could then use these values in an expression, such as this:
    -- @ProductStatus IN (:avail, :back, :disc)@
    -- For more information on expression attribute values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
    expressionAttributeValues :: Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue),
    returnConsumedCapacity :: Core.Maybe Types.ReturnConsumedCapacity,
    -- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
    returnItemCollectionMetrics :: Core.Maybe Types.ReturnItemCollectionMetrics,
    -- | Use @ReturnValues@ if you want to get the item attributes as they appear before or after they are updated. For @UpdateItem@ , the valid values are:
    --
    --
    --     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)
    --
    --
    --     * @ALL_OLD@ - Returns all of the attributes of the item, as they appeared before the UpdateItem operation.
    --
    --
    --     * @UPDATED_OLD@ - Returns only the updated attributes, as they appeared before the UpdateItem operation.
    --
    --
    --     * @ALL_NEW@ - Returns all of the attributes of the item, as they appear after the UpdateItem operation.
    --
    --
    --     * @UPDATED_NEW@ - Returns only the updated attributes, as they appear after the UpdateItem operation.
    --
    --
    -- There is no additional cost associated with requesting a return value aside from the small network and processing overhead of receiving a larger response. No read capacity units are consumed.
    -- The values returned are strongly consistent.
    returnValues :: Core.Maybe Types.ReturnValue,
    -- | An expression that defines one or more attributes to be updated, the action to be performed on them, and new values for them.
    --
    -- The following action values are available for @UpdateExpression@ .
    --
    --     * @SET@ - Adds one or more attributes and values to an item. If any of these attributes already exist, they are replaced by the new values. You can also use @SET@ to add or subtract from an attribute that is of type Number. For example: @SET myNum = myNum + :val@
    -- @SET@ supports the following functions:
    --
    --     * @if_not_exists (path, operand)@ - if the item does not contain an attribute at the specified path, then @if_not_exists@ evaluates to operand; otherwise, it evaluates to path. You can use this function to avoid overwriting an attribute that may already be present in the item.
    --
    --
    --     * @list_append (operand, operand)@ - evaluates to a list with a new element added to it. You can append the new element to the start or the end of the list by reversing the order of the operands.
    --
    --
    -- These function names are case-sensitive.
    --
    --
    --     * @REMOVE@ - Removes one or more attributes from an item.
    --
    --
    --     * @ADD@ - Adds the specified value to the item, if the attribute does not already exist. If the attribute does exist, then the behavior of @ADD@ depends on the data type of the attribute:
    --
    --     * If the existing attribute is a number, and if @Value@ is also a number, then @Value@ is mathematically added to the existing attribute. If @Value@ is a negative number, then it is subtracted from the existing attribute.
    --
    --
    --     * If the existing data type is a set and if @Value@ is also a set, then @Value@ is added to the existing set. For example, if the attribute value is the set @[1,2]@ , and the @ADD@ action specified @[3]@ , then the final attribute value is @[1,2,3]@ . An error occurs if an @ADD@ action is specified for a set attribute and the attribute type specified does not match the existing set type.
    -- Both sets must have the same primitive data type. For example, if the existing data type is a set of strings, the @Value@ must also be a set of strings.
    --
    --
    -- /Important:/ The @ADD@ action only supports Number and set data types. In addition, @ADD@ can only be used on top-level attributes, not nested attributes.
    --
    --
    --     * @DELETE@ - Deletes an element from a set.
    -- If a set of values is specified, then those values are subtracted from the old set. For example, if the attribute value was the set @[a,b,c]@ and the @DELETE@ action specifies @[a,c]@ , then the final attribute value is @[b]@ . Specifying an empty set is an error.
    -- /Important:/ The @DELETE@ action only supports set data types. In addition, @DELETE@ can only be used on top-level attributes, not nested attributes.
    --
    --
    -- You can have many actions in a single expression, such as the following: @SET a=:value1, b=:value2 DELETE :value3, :value4, :value5@
    -- For more information on update expressions, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.Modifying.html Modifying Items and Attributes> in the /Amazon DynamoDB Developer Guide/ .
    updateExpression :: Core.Maybe Types.UpdateExpression
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateItem' value with any optional fields omitted.
mkUpdateItem ::
  -- | 'tableName'
  Types.TableName ->
  UpdateItem
mkUpdateItem tableName =
  UpdateItem'
    { tableName,
      key = Core.mempty,
      attributeUpdates = Core.Nothing,
      conditionExpression = Core.Nothing,
      conditionalOperator = Core.Nothing,
      expected = Core.Nothing,
      expressionAttributeNames = Core.Nothing,
      expressionAttributeValues = Core.Nothing,
      returnConsumedCapacity = Core.Nothing,
      returnItemCollectionMetrics = Core.Nothing,
      returnValues = Core.Nothing,
      updateExpression = Core.Nothing
    }

-- | The name of the table containing the item to update.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiTableName :: Lens.Lens' UpdateItem Types.TableName
uiTableName = Lens.field @"tableName"
{-# DEPRECATED uiTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The primary key of the item to be updated. Each element consists of an attribute name and a value for that attribute.
--
-- For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiKey :: Lens.Lens' UpdateItem (Core.HashMap Types.AttributeName Types.AttributeValue)
uiKey = Lens.field @"key"
{-# DEPRECATED uiKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | This is a legacy parameter. Use @UpdateExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributeUpdates.html AttributeUpdates> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'attributeUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiAttributeUpdates :: Lens.Lens' UpdateItem (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValueUpdate))
uiAttributeUpdates = Lens.field @"attributeUpdates"
{-# DEPRECATED uiAttributeUpdates "Use generic-lens or generic-optics with 'attributeUpdates' instead." #-}

-- | A condition that must be satisfied in order for a conditional update to succeed.
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
-- For more information about condition expressions, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'conditionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiConditionExpression :: Lens.Lens' UpdateItem (Core.Maybe Types.ConditionExpression)
uiConditionExpression = Lens.field @"conditionExpression"
{-# DEPRECATED uiConditionExpression "Use generic-lens or generic-optics with 'conditionExpression' instead." #-}

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'conditionalOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiConditionalOperator :: Lens.Lens' UpdateItem (Core.Maybe Types.ConditionalOperator)
uiConditionalOperator = Lens.field @"conditionalOperator"
{-# DEPRECATED uiConditionalOperator "Use generic-lens or generic-optics with 'conditionalOperator' instead." #-}

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiExpected :: Lens.Lens' UpdateItem (Core.Maybe (Core.HashMap Types.AttributeName Types.ExpectedAttributeValue))
uiExpected = Lens.field @"expected"
{-# DEPRECATED uiExpected "Use generic-lens or generic-optics with 'expected' instead." #-}

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
-- The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ .) To work around this, you could specify the following for @ExpressionAttributeNames@ :
--
--     * @{"#P":"Percentile"}@
--
--
-- You could then use this substitution in an expression, as in this example:
--
--     * @#P = :val@
--
--
-- For more information about expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiExpressionAttributeNames :: Lens.Lens' UpdateItem (Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName))
uiExpressionAttributeNames = Lens.field @"expressionAttributeNames"
{-# DEPRECATED uiExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the @ProductStatus@ attribute was one of the following:
-- @Available | Backordered | Discontinued@
-- You would first need to specify @ExpressionAttributeValues@ as follows:
-- @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@
-- You could then use these values in an expression, such as this:
-- @ProductStatus IN (:avail, :back, :disc)@
-- For more information on expression attribute values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expressionAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiExpressionAttributeValues :: Lens.Lens' UpdateItem (Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue))
uiExpressionAttributeValues = Lens.field @"expressionAttributeValues"
{-# DEPRECATED uiExpressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiReturnConsumedCapacity :: Lens.Lens' UpdateItem (Core.Maybe Types.ReturnConsumedCapacity)
uiReturnConsumedCapacity = Lens.field @"returnConsumedCapacity"
{-# DEPRECATED uiReturnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead." #-}

-- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
--
-- /Note:/ Consider using 'returnItemCollectionMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiReturnItemCollectionMetrics :: Lens.Lens' UpdateItem (Core.Maybe Types.ReturnItemCollectionMetrics)
uiReturnItemCollectionMetrics = Lens.field @"returnItemCollectionMetrics"
{-# DEPRECATED uiReturnItemCollectionMetrics "Use generic-lens or generic-optics with 'returnItemCollectionMetrics' instead." #-}

-- | Use @ReturnValues@ if you want to get the item attributes as they appear before or after they are updated. For @UpdateItem@ , the valid values are:
--
--
--     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)
--
--
--     * @ALL_OLD@ - Returns all of the attributes of the item, as they appeared before the UpdateItem operation.
--
--
--     * @UPDATED_OLD@ - Returns only the updated attributes, as they appeared before the UpdateItem operation.
--
--
--     * @ALL_NEW@ - Returns all of the attributes of the item, as they appear after the UpdateItem operation.
--
--
--     * @UPDATED_NEW@ - Returns only the updated attributes, as they appear after the UpdateItem operation.
--
--
-- There is no additional cost associated with requesting a return value aside from the small network and processing overhead of receiving a larger response. No read capacity units are consumed.
-- The values returned are strongly consistent.
--
-- /Note:/ Consider using 'returnValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiReturnValues :: Lens.Lens' UpdateItem (Core.Maybe Types.ReturnValue)
uiReturnValues = Lens.field @"returnValues"
{-# DEPRECATED uiReturnValues "Use generic-lens or generic-optics with 'returnValues' instead." #-}

-- | An expression that defines one or more attributes to be updated, the action to be performed on them, and new values for them.
--
-- The following action values are available for @UpdateExpression@ .
--
--     * @SET@ - Adds one or more attributes and values to an item. If any of these attributes already exist, they are replaced by the new values. You can also use @SET@ to add or subtract from an attribute that is of type Number. For example: @SET myNum = myNum + :val@
-- @SET@ supports the following functions:
--
--     * @if_not_exists (path, operand)@ - if the item does not contain an attribute at the specified path, then @if_not_exists@ evaluates to operand; otherwise, it evaluates to path. You can use this function to avoid overwriting an attribute that may already be present in the item.
--
--
--     * @list_append (operand, operand)@ - evaluates to a list with a new element added to it. You can append the new element to the start or the end of the list by reversing the order of the operands.
--
--
-- These function names are case-sensitive.
--
--
--     * @REMOVE@ - Removes one or more attributes from an item.
--
--
--     * @ADD@ - Adds the specified value to the item, if the attribute does not already exist. If the attribute does exist, then the behavior of @ADD@ depends on the data type of the attribute:
--
--     * If the existing attribute is a number, and if @Value@ is also a number, then @Value@ is mathematically added to the existing attribute. If @Value@ is a negative number, then it is subtracted from the existing attribute.
--
--
--     * If the existing data type is a set and if @Value@ is also a set, then @Value@ is added to the existing set. For example, if the attribute value is the set @[1,2]@ , and the @ADD@ action specified @[3]@ , then the final attribute value is @[1,2,3]@ . An error occurs if an @ADD@ action is specified for a set attribute and the attribute type specified does not match the existing set type.
-- Both sets must have the same primitive data type. For example, if the existing data type is a set of strings, the @Value@ must also be a set of strings.
--
--
-- /Important:/ The @ADD@ action only supports Number and set data types. In addition, @ADD@ can only be used on top-level attributes, not nested attributes.
--
--
--     * @DELETE@ - Deletes an element from a set.
-- If a set of values is specified, then those values are subtracted from the old set. For example, if the attribute value was the set @[a,b,c]@ and the @DELETE@ action specifies @[a,c]@ , then the final attribute value is @[b]@ . Specifying an empty set is an error.
-- /Important:/ The @DELETE@ action only supports set data types. In addition, @DELETE@ can only be used on top-level attributes, not nested attributes.
--
--
-- You can have many actions in a single expression, such as the following: @SET a=:value1, b=:value2 DELETE :value3, :value4, :value5@
-- For more information on update expressions, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.Modifying.html Modifying Items and Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'updateExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiUpdateExpression :: Lens.Lens' UpdateItem (Core.Maybe Types.UpdateExpression)
uiUpdateExpression = Lens.field @"updateExpression"
{-# DEPRECATED uiUpdateExpression "Use generic-lens or generic-optics with 'updateExpression' instead." #-}

instance Core.FromJSON UpdateItem where
  toJSON UpdateItem {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TableName" Core..= tableName),
            Core.Just ("Key" Core..= key),
            ("AttributeUpdates" Core..=) Core.<$> attributeUpdates,
            ("ConditionExpression" Core..=) Core.<$> conditionExpression,
            ("ConditionalOperator" Core..=) Core.<$> conditionalOperator,
            ("Expected" Core..=) Core.<$> expected,
            ("ExpressionAttributeNames" Core..=)
              Core.<$> expressionAttributeNames,
            ("ExpressionAttributeValues" Core..=)
              Core.<$> expressionAttributeValues,
            ("ReturnConsumedCapacity" Core..=) Core.<$> returnConsumedCapacity,
            ("ReturnItemCollectionMetrics" Core..=)
              Core.<$> returnItemCollectionMetrics,
            ("ReturnValues" Core..=) Core.<$> returnValues,
            ("UpdateExpression" Core..=) Core.<$> updateExpression
          ]
      )

instance Core.AWSRequest UpdateItem where
  type Rs UpdateItem = UpdateItemResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.UpdateItem")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateItemResponse'
            Core.<$> (x Core..:? "Attributes")
            Core.<*> (x Core..:? "ConsumedCapacity")
            Core.<*> (x Core..:? "ItemCollectionMetrics")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of an @UpdateItem@ operation.
--
-- /See:/ 'mkUpdateItemResponse' smart constructor.
data UpdateItemResponse = UpdateItemResponse'
  { -- | A map of attribute values as they appear before or after the @UpdateItem@ operation, as determined by the @ReturnValues@ parameter.
    --
    -- The @Attributes@ map is only present if @ReturnValues@ was specified as something other than @NONE@ in the request. Each element represents one attribute.
    attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | The capacity units consumed by the @UpdateItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
    consumedCapacity :: Core.Maybe Types.ConsumedCapacity,
    -- | Information about item collections, if any, that were affected by the @UpdateItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response.
    --
    -- Each @ItemCollectionMetrics@ element consists of:
    --
    --     * @ItemCollectionKey@ - The partition key value of the item collection. This is the same as the partition key value of the item itself.
    --
    --
    --     * @SizeEstimateRangeGB@ - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit.
    -- The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
    itemCollectionMetrics :: Core.Maybe Types.ItemCollectionMetrics,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateItemResponse' value with any optional fields omitted.
mkUpdateItemResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateItemResponse
mkUpdateItemResponse responseStatus =
  UpdateItemResponse'
    { attributes = Core.Nothing,
      consumedCapacity = Core.Nothing,
      itemCollectionMetrics = Core.Nothing,
      responseStatus
    }

-- | A map of attribute values as they appear before or after the @UpdateItem@ operation, as determined by the @ReturnValues@ parameter.
--
-- The @Attributes@ map is only present if @ReturnValues@ was specified as something other than @NONE@ in the request. Each element represents one attribute.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirrsAttributes :: Lens.Lens' UpdateItemResponse (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
uirrsAttributes = Lens.field @"attributes"
{-# DEPRECATED uirrsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The capacity units consumed by the @UpdateItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirrsConsumedCapacity :: Lens.Lens' UpdateItemResponse (Core.Maybe Types.ConsumedCapacity)
uirrsConsumedCapacity = Lens.field @"consumedCapacity"
{-# DEPRECATED uirrsConsumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead." #-}

-- | Information about item collections, if any, that were affected by the @UpdateItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response.
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
uirrsItemCollectionMetrics :: Lens.Lens' UpdateItemResponse (Core.Maybe Types.ItemCollectionMetrics)
uirrsItemCollectionMetrics = Lens.field @"itemCollectionMetrics"
{-# DEPRECATED uirrsItemCollectionMetrics "Use generic-lens or generic-optics with 'itemCollectionMetrics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirrsResponseStatus :: Lens.Lens' UpdateItemResponse Core.Int
uirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
