{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteItem (..)
    , mkDeleteItem
    -- ** Request lenses
    , diTableName
    , diKey
    , diConditionExpression
    , diConditionalOperator
    , diExpected
    , diExpressionAttributeNames
    , diExpressionAttributeValues
    , diReturnConsumedCapacity
    , diReturnItemCollectionMetrics
    , diReturnValues

    -- * Destructuring the response
    , DeleteItemResponse (..)
    , mkDeleteItemResponse
    -- ** Response lenses
    , dirrsAttributes
    , dirrsConsumedCapacity
    , dirrsItemCollectionMetrics
    , dirrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteItem@ operation.
--
-- /See:/ 'mkDeleteItem' smart constructor.
data DeleteItem = DeleteItem'
  { tableName :: Types.TableName
    -- ^ The name of the table from which to delete the item.
  , key :: Core.HashMap Types.AttributeName Types.AttributeValue
    -- ^ A map of attribute names to @AttributeValue@ objects, representing the primary key of the item to delete.
--
-- For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
  , conditionExpression :: Core.Maybe Types.ConditionExpression
    -- ^ A condition that must be satisfied in order for a conditional @DeleteItem@ to succeed.
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
  , conditionalOperator :: Core.Maybe Types.ConditionalOperator
    -- ^ This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
  , expected :: Core.Maybe (Core.HashMap Types.AttributeName Types.ExpectedAttributeValue)
    -- ^ This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
  , expressionAttributeNames :: Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName)
    -- ^ One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :
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
  , expressionAttributeValues :: Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue)
    -- ^ One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following: 
-- @Available | Backordered | Discontinued@ 
-- You would first need to specify @ExpressionAttributeValues@ as follows:
-- @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@ 
-- You could then use these values in an expression, such as this:
-- @ProductStatus IN (:avail, :back, :disc)@ 
-- For more information on expression attribute values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
  , returnConsumedCapacity :: Core.Maybe Types.ReturnConsumedCapacity
  , returnItemCollectionMetrics :: Core.Maybe Types.ReturnItemCollectionMetrics
    -- ^ Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
  , returnValues :: Core.Maybe Types.ReturnValue
    -- ^ Use @ReturnValues@ if you want to get the item attributes as they appeared before they were deleted. For @DeleteItem@ , the valid values are:
--
--
--     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)
--
--
--     * @ALL_OLD@ - The content of the old item is returned.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteItem' value with any optional fields omitted.
mkDeleteItem
    :: Types.TableName -- ^ 'tableName'
    -> DeleteItem
mkDeleteItem tableName
  = DeleteItem'{tableName, key = Core.mempty,
                conditionExpression = Core.Nothing,
                conditionalOperator = Core.Nothing, expected = Core.Nothing,
                expressionAttributeNames = Core.Nothing,
                expressionAttributeValues = Core.Nothing,
                returnConsumedCapacity = Core.Nothing,
                returnItemCollectionMetrics = Core.Nothing,
                returnValues = Core.Nothing}

-- | The name of the table from which to delete the item.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTableName :: Lens.Lens' DeleteItem Types.TableName
diTableName = Lens.field @"tableName"
{-# INLINEABLE diTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | A map of attribute names to @AttributeValue@ objects, representing the primary key of the item to delete.
--
-- For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diKey :: Lens.Lens' DeleteItem (Core.HashMap Types.AttributeName Types.AttributeValue)
diKey = Lens.field @"key"
{-# INLINEABLE diKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

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
diConditionExpression :: Lens.Lens' DeleteItem (Core.Maybe Types.ConditionExpression)
diConditionExpression = Lens.field @"conditionExpression"
{-# INLINEABLE diConditionExpression #-}
{-# DEPRECATED conditionExpression "Use generic-lens or generic-optics with 'conditionExpression' instead"  #-}

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'conditionalOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diConditionalOperator :: Lens.Lens' DeleteItem (Core.Maybe Types.ConditionalOperator)
diConditionalOperator = Lens.field @"conditionalOperator"
{-# INLINEABLE diConditionalOperator #-}
{-# DEPRECATED conditionalOperator "Use generic-lens or generic-optics with 'conditionalOperator' instead"  #-}

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diExpected :: Lens.Lens' DeleteItem (Core.Maybe (Core.HashMap Types.AttributeName Types.ExpectedAttributeValue))
diExpected = Lens.field @"expected"
{-# INLINEABLE diExpected #-}
{-# DEPRECATED expected "Use generic-lens or generic-optics with 'expected' instead"  #-}

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
diExpressionAttributeNames :: Lens.Lens' DeleteItem (Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName))
diExpressionAttributeNames = Lens.field @"expressionAttributeNames"
{-# INLINEABLE diExpressionAttributeNames #-}
{-# DEPRECATED expressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead"  #-}

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
diExpressionAttributeValues :: Lens.Lens' DeleteItem (Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue))
diExpressionAttributeValues = Lens.field @"expressionAttributeValues"
{-# INLINEABLE diExpressionAttributeValues #-}
{-# DEPRECATED expressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diReturnConsumedCapacity :: Lens.Lens' DeleteItem (Core.Maybe Types.ReturnConsumedCapacity)
diReturnConsumedCapacity = Lens.field @"returnConsumedCapacity"
{-# INLINEABLE diReturnConsumedCapacity #-}
{-# DEPRECATED returnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead"  #-}

-- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
--
-- /Note:/ Consider using 'returnItemCollectionMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diReturnItemCollectionMetrics :: Lens.Lens' DeleteItem (Core.Maybe Types.ReturnItemCollectionMetrics)
diReturnItemCollectionMetrics = Lens.field @"returnItemCollectionMetrics"
{-# INLINEABLE diReturnItemCollectionMetrics #-}
{-# DEPRECATED returnItemCollectionMetrics "Use generic-lens or generic-optics with 'returnItemCollectionMetrics' instead"  #-}

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
diReturnValues :: Lens.Lens' DeleteItem (Core.Maybe Types.ReturnValue)
diReturnValues = Lens.field @"returnValues"
{-# INLINEABLE diReturnValues #-}
{-# DEPRECATED returnValues "Use generic-lens or generic-optics with 'returnValues' instead"  #-}

instance Core.ToQuery DeleteItem where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteItem where
        toHeaders DeleteItem{..}
          = Core.pure ("X-Amz-Target", "DynamoDB_20120810.DeleteItem")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DeleteItem where
        toJSON DeleteItem{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TableName" Core..= tableName),
                  Core.Just ("Key" Core..= key),
                  ("ConditionExpression" Core..=) Core.<$> conditionExpression,
                  ("ConditionalOperator" Core..=) Core.<$> conditionalOperator,
                  ("Expected" Core..=) Core.<$> expected,
                  ("ExpressionAttributeNames" Core..=) Core.<$>
                    expressionAttributeNames,
                  ("ExpressionAttributeValues" Core..=) Core.<$>
                    expressionAttributeValues,
                  ("ReturnConsumedCapacity" Core..=) Core.<$> returnConsumedCapacity,
                  ("ReturnItemCollectionMetrics" Core..=) Core.<$>
                    returnItemCollectionMetrics,
                  ("ReturnValues" Core..=) Core.<$> returnValues])

instance Core.AWSRequest DeleteItem where
        type Rs DeleteItem = DeleteItemResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteItemResponse' Core.<$>
                   (x Core..:? "Attributes") Core.<*> x Core..:? "ConsumedCapacity"
                     Core.<*> x Core..:? "ItemCollectionMetrics"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @DeleteItem@ operation.
--
-- /See:/ 'mkDeleteItemResponse' smart constructor.
data DeleteItemResponse = DeleteItemResponse'
  { attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue)
    -- ^ A map of attribute names to @AttributeValue@ objects, representing the item as it appeared before the @DeleteItem@ operation. This map appears in the response only if @ReturnValues@ was specified as @ALL_OLD@ in the request.
  , consumedCapacity :: Core.Maybe Types.ConsumedCapacity
    -- ^ The capacity units consumed by the @DeleteItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Mode> in the /Amazon DynamoDB Developer Guide/ .
  , itemCollectionMetrics :: Core.Maybe Types.ItemCollectionMetrics
    -- ^ Information about item collections, if any, that were affected by the @DeleteItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response.
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
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteItemResponse' value with any optional fields omitted.
mkDeleteItemResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteItemResponse
mkDeleteItemResponse responseStatus
  = DeleteItemResponse'{attributes = Core.Nothing,
                        consumedCapacity = Core.Nothing,
                        itemCollectionMetrics = Core.Nothing, responseStatus}

-- | A map of attribute names to @AttributeValue@ objects, representing the item as it appeared before the @DeleteItem@ operation. This map appears in the response only if @ReturnValues@ was specified as @ALL_OLD@ in the request.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsAttributes :: Lens.Lens' DeleteItemResponse (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
dirrsAttributes = Lens.field @"attributes"
{-# INLINEABLE dirrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The capacity units consumed by the @DeleteItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Mode> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsConsumedCapacity :: Lens.Lens' DeleteItemResponse (Core.Maybe Types.ConsumedCapacity)
dirrsConsumedCapacity = Lens.field @"consumedCapacity"
{-# INLINEABLE dirrsConsumedCapacity #-}
{-# DEPRECATED consumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead"  #-}

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
dirrsItemCollectionMetrics :: Lens.Lens' DeleteItemResponse (Core.Maybe Types.ItemCollectionMetrics)
dirrsItemCollectionMetrics = Lens.field @"itemCollectionMetrics"
{-# INLINEABLE dirrsItemCollectionMetrics #-}
{-# DEPRECATED itemCollectionMetrics "Use generic-lens or generic-optics with 'itemCollectionMetrics' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DeleteItemResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
