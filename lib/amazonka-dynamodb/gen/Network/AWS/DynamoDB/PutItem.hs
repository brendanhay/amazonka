{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.PutItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new item, or replaces an old item with a new item. If an item that has the same primary key as the new item already exists in the specified table, the new item completely replaces the existing item. You can perform a conditional put operation (add a new item if one with the specified primary key doesn't exist), or replace an existing item if it has certain attribute values. You can return the item's attribute values in the same operation, using the @ReturnValues@ parameter.
--
-- /Important:/ This topic provides general information about the @PutItem@ API.
-- For information on how to call the @PutItem@ API using the AWS SDK in specific languages, see the following:
--
--     * <http://docs.aws.amazon.com/goto/aws-cli/dynamodb-2012-08-10/PutItem PutItem in the AWS Command Line Interface> 
--
--
--     * <http://docs.aws.amazon.com/goto/DotNetSDKV3/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for .NET> 
--
--
--     * <http://docs.aws.amazon.com/goto/SdkForCpp/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for C++> 
--
--
--     * <http://docs.aws.amazon.com/goto/SdkForGoV1/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for Go> 
--
--
--     * <http://docs.aws.amazon.com/goto/SdkForJava/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for Java> 
--
--
--     * <http://docs.aws.amazon.com/goto/AWSJavaScriptSDK/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for JavaScript> 
--
--
--     * <http://docs.aws.amazon.com/goto/SdkForPHPV3/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for PHP V3> 
--
--
--     * <http://docs.aws.amazon.com/goto/boto3/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for Python> 
--
--
--     * <http://docs.aws.amazon.com/goto/SdkForRubyV2/dynamodb-2012-08-10/PutItem PutItem in the AWS SDK for Ruby V2> 
--
--
-- When you add an item, the primary key attributes are the only required attributes. Attribute values cannot be null.
-- Empty String and Binary attribute values are allowed. Attribute values of type String and Binary must have a length greater than zero if the attribute is used as a key attribute for a table or index. Set type attributes cannot be empty. 
-- Invalid Requests with empty values will be rejected with a @ValidationException@ exception.
-- For more information about @PutItem@ , see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithItems.html Working with Items> in the /Amazon DynamoDB Developer Guide/ .
module Network.AWS.DynamoDB.PutItem
    (
    -- * Creating a request
      PutItem (..)
    , mkPutItem
    -- ** Request lenses
    , piTableName
    , piItem
    , piConditionExpression
    , piConditionalOperator
    , piExpected
    , piExpressionAttributeNames
    , piExpressionAttributeValues
    , piReturnConsumedCapacity
    , piReturnItemCollectionMetrics
    , piReturnValues

    -- * Destructuring the response
    , PutItemResponse (..)
    , mkPutItemResponse
    -- ** Response lenses
    , pirrsAttributes
    , pirrsConsumedCapacity
    , pirrsItemCollectionMetrics
    , pirrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PutItem@ operation.
--
-- /See:/ 'mkPutItem' smart constructor.
data PutItem = PutItem'
  { tableName :: Types.TableName
    -- ^ The name of the table to contain the item.
  , item :: Core.HashMap Types.AttributeName Types.AttributeValue
    -- ^ A map of attribute name/value pairs, one for each attribute. Only the primary key attributes are required; you can optionally provide other attribute name-value pairs for the item.
--
-- You must provide all of the attributes for the primary key. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide both values for both the partition key and the sort key.
-- If you specify any attributes that are part of an index key, then the data types for those attributes must match those of the schema in the table's attribute definition.
-- Empty String and Binary attribute values are allowed. Attribute values of type String and Binary must have a length greater than zero if the attribute is used as a key attribute for a table or index.
-- For more information about primary keys, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.CoreComponents.html#HowItWorks.CoreComponents.PrimaryKey Primary Key> in the /Amazon DynamoDB Developer Guide/ .
-- Each element in the @Item@ map is an @AttributeValue@ object.
  , conditionExpression :: Core.Maybe Types.ConditionExpression
    -- ^ A condition that must be satisfied in order for a conditional @PutItem@ operation to succeed.
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
-- For more information on condition expressions, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
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
    -- ^ Use @ReturnValues@ if you want to get the item attributes as they appeared before they were updated with the @PutItem@ request. For @PutItem@ , the valid values are:
--
--
--     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)
--
--
--     * @ALL_OLD@ - If @PutItem@ overwrote an attribute name-value pair, then the content of the old item is returned.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutItem' value with any optional fields omitted.
mkPutItem
    :: Types.TableName -- ^ 'tableName'
    -> PutItem
mkPutItem tableName
  = PutItem'{tableName, item = Core.mempty,
             conditionExpression = Core.Nothing,
             conditionalOperator = Core.Nothing, expected = Core.Nothing,
             expressionAttributeNames = Core.Nothing,
             expressionAttributeValues = Core.Nothing,
             returnConsumedCapacity = Core.Nothing,
             returnItemCollectionMetrics = Core.Nothing,
             returnValues = Core.Nothing}

-- | The name of the table to contain the item.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piTableName :: Lens.Lens' PutItem Types.TableName
piTableName = Lens.field @"tableName"
{-# INLINEABLE piTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | A map of attribute name/value pairs, one for each attribute. Only the primary key attributes are required; you can optionally provide other attribute name-value pairs for the item.
--
-- You must provide all of the attributes for the primary key. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide both values for both the partition key and the sort key.
-- If you specify any attributes that are part of an index key, then the data types for those attributes must match those of the schema in the table's attribute definition.
-- Empty String and Binary attribute values are allowed. Attribute values of type String and Binary must have a length greater than zero if the attribute is used as a key attribute for a table or index.
-- For more information about primary keys, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.CoreComponents.html#HowItWorks.CoreComponents.PrimaryKey Primary Key> in the /Amazon DynamoDB Developer Guide/ .
-- Each element in the @Item@ map is an @AttributeValue@ object.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piItem :: Lens.Lens' PutItem (Core.HashMap Types.AttributeName Types.AttributeValue)
piItem = Lens.field @"item"
{-# INLINEABLE piItem #-}
{-# DEPRECATED item "Use generic-lens or generic-optics with 'item' instead"  #-}

-- | A condition that must be satisfied in order for a conditional @PutItem@ operation to succeed.
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
-- For more information on condition expressions, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'conditionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piConditionExpression :: Lens.Lens' PutItem (Core.Maybe Types.ConditionExpression)
piConditionExpression = Lens.field @"conditionExpression"
{-# INLINEABLE piConditionExpression #-}
{-# DEPRECATED conditionExpression "Use generic-lens or generic-optics with 'conditionExpression' instead"  #-}

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'conditionalOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piConditionalOperator :: Lens.Lens' PutItem (Core.Maybe Types.ConditionalOperator)
piConditionalOperator = Lens.field @"conditionalOperator"
{-# INLINEABLE piConditionalOperator #-}
{-# DEPRECATED conditionalOperator "Use generic-lens or generic-optics with 'conditionalOperator' instead"  #-}

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piExpected :: Lens.Lens' PutItem (Core.Maybe (Core.HashMap Types.AttributeName Types.ExpectedAttributeValue))
piExpected = Lens.field @"expected"
{-# INLINEABLE piExpected #-}
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
piExpressionAttributeNames :: Lens.Lens' PutItem (Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName))
piExpressionAttributeNames = Lens.field @"expressionAttributeNames"
{-# INLINEABLE piExpressionAttributeNames #-}
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
piExpressionAttributeValues :: Lens.Lens' PutItem (Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue))
piExpressionAttributeValues = Lens.field @"expressionAttributeValues"
{-# INLINEABLE piExpressionAttributeValues #-}
{-# DEPRECATED expressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piReturnConsumedCapacity :: Lens.Lens' PutItem (Core.Maybe Types.ReturnConsumedCapacity)
piReturnConsumedCapacity = Lens.field @"returnConsumedCapacity"
{-# INLINEABLE piReturnConsumedCapacity #-}
{-# DEPRECATED returnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead"  #-}

-- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
--
-- /Note:/ Consider using 'returnItemCollectionMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piReturnItemCollectionMetrics :: Lens.Lens' PutItem (Core.Maybe Types.ReturnItemCollectionMetrics)
piReturnItemCollectionMetrics = Lens.field @"returnItemCollectionMetrics"
{-# INLINEABLE piReturnItemCollectionMetrics #-}
{-# DEPRECATED returnItemCollectionMetrics "Use generic-lens or generic-optics with 'returnItemCollectionMetrics' instead"  #-}

-- | Use @ReturnValues@ if you want to get the item attributes as they appeared before they were updated with the @PutItem@ request. For @PutItem@ , the valid values are:
--
--
--     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)
--
--
--     * @ALL_OLD@ - If @PutItem@ overwrote an attribute name-value pair, then the content of the old item is returned.
--
--
--
-- /Note:/ Consider using 'returnValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piReturnValues :: Lens.Lens' PutItem (Core.Maybe Types.ReturnValue)
piReturnValues = Lens.field @"returnValues"
{-# INLINEABLE piReturnValues #-}
{-# DEPRECATED returnValues "Use generic-lens or generic-optics with 'returnValues' instead"  #-}

instance Core.ToQuery PutItem where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutItem where
        toHeaders PutItem{..}
          = Core.pure ("X-Amz-Target", "DynamoDB_20120810.PutItem") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON PutItem where
        toJSON PutItem{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TableName" Core..= tableName),
                  Core.Just ("Item" Core..= item),
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

instance Core.AWSRequest PutItem where
        type Rs PutItem = PutItemResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutItemResponse' Core.<$>
                   (x Core..:? "Attributes") Core.<*> x Core..:? "ConsumedCapacity"
                     Core.<*> x Core..:? "ItemCollectionMetrics"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @PutItem@ operation.
--
-- /See:/ 'mkPutItemResponse' smart constructor.
data PutItemResponse = PutItemResponse'
  { attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue)
    -- ^ The attribute values as they appeared before the @PutItem@ operation, but only if @ReturnValues@ is specified as @ALL_OLD@ in the request. Each element consists of an attribute name and an attribute value.
  , consumedCapacity :: Core.Maybe Types.ConsumedCapacity
    -- ^ The capacity units consumed by the @PutItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read/Write Capacity Mode> in the /Amazon DynamoDB Developer Guide/ .
  , itemCollectionMetrics :: Core.Maybe Types.ItemCollectionMetrics
    -- ^ Information about item collections, if any, that were affected by the @PutItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response.
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

-- | Creates a 'PutItemResponse' value with any optional fields omitted.
mkPutItemResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutItemResponse
mkPutItemResponse responseStatus
  = PutItemResponse'{attributes = Core.Nothing,
                     consumedCapacity = Core.Nothing,
                     itemCollectionMetrics = Core.Nothing, responseStatus}

-- | The attribute values as they appeared before the @PutItem@ operation, but only if @ReturnValues@ is specified as @ALL_OLD@ in the request. Each element consists of an attribute name and an attribute value.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsAttributes :: Lens.Lens' PutItemResponse (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
pirrsAttributes = Lens.field @"attributes"
{-# INLINEABLE pirrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The capacity units consumed by the @PutItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read/Write Capacity Mode> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsConsumedCapacity :: Lens.Lens' PutItemResponse (Core.Maybe Types.ConsumedCapacity)
pirrsConsumedCapacity = Lens.field @"consumedCapacity"
{-# INLINEABLE pirrsConsumedCapacity #-}
{-# DEPRECATED consumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead"  #-}

-- | Information about item collections, if any, that were affected by the @PutItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response.
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
pirrsItemCollectionMetrics :: Lens.Lens' PutItemResponse (Core.Maybe Types.ItemCollectionMetrics)
pirrsItemCollectionMetrics = Lens.field @"itemCollectionMetrics"
{-# INLINEABLE pirrsItemCollectionMetrics #-}
{-# DEPRECATED itemCollectionMetrics "Use generic-lens or generic-optics with 'itemCollectionMetrics' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsResponseStatus :: Lens.Lens' PutItemResponse Core.Int
pirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
