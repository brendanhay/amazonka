{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.GetItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetItem@ operation returns a set of attributes for the item with the given primary key. If there is no matching item, @GetItem@ does not return any data and there will be no @Item@ element in the response.
--
-- @GetItem@ provides an eventually consistent read by default. If your application requires a strongly consistent read, set @ConsistentRead@ to @true@ . Although a strongly consistent read might take more time than an eventually consistent read, it always returns the last updated value.
module Network.AWS.DynamoDB.GetItem
  ( -- * Creating a request
    GetItem (..),
    mkGetItem,

    -- ** Request lenses
    giTableName,
    giKey,
    giAttributesToGet,
    giConsistentRead,
    giExpressionAttributeNames,
    giProjectionExpression,
    giReturnConsumedCapacity,

    -- * Destructuring the response
    GetItemResponse (..),
    mkGetItemResponse,

    -- ** Response lenses
    girrsConsumedCapacity,
    girrsItem,
    girrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetItem@ operation.
--
-- /See:/ 'mkGetItem' smart constructor.
data GetItem = GetItem'
  { -- | The name of the table containing the requested item.
    tableName :: Types.TableName,
    -- | A map of attribute names to @AttributeValue@ objects, representing the primary key of the item to retrieve.
    --
    -- For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
    key :: Core.HashMap Types.AttributeName Types.AttributeValue,
    -- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
    attributesToGet :: Core.Maybe (Core.NonEmpty Types.AttributeName),
    -- | Determines the read consistency model: If set to @true@ , then the operation uses strongly consistent reads; otherwise, the operation uses eventually consistent reads.
    consistentRead :: Core.Maybe Core.Bool,
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
    expressionAttributeNames :: Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName),
    -- | A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.
    --
    -- If no attribute names are specified, then all attributes are returned. If any of the requested attributes are not found, they do not appear in the result.
    -- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
    projectionExpression :: Core.Maybe Types.ProjectionExpression,
    returnConsumedCapacity :: Core.Maybe Types.ReturnConsumedCapacity
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetItem' value with any optional fields omitted.
mkGetItem ::
  -- | 'tableName'
  Types.TableName ->
  GetItem
mkGetItem tableName =
  GetItem'
    { tableName,
      key = Core.mempty,
      attributesToGet = Core.Nothing,
      consistentRead = Core.Nothing,
      expressionAttributeNames = Core.Nothing,
      projectionExpression = Core.Nothing,
      returnConsumedCapacity = Core.Nothing
    }

-- | The name of the table containing the requested item.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giTableName :: Lens.Lens' GetItem Types.TableName
giTableName = Lens.field @"tableName"
{-# DEPRECATED giTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | A map of attribute names to @AttributeValue@ objects, representing the primary key of the item to retrieve.
--
-- For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giKey :: Lens.Lens' GetItem (Core.HashMap Types.AttributeName Types.AttributeValue)
giKey = Lens.field @"key"
{-# DEPRECATED giKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'attributesToGet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giAttributesToGet :: Lens.Lens' GetItem (Core.Maybe (Core.NonEmpty Types.AttributeName))
giAttributesToGet = Lens.field @"attributesToGet"
{-# DEPRECATED giAttributesToGet "Use generic-lens or generic-optics with 'attributesToGet' instead." #-}

-- | Determines the read consistency model: If set to @true@ , then the operation uses strongly consistent reads; otherwise, the operation uses eventually consistent reads.
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giConsistentRead :: Lens.Lens' GetItem (Core.Maybe Core.Bool)
giConsistentRead = Lens.field @"consistentRead"
{-# DEPRECATED giConsistentRead "Use generic-lens or generic-optics with 'consistentRead' instead." #-}

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
giExpressionAttributeNames :: Lens.Lens' GetItem (Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName))
giExpressionAttributeNames = Lens.field @"expressionAttributeNames"
{-# DEPRECATED giExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.
--
-- If no attribute names are specified, then all attributes are returned. If any of the requested attributes are not found, they do not appear in the result.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'projectionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giProjectionExpression :: Lens.Lens' GetItem (Core.Maybe Types.ProjectionExpression)
giProjectionExpression = Lens.field @"projectionExpression"
{-# DEPRECATED giProjectionExpression "Use generic-lens or generic-optics with 'projectionExpression' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giReturnConsumedCapacity :: Lens.Lens' GetItem (Core.Maybe Types.ReturnConsumedCapacity)
giReturnConsumedCapacity = Lens.field @"returnConsumedCapacity"
{-# DEPRECATED giReturnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead." #-}

instance Core.FromJSON GetItem where
  toJSON GetItem {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TableName" Core..= tableName),
            Core.Just ("Key" Core..= key),
            ("AttributesToGet" Core..=) Core.<$> attributesToGet,
            ("ConsistentRead" Core..=) Core.<$> consistentRead,
            ("ExpressionAttributeNames" Core..=)
              Core.<$> expressionAttributeNames,
            ("ProjectionExpression" Core..=) Core.<$> projectionExpression,
            ("ReturnConsumedCapacity" Core..=)
              Core.<$> returnConsumedCapacity
          ]
      )

instance Core.AWSRequest GetItem where
  type Rs GetItem = GetItemResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.GetItem")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetItemResponse'
            Core.<$> (x Core..:? "ConsumedCapacity")
            Core.<*> (x Core..:? "Item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @GetItem@ operation.
--
-- /See:/ 'mkGetItemResponse' smart constructor.
data GetItemResponse = GetItemResponse'
  { -- | The capacity units consumed by the @GetItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read/Write Capacity Mode> in the /Amazon DynamoDB Developer Guide/ .
    consumedCapacity :: Core.Maybe Types.ConsumedCapacity,
    -- | A map of attribute names to @AttributeValue@ objects, as specified by @ProjectionExpression@ .
    item :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetItemResponse' value with any optional fields omitted.
mkGetItemResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetItemResponse
mkGetItemResponse responseStatus =
  GetItemResponse'
    { consumedCapacity = Core.Nothing,
      item = Core.Nothing,
      responseStatus
    }

-- | The capacity units consumed by the @GetItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read/Write Capacity Mode> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsConsumedCapacity :: Lens.Lens' GetItemResponse (Core.Maybe Types.ConsumedCapacity)
girrsConsumedCapacity = Lens.field @"consumedCapacity"
{-# DEPRECATED girrsConsumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead." #-}

-- | A map of attribute names to @AttributeValue@ objects, as specified by @ProjectionExpression@ .
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsItem :: Lens.Lens' GetItemResponse (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
girrsItem = Lens.field @"item"
{-# DEPRECATED girrsItem "Use generic-lens or generic-optics with 'item' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsResponseStatus :: Lens.Lens' GetItemResponse Core.Int
girrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED girrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
