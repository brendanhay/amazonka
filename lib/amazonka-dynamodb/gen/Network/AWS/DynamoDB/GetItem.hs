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
    giProjectionExpression,
    giAttributesToGet,
    giExpressionAttributeNames,
    giConsistentRead,
    giReturnConsumedCapacity,
    giKey,
    giTableName,

    -- * Destructuring the response
    GetItemResponse (..),
    mkGetItemResponse,

    -- ** Response lenses
    girsConsumedCapacity,
    girsItem,
    girsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetItem@ operation.
--
-- /See:/ 'mkGetItem' smart constructor.
data GetItem = GetItem'
  { -- | A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.
    --
    -- If no attribute names are specified, then all attributes are returned. If any of the requested attributes are not found, they do not appear in the result.
    -- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
    projectionExpression :: Lude.Maybe Lude.Text,
    -- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
    attributesToGet :: Lude.Maybe (Lude.NonEmpty Lude.Text),
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
    expressionAttributeNames :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Determines the read consistency model: If set to @true@ , then the operation uses strongly consistent reads; otherwise, the operation uses eventually consistent reads.
    consistentRead :: Lude.Maybe Lude.Bool,
    returnConsumedCapacity :: Lude.Maybe ReturnConsumedCapacity,
    -- | A map of attribute names to @AttributeValue@ objects, representing the primary key of the item to retrieve.
    --
    -- For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
    key :: Lude.HashMap Lude.Text (AttributeValue),
    -- | The name of the table containing the requested item.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetItem' with the minimum fields required to make a request.
--
-- * 'projectionExpression' - A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.
--
-- If no attribute names are specified, then all attributes are returned. If any of the requested attributes are not found, they do not appear in the result.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
-- * 'attributesToGet' - This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
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
-- * 'consistentRead' - Determines the read consistency model: If set to @true@ , then the operation uses strongly consistent reads; otherwise, the operation uses eventually consistent reads.
-- * 'returnConsumedCapacity' -
-- * 'key' - A map of attribute names to @AttributeValue@ objects, representing the primary key of the item to retrieve.
--
-- For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
-- * 'tableName' - The name of the table containing the requested item.
mkGetItem ::
  -- | 'tableName'
  Lude.Text ->
  GetItem
mkGetItem pTableName_ =
  GetItem'
    { projectionExpression = Lude.Nothing,
      attributesToGet = Lude.Nothing,
      expressionAttributeNames = Lude.Nothing,
      consistentRead = Lude.Nothing,
      returnConsumedCapacity = Lude.Nothing,
      key = Lude.mempty,
      tableName = pTableName_
    }

-- | A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.
--
-- If no attribute names are specified, then all attributes are returned. If any of the requested attributes are not found, they do not appear in the result.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'projectionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giProjectionExpression :: Lens.Lens' GetItem (Lude.Maybe Lude.Text)
giProjectionExpression = Lens.lens (projectionExpression :: GetItem -> Lude.Maybe Lude.Text) (\s a -> s {projectionExpression = a} :: GetItem)
{-# DEPRECATED giProjectionExpression "Use generic-lens or generic-optics with 'projectionExpression' instead." #-}

-- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'attributesToGet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giAttributesToGet :: Lens.Lens' GetItem (Lude.Maybe (Lude.NonEmpty Lude.Text))
giAttributesToGet = Lens.lens (attributesToGet :: GetItem -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {attributesToGet = a} :: GetItem)
{-# DEPRECATED giAttributesToGet "Use generic-lens or generic-optics with 'attributesToGet' instead." #-}

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
giExpressionAttributeNames :: Lens.Lens' GetItem (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
giExpressionAttributeNames = Lens.lens (expressionAttributeNames :: GetItem -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {expressionAttributeNames = a} :: GetItem)
{-# DEPRECATED giExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | Determines the read consistency model: If set to @true@ , then the operation uses strongly consistent reads; otherwise, the operation uses eventually consistent reads.
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giConsistentRead :: Lens.Lens' GetItem (Lude.Maybe Lude.Bool)
giConsistentRead = Lens.lens (consistentRead :: GetItem -> Lude.Maybe Lude.Bool) (\s a -> s {consistentRead = a} :: GetItem)
{-# DEPRECATED giConsistentRead "Use generic-lens or generic-optics with 'consistentRead' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giReturnConsumedCapacity :: Lens.Lens' GetItem (Lude.Maybe ReturnConsumedCapacity)
giReturnConsumedCapacity = Lens.lens (returnConsumedCapacity :: GetItem -> Lude.Maybe ReturnConsumedCapacity) (\s a -> s {returnConsumedCapacity = a} :: GetItem)
{-# DEPRECATED giReturnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead." #-}

-- | A map of attribute names to @AttributeValue@ objects, representing the primary key of the item to retrieve.
--
-- For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giKey :: Lens.Lens' GetItem (Lude.HashMap Lude.Text (AttributeValue))
giKey = Lens.lens (key :: GetItem -> Lude.HashMap Lude.Text (AttributeValue)) (\s a -> s {key = a} :: GetItem)
{-# DEPRECATED giKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The name of the table containing the requested item.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giTableName :: Lens.Lens' GetItem Lude.Text
giTableName = Lens.lens (tableName :: GetItem -> Lude.Text) (\s a -> s {tableName = a} :: GetItem)
{-# DEPRECATED giTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest GetItem where
  type Rs GetItem = GetItemResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetItemResponse'
            Lude.<$> (x Lude..?> "ConsumedCapacity")
            Lude.<*> (x Lude..?> "Item" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetItem where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.GetItem" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetItem where
  toJSON GetItem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProjectionExpression" Lude..=) Lude.<$> projectionExpression,
            ("AttributesToGet" Lude..=) Lude.<$> attributesToGet,
            ("ExpressionAttributeNames" Lude..=)
              Lude.<$> expressionAttributeNames,
            ("ConsistentRead" Lude..=) Lude.<$> consistentRead,
            ("ReturnConsumedCapacity" Lude..=) Lude.<$> returnConsumedCapacity,
            Lude.Just ("Key" Lude..= key),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath GetItem where
  toPath = Lude.const "/"

instance Lude.ToQuery GetItem where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetItem@ operation.
--
-- /See:/ 'mkGetItemResponse' smart constructor.
data GetItemResponse = GetItemResponse'
  { -- | The capacity units consumed by the @GetItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read/Write Capacity Mode> in the /Amazon DynamoDB Developer Guide/ .
    consumedCapacity :: Lude.Maybe ConsumedCapacity,
    -- | A map of attribute names to @AttributeValue@ objects, as specified by @ProjectionExpression@ .
    item :: Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetItemResponse' with the minimum fields required to make a request.
--
-- * 'consumedCapacity' - The capacity units consumed by the @GetItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read/Write Capacity Mode> in the /Amazon DynamoDB Developer Guide/ .
-- * 'item' - A map of attribute names to @AttributeValue@ objects, as specified by @ProjectionExpression@ .
-- * 'responseStatus' - The response status code.
mkGetItemResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetItemResponse
mkGetItemResponse pResponseStatus_ =
  GetItemResponse'
    { consumedCapacity = Lude.Nothing,
      item = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The capacity units consumed by the @GetItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Read/Write Capacity Mode> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsConsumedCapacity :: Lens.Lens' GetItemResponse (Lude.Maybe ConsumedCapacity)
girsConsumedCapacity = Lens.lens (consumedCapacity :: GetItemResponse -> Lude.Maybe ConsumedCapacity) (\s a -> s {consumedCapacity = a} :: GetItemResponse)
{-# DEPRECATED girsConsumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead." #-}

-- | A map of attribute names to @AttributeValue@ objects, as specified by @ProjectionExpression@ .
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsItem :: Lens.Lens' GetItemResponse (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
girsItem = Lens.lens (item :: GetItemResponse -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {item = a} :: GetItemResponse)
{-# DEPRECATED girsItem "Use generic-lens or generic-optics with 'item' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsResponseStatus :: Lens.Lens' GetItemResponse Lude.Int
girsResponseStatus = Lens.lens (responseStatus :: GetItemResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetItemResponse)
{-# DEPRECATED girsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
