{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.BatchGetItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @BatchGetItem@ operation returns the attributes of one or more items from one or more tables. You identify requested items by primary key.
--
-- A single operation can retrieve up to 16 MB of data, which can contain as many as 100 items. @BatchGetItem@ returns a partial result if the response size limit is exceeded, the table's provisioned throughput is exceeded, or an internal processing failure occurs. If a partial result is returned, the operation returns a value for @UnprocessedKeys@ . You can use this value to retry the operation starting with the next item to get.
-- /Important:/ If you request more than 100 items, @BatchGetItem@ returns a @ValidationException@ with the message "Too many items requested for the BatchGetItem call."
-- For example, if you ask to retrieve 100 items, but each individual item is 300 KB in size, the system returns 52 items (so as not to exceed the 16 MB limit). It also returns an appropriate @UnprocessedKeys@ value so you can get the next page of results. If desired, your application can include its own logic to assemble the pages of results into one dataset.
-- If /none/ of the items can be processed due to insufficient provisioned throughput on all of the tables in the request, then @BatchGetItem@ returns a @ProvisionedThroughputExceededException@ . If /at least one/ of the items is successfully processed, then @BatchGetItem@ completes successfully, while returning the keys of the unread items in @UnprocessedKeys@ .
-- /Important:/ If DynamoDB returns any unprocessed items, you should retry the batch operation on those items. However, /we strongly recommend that you use an exponential backoff algorithm/ . If you retry the batch operation immediately, the underlying read or write requests can still fail due to throttling on the individual tables. If you delay the batch operation using exponential backoff, the individual requests in the batch are much more likely to succeed.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#BatchOperations Batch Operations and Error Handling> in the /Amazon DynamoDB Developer Guide/ .
-- By default, @BatchGetItem@ performs eventually consistent reads on every table in the request. If you want strongly consistent reads instead, you can set @ConsistentRead@ to @true@ for any or all tables.
-- In order to minimize response latency, @BatchGetItem@ retrieves items in parallel.
-- When designing your application, keep in mind that DynamoDB does not return items in any particular order. To help parse the response by item, include the primary key values for the items in your request in the @ProjectionExpression@ parameter.
-- If a requested item does not exist, it is not returned in the result. Requests for nonexistent items consume the minimum read capacity units according to the type of read. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#CapacityUnitCalculations Working with Tables> in the /Amazon DynamoDB Developer Guide/ .
module Network.AWS.DynamoDB.BatchGetItem
  ( -- * Creating a request
    BatchGetItem (..),
    mkBatchGetItem,

    -- ** Request lenses
    bgiReturnConsumedCapacity,
    bgiRequestItems,

    -- * Destructuring the response
    BatchGetItemResponse (..),
    mkBatchGetItemResponse,

    -- ** Response lenses
    bgirsUnprocessedKeys,
    bgirsResponses,
    bgirsConsumedCapacity,
    bgirsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @BatchGetItem@ operation.
--
-- /See:/ 'mkBatchGetItem' smart constructor.
data BatchGetItem = BatchGetItem'
  { returnConsumedCapacity :: Lude.Maybe ReturnConsumedCapacity,
    -- | A map of one or more table names and, for each table, a map that describes one or more items to retrieve from that table. Each table name can be used only once per @BatchGetItem@ request.
    --
    -- Each element in the map of items to retrieve consists of the following:
    --
    --     * @ConsistentRead@ - If @true@ , a strongly consistent read is used; if @false@ (the default), an eventually consistent read is used.
    --
    --
    --     * @ExpressionAttributeNames@ - One or more substitution tokens for attribute names in the @ProjectionExpression@ parameter. The following are some use cases for using @ExpressionAttributeNames@ :
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
    -- For more information about expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
    --
    --
    --     * @Keys@ - An array of primary key attribute values that define specific items in the table. For each primary key, you must provide /all/ of the key attributes. For example, with a simple primary key, you only need to provide the partition key value. For a composite key, you must provide /both/ the partition key value and the sort key value.
    --
    --
    --     * @ProjectionExpression@ - A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.
    -- If no attribute names are specified, then all attributes are returned. If any of the requested attributes are not found, they do not appear in the result.
    -- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
    --
    --
    --     * @AttributesToGet@ - This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
    requestItems :: Lude.HashMap Lude.Text (KeysAndAttributes)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetItem' with the minimum fields required to make a request.
--
-- * 'returnConsumedCapacity' -
-- * 'requestItems' - A map of one or more table names and, for each table, a map that describes one or more items to retrieve from that table. Each table name can be used only once per @BatchGetItem@ request.
--
-- Each element in the map of items to retrieve consists of the following:
--
--     * @ConsistentRead@ - If @true@ , a strongly consistent read is used; if @false@ (the default), an eventually consistent read is used.
--
--
--     * @ExpressionAttributeNames@ - One or more substitution tokens for attribute names in the @ProjectionExpression@ parameter. The following are some use cases for using @ExpressionAttributeNames@ :
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
-- For more information about expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
--
--     * @Keys@ - An array of primary key attribute values that define specific items in the table. For each primary key, you must provide /all/ of the key attributes. For example, with a simple primary key, you only need to provide the partition key value. For a composite key, you must provide /both/ the partition key value and the sort key value.
--
--
--     * @ProjectionExpression@ - A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.
-- If no attribute names are specified, then all attributes are returned. If any of the requested attributes are not found, they do not appear in the result.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
--
--     * @AttributesToGet@ - This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
mkBatchGetItem ::
  BatchGetItem
mkBatchGetItem =
  BatchGetItem'
    { returnConsumedCapacity = Lude.Nothing,
      requestItems = Lude.mempty
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgiReturnConsumedCapacity :: Lens.Lens' BatchGetItem (Lude.Maybe ReturnConsumedCapacity)
bgiReturnConsumedCapacity = Lens.lens (returnConsumedCapacity :: BatchGetItem -> Lude.Maybe ReturnConsumedCapacity) (\s a -> s {returnConsumedCapacity = a} :: BatchGetItem)
{-# DEPRECATED bgiReturnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead." #-}

-- | A map of one or more table names and, for each table, a map that describes one or more items to retrieve from that table. Each table name can be used only once per @BatchGetItem@ request.
--
-- Each element in the map of items to retrieve consists of the following:
--
--     * @ConsistentRead@ - If @true@ , a strongly consistent read is used; if @false@ (the default), an eventually consistent read is used.
--
--
--     * @ExpressionAttributeNames@ - One or more substitution tokens for attribute names in the @ProjectionExpression@ parameter. The following are some use cases for using @ExpressionAttributeNames@ :
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
-- For more information about expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
--
--     * @Keys@ - An array of primary key attribute values that define specific items in the table. For each primary key, you must provide /all/ of the key attributes. For example, with a simple primary key, you only need to provide the partition key value. For a composite key, you must provide /both/ the partition key value and the sort key value.
--
--
--     * @ProjectionExpression@ - A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.
-- If no attribute names are specified, then all attributes are returned. If any of the requested attributes are not found, they do not appear in the result.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
--
--     * @AttributesToGet@ - This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
--
--
--
-- /Note:/ Consider using 'requestItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgiRequestItems :: Lens.Lens' BatchGetItem (Lude.HashMap Lude.Text (KeysAndAttributes))
bgiRequestItems = Lens.lens (requestItems :: BatchGetItem -> Lude.HashMap Lude.Text (KeysAndAttributes)) (\s a -> s {requestItems = a} :: BatchGetItem)
{-# DEPRECATED bgiRequestItems "Use generic-lens or generic-optics with 'requestItems' instead." #-}

instance Lude.AWSRequest BatchGetItem where
  type Rs BatchGetItem = BatchGetItemResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetItemResponse'
            Lude.<$> (x Lude..?> "UnprocessedKeys" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Responses" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ConsumedCapacity" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetItem where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.BatchGetItem" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetItem where
  toJSON BatchGetItem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ReturnConsumedCapacity" Lude..=)
              Lude.<$> returnConsumedCapacity,
            Lude.Just ("RequestItems" Lude..= requestItems)
          ]
      )

instance Lude.ToPath BatchGetItem where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetItem where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @BatchGetItem@ operation.
--
-- /See:/ 'mkBatchGetItemResponse' smart constructor.
data BatchGetItemResponse = BatchGetItemResponse'
  { -- | A map of tables and their respective keys that were not processed with the current response. The @UnprocessedKeys@ value is in the same form as @RequestItems@ , so the value can be provided directly to a subsequent @BatchGetItem@ operation. For more information, see @RequestItems@ in the Request Parameters section.
    --
    -- Each element consists of:
    --
    --     * @Keys@ - An array of primary key attribute values that define specific items in the table.
    --
    --
    --     * @ProjectionExpression@ - One or more attributes to be retrieved from the table or index. By default, all attributes are returned. If a requested attribute is not found, it does not appear in the result.
    --
    --
    --     * @ConsistentRead@ - The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
    --
    --
    -- If there are no unprocessed keys remaining, the response contains an empty @UnprocessedKeys@ map.
    unprocessedKeys :: Lude.Maybe (Lude.HashMap Lude.Text (KeysAndAttributes)),
    -- | A map of table name to a list of items. Each object in @Responses@ consists of a table name, along with a map of attribute data consisting of the data type and attribute value.
    responses :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.HashMap Lude.Text (AttributeValue)])),
    -- | The read capacity units consumed by the entire @BatchGetItem@ operation.
    --
    -- Each element consists of:
    --
    --     * @TableName@ - The table that consumed the provisioned throughput.
    --
    --
    --     * @CapacityUnits@ - The total number of capacity units consumed.
    consumedCapacity :: Lude.Maybe [ConsumedCapacity],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetItemResponse' with the minimum fields required to make a request.
--
-- * 'unprocessedKeys' - A map of tables and their respective keys that were not processed with the current response. The @UnprocessedKeys@ value is in the same form as @RequestItems@ , so the value can be provided directly to a subsequent @BatchGetItem@ operation. For more information, see @RequestItems@ in the Request Parameters section.
--
-- Each element consists of:
--
--     * @Keys@ - An array of primary key attribute values that define specific items in the table.
--
--
--     * @ProjectionExpression@ - One or more attributes to be retrieved from the table or index. By default, all attributes are returned. If a requested attribute is not found, it does not appear in the result.
--
--
--     * @ConsistentRead@ - The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
--
--
-- If there are no unprocessed keys remaining, the response contains an empty @UnprocessedKeys@ map.
-- * 'responses' - A map of table name to a list of items. Each object in @Responses@ consists of a table name, along with a map of attribute data consisting of the data type and attribute value.
-- * 'consumedCapacity' - The read capacity units consumed by the entire @BatchGetItem@ operation.
--
-- Each element consists of:
--
--     * @TableName@ - The table that consumed the provisioned throughput.
--
--
--     * @CapacityUnits@ - The total number of capacity units consumed.
--
--
-- * 'responseStatus' - The response status code.
mkBatchGetItemResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetItemResponse
mkBatchGetItemResponse pResponseStatus_ =
  BatchGetItemResponse'
    { unprocessedKeys = Lude.Nothing,
      responses = Lude.Nothing,
      consumedCapacity = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A map of tables and their respective keys that were not processed with the current response. The @UnprocessedKeys@ value is in the same form as @RequestItems@ , so the value can be provided directly to a subsequent @BatchGetItem@ operation. For more information, see @RequestItems@ in the Request Parameters section.
--
-- Each element consists of:
--
--     * @Keys@ - An array of primary key attribute values that define specific items in the table.
--
--
--     * @ProjectionExpression@ - One or more attributes to be retrieved from the table or index. By default, all attributes are returned. If a requested attribute is not found, it does not appear in the result.
--
--
--     * @ConsistentRead@ - The consistency of a read operation. If set to @true@ , then a strongly consistent read is used; otherwise, an eventually consistent read is used.
--
--
-- If there are no unprocessed keys remaining, the response contains an empty @UnprocessedKeys@ map.
--
-- /Note:/ Consider using 'unprocessedKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgirsUnprocessedKeys :: Lens.Lens' BatchGetItemResponse (Lude.Maybe (Lude.HashMap Lude.Text (KeysAndAttributes)))
bgirsUnprocessedKeys = Lens.lens (unprocessedKeys :: BatchGetItemResponse -> Lude.Maybe (Lude.HashMap Lude.Text (KeysAndAttributes))) (\s a -> s {unprocessedKeys = a} :: BatchGetItemResponse)
{-# DEPRECATED bgirsUnprocessedKeys "Use generic-lens or generic-optics with 'unprocessedKeys' instead." #-}

-- | A map of table name to a list of items. Each object in @Responses@ consists of a table name, along with a map of attribute data consisting of the data type and attribute value.
--
-- /Note:/ Consider using 'responses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgirsResponses :: Lens.Lens' BatchGetItemResponse (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.HashMap Lude.Text (AttributeValue)])))
bgirsResponses = Lens.lens (responses :: BatchGetItemResponse -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.HashMap Lude.Text (AttributeValue)]))) (\s a -> s {responses = a} :: BatchGetItemResponse)
{-# DEPRECATED bgirsResponses "Use generic-lens or generic-optics with 'responses' instead." #-}

-- | The read capacity units consumed by the entire @BatchGetItem@ operation.
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
bgirsConsumedCapacity :: Lens.Lens' BatchGetItemResponse (Lude.Maybe [ConsumedCapacity])
bgirsConsumedCapacity = Lens.lens (consumedCapacity :: BatchGetItemResponse -> Lude.Maybe [ConsumedCapacity]) (\s a -> s {consumedCapacity = a} :: BatchGetItemResponse)
{-# DEPRECATED bgirsConsumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgirsResponseStatus :: Lens.Lens' BatchGetItemResponse Lude.Int
bgirsResponseStatus = Lens.lens (responseStatus :: BatchGetItemResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetItemResponse)
{-# DEPRECATED bgirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
