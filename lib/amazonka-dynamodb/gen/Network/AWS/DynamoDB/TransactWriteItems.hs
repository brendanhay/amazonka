{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.TransactWriteItems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @TransactWriteItems@ is a synchronous write operation that groups up to 25 action requests. These actions can target items in different tables, but not in different AWS accounts or Regions, and no two actions can target the same item. For example, you cannot both @ConditionCheck@ and @Update@ the same item. The aggregate size of the items in the transaction cannot exceed 4 MB.
--
-- The actions are completed atomically so that either all of them succeed, or all of them fail. They are defined by the following objects:
--
--     * @Put@    Initiates a @PutItem@ operation to write a new item. This structure specifies the primary key of the item to be written, the name of the table to write it in, an optional condition expression that must be satisfied for the write to succeed, a list of the item's attributes, and a field indicating whether to retrieve the item's attributes if the condition is not met.
--
--
--     * @Update@    Initiates an @UpdateItem@ operation to update an existing item. This structure specifies the primary key of the item to be updated, the name of the table where it resides, an optional condition expression that must be satisfied for the update to succeed, an expression that defines one or more attributes to be updated, and a field indicating whether to retrieve the item's attributes if the condition is not met.
--
--
--     * @Delete@    Initiates a @DeleteItem@ operation to delete an existing item. This structure specifies the primary key of the item to be deleted, the name of the table where it resides, an optional condition expression that must be satisfied for the deletion to succeed, and a field indicating whether to retrieve the item's attributes if the condition is not met.
--
--
--     * @ConditionCheck@    Applies a condition to an item that is not being modified by the transaction. This structure specifies the primary key of the item to be checked, the name of the table where it resides, a condition expression that must be satisfied for the transaction to succeed, and a field indicating whether to retrieve the item's attributes if the condition is not met.
--
--
-- DynamoDB rejects the entire @TransactWriteItems@ request if any of the following is true:
--
--     * A condition in one of the condition expressions is not met.
--
--
--     * An ongoing operation is in the process of updating the same item.
--
--
--     * There is insufficient provisioned capacity for the transaction to be completed.
--
--
--     * An item size becomes too large (bigger than 400 KB), a local secondary index (LSI) becomes too large, or a similar validation error occurs because of changes made by the transaction.
--
--
--     * The aggregate size of the items in the transaction exceeds 4 MB.
--
--
--     * There is a user error, such as an invalid data format.
module Network.AWS.DynamoDB.TransactWriteItems
  ( -- * Creating a request
    TransactWriteItems (..),
    mkTransactWriteItems,

    -- ** Request lenses
    twiTransactItems,
    twiReturnConsumedCapacity,
    twiReturnItemCollectionMetrics,
    twiClientRequestToken,

    -- * Destructuring the response
    TransactWriteItemsResponse (..),
    mkTransactWriteItemsResponse,

    -- ** Response lenses
    twirsItemCollectionMetrics,
    twirsConsumedCapacity,
    twirsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTransactWriteItems' smart constructor.
data TransactWriteItems = TransactWriteItems'
  { -- | An ordered array of up to 25 @TransactWriteItem@ objects, each of which contains a @ConditionCheck@ , @Put@ , @Update@ , or @Delete@ object. These can operate on items in different tables, but the tables must reside in the same AWS account and Region, and no two of them can operate on the same item.
    transactItems :: Lude.NonEmpty TransactWriteItem,
    returnConsumedCapacity :: Lude.Maybe ReturnConsumedCapacity,
    -- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections (if any), that were modified during the operation and are returned in the response. If set to @NONE@ (the default), no statistics are returned.
    returnItemCollectionMetrics :: Lude.Maybe ReturnItemCollectionMetrics,
    -- | Providing a @ClientRequestToken@ makes the call to @TransactWriteItems@ idempotent, meaning that multiple identical calls have the same effect as one single call.
    --
    -- Although multiple identical calls using the same client request token produce the same result on the server (no side effects), the responses to the calls might not be the same. If the @ReturnConsumedCapacity>@ parameter is set, then the initial @TransactWriteItems@ call returns the amount of write capacity units consumed in making the changes. Subsequent @TransactWriteItems@ calls with the same client token return the number of read capacity units consumed in reading the item.
    -- A client request token is valid for 10 minutes after the first request that uses it is completed. After 10 minutes, any request with the same client token is treated as a new request. Do not resubmit the same request with the same client token for more than 10 minutes, or the result might not be idempotent.
    -- If you submit a request with the same client token but a change in other parameters within the 10-minute idempotency window, DynamoDB returns an @IdempotentParameterMismatch@ exception.
    clientRequestToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransactWriteItems' with the minimum fields required to make a request.
--
-- * 'transactItems' - An ordered array of up to 25 @TransactWriteItem@ objects, each of which contains a @ConditionCheck@ , @Put@ , @Update@ , or @Delete@ object. These can operate on items in different tables, but the tables must reside in the same AWS account and Region, and no two of them can operate on the same item.
-- * 'returnConsumedCapacity' -
-- * 'returnItemCollectionMetrics' - Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections (if any), that were modified during the operation and are returned in the response. If set to @NONE@ (the default), no statistics are returned.
-- * 'clientRequestToken' - Providing a @ClientRequestToken@ makes the call to @TransactWriteItems@ idempotent, meaning that multiple identical calls have the same effect as one single call.
--
-- Although multiple identical calls using the same client request token produce the same result on the server (no side effects), the responses to the calls might not be the same. If the @ReturnConsumedCapacity>@ parameter is set, then the initial @TransactWriteItems@ call returns the amount of write capacity units consumed in making the changes. Subsequent @TransactWriteItems@ calls with the same client token return the number of read capacity units consumed in reading the item.
-- A client request token is valid for 10 minutes after the first request that uses it is completed. After 10 minutes, any request with the same client token is treated as a new request. Do not resubmit the same request with the same client token for more than 10 minutes, or the result might not be idempotent.
-- If you submit a request with the same client token but a change in other parameters within the 10-minute idempotency window, DynamoDB returns an @IdempotentParameterMismatch@ exception.
mkTransactWriteItems ::
  -- | 'transactItems'
  Lude.NonEmpty TransactWriteItem ->
  TransactWriteItems
mkTransactWriteItems pTransactItems_ =
  TransactWriteItems'
    { transactItems = pTransactItems_,
      returnConsumedCapacity = Lude.Nothing,
      returnItemCollectionMetrics = Lude.Nothing,
      clientRequestToken = Lude.Nothing
    }

-- | An ordered array of up to 25 @TransactWriteItem@ objects, each of which contains a @ConditionCheck@ , @Put@ , @Update@ , or @Delete@ object. These can operate on items in different tables, but the tables must reside in the same AWS account and Region, and no two of them can operate on the same item.
--
-- /Note:/ Consider using 'transactItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiTransactItems :: Lens.Lens' TransactWriteItems (Lude.NonEmpty TransactWriteItem)
twiTransactItems = Lens.lens (transactItems :: TransactWriteItems -> Lude.NonEmpty TransactWriteItem) (\s a -> s {transactItems = a} :: TransactWriteItems)
{-# DEPRECATED twiTransactItems "Use generic-lens or generic-optics with 'transactItems' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiReturnConsumedCapacity :: Lens.Lens' TransactWriteItems (Lude.Maybe ReturnConsumedCapacity)
twiReturnConsumedCapacity = Lens.lens (returnConsumedCapacity :: TransactWriteItems -> Lude.Maybe ReturnConsumedCapacity) (\s a -> s {returnConsumedCapacity = a} :: TransactWriteItems)
{-# DEPRECATED twiReturnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead." #-}

-- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections (if any), that were modified during the operation and are returned in the response. If set to @NONE@ (the default), no statistics are returned.
--
-- /Note:/ Consider using 'returnItemCollectionMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiReturnItemCollectionMetrics :: Lens.Lens' TransactWriteItems (Lude.Maybe ReturnItemCollectionMetrics)
twiReturnItemCollectionMetrics = Lens.lens (returnItemCollectionMetrics :: TransactWriteItems -> Lude.Maybe ReturnItemCollectionMetrics) (\s a -> s {returnItemCollectionMetrics = a} :: TransactWriteItems)
{-# DEPRECATED twiReturnItemCollectionMetrics "Use generic-lens or generic-optics with 'returnItemCollectionMetrics' instead." #-}

-- | Providing a @ClientRequestToken@ makes the call to @TransactWriteItems@ idempotent, meaning that multiple identical calls have the same effect as one single call.
--
-- Although multiple identical calls using the same client request token produce the same result on the server (no side effects), the responses to the calls might not be the same. If the @ReturnConsumedCapacity>@ parameter is set, then the initial @TransactWriteItems@ call returns the amount of write capacity units consumed in making the changes. Subsequent @TransactWriteItems@ calls with the same client token return the number of read capacity units consumed in reading the item.
-- A client request token is valid for 10 minutes after the first request that uses it is completed. After 10 minutes, any request with the same client token is treated as a new request. Do not resubmit the same request with the same client token for more than 10 minutes, or the result might not be idempotent.
-- If you submit a request with the same client token but a change in other parameters within the 10-minute idempotency window, DynamoDB returns an @IdempotentParameterMismatch@ exception.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiClientRequestToken :: Lens.Lens' TransactWriteItems (Lude.Maybe Lude.Text)
twiClientRequestToken = Lens.lens (clientRequestToken :: TransactWriteItems -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: TransactWriteItems)
{-# DEPRECATED twiClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest TransactWriteItems where
  type Rs TransactWriteItems = TransactWriteItemsResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          TransactWriteItemsResponse'
            Lude.<$> (x Lude..?> "ItemCollectionMetrics" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ConsumedCapacity" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TransactWriteItems where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.TransactWriteItems" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TransactWriteItems where
  toJSON TransactWriteItems' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TransactItems" Lude..= transactItems),
            ("ReturnConsumedCapacity" Lude..=) Lude.<$> returnConsumedCapacity,
            ("ReturnItemCollectionMetrics" Lude..=)
              Lude.<$> returnItemCollectionMetrics,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken
          ]
      )

instance Lude.ToPath TransactWriteItems where
  toPath = Lude.const "/"

instance Lude.ToQuery TransactWriteItems where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTransactWriteItemsResponse' smart constructor.
data TransactWriteItemsResponse = TransactWriteItemsResponse'
  { -- | A list of tables that were processed by @TransactWriteItems@ and, for each table, information about any item collections that were affected by individual @UpdateItem@ , @PutItem@ , or @DeleteItem@ operations.
    itemCollectionMetrics :: Lude.Maybe (Lude.HashMap Lude.Text ([ItemCollectionMetrics])),
    -- | The capacity units consumed by the entire @TransactWriteItems@ operation. The values of the list are ordered according to the ordering of the @TransactItems@ request parameter.
    consumedCapacity :: Lude.Maybe [ConsumedCapacity],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransactWriteItemsResponse' with the minimum fields required to make a request.
--
-- * 'itemCollectionMetrics' - A list of tables that were processed by @TransactWriteItems@ and, for each table, information about any item collections that were affected by individual @UpdateItem@ , @PutItem@ , or @DeleteItem@ operations.
-- * 'consumedCapacity' - The capacity units consumed by the entire @TransactWriteItems@ operation. The values of the list are ordered according to the ordering of the @TransactItems@ request parameter.
-- * 'responseStatus' - The response status code.
mkTransactWriteItemsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TransactWriteItemsResponse
mkTransactWriteItemsResponse pResponseStatus_ =
  TransactWriteItemsResponse'
    { itemCollectionMetrics = Lude.Nothing,
      consumedCapacity = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of tables that were processed by @TransactWriteItems@ and, for each table, information about any item collections that were affected by individual @UpdateItem@ , @PutItem@ , or @DeleteItem@ operations.
--
-- /Note:/ Consider using 'itemCollectionMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twirsItemCollectionMetrics :: Lens.Lens' TransactWriteItemsResponse (Lude.Maybe (Lude.HashMap Lude.Text ([ItemCollectionMetrics])))
twirsItemCollectionMetrics = Lens.lens (itemCollectionMetrics :: TransactWriteItemsResponse -> Lude.Maybe (Lude.HashMap Lude.Text ([ItemCollectionMetrics]))) (\s a -> s {itemCollectionMetrics = a} :: TransactWriteItemsResponse)
{-# DEPRECATED twirsItemCollectionMetrics "Use generic-lens or generic-optics with 'itemCollectionMetrics' instead." #-}

-- | The capacity units consumed by the entire @TransactWriteItems@ operation. The values of the list are ordered according to the ordering of the @TransactItems@ request parameter.
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twirsConsumedCapacity :: Lens.Lens' TransactWriteItemsResponse (Lude.Maybe [ConsumedCapacity])
twirsConsumedCapacity = Lens.lens (consumedCapacity :: TransactWriteItemsResponse -> Lude.Maybe [ConsumedCapacity]) (\s a -> s {consumedCapacity = a} :: TransactWriteItemsResponse)
{-# DEPRECATED twirsConsumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twirsResponseStatus :: Lens.Lens' TransactWriteItemsResponse Lude.Int
twirsResponseStatus = Lens.lens (responseStatus :: TransactWriteItemsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TransactWriteItemsResponse)
{-# DEPRECATED twirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
