{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.TransactGetItems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @TransactGetItems@ is a synchronous operation that atomically retrieves multiple items from one or more tables (but not from indexes) in a single account and Region. A @TransactGetItems@ call can contain up to 25 @TransactGetItem@ objects, each of which contains a @Get@ structure that specifies an item to retrieve from a table in the account and Region. A call to @TransactGetItems@ cannot retrieve items from tables in more than one AWS account or Region. The aggregate size of the items in the transaction cannot exceed 4 MB.
--
-- DynamoDB rejects the entire @TransactGetItems@ request if any of the following is true:
--
--     * A conflicting operation is in the process of updating an item to be read.
--
--
--     * There is insufficient provisioned capacity for the transaction to be completed.
--
--
--     * There is a user error, such as an invalid data format.
--
--
--     * The aggregate size of the items in the transaction cannot exceed 4 MB.
module Network.AWS.DynamoDB.TransactGetItems
  ( -- * Creating a request
    TransactGetItems (..),
    mkTransactGetItems,

    -- ** Request lenses
    tgiTransactItems,
    tgiReturnConsumedCapacity,

    -- * Destructuring the response
    TransactGetItemsResponse (..),
    mkTransactGetItemsResponse,

    -- ** Response lenses
    tgirsResponses,
    tgirsConsumedCapacity,
    tgirsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTransactGetItems' smart constructor.
data TransactGetItems = TransactGetItems'
  { -- | An ordered array of up to 25 @TransactGetItem@ objects, each of which contains a @Get@ structure.
    transactItems :: Lude.NonEmpty TransactGetItem,
    -- | A value of @TOTAL@ causes consumed capacity information to be returned, and a value of @NONE@ prevents that information from being returned. No other value is valid.
    returnConsumedCapacity :: Lude.Maybe ReturnConsumedCapacity
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransactGetItems' with the minimum fields required to make a request.
--
-- * 'transactItems' - An ordered array of up to 25 @TransactGetItem@ objects, each of which contains a @Get@ structure.
-- * 'returnConsumedCapacity' - A value of @TOTAL@ causes consumed capacity information to be returned, and a value of @NONE@ prevents that information from being returned. No other value is valid.
mkTransactGetItems ::
  -- | 'transactItems'
  Lude.NonEmpty TransactGetItem ->
  TransactGetItems
mkTransactGetItems pTransactItems_ =
  TransactGetItems'
    { transactItems = pTransactItems_,
      returnConsumedCapacity = Lude.Nothing
    }

-- | An ordered array of up to 25 @TransactGetItem@ objects, each of which contains a @Get@ structure.
--
-- /Note:/ Consider using 'transactItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgiTransactItems :: Lens.Lens' TransactGetItems (Lude.NonEmpty TransactGetItem)
tgiTransactItems = Lens.lens (transactItems :: TransactGetItems -> Lude.NonEmpty TransactGetItem) (\s a -> s {transactItems = a} :: TransactGetItems)
{-# DEPRECATED tgiTransactItems "Use generic-lens or generic-optics with 'transactItems' instead." #-}

-- | A value of @TOTAL@ causes consumed capacity information to be returned, and a value of @NONE@ prevents that information from being returned. No other value is valid.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgiReturnConsumedCapacity :: Lens.Lens' TransactGetItems (Lude.Maybe ReturnConsumedCapacity)
tgiReturnConsumedCapacity = Lens.lens (returnConsumedCapacity :: TransactGetItems -> Lude.Maybe ReturnConsumedCapacity) (\s a -> s {returnConsumedCapacity = a} :: TransactGetItems)
{-# DEPRECATED tgiReturnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead." #-}

instance Lude.AWSRequest TransactGetItems where
  type Rs TransactGetItems = TransactGetItemsResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          TransactGetItemsResponse'
            Lude.<$> (x Lude..?> "Responses")
            Lude.<*> (x Lude..?> "ConsumedCapacity" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TransactGetItems where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.TransactGetItems" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TransactGetItems where
  toJSON TransactGetItems' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TransactItems" Lude..= transactItems),
            ("ReturnConsumedCapacity" Lude..=)
              Lude.<$> returnConsumedCapacity
          ]
      )

instance Lude.ToPath TransactGetItems where
  toPath = Lude.const "/"

instance Lude.ToQuery TransactGetItems where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTransactGetItemsResponse' smart constructor.
data TransactGetItemsResponse = TransactGetItemsResponse'
  { -- | An ordered array of up to 25 @ItemResponse@ objects, each of which corresponds to the @TransactGetItem@ object in the same position in the /TransactItems/ array. Each @ItemResponse@ object contains a Map of the name-value pairs that are the projected attributes of the requested item.
    --
    -- If a requested item could not be retrieved, the corresponding @ItemResponse@ object is Null, or if the requested item has no projected attributes, the corresponding @ItemResponse@ object is an empty Map.
    responses :: Lude.Maybe (Lude.NonEmpty ItemResponse),
    -- | If the /ReturnConsumedCapacity/ value was @TOTAL@ , this is an array of @ConsumedCapacity@ objects, one for each table addressed by @TransactGetItem@ objects in the /TransactItems/ parameter. These @ConsumedCapacity@ objects report the read-capacity units consumed by the @TransactGetItems@ call in that table.
    consumedCapacity :: Lude.Maybe [ConsumedCapacity],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransactGetItemsResponse' with the minimum fields required to make a request.
--
-- * 'responses' - An ordered array of up to 25 @ItemResponse@ objects, each of which corresponds to the @TransactGetItem@ object in the same position in the /TransactItems/ array. Each @ItemResponse@ object contains a Map of the name-value pairs that are the projected attributes of the requested item.
--
-- If a requested item could not be retrieved, the corresponding @ItemResponse@ object is Null, or if the requested item has no projected attributes, the corresponding @ItemResponse@ object is an empty Map.
-- * 'consumedCapacity' - If the /ReturnConsumedCapacity/ value was @TOTAL@ , this is an array of @ConsumedCapacity@ objects, one for each table addressed by @TransactGetItem@ objects in the /TransactItems/ parameter. These @ConsumedCapacity@ objects report the read-capacity units consumed by the @TransactGetItems@ call in that table.
-- * 'responseStatus' - The response status code.
mkTransactGetItemsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TransactGetItemsResponse
mkTransactGetItemsResponse pResponseStatus_ =
  TransactGetItemsResponse'
    { responses = Lude.Nothing,
      consumedCapacity = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An ordered array of up to 25 @ItemResponse@ objects, each of which corresponds to the @TransactGetItem@ object in the same position in the /TransactItems/ array. Each @ItemResponse@ object contains a Map of the name-value pairs that are the projected attributes of the requested item.
--
-- If a requested item could not be retrieved, the corresponding @ItemResponse@ object is Null, or if the requested item has no projected attributes, the corresponding @ItemResponse@ object is an empty Map.
--
-- /Note:/ Consider using 'responses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgirsResponses :: Lens.Lens' TransactGetItemsResponse (Lude.Maybe (Lude.NonEmpty ItemResponse))
tgirsResponses = Lens.lens (responses :: TransactGetItemsResponse -> Lude.Maybe (Lude.NonEmpty ItemResponse)) (\s a -> s {responses = a} :: TransactGetItemsResponse)
{-# DEPRECATED tgirsResponses "Use generic-lens or generic-optics with 'responses' instead." #-}

-- | If the /ReturnConsumedCapacity/ value was @TOTAL@ , this is an array of @ConsumedCapacity@ objects, one for each table addressed by @TransactGetItem@ objects in the /TransactItems/ parameter. These @ConsumedCapacity@ objects report the read-capacity units consumed by the @TransactGetItems@ call in that table.
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgirsConsumedCapacity :: Lens.Lens' TransactGetItemsResponse (Lude.Maybe [ConsumedCapacity])
tgirsConsumedCapacity = Lens.lens (consumedCapacity :: TransactGetItemsResponse -> Lude.Maybe [ConsumedCapacity]) (\s a -> s {consumedCapacity = a} :: TransactGetItemsResponse)
{-# DEPRECATED tgirsConsumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgirsResponseStatus :: Lens.Lens' TransactGetItemsResponse Lude.Int
tgirsResponseStatus = Lens.lens (responseStatus :: TransactGetItemsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TransactGetItemsResponse)
{-# DEPRECATED tgirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
