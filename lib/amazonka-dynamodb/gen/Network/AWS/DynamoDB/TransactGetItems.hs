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
    tgirrsConsumedCapacity,
    tgirrsResponses,
    tgirrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTransactGetItems' smart constructor.
data TransactGetItems = TransactGetItems'
  { -- | An ordered array of up to 25 @TransactGetItem@ objects, each of which contains a @Get@ structure.
    transactItems :: Core.NonEmpty Types.TransactGetItem,
    -- | A value of @TOTAL@ causes consumed capacity information to be returned, and a value of @NONE@ prevents that information from being returned. No other value is valid.
    returnConsumedCapacity :: Core.Maybe Types.ReturnConsumedCapacity
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransactGetItems' value with any optional fields omitted.
mkTransactGetItems ::
  -- | 'transactItems'
  Core.NonEmpty Types.TransactGetItem ->
  TransactGetItems
mkTransactGetItems transactItems =
  TransactGetItems'
    { transactItems,
      returnConsumedCapacity = Core.Nothing
    }

-- | An ordered array of up to 25 @TransactGetItem@ objects, each of which contains a @Get@ structure.
--
-- /Note:/ Consider using 'transactItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgiTransactItems :: Lens.Lens' TransactGetItems (Core.NonEmpty Types.TransactGetItem)
tgiTransactItems = Lens.field @"transactItems"
{-# DEPRECATED tgiTransactItems "Use generic-lens or generic-optics with 'transactItems' instead." #-}

-- | A value of @TOTAL@ causes consumed capacity information to be returned, and a value of @NONE@ prevents that information from being returned. No other value is valid.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgiReturnConsumedCapacity :: Lens.Lens' TransactGetItems (Core.Maybe Types.ReturnConsumedCapacity)
tgiReturnConsumedCapacity = Lens.field @"returnConsumedCapacity"
{-# DEPRECATED tgiReturnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead." #-}

instance Core.FromJSON TransactGetItems where
  toJSON TransactGetItems {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TransactItems" Core..= transactItems),
            ("ReturnConsumedCapacity" Core..=)
              Core.<$> returnConsumedCapacity
          ]
      )

instance Core.AWSRequest TransactGetItems where
  type Rs TransactGetItems = TransactGetItemsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.TransactGetItems")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          TransactGetItemsResponse'
            Core.<$> (x Core..:? "ConsumedCapacity")
            Core.<*> (x Core..:? "Responses")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTransactGetItemsResponse' smart constructor.
data TransactGetItemsResponse = TransactGetItemsResponse'
  { -- | If the /ReturnConsumedCapacity/ value was @TOTAL@ , this is an array of @ConsumedCapacity@ objects, one for each table addressed by @TransactGetItem@ objects in the /TransactItems/ parameter. These @ConsumedCapacity@ objects report the read-capacity units consumed by the @TransactGetItems@ call in that table.
    consumedCapacity :: Core.Maybe [Types.ConsumedCapacity],
    -- | An ordered array of up to 25 @ItemResponse@ objects, each of which corresponds to the @TransactGetItem@ object in the same position in the /TransactItems/ array. Each @ItemResponse@ object contains a Map of the name-value pairs that are the projected attributes of the requested item.
    --
    -- If a requested item could not be retrieved, the corresponding @ItemResponse@ object is Null, or if the requested item has no projected attributes, the corresponding @ItemResponse@ object is an empty Map.
    responses :: Core.Maybe (Core.NonEmpty Types.ItemResponse),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransactGetItemsResponse' value with any optional fields omitted.
mkTransactGetItemsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TransactGetItemsResponse
mkTransactGetItemsResponse responseStatus =
  TransactGetItemsResponse'
    { consumedCapacity = Core.Nothing,
      responses = Core.Nothing,
      responseStatus
    }

-- | If the /ReturnConsumedCapacity/ value was @TOTAL@ , this is an array of @ConsumedCapacity@ objects, one for each table addressed by @TransactGetItem@ objects in the /TransactItems/ parameter. These @ConsumedCapacity@ objects report the read-capacity units consumed by the @TransactGetItems@ call in that table.
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgirrsConsumedCapacity :: Lens.Lens' TransactGetItemsResponse (Core.Maybe [Types.ConsumedCapacity])
tgirrsConsumedCapacity = Lens.field @"consumedCapacity"
{-# DEPRECATED tgirrsConsumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead." #-}

-- | An ordered array of up to 25 @ItemResponse@ objects, each of which corresponds to the @TransactGetItem@ object in the same position in the /TransactItems/ array. Each @ItemResponse@ object contains a Map of the name-value pairs that are the projected attributes of the requested item.
--
-- If a requested item could not be retrieved, the corresponding @ItemResponse@ object is Null, or if the requested item has no projected attributes, the corresponding @ItemResponse@ object is an empty Map.
--
-- /Note:/ Consider using 'responses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgirrsResponses :: Lens.Lens' TransactGetItemsResponse (Core.Maybe (Core.NonEmpty Types.ItemResponse))
tgirrsResponses = Lens.field @"responses"
{-# DEPRECATED tgirrsResponses "Use generic-lens or generic-optics with 'responses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgirrsResponseStatus :: Lens.Lens' TransactGetItemsResponse Core.Int
tgirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tgirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
