{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.PutInventory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Bulk update custom inventory items on one more instance. The request adds an inventory item, if it doesn't already exist, or updates an inventory item, if it does exist.
module Network.AWS.SSM.PutInventory
  ( -- * Creating a request
    PutInventory (..),
    mkPutInventory,

    -- ** Request lenses
    piInstanceId,
    piItems,

    -- * Destructuring the response
    PutInventoryResponse (..),
    mkPutInventoryResponse,

    -- ** Response lenses
    pirrsMessage,
    pirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkPutInventory' smart constructor.
data PutInventory = PutInventory'
  { -- | An instance ID where you want to add or update inventory items.
    instanceId :: Types.InstanceId,
    -- | The inventory items that you want to add or update on instances.
    items :: Core.NonEmpty Types.InventoryItem
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutInventory' value with any optional fields omitted.
mkPutInventory ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'items'
  Core.NonEmpty Types.InventoryItem ->
  PutInventory
mkPutInventory instanceId items = PutInventory' {instanceId, items}

-- | An instance ID where you want to add or update inventory items.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piInstanceId :: Lens.Lens' PutInventory Types.InstanceId
piInstanceId = Lens.field @"instanceId"
{-# DEPRECATED piInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The inventory items that you want to add or update on instances.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piItems :: Lens.Lens' PutInventory (Core.NonEmpty Types.InventoryItem)
piItems = Lens.field @"items"
{-# DEPRECATED piItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.FromJSON PutInventory where
  toJSON PutInventory {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceId" Core..= instanceId),
            Core.Just ("Items" Core..= items)
          ]
      )

instance Core.AWSRequest PutInventory where
  type Rs PutInventory = PutInventoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.PutInventory")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutInventoryResponse'
            Core.<$> (x Core..:? "Message") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutInventoryResponse' smart constructor.
data PutInventoryResponse = PutInventoryResponse'
  { -- | Information about the request.
    message :: Core.Maybe Types.Message,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutInventoryResponse' value with any optional fields omitted.
mkPutInventoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutInventoryResponse
mkPutInventoryResponse responseStatus =
  PutInventoryResponse' {message = Core.Nothing, responseStatus}

-- | Information about the request.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsMessage :: Lens.Lens' PutInventoryResponse (Core.Maybe Types.Message)
pirrsMessage = Lens.field @"message"
{-# DEPRECATED pirrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrsResponseStatus :: Lens.Lens' PutInventoryResponse Core.Int
pirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
