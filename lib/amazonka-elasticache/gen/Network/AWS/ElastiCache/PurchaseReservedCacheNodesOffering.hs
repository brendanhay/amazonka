{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to purchase a reserved cache node offering.
module Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
  ( -- * Creating a request
    PurchaseReservedCacheNodesOffering (..),
    mkPurchaseReservedCacheNodesOffering,

    -- ** Request lenses
    prcnoReservedCacheNodesOfferingId,
    prcnoCacheNodeCount,
    prcnoReservedCacheNodeId,

    -- * Destructuring the response
    PurchaseReservedCacheNodesOfferingResponse (..),
    mkPurchaseReservedCacheNodesOfferingResponse,

    -- ** Response lenses
    prcnorrsReservedCacheNode,
    prcnorrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PurchaseReservedCacheNodesOffering@ operation.
--
-- /See:/ 'mkPurchaseReservedCacheNodesOffering' smart constructor.
data PurchaseReservedCacheNodesOffering = PurchaseReservedCacheNodesOffering'
  { -- | The ID of the reserved cache node offering to purchase.
    --
    -- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
    reservedCacheNodesOfferingId :: Types.String,
    -- | The number of cache node instances to reserve.
    --
    -- Default: @1@
    cacheNodeCount :: Core.Maybe Core.Int,
    -- | A customer-specified identifier to track this reservation.
    --
    -- Example: myreservationID
    reservedCacheNodeId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseReservedCacheNodesOffering' value with any optional fields omitted.
mkPurchaseReservedCacheNodesOffering ::
  -- | 'reservedCacheNodesOfferingId'
  Types.String ->
  PurchaseReservedCacheNodesOffering
mkPurchaseReservedCacheNodesOffering reservedCacheNodesOfferingId =
  PurchaseReservedCacheNodesOffering'
    { reservedCacheNodesOfferingId,
      cacheNodeCount = Core.Nothing,
      reservedCacheNodeId = Core.Nothing
    }

-- | The ID of the reserved cache node offering to purchase.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
--
-- /Note:/ Consider using 'reservedCacheNodesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcnoReservedCacheNodesOfferingId :: Lens.Lens' PurchaseReservedCacheNodesOffering Types.String
prcnoReservedCacheNodesOfferingId = Lens.field @"reservedCacheNodesOfferingId"
{-# DEPRECATED prcnoReservedCacheNodesOfferingId "Use generic-lens or generic-optics with 'reservedCacheNodesOfferingId' instead." #-}

-- | The number of cache node instances to reserve.
--
-- Default: @1@
--
-- /Note:/ Consider using 'cacheNodeCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcnoCacheNodeCount :: Lens.Lens' PurchaseReservedCacheNodesOffering (Core.Maybe Core.Int)
prcnoCacheNodeCount = Lens.field @"cacheNodeCount"
{-# DEPRECATED prcnoCacheNodeCount "Use generic-lens or generic-optics with 'cacheNodeCount' instead." #-}

-- | A customer-specified identifier to track this reservation.
--
-- Example: myreservationID
--
-- /Note:/ Consider using 'reservedCacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcnoReservedCacheNodeId :: Lens.Lens' PurchaseReservedCacheNodesOffering (Core.Maybe Types.String)
prcnoReservedCacheNodeId = Lens.field @"reservedCacheNodeId"
{-# DEPRECATED prcnoReservedCacheNodeId "Use generic-lens or generic-optics with 'reservedCacheNodeId' instead." #-}

instance Core.AWSRequest PurchaseReservedCacheNodesOffering where
  type
    Rs PurchaseReservedCacheNodesOffering =
      PurchaseReservedCacheNodesOfferingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "PurchaseReservedCacheNodesOffering")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue
                            "ReservedCacheNodesOfferingId"
                            reservedCacheNodesOfferingId
                        )
                Core.<> (Core.toQueryValue "CacheNodeCount" Core.<$> cacheNodeCount)
                Core.<> ( Core.toQueryValue "ReservedCacheNodeId"
                            Core.<$> reservedCacheNodeId
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "PurchaseReservedCacheNodesOfferingResult"
      ( \s h x ->
          PurchaseReservedCacheNodesOfferingResponse'
            Core.<$> (x Core..@? "ReservedCacheNode")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPurchaseReservedCacheNodesOfferingResponse' smart constructor.
data PurchaseReservedCacheNodesOfferingResponse = PurchaseReservedCacheNodesOfferingResponse'
  { reservedCacheNode :: Core.Maybe Types.ReservedCacheNode,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PurchaseReservedCacheNodesOfferingResponse' value with any optional fields omitted.
mkPurchaseReservedCacheNodesOfferingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PurchaseReservedCacheNodesOfferingResponse
mkPurchaseReservedCacheNodesOfferingResponse responseStatus =
  PurchaseReservedCacheNodesOfferingResponse'
    { reservedCacheNode =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'reservedCacheNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcnorrsReservedCacheNode :: Lens.Lens' PurchaseReservedCacheNodesOfferingResponse (Core.Maybe Types.ReservedCacheNode)
prcnorrsReservedCacheNode = Lens.field @"reservedCacheNode"
{-# DEPRECATED prcnorrsReservedCacheNode "Use generic-lens or generic-optics with 'reservedCacheNode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcnorrsResponseStatus :: Lens.Lens' PurchaseReservedCacheNodesOfferingResponse Core.Int
prcnorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED prcnorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
