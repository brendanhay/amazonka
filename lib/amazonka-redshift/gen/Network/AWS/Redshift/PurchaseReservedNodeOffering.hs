{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.PurchaseReservedNodeOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to purchase reserved nodes. Amazon Redshift offers a predefined set of reserved node offerings. You can purchase one or more of the offerings. You can call the 'DescribeReservedNodeOfferings' API to obtain the available reserved node offerings. You can call this API by providing a specific reserved node offering and the number of nodes you want to reserve.
--
-- For more information about reserved node offerings, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html Purchasing Reserved Nodes> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.PurchaseReservedNodeOffering
  ( -- * Creating a request
    PurchaseReservedNodeOffering (..),
    mkPurchaseReservedNodeOffering,

    -- ** Request lenses
    prnoReservedNodeOfferingId,
    prnoNodeCount,

    -- * Destructuring the response
    PurchaseReservedNodeOfferingResponse (..),
    mkPurchaseReservedNodeOfferingResponse,

    -- ** Response lenses
    prnorrsReservedNode,
    prnorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkPurchaseReservedNodeOffering' smart constructor.
data PurchaseReservedNodeOffering = PurchaseReservedNodeOffering'
  { -- | The unique identifier of the reserved node offering you want to purchase.
    reservedNodeOfferingId :: Types.ReservedNodeOfferingId,
    -- | The number of reserved nodes that you want to purchase.
    --
    -- Default: @1@
    nodeCount :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseReservedNodeOffering' value with any optional fields omitted.
mkPurchaseReservedNodeOffering ::
  -- | 'reservedNodeOfferingId'
  Types.ReservedNodeOfferingId ->
  PurchaseReservedNodeOffering
mkPurchaseReservedNodeOffering reservedNodeOfferingId =
  PurchaseReservedNodeOffering'
    { reservedNodeOfferingId,
      nodeCount = Core.Nothing
    }

-- | The unique identifier of the reserved node offering you want to purchase.
--
-- /Note:/ Consider using 'reservedNodeOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prnoReservedNodeOfferingId :: Lens.Lens' PurchaseReservedNodeOffering Types.ReservedNodeOfferingId
prnoReservedNodeOfferingId = Lens.field @"reservedNodeOfferingId"
{-# DEPRECATED prnoReservedNodeOfferingId "Use generic-lens or generic-optics with 'reservedNodeOfferingId' instead." #-}

-- | The number of reserved nodes that you want to purchase.
--
-- Default: @1@
--
-- /Note:/ Consider using 'nodeCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prnoNodeCount :: Lens.Lens' PurchaseReservedNodeOffering (Core.Maybe Core.Int)
prnoNodeCount = Lens.field @"nodeCount"
{-# DEPRECATED prnoNodeCount "Use generic-lens or generic-optics with 'nodeCount' instead." #-}

instance Core.AWSRequest PurchaseReservedNodeOffering where
  type
    Rs PurchaseReservedNodeOffering =
      PurchaseReservedNodeOfferingResponse
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
            ( Core.pure ("Action", "PurchaseReservedNodeOffering")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ReservedNodeOfferingId" reservedNodeOfferingId)
                Core.<> (Core.toQueryValue "NodeCount" Core.<$> nodeCount)
            )
      }
  response =
    Response.receiveXMLWrapper
      "PurchaseReservedNodeOfferingResult"
      ( \s h x ->
          PurchaseReservedNodeOfferingResponse'
            Core.<$> (x Core..@? "ReservedNode") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPurchaseReservedNodeOfferingResponse' smart constructor.
data PurchaseReservedNodeOfferingResponse = PurchaseReservedNodeOfferingResponse'
  { reservedNode :: Core.Maybe Types.ReservedNode,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PurchaseReservedNodeOfferingResponse' value with any optional fields omitted.
mkPurchaseReservedNodeOfferingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PurchaseReservedNodeOfferingResponse
mkPurchaseReservedNodeOfferingResponse responseStatus =
  PurchaseReservedNodeOfferingResponse'
    { reservedNode =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'reservedNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prnorrsReservedNode :: Lens.Lens' PurchaseReservedNodeOfferingResponse (Core.Maybe Types.ReservedNode)
prnorrsReservedNode = Lens.field @"reservedNode"
{-# DEPRECATED prnorrsReservedNode "Use generic-lens or generic-optics with 'reservedNode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prnorrsResponseStatus :: Lens.Lens' PurchaseReservedNodeOfferingResponse Core.Int
prnorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED prnorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
