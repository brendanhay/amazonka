{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a reference (route) to a prefix list in a specified transit gateway route table.
module Network.AWS.EC2.DeleteTransitGatewayPrefixListReference
  ( -- * Creating a request
    DeleteTransitGatewayPrefixListReference (..),
    mkDeleteTransitGatewayPrefixListReference,

    -- ** Request lenses
    dtgplrTransitGatewayRouteTableId,
    dtgplrPrefixListId,
    dtgplrDryRun,

    -- * Destructuring the response
    DeleteTransitGatewayPrefixListReferenceResponse (..),
    mkDeleteTransitGatewayPrefixListReferenceResponse,

    -- ** Response lenses
    dtgplrrrsTransitGatewayPrefixListReference,
    dtgplrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTransitGatewayPrefixListReference' smart constructor.
data DeleteTransitGatewayPrefixListReference = DeleteTransitGatewayPrefixListReference'
  { -- | The ID of the route table.
    transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId,
    -- | The ID of the prefix list.
    prefixListId :: Types.PrefixListResourceId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGatewayPrefixListReference' value with any optional fields omitted.
mkDeleteTransitGatewayPrefixListReference ::
  -- | 'transitGatewayRouteTableId'
  Types.TransitGatewayRouteTableId ->
  -- | 'prefixListId'
  Types.PrefixListResourceId ->
  DeleteTransitGatewayPrefixListReference
mkDeleteTransitGatewayPrefixListReference
  transitGatewayRouteTableId
  prefixListId =
    DeleteTransitGatewayPrefixListReference'
      { transitGatewayRouteTableId,
        prefixListId,
        dryRun = Core.Nothing
      }

-- | The ID of the route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgplrTransitGatewayRouteTableId :: Lens.Lens' DeleteTransitGatewayPrefixListReference Types.TransitGatewayRouteTableId
dtgplrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED dtgplrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgplrPrefixListId :: Lens.Lens' DeleteTransitGatewayPrefixListReference Types.PrefixListResourceId
dtgplrPrefixListId = Lens.field @"prefixListId"
{-# DEPRECATED dtgplrPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgplrDryRun :: Lens.Lens' DeleteTransitGatewayPrefixListReference (Core.Maybe Core.Bool)
dtgplrDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtgplrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteTransitGatewayPrefixListReference where
  type
    Rs DeleteTransitGatewayPrefixListReference =
      DeleteTransitGatewayPrefixListReferenceResponse
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
            ( Core.pure ("Action", "DeleteTransitGatewayPrefixListReference")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "TransitGatewayRouteTableId"
                            transitGatewayRouteTableId
                        )
                Core.<> (Core.toQueryValue "PrefixListId" prefixListId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayPrefixListReferenceResponse'
            Core.<$> (x Core..@? "transitGatewayPrefixListReference")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTransitGatewayPrefixListReferenceResponse' smart constructor.
data DeleteTransitGatewayPrefixListReferenceResponse = DeleteTransitGatewayPrefixListReferenceResponse'
  { -- | Information about the deleted prefix list reference.
    transitGatewayPrefixListReference :: Core.Maybe Types.TransitGatewayPrefixListReference,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGatewayPrefixListReferenceResponse' value with any optional fields omitted.
mkDeleteTransitGatewayPrefixListReferenceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTransitGatewayPrefixListReferenceResponse
mkDeleteTransitGatewayPrefixListReferenceResponse responseStatus =
  DeleteTransitGatewayPrefixListReferenceResponse'
    { transitGatewayPrefixListReference =
        Core.Nothing,
      responseStatus
    }

-- | Information about the deleted prefix list reference.
--
-- /Note:/ Consider using 'transitGatewayPrefixListReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgplrrrsTransitGatewayPrefixListReference :: Lens.Lens' DeleteTransitGatewayPrefixListReferenceResponse (Core.Maybe Types.TransitGatewayPrefixListReference)
dtgplrrrsTransitGatewayPrefixListReference = Lens.field @"transitGatewayPrefixListReference"
{-# DEPRECATED dtgplrrrsTransitGatewayPrefixListReference "Use generic-lens or generic-optics with 'transitGatewayPrefixListReference' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgplrrrsResponseStatus :: Lens.Lens' DeleteTransitGatewayPrefixListReferenceResponse Core.Int
dtgplrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgplrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
