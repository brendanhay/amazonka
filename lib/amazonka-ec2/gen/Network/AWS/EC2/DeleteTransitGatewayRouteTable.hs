{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway route table. You must disassociate the route table from any transit gateway route tables before you can delete it.
module Network.AWS.EC2.DeleteTransitGatewayRouteTable
  ( -- * Creating a request
    DeleteTransitGatewayRouteTable (..),
    mkDeleteTransitGatewayRouteTable,

    -- ** Request lenses
    dtgrtTransitGatewayRouteTableId,
    dtgrtDryRun,

    -- * Destructuring the response
    DeleteTransitGatewayRouteTableResponse (..),
    mkDeleteTransitGatewayRouteTableResponse,

    -- ** Response lenses
    dtgrtrrsTransitGatewayRouteTable,
    dtgrtrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTransitGatewayRouteTable' smart constructor.
data DeleteTransitGatewayRouteTable = DeleteTransitGatewayRouteTable'
  { -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGatewayRouteTable' value with any optional fields omitted.
mkDeleteTransitGatewayRouteTable ::
  -- | 'transitGatewayRouteTableId'
  Types.TransitGatewayRouteTableId ->
  DeleteTransitGatewayRouteTable
mkDeleteTransitGatewayRouteTable transitGatewayRouteTableId =
  DeleteTransitGatewayRouteTable'
    { transitGatewayRouteTableId,
      dryRun = Core.Nothing
    }

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtTransitGatewayRouteTableId :: Lens.Lens' DeleteTransitGatewayRouteTable Types.TransitGatewayRouteTableId
dtgrtTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED dtgrtTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtDryRun :: Lens.Lens' DeleteTransitGatewayRouteTable (Core.Maybe Core.Bool)
dtgrtDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtgrtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteTransitGatewayRouteTable where
  type
    Rs DeleteTransitGatewayRouteTable =
      DeleteTransitGatewayRouteTableResponse
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
            ( Core.pure ("Action", "DeleteTransitGatewayRouteTable")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "TransitGatewayRouteTableId"
                            transitGatewayRouteTableId
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayRouteTableResponse'
            Core.<$> (x Core..@? "transitGatewayRouteTable")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTransitGatewayRouteTableResponse' smart constructor.
data DeleteTransitGatewayRouteTableResponse = DeleteTransitGatewayRouteTableResponse'
  { -- | Information about the deleted transit gateway route table.
    transitGatewayRouteTable :: Core.Maybe Types.TransitGatewayRouteTable,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteTransitGatewayRouteTableResponse' value with any optional fields omitted.
mkDeleteTransitGatewayRouteTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTransitGatewayRouteTableResponse
mkDeleteTransitGatewayRouteTableResponse responseStatus =
  DeleteTransitGatewayRouteTableResponse'
    { transitGatewayRouteTable =
        Core.Nothing,
      responseStatus
    }

-- | Information about the deleted transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtrrsTransitGatewayRouteTable :: Lens.Lens' DeleteTransitGatewayRouteTableResponse (Core.Maybe Types.TransitGatewayRouteTable)
dtgrtrrsTransitGatewayRouteTable = Lens.field @"transitGatewayRouteTable"
{-# DEPRECATED dtgrtrrsTransitGatewayRouteTable "Use generic-lens or generic-optics with 'transitGatewayRouteTable' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtrrsResponseStatus :: Lens.Lens' DeleteTransitGatewayRouteTableResponse Core.Int
dtgrtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgrtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
