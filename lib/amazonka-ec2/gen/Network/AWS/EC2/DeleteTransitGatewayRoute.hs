{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified transit gateway route table.
module Network.AWS.EC2.DeleteTransitGatewayRoute
  ( -- * Creating a request
    DeleteTransitGatewayRoute (..),
    mkDeleteTransitGatewayRoute,

    -- ** Request lenses
    dtgrTransitGatewayRouteTableId,
    dtgrDestinationCidrBlock,
    dtgrDryRun,

    -- * Destructuring the response
    DeleteTransitGatewayRouteResponse (..),
    mkDeleteTransitGatewayRouteResponse,

    -- ** Response lenses
    dtgrrrsRoute,
    dtgrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTransitGatewayRoute' smart constructor.
data DeleteTransitGatewayRoute = DeleteTransitGatewayRoute'
  { -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId,
    -- | The CIDR range for the route. This must match the CIDR for the route exactly.
    destinationCidrBlock :: Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGatewayRoute' value with any optional fields omitted.
mkDeleteTransitGatewayRoute ::
  -- | 'transitGatewayRouteTableId'
  Types.TransitGatewayRouteTableId ->
  -- | 'destinationCidrBlock'
  Types.String ->
  DeleteTransitGatewayRoute
mkDeleteTransitGatewayRoute
  transitGatewayRouteTableId
  destinationCidrBlock =
    DeleteTransitGatewayRoute'
      { transitGatewayRouteTableId,
        destinationCidrBlock,
        dryRun = Core.Nothing
      }

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrTransitGatewayRouteTableId :: Lens.Lens' DeleteTransitGatewayRoute Types.TransitGatewayRouteTableId
dtgrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED dtgrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The CIDR range for the route. This must match the CIDR for the route exactly.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrDestinationCidrBlock :: Lens.Lens' DeleteTransitGatewayRoute Types.String
dtgrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# DEPRECATED dtgrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrDryRun :: Lens.Lens' DeleteTransitGatewayRoute (Core.Maybe Core.Bool)
dtgrDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteTransitGatewayRoute where
  type
    Rs DeleteTransitGatewayRoute =
      DeleteTransitGatewayRouteResponse
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
            ( Core.pure ("Action", "DeleteTransitGatewayRoute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "TransitGatewayRouteTableId"
                            transitGatewayRouteTableId
                        )
                Core.<> (Core.toQueryValue "DestinationCidrBlock" destinationCidrBlock)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayRouteResponse'
            Core.<$> (x Core..@? "route") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTransitGatewayRouteResponse' smart constructor.
data DeleteTransitGatewayRouteResponse = DeleteTransitGatewayRouteResponse'
  { -- | Information about the route.
    route :: Core.Maybe Types.TransitGatewayRoute,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGatewayRouteResponse' value with any optional fields omitted.
mkDeleteTransitGatewayRouteResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTransitGatewayRouteResponse
mkDeleteTransitGatewayRouteResponse responseStatus =
  DeleteTransitGatewayRouteResponse'
    { route = Core.Nothing,
      responseStatus
    }

-- | Information about the route.
--
-- /Note:/ Consider using 'route' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrrsRoute :: Lens.Lens' DeleteTransitGatewayRouteResponse (Core.Maybe Types.TransitGatewayRoute)
dtgrrrsRoute = Lens.field @"route"
{-# DEPRECATED dtgrrrsRoute "Use generic-lens or generic-optics with 'route' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrrsResponseStatus :: Lens.Lens' DeleteTransitGatewayRouteResponse Core.Int
dtgrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
