{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteLocalGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified local gateway route table.
module Network.AWS.EC2.DeleteLocalGatewayRoute
  ( -- * Creating a request
    DeleteLocalGatewayRoute (..),
    mkDeleteLocalGatewayRoute,

    -- ** Request lenses
    dlgrDestinationCidrBlock,
    dlgrLocalGatewayRouteTableId,
    dlgrDryRun,

    -- * Destructuring the response
    DeleteLocalGatewayRouteResponse (..),
    mkDeleteLocalGatewayRouteResponse,

    -- ** Response lenses
    dlgrrrsRoute,
    dlgrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLocalGatewayRoute' smart constructor.
data DeleteLocalGatewayRoute = DeleteLocalGatewayRoute'
  { -- | The CIDR range for the route. This must match the CIDR for the route exactly.
    destinationCidrBlock :: Types.DestinationCidrBlock,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Types.LocalGatewayRouteTableId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLocalGatewayRoute' value with any optional fields omitted.
mkDeleteLocalGatewayRoute ::
  -- | 'destinationCidrBlock'
  Types.DestinationCidrBlock ->
  -- | 'localGatewayRouteTableId'
  Types.LocalGatewayRouteTableId ->
  DeleteLocalGatewayRoute
mkDeleteLocalGatewayRoute
  destinationCidrBlock
  localGatewayRouteTableId =
    DeleteLocalGatewayRoute'
      { destinationCidrBlock,
        localGatewayRouteTableId,
        dryRun = Core.Nothing
      }

-- | The CIDR range for the route. This must match the CIDR for the route exactly.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrDestinationCidrBlock :: Lens.Lens' DeleteLocalGatewayRoute Types.DestinationCidrBlock
dlgrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# DEPRECATED dlgrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrLocalGatewayRouteTableId :: Lens.Lens' DeleteLocalGatewayRoute Types.LocalGatewayRouteTableId
dlgrLocalGatewayRouteTableId = Lens.field @"localGatewayRouteTableId"
{-# DEPRECATED dlgrLocalGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrDryRun :: Lens.Lens' DeleteLocalGatewayRoute (Core.Maybe Core.Bool)
dlgrDryRun = Lens.field @"dryRun"
{-# DEPRECATED dlgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteLocalGatewayRoute where
  type Rs DeleteLocalGatewayRoute = DeleteLocalGatewayRouteResponse
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
            ( Core.pure ("Action", "DeleteLocalGatewayRoute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DestinationCidrBlock" destinationCidrBlock)
                Core.<> ( Core.toQueryValue
                            "LocalGatewayRouteTableId"
                            localGatewayRouteTableId
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteLocalGatewayRouteResponse'
            Core.<$> (x Core..@? "route") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteLocalGatewayRouteResponse' smart constructor.
data DeleteLocalGatewayRouteResponse = DeleteLocalGatewayRouteResponse'
  { -- | Information about the route.
    route :: Core.Maybe Types.LocalGatewayRoute,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLocalGatewayRouteResponse' value with any optional fields omitted.
mkDeleteLocalGatewayRouteResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteLocalGatewayRouteResponse
mkDeleteLocalGatewayRouteResponse responseStatus =
  DeleteLocalGatewayRouteResponse'
    { route = Core.Nothing,
      responseStatus
    }

-- | Information about the route.
--
-- /Note:/ Consider using 'route' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrrrsRoute :: Lens.Lens' DeleteLocalGatewayRouteResponse (Core.Maybe Types.LocalGatewayRoute)
dlgrrrsRoute = Lens.field @"route"
{-# DEPRECATED dlgrrrsRoute "Use generic-lens or generic-optics with 'route' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrrrsResponseStatus :: Lens.Lens' DeleteLocalGatewayRouteResponse Core.Int
dlgrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlgrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
