{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified route table.
module Network.AWS.EC2.DeleteRoute
  ( -- * Creating a request
    DeleteRoute (..),
    mkDeleteRoute,

    -- ** Request lenses
    drRouteTableId,
    drDestinationCidrBlock,
    drDestinationIpv6CidrBlock,
    drDestinationPrefixListId,
    drDryRun,

    -- * Destructuring the response
    DeleteRouteResponse (..),
    mkDeleteRouteResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRoute' smart constructor.
data DeleteRoute = DeleteRoute'
  { -- | The ID of the route table.
    routeTableId :: Types.RouteTableId,
    -- | The IPv4 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
    destinationCidrBlock :: Core.Maybe Types.String,
    -- | The IPv6 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
    destinationIpv6CidrBlock :: Core.Maybe Types.String,
    -- | The ID of the prefix list for the route.
    destinationPrefixListId :: Core.Maybe Types.PrefixListResourceId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRoute' value with any optional fields omitted.
mkDeleteRoute ::
  -- | 'routeTableId'
  Types.RouteTableId ->
  DeleteRoute
mkDeleteRoute routeTableId =
  DeleteRoute'
    { routeTableId,
      destinationCidrBlock = Core.Nothing,
      destinationIpv6CidrBlock = Core.Nothing,
      destinationPrefixListId = Core.Nothing,
      dryRun = Core.Nothing
    }

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRouteTableId :: Lens.Lens' DeleteRoute Types.RouteTableId
drRouteTableId = Lens.field @"routeTableId"
{-# DEPRECATED drRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

-- | The IPv4 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDestinationCidrBlock :: Lens.Lens' DeleteRoute (Core.Maybe Types.String)
drDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# DEPRECATED drDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The IPv6 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
--
-- /Note:/ Consider using 'destinationIpv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDestinationIpv6CidrBlock :: Lens.Lens' DeleteRoute (Core.Maybe Types.String)
drDestinationIpv6CidrBlock = Lens.field @"destinationIpv6CidrBlock"
{-# DEPRECATED drDestinationIpv6CidrBlock "Use generic-lens or generic-optics with 'destinationIpv6CidrBlock' instead." #-}

-- | The ID of the prefix list for the route.
--
-- /Note:/ Consider using 'destinationPrefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDestinationPrefixListId :: Lens.Lens' DeleteRoute (Core.Maybe Types.PrefixListResourceId)
drDestinationPrefixListId = Lens.field @"destinationPrefixListId"
{-# DEPRECATED drDestinationPrefixListId "Use generic-lens or generic-optics with 'destinationPrefixListId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDryRun :: Lens.Lens' DeleteRoute (Core.Maybe Core.Bool)
drDryRun = Lens.field @"dryRun"
{-# DEPRECATED drDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteRoute where
  type Rs DeleteRoute = DeleteRouteResponse
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
            ( Core.pure ("Action", "DeleteRoute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "RouteTableId" routeTableId)
                Core.<> ( Core.toQueryValue "DestinationCidrBlock"
                            Core.<$> destinationCidrBlock
                        )
                Core.<> ( Core.toQueryValue "DestinationIpv6CidrBlock"
                            Core.<$> destinationIpv6CidrBlock
                        )
                Core.<> ( Core.toQueryValue "DestinationPrefixListId"
                            Core.<$> destinationPrefixListId
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response = Response.receiveNull DeleteRouteResponse'

-- | /See:/ 'mkDeleteRouteResponse' smart constructor.
data DeleteRouteResponse = DeleteRouteResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRouteResponse' value with any optional fields omitted.
mkDeleteRouteResponse ::
  DeleteRouteResponse
mkDeleteRouteResponse = DeleteRouteResponse'
