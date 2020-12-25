{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route table. You must disassociate the route table from any subnets before you can delete it. You can't delete the main route table.
module Network.AWS.EC2.DeleteRouteTable
  ( -- * Creating a request
    DeleteRouteTable (..),
    mkDeleteRouteTable,

    -- ** Request lenses
    drtfRouteTableId,
    drtfDryRun,

    -- * Destructuring the response
    DeleteRouteTableResponse (..),
    mkDeleteRouteTableResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRouteTable' smart constructor.
data DeleteRouteTable = DeleteRouteTable'
  { -- | The ID of the route table.
    routeTableId :: Types.RouteTableId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRouteTable' value with any optional fields omitted.
mkDeleteRouteTable ::
  -- | 'routeTableId'
  Types.RouteTableId ->
  DeleteRouteTable
mkDeleteRouteTable routeTableId =
  DeleteRouteTable' {routeTableId, dryRun = Core.Nothing}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtfRouteTableId :: Lens.Lens' DeleteRouteTable Types.RouteTableId
drtfRouteTableId = Lens.field @"routeTableId"
{-# DEPRECATED drtfRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtfDryRun :: Lens.Lens' DeleteRouteTable (Core.Maybe Core.Bool)
drtfDryRun = Lens.field @"dryRun"
{-# DEPRECATED drtfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteRouteTable where
  type Rs DeleteRouteTable = DeleteRouteTableResponse
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
            ( Core.pure ("Action", "DeleteRouteTable")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "RouteTableId" routeTableId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response = Response.receiveNull DeleteRouteTableResponse'

-- | /See:/ 'mkDeleteRouteTableResponse' smart constructor.
data DeleteRouteTableResponse = DeleteRouteTableResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRouteTableResponse' value with any optional fields omitted.
mkDeleteRouteTableResponse ::
  DeleteRouteTableResponse
mkDeleteRouteTableResponse = DeleteRouteTableResponse'
