{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a subnet or gateway from a route table.
--
-- After you perform this action, the subnet no longer uses the routes in the route table. Instead, it uses the routes in the VPC's main route table. For more information about route tables, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.DisassociateRouteTable
  ( -- * Creating a request
    DisassociateRouteTable (..),
    mkDisassociateRouteTable,

    -- ** Request lenses
    drtAssociationId,
    drtDryRun,

    -- * Destructuring the response
    DisassociateRouteTableResponse (..),
    mkDisassociateRouteTableResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateRouteTable' smart constructor.
data DisassociateRouteTable = DisassociateRouteTable'
  { -- | The association ID representing the current association between the route table and subnet or gateway.
    associationId :: Types.RouteTableAssociationId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateRouteTable' value with any optional fields omitted.
mkDisassociateRouteTable ::
  -- | 'associationId'
  Types.RouteTableAssociationId ->
  DisassociateRouteTable
mkDisassociateRouteTable associationId =
  DisassociateRouteTable' {associationId, dryRun = Core.Nothing}

-- | The association ID representing the current association between the route table and subnet or gateway.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtAssociationId :: Lens.Lens' DisassociateRouteTable Types.RouteTableAssociationId
drtAssociationId = Lens.field @"associationId"
{-# DEPRECATED drtAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtDryRun :: Lens.Lens' DisassociateRouteTable (Core.Maybe Core.Bool)
drtDryRun = Lens.field @"dryRun"
{-# DEPRECATED drtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DisassociateRouteTable where
  type Rs DisassociateRouteTable = DisassociateRouteTableResponse
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
            ( Core.pure ("Action", "DisassociateRouteTable")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "AssociationId" associationId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response = Response.receiveNull DisassociateRouteTableResponse'

-- | /See:/ 'mkDisassociateRouteTableResponse' smart constructor.
data DisassociateRouteTableResponse = DisassociateRouteTableResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateRouteTableResponse' value with any optional fields omitted.
mkDisassociateRouteTableResponse ::
  DisassociateRouteTableResponse
mkDisassociateRouteTableResponse = DisassociateRouteTableResponse'
