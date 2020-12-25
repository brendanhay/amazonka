{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a subnet in your VPC or an internet gateway or virtual private gateway attached to your VPC with a route table in your VPC. This association causes traffic from the subnet or gateway to be routed according to the routes in the route table. The action returns an association ID, which you need in order to disassociate the route table later. A route table can be associated with multiple subnets.
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.AssociateRouteTable
  ( -- * Creating a request
    AssociateRouteTable (..),
    mkAssociateRouteTable,

    -- ** Request lenses
    artRouteTableId,
    artDryRun,
    artGatewayId,
    artSubnetId,

    -- * Destructuring the response
    AssociateRouteTableResponse (..),
    mkAssociateRouteTableResponse,

    -- ** Response lenses
    artrrsAssociationId,
    artrrsAssociationState,
    artrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateRouteTable' smart constructor.
data AssociateRouteTable = AssociateRouteTable'
  { -- | The ID of the route table.
    routeTableId :: Types.RouteTableId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the internet gateway or virtual private gateway.
    gatewayId :: Core.Maybe Types.RouteGatewayId,
    -- | The ID of the subnet.
    subnetId :: Core.Maybe Types.SubnetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateRouteTable' value with any optional fields omitted.
mkAssociateRouteTable ::
  -- | 'routeTableId'
  Types.RouteTableId ->
  AssociateRouteTable
mkAssociateRouteTable routeTableId =
  AssociateRouteTable'
    { routeTableId,
      dryRun = Core.Nothing,
      gatewayId = Core.Nothing,
      subnetId = Core.Nothing
    }

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artRouteTableId :: Lens.Lens' AssociateRouteTable Types.RouteTableId
artRouteTableId = Lens.field @"routeTableId"
{-# DEPRECATED artRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artDryRun :: Lens.Lens' AssociateRouteTable (Core.Maybe Core.Bool)
artDryRun = Lens.field @"dryRun"
{-# DEPRECATED artDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the internet gateway or virtual private gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artGatewayId :: Lens.Lens' AssociateRouteTable (Core.Maybe Types.RouteGatewayId)
artGatewayId = Lens.field @"gatewayId"
{-# DEPRECATED artGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artSubnetId :: Lens.Lens' AssociateRouteTable (Core.Maybe Types.SubnetId)
artSubnetId = Lens.field @"subnetId"
{-# DEPRECATED artSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

instance Core.AWSRequest AssociateRouteTable where
  type Rs AssociateRouteTable = AssociateRouteTableResponse
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
            ( Core.pure ("Action", "AssociateRouteTable")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "RouteTableId" routeTableId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "GatewayId" Core.<$> gatewayId)
                Core.<> (Core.toQueryValue "SubnetId" Core.<$> subnetId)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateRouteTableResponse'
            Core.<$> (x Core..@? "associationId")
            Core.<*> (x Core..@? "associationState")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateRouteTableResponse' smart constructor.
data AssociateRouteTableResponse = AssociateRouteTableResponse'
  { -- | The route table association ID. This ID is required for disassociating the route table.
    associationId :: Core.Maybe Types.String,
    -- | The state of the association.
    associationState :: Core.Maybe Types.RouteTableAssociationState,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateRouteTableResponse' value with any optional fields omitted.
mkAssociateRouteTableResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateRouteTableResponse
mkAssociateRouteTableResponse responseStatus =
  AssociateRouteTableResponse'
    { associationId = Core.Nothing,
      associationState = Core.Nothing,
      responseStatus
    }

-- | The route table association ID. This ID is required for disassociating the route table.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artrrsAssociationId :: Lens.Lens' AssociateRouteTableResponse (Core.Maybe Types.String)
artrrsAssociationId = Lens.field @"associationId"
{-# DEPRECATED artrrsAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The state of the association.
--
-- /Note:/ Consider using 'associationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artrrsAssociationState :: Lens.Lens' AssociateRouteTableResponse (Core.Maybe Types.RouteTableAssociationState)
artrrsAssociationState = Lens.field @"associationState"
{-# DEPRECATED artrrsAssociationState "Use generic-lens or generic-optics with 'associationState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artrrsResponseStatus :: Lens.Lens' AssociateRouteTableResponse Core.Int
artrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED artrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
