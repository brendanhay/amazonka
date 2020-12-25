{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces an existing route within a route table in a VPC. You must provide only one of the following: internet gateway, virtual private gateway, NAT instance, NAT gateway, VPC peering connection, network interface, egress-only internet gateway, or transit gateway.
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.ReplaceRoute
  ( -- * Creating a request
    ReplaceRoute (..),
    mkReplaceRoute,

    -- ** Request lenses
    rrRouteTableId,
    rrCarrierGatewayId,
    rrDestinationCidrBlock,
    rrDestinationIpv6CidrBlock,
    rrDestinationPrefixListId,
    rrDryRun,
    rrEgressOnlyInternetGatewayId,
    rrGatewayId,
    rrInstanceId,
    rrLocalGatewayId,
    rrLocalTarget,
    rrNatGatewayId,
    rrNetworkInterfaceId,
    rrTransitGatewayId,
    rrVpcEndpointId,
    rrVpcPeeringConnectionId,

    -- * Destructuring the response
    ReplaceRouteResponse (..),
    mkReplaceRouteResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkReplaceRoute' smart constructor.
data ReplaceRoute = ReplaceRoute'
  { -- | The ID of the route table.
    routeTableId :: Types.RouteTableId,
    -- | [IPv4 traffic only] The ID of a carrier gateway.
    carrierGatewayId :: Core.Maybe Types.CarrierGatewayId,
    -- | The IPv4 CIDR address block used for the destination match. The value that you provide must match the CIDR of an existing route in the table.
    destinationCidrBlock :: Core.Maybe Types.DestinationCidrBlock,
    -- | The IPv6 CIDR address block used for the destination match. The value that you provide must match the CIDR of an existing route in the table.
    destinationIpv6CidrBlock :: Core.Maybe Types.DestinationIpv6CidrBlock,
    -- | The ID of the prefix list for the route.
    destinationPrefixListId :: Core.Maybe Types.DestinationPrefixListId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | [IPv6 traffic only] The ID of an egress-only internet gateway.
    egressOnlyInternetGatewayId :: Core.Maybe Types.EgressOnlyInternetGatewayId,
    -- | The ID of an internet gateway or virtual private gateway.
    gatewayId :: Core.Maybe Types.GatewayId,
    -- | The ID of a NAT instance in your VPC.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The ID of the local gateway.
    localGatewayId :: Core.Maybe Types.LocalGatewayId,
    -- | Specifies whether to reset the local route to its default target (@local@ ).
    localTarget :: Core.Maybe Core.Bool,
    -- | [IPv4 traffic only] The ID of a NAT gateway.
    natGatewayId :: Core.Maybe Types.NatGatewayId,
    -- | The ID of a network interface.
    networkInterfaceId :: Core.Maybe Types.NetworkInterfaceId,
    -- | The ID of a transit gateway.
    transitGatewayId :: Core.Maybe Types.TransitGatewayId,
    -- | The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints only.
    vpcEndpointId :: Core.Maybe Types.VpcEndpointId,
    -- | The ID of a VPC peering connection.
    vpcPeeringConnectionId :: Core.Maybe Types.VpcPeeringConnectionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceRoute' value with any optional fields omitted.
mkReplaceRoute ::
  -- | 'routeTableId'
  Types.RouteTableId ->
  ReplaceRoute
mkReplaceRoute routeTableId =
  ReplaceRoute'
    { routeTableId,
      carrierGatewayId = Core.Nothing,
      destinationCidrBlock = Core.Nothing,
      destinationIpv6CidrBlock = Core.Nothing,
      destinationPrefixListId = Core.Nothing,
      dryRun = Core.Nothing,
      egressOnlyInternetGatewayId = Core.Nothing,
      gatewayId = Core.Nothing,
      instanceId = Core.Nothing,
      localGatewayId = Core.Nothing,
      localTarget = Core.Nothing,
      natGatewayId = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      transitGatewayId = Core.Nothing,
      vpcEndpointId = Core.Nothing,
      vpcPeeringConnectionId = Core.Nothing
    }

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrRouteTableId :: Lens.Lens' ReplaceRoute Types.RouteTableId
rrRouteTableId = Lens.field @"routeTableId"
{-# DEPRECATED rrRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

-- | [IPv4 traffic only] The ID of a carrier gateway.
--
-- /Note:/ Consider using 'carrierGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrCarrierGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.CarrierGatewayId)
rrCarrierGatewayId = Lens.field @"carrierGatewayId"
{-# DEPRECATED rrCarrierGatewayId "Use generic-lens or generic-optics with 'carrierGatewayId' instead." #-}

-- | The IPv4 CIDR address block used for the destination match. The value that you provide must match the CIDR of an existing route in the table.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDestinationCidrBlock :: Lens.Lens' ReplaceRoute (Core.Maybe Types.DestinationCidrBlock)
rrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# DEPRECATED rrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The IPv6 CIDR address block used for the destination match. The value that you provide must match the CIDR of an existing route in the table.
--
-- /Note:/ Consider using 'destinationIpv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDestinationIpv6CidrBlock :: Lens.Lens' ReplaceRoute (Core.Maybe Types.DestinationIpv6CidrBlock)
rrDestinationIpv6CidrBlock = Lens.field @"destinationIpv6CidrBlock"
{-# DEPRECATED rrDestinationIpv6CidrBlock "Use generic-lens or generic-optics with 'destinationIpv6CidrBlock' instead." #-}

-- | The ID of the prefix list for the route.
--
-- /Note:/ Consider using 'destinationPrefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDestinationPrefixListId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.DestinationPrefixListId)
rrDestinationPrefixListId = Lens.field @"destinationPrefixListId"
{-# DEPRECATED rrDestinationPrefixListId "Use generic-lens or generic-optics with 'destinationPrefixListId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDryRun :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Bool)
rrDryRun = Lens.field @"dryRun"
{-# DEPRECATED rrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | [IPv6 traffic only] The ID of an egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrEgressOnlyInternetGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.EgressOnlyInternetGatewayId)
rrEgressOnlyInternetGatewayId = Lens.field @"egressOnlyInternetGatewayId"
{-# DEPRECATED rrEgressOnlyInternetGatewayId "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayId' instead." #-}

-- | The ID of an internet gateway or virtual private gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.GatewayId)
rrGatewayId = Lens.field @"gatewayId"
{-# DEPRECATED rrGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | The ID of a NAT instance in your VPC.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrInstanceId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.InstanceId)
rrInstanceId = Lens.field @"instanceId"
{-# DEPRECATED rrInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrLocalGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.LocalGatewayId)
rrLocalGatewayId = Lens.field @"localGatewayId"
{-# DEPRECATED rrLocalGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead." #-}

-- | Specifies whether to reset the local route to its default target (@local@ ).
--
-- /Note:/ Consider using 'localTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrLocalTarget :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Bool)
rrLocalTarget = Lens.field @"localTarget"
{-# DEPRECATED rrLocalTarget "Use generic-lens or generic-optics with 'localTarget' instead." #-}

-- | [IPv4 traffic only] The ID of a NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrNatGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.NatGatewayId)
rrNatGatewayId = Lens.field @"natGatewayId"
{-# DEPRECATED rrNatGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead." #-}

-- | The ID of a network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrNetworkInterfaceId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.NetworkInterfaceId)
rrNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED rrNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of a transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrTransitGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.TransitGatewayId)
rrTransitGatewayId = Lens.field @"transitGatewayId"
{-# DEPRECATED rrTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints only.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrVpcEndpointId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.VpcEndpointId)
rrVpcEndpointId = Lens.field @"vpcEndpointId"
{-# DEPRECATED rrVpcEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead." #-}

-- | The ID of a VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrVpcPeeringConnectionId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.VpcPeeringConnectionId)
rrVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# DEPRECATED rrVpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

instance Core.AWSRequest ReplaceRoute where
  type Rs ReplaceRoute = ReplaceRouteResponse
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
            ( Core.pure ("Action", "ReplaceRoute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "RouteTableId" routeTableId)
                Core.<> (Core.toQueryValue "CarrierGatewayId" Core.<$> carrierGatewayId)
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
                Core.<> ( Core.toQueryValue "EgressOnlyInternetGatewayId"
                            Core.<$> egressOnlyInternetGatewayId
                        )
                Core.<> (Core.toQueryValue "GatewayId" Core.<$> gatewayId)
                Core.<> (Core.toQueryValue "InstanceId" Core.<$> instanceId)
                Core.<> (Core.toQueryValue "LocalGatewayId" Core.<$> localGatewayId)
                Core.<> (Core.toQueryValue "LocalTarget" Core.<$> localTarget)
                Core.<> (Core.toQueryValue "NatGatewayId" Core.<$> natGatewayId)
                Core.<> ( Core.toQueryValue "NetworkInterfaceId"
                            Core.<$> networkInterfaceId
                        )
                Core.<> (Core.toQueryValue "TransitGatewayId" Core.<$> transitGatewayId)
                Core.<> (Core.toQueryValue "VpcEndpointId" Core.<$> vpcEndpointId)
                Core.<> ( Core.toQueryValue "VpcPeeringConnectionId"
                            Core.<$> vpcPeeringConnectionId
                        )
            )
      }
  response = Response.receiveNull ReplaceRouteResponse'

-- | /See:/ 'mkReplaceRouteResponse' smart constructor.
data ReplaceRouteResponse = ReplaceRouteResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceRouteResponse' value with any optional fields omitted.
mkReplaceRouteResponse ::
  ReplaceRouteResponse
mkReplaceRouteResponse = ReplaceRouteResponse'
