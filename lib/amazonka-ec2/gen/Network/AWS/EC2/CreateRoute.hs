{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route in a route table within a VPC.
--
-- You must specify one of the following targets: internet gateway or virtual private gateway, NAT instance, NAT gateway, VPC peering connection, network interface, egress-only internet gateway, or transit gateway.
-- When determining how to route traffic, we use the route with the most specific match. For example, traffic is destined for the IPv4 address @192.0.2.3@ , and the route table includes the following two IPv4 routes:
--
--     * @192.0.2.0/24@ (goes to some target A)
--
--
--     * @192.0.2.0/28@ (goes to some target B)
--
--
-- Both routes apply to the traffic destined for @192.0.2.3@ . However, the second route in the list covers a smaller number of IP addresses and is therefore more specific, so we use that route to determine where to target the traffic.
-- For more information about route tables, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateRoute
  ( -- * Creating a request
    CreateRoute (..),
    mkCreateRoute,

    -- ** Request lenses
    crRouteTableId,
    crCarrierGatewayId,
    crDestinationCidrBlock,
    crDestinationIpv6CidrBlock,
    crDestinationPrefixListId,
    crDryRun,
    crEgressOnlyInternetGatewayId,
    crGatewayId,
    crInstanceId,
    crLocalGatewayId,
    crNatGatewayId,
    crNetworkInterfaceId,
    crTransitGatewayId,
    crVpcEndpointId,
    crVpcPeeringConnectionId,

    -- * Destructuring the response
    CreateRouteResponse (..),
    mkCreateRouteResponse,

    -- ** Response lenses
    crrrsReturn,
    crrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRoute' smart constructor.
data CreateRoute = CreateRoute'
  { -- | The ID of the route table for the route.
    routeTableId :: Types.RouteTableId,
    -- | The ID of the carrier gateway.
    --
    -- You can only use this option when the VPC contains a subnet which is associated with a Wavelength Zone.
    carrierGatewayId :: Core.Maybe Types.CarrierGatewayId,
    -- | The IPv4 CIDR address block used for the destination match. Routing decisions are based on the most specific match. We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
    destinationCidrBlock :: Core.Maybe Types.String,
    -- | The IPv6 CIDR block used for the destination match. Routing decisions are based on the most specific match.
    destinationIpv6CidrBlock :: Core.Maybe Types.String,
    -- | The ID of a prefix list used for the destination match.
    destinationPrefixListId :: Core.Maybe Types.PrefixListResourceId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | [IPv6 traffic only] The ID of an egress-only internet gateway.
    egressOnlyInternetGatewayId :: Core.Maybe Types.EgressOnlyInternetGatewayId,
    -- | The ID of an internet gateway or virtual private gateway attached to your VPC.
    gatewayId :: Core.Maybe Types.RouteGatewayId,
    -- | The ID of a NAT instance in your VPC. The operation fails if you specify an instance ID unless exactly one network interface is attached.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The ID of the local gateway.
    localGatewayId :: Core.Maybe Types.LocalGatewayId,
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

-- | Creates a 'CreateRoute' value with any optional fields omitted.
mkCreateRoute ::
  -- | 'routeTableId'
  Types.RouteTableId ->
  CreateRoute
mkCreateRoute routeTableId =
  CreateRoute'
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
      natGatewayId = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      transitGatewayId = Core.Nothing,
      vpcEndpointId = Core.Nothing,
      vpcPeeringConnectionId = Core.Nothing
    }

-- | The ID of the route table for the route.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRouteTableId :: Lens.Lens' CreateRoute Types.RouteTableId
crRouteTableId = Lens.field @"routeTableId"
{-# DEPRECATED crRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

-- | The ID of the carrier gateway.
--
-- You can only use this option when the VPC contains a subnet which is associated with a Wavelength Zone.
--
-- /Note:/ Consider using 'carrierGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCarrierGatewayId :: Lens.Lens' CreateRoute (Core.Maybe Types.CarrierGatewayId)
crCarrierGatewayId = Lens.field @"carrierGatewayId"
{-# DEPRECATED crCarrierGatewayId "Use generic-lens or generic-optics with 'carrierGatewayId' instead." #-}

-- | The IPv4 CIDR address block used for the destination match. Routing decisions are based on the most specific match. We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDestinationCidrBlock :: Lens.Lens' CreateRoute (Core.Maybe Types.String)
crDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# DEPRECATED crDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The IPv6 CIDR block used for the destination match. Routing decisions are based on the most specific match.
--
-- /Note:/ Consider using 'destinationIpv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDestinationIpv6CidrBlock :: Lens.Lens' CreateRoute (Core.Maybe Types.String)
crDestinationIpv6CidrBlock = Lens.field @"destinationIpv6CidrBlock"
{-# DEPRECATED crDestinationIpv6CidrBlock "Use generic-lens or generic-optics with 'destinationIpv6CidrBlock' instead." #-}

-- | The ID of a prefix list used for the destination match.
--
-- /Note:/ Consider using 'destinationPrefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDestinationPrefixListId :: Lens.Lens' CreateRoute (Core.Maybe Types.PrefixListResourceId)
crDestinationPrefixListId = Lens.field @"destinationPrefixListId"
{-# DEPRECATED crDestinationPrefixListId "Use generic-lens or generic-optics with 'destinationPrefixListId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDryRun :: Lens.Lens' CreateRoute (Core.Maybe Core.Bool)
crDryRun = Lens.field @"dryRun"
{-# DEPRECATED crDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | [IPv6 traffic only] The ID of an egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEgressOnlyInternetGatewayId :: Lens.Lens' CreateRoute (Core.Maybe Types.EgressOnlyInternetGatewayId)
crEgressOnlyInternetGatewayId = Lens.field @"egressOnlyInternetGatewayId"
{-# DEPRECATED crEgressOnlyInternetGatewayId "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayId' instead." #-}

-- | The ID of an internet gateway or virtual private gateway attached to your VPC.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crGatewayId :: Lens.Lens' CreateRoute (Core.Maybe Types.RouteGatewayId)
crGatewayId = Lens.field @"gatewayId"
{-# DEPRECATED crGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | The ID of a NAT instance in your VPC. The operation fails if you specify an instance ID unless exactly one network interface is attached.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crInstanceId :: Lens.Lens' CreateRoute (Core.Maybe Types.InstanceId)
crInstanceId = Lens.field @"instanceId"
{-# DEPRECATED crInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crLocalGatewayId :: Lens.Lens' CreateRoute (Core.Maybe Types.LocalGatewayId)
crLocalGatewayId = Lens.field @"localGatewayId"
{-# DEPRECATED crLocalGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead." #-}

-- | [IPv4 traffic only] The ID of a NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crNatGatewayId :: Lens.Lens' CreateRoute (Core.Maybe Types.NatGatewayId)
crNatGatewayId = Lens.field @"natGatewayId"
{-# DEPRECATED crNatGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead." #-}

-- | The ID of a network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crNetworkInterfaceId :: Lens.Lens' CreateRoute (Core.Maybe Types.NetworkInterfaceId)
crNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED crNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of a transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTransitGatewayId :: Lens.Lens' CreateRoute (Core.Maybe Types.TransitGatewayId)
crTransitGatewayId = Lens.field @"transitGatewayId"
{-# DEPRECATED crTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints only.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crVpcEndpointId :: Lens.Lens' CreateRoute (Core.Maybe Types.VpcEndpointId)
crVpcEndpointId = Lens.field @"vpcEndpointId"
{-# DEPRECATED crVpcEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead." #-}

-- | The ID of a VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crVpcPeeringConnectionId :: Lens.Lens' CreateRoute (Core.Maybe Types.VpcPeeringConnectionId)
crVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# DEPRECATED crVpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

instance Core.AWSRequest CreateRoute where
  type Rs CreateRoute = CreateRouteResponse
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
            ( Core.pure ("Action", "CreateRoute")
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
  response =
    Response.receiveXML
      ( \s h x ->
          CreateRouteResponse'
            Core.<$> (x Core..@? "return") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateRouteResponse' smart constructor.
data CreateRouteResponse = CreateRouteResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRouteResponse' value with any optional fields omitted.
mkCreateRouteResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRouteResponse
mkCreateRouteResponse responseStatus =
  CreateRouteResponse' {return = Core.Nothing, responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsReturn :: Lens.Lens' CreateRouteResponse (Core.Maybe Core.Bool)
crrrsReturn = Lens.field @"return"
{-# DEPRECATED crrrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateRouteResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
