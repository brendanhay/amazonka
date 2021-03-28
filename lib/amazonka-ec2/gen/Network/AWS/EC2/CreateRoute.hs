{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateRoute (..)
    , mkCreateRoute
    -- ** Request lenses
    , crRouteTableId
    , crCarrierGatewayId
    , crDestinationCidrBlock
    , crDestinationIpv6CidrBlock
    , crDestinationPrefixListId
    , crDryRun
    , crEgressOnlyInternetGatewayId
    , crGatewayId
    , crInstanceId
    , crLocalGatewayId
    , crNatGatewayId
    , crNetworkInterfaceId
    , crTransitGatewayId
    , crVpcEndpointId
    , crVpcPeeringConnectionId

    -- * Destructuring the response
    , CreateRouteResponse (..)
    , mkCreateRouteResponse
    -- ** Response lenses
    , crrrsReturn
    , crrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRoute' smart constructor.
data CreateRoute = CreateRoute'
  { routeTableId :: Types.RouteTableId
    -- ^ The ID of the route table for the route.
  , carrierGatewayId :: Core.Maybe Types.CarrierGatewayId
    -- ^ The ID of the carrier gateway.
--
-- You can only use this option when the VPC contains a subnet which is associated with a Wavelength Zone.
  , destinationCidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv4 CIDR address block used for the destination match. Routing decisions are based on the most specific match. We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
  , destinationIpv6CidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR block used for the destination match. Routing decisions are based on the most specific match.
  , destinationPrefixListId :: Core.Maybe Types.PrefixListResourceId
    -- ^ The ID of a prefix list used for the destination match.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , egressOnlyInternetGatewayId :: Core.Maybe Types.EgressOnlyInternetGatewayId
    -- ^ [IPv6 traffic only] The ID of an egress-only internet gateway.
  , gatewayId :: Core.Maybe Types.RouteGatewayId
    -- ^ The ID of an internet gateway or virtual private gateway attached to your VPC.
  , instanceId :: Core.Maybe Types.InstanceId
    -- ^ The ID of a NAT instance in your VPC. The operation fails if you specify an instance ID unless exactly one network interface is attached.
  , localGatewayId :: Core.Maybe Types.LocalGatewayId
    -- ^ The ID of the local gateway.
  , natGatewayId :: Core.Maybe Types.NatGatewayId
    -- ^ [IPv4 traffic only] The ID of a NAT gateway.
  , networkInterfaceId :: Core.Maybe Types.NetworkInterfaceId
    -- ^ The ID of a network interface.
  , transitGatewayId :: Core.Maybe Types.TransitGatewayId
    -- ^ The ID of a transit gateway.
  , vpcEndpointId :: Core.Maybe Types.VpcEndpointId
    -- ^ The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints only.
  , vpcPeeringConnectionId :: Core.Maybe Types.VpcPeeringConnectionId
    -- ^ The ID of a VPC peering connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRoute' value with any optional fields omitted.
mkCreateRoute
    :: Types.RouteTableId -- ^ 'routeTableId'
    -> CreateRoute
mkCreateRoute routeTableId
  = CreateRoute'{routeTableId, carrierGatewayId = Core.Nothing,
                 destinationCidrBlock = Core.Nothing,
                 destinationIpv6CidrBlock = Core.Nothing,
                 destinationPrefixListId = Core.Nothing, dryRun = Core.Nothing,
                 egressOnlyInternetGatewayId = Core.Nothing,
                 gatewayId = Core.Nothing, instanceId = Core.Nothing,
                 localGatewayId = Core.Nothing, natGatewayId = Core.Nothing,
                 networkInterfaceId = Core.Nothing, transitGatewayId = Core.Nothing,
                 vpcEndpointId = Core.Nothing,
                 vpcPeeringConnectionId = Core.Nothing}

-- | The ID of the route table for the route.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRouteTableId :: Lens.Lens' CreateRoute Types.RouteTableId
crRouteTableId = Lens.field @"routeTableId"
{-# INLINEABLE crRouteTableId #-}
{-# DEPRECATED routeTableId "Use generic-lens or generic-optics with 'routeTableId' instead"  #-}

-- | The ID of the carrier gateway.
--
-- You can only use this option when the VPC contains a subnet which is associated with a Wavelength Zone.
--
-- /Note:/ Consider using 'carrierGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCarrierGatewayId :: Lens.Lens' CreateRoute (Core.Maybe Types.CarrierGatewayId)
crCarrierGatewayId = Lens.field @"carrierGatewayId"
{-# INLINEABLE crCarrierGatewayId #-}
{-# DEPRECATED carrierGatewayId "Use generic-lens or generic-optics with 'carrierGatewayId' instead"  #-}

-- | The IPv4 CIDR address block used for the destination match. Routing decisions are based on the most specific match. We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDestinationCidrBlock :: Lens.Lens' CreateRoute (Core.Maybe Core.Text)
crDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE crDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The IPv6 CIDR block used for the destination match. Routing decisions are based on the most specific match.
--
-- /Note:/ Consider using 'destinationIpv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDestinationIpv6CidrBlock :: Lens.Lens' CreateRoute (Core.Maybe Core.Text)
crDestinationIpv6CidrBlock = Lens.field @"destinationIpv6CidrBlock"
{-# INLINEABLE crDestinationIpv6CidrBlock #-}
{-# DEPRECATED destinationIpv6CidrBlock "Use generic-lens or generic-optics with 'destinationIpv6CidrBlock' instead"  #-}

-- | The ID of a prefix list used for the destination match.
--
-- /Note:/ Consider using 'destinationPrefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDestinationPrefixListId :: Lens.Lens' CreateRoute (Core.Maybe Types.PrefixListResourceId)
crDestinationPrefixListId = Lens.field @"destinationPrefixListId"
{-# INLINEABLE crDestinationPrefixListId #-}
{-# DEPRECATED destinationPrefixListId "Use generic-lens or generic-optics with 'destinationPrefixListId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDryRun :: Lens.Lens' CreateRoute (Core.Maybe Core.Bool)
crDryRun = Lens.field @"dryRun"
{-# INLINEABLE crDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | [IPv6 traffic only] The ID of an egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEgressOnlyInternetGatewayId :: Lens.Lens' CreateRoute (Core.Maybe Types.EgressOnlyInternetGatewayId)
crEgressOnlyInternetGatewayId = Lens.field @"egressOnlyInternetGatewayId"
{-# INLINEABLE crEgressOnlyInternetGatewayId #-}
{-# DEPRECATED egressOnlyInternetGatewayId "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayId' instead"  #-}

-- | The ID of an internet gateway or virtual private gateway attached to your VPC.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crGatewayId :: Lens.Lens' CreateRoute (Core.Maybe Types.RouteGatewayId)
crGatewayId = Lens.field @"gatewayId"
{-# INLINEABLE crGatewayId #-}
{-# DEPRECATED gatewayId "Use generic-lens or generic-optics with 'gatewayId' instead"  #-}

-- | The ID of a NAT instance in your VPC. The operation fails if you specify an instance ID unless exactly one network interface is attached.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crInstanceId :: Lens.Lens' CreateRoute (Core.Maybe Types.InstanceId)
crInstanceId = Lens.field @"instanceId"
{-# INLINEABLE crInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crLocalGatewayId :: Lens.Lens' CreateRoute (Core.Maybe Types.LocalGatewayId)
crLocalGatewayId = Lens.field @"localGatewayId"
{-# INLINEABLE crLocalGatewayId #-}
{-# DEPRECATED localGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead"  #-}

-- | [IPv4 traffic only] The ID of a NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crNatGatewayId :: Lens.Lens' CreateRoute (Core.Maybe Types.NatGatewayId)
crNatGatewayId = Lens.field @"natGatewayId"
{-# INLINEABLE crNatGatewayId #-}
{-# DEPRECATED natGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead"  #-}

-- | The ID of a network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crNetworkInterfaceId :: Lens.Lens' CreateRoute (Core.Maybe Types.NetworkInterfaceId)
crNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE crNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The ID of a transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTransitGatewayId :: Lens.Lens' CreateRoute (Core.Maybe Types.TransitGatewayId)
crTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE crTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

-- | The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints only.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crVpcEndpointId :: Lens.Lens' CreateRoute (Core.Maybe Types.VpcEndpointId)
crVpcEndpointId = Lens.field @"vpcEndpointId"
{-# INLINEABLE crVpcEndpointId #-}
{-# DEPRECATED vpcEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead"  #-}

-- | The ID of a VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crVpcPeeringConnectionId :: Lens.Lens' CreateRoute (Core.Maybe Types.VpcPeeringConnectionId)
crVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# INLINEABLE crVpcPeeringConnectionId #-}
{-# DEPRECATED vpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead"  #-}

instance Core.ToQuery CreateRoute where
        toQuery CreateRoute{..}
          = Core.toQueryPair "Action" ("CreateRoute" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "RouteTableId" routeTableId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CarrierGatewayId")
                carrierGatewayId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DestinationCidrBlock")
                destinationCidrBlock
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "DestinationIpv6CidrBlock")
                destinationIpv6CidrBlock
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DestinationPrefixListId")
                destinationPrefixListId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "EgressOnlyInternetGatewayId")
                egressOnlyInternetGatewayId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GatewayId") gatewayId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceId") instanceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LocalGatewayId")
                localGatewayId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NatGatewayId")
                natGatewayId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NetworkInterfaceId")
                networkInterfaceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TransitGatewayId")
                transitGatewayId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VpcEndpointId")
                vpcEndpointId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VpcPeeringConnectionId")
                vpcPeeringConnectionId

instance Core.ToHeaders CreateRoute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateRoute where
        type Rs CreateRoute = CreateRouteResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateRouteResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateRouteResponse' smart constructor.
data CreateRouteResponse = CreateRouteResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRouteResponse' value with any optional fields omitted.
mkCreateRouteResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateRouteResponse
mkCreateRouteResponse responseStatus
  = CreateRouteResponse'{return = Core.Nothing, responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsReturn :: Lens.Lens' CreateRouteResponse (Core.Maybe Core.Bool)
crrrsReturn = Lens.field @"return"
{-# INLINEABLE crrrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateRouteResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
