{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ReplaceRoute (..)
    , mkReplaceRoute
    -- ** Request lenses
    , rrRouteTableId
    , rrCarrierGatewayId
    , rrDestinationCidrBlock
    , rrDestinationIpv6CidrBlock
    , rrDestinationPrefixListId
    , rrDryRun
    , rrEgressOnlyInternetGatewayId
    , rrGatewayId
    , rrInstanceId
    , rrLocalGatewayId
    , rrLocalTarget
    , rrNatGatewayId
    , rrNetworkInterfaceId
    , rrTransitGatewayId
    , rrVpcEndpointId
    , rrVpcPeeringConnectionId

    -- * Destructuring the response
    , ReplaceRouteResponse (..)
    , mkReplaceRouteResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkReplaceRoute' smart constructor.
data ReplaceRoute = ReplaceRoute'
  { routeTableId :: Types.RouteTableId
    -- ^ The ID of the route table.
  , carrierGatewayId :: Core.Maybe Types.CarrierGatewayId
    -- ^ [IPv4 traffic only] The ID of a carrier gateway.
  , destinationCidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv4 CIDR address block used for the destination match. The value that you provide must match the CIDR of an existing route in the table.
  , destinationIpv6CidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR address block used for the destination match. The value that you provide must match the CIDR of an existing route in the table.
  , destinationPrefixListId :: Core.Maybe Types.DestinationPrefixListId
    -- ^ The ID of the prefix list for the route.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , egressOnlyInternetGatewayId :: Core.Maybe Types.EgressOnlyInternetGatewayId
    -- ^ [IPv6 traffic only] The ID of an egress-only internet gateway.
  , gatewayId :: Core.Maybe Types.GatewayId
    -- ^ The ID of an internet gateway or virtual private gateway.
  , instanceId :: Core.Maybe Types.InstanceId
    -- ^ The ID of a NAT instance in your VPC.
  , localGatewayId :: Core.Maybe Types.LocalGatewayId
    -- ^ The ID of the local gateway.
  , localTarget :: Core.Maybe Core.Bool
    -- ^ Specifies whether to reset the local route to its default target (@local@ ).
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

-- | Creates a 'ReplaceRoute' value with any optional fields omitted.
mkReplaceRoute
    :: Types.RouteTableId -- ^ 'routeTableId'
    -> ReplaceRoute
mkReplaceRoute routeTableId
  = ReplaceRoute'{routeTableId, carrierGatewayId = Core.Nothing,
                  destinationCidrBlock = Core.Nothing,
                  destinationIpv6CidrBlock = Core.Nothing,
                  destinationPrefixListId = Core.Nothing, dryRun = Core.Nothing,
                  egressOnlyInternetGatewayId = Core.Nothing,
                  gatewayId = Core.Nothing, instanceId = Core.Nothing,
                  localGatewayId = Core.Nothing, localTarget = Core.Nothing,
                  natGatewayId = Core.Nothing, networkInterfaceId = Core.Nothing,
                  transitGatewayId = Core.Nothing, vpcEndpointId = Core.Nothing,
                  vpcPeeringConnectionId = Core.Nothing}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrRouteTableId :: Lens.Lens' ReplaceRoute Types.RouteTableId
rrRouteTableId = Lens.field @"routeTableId"
{-# INLINEABLE rrRouteTableId #-}
{-# DEPRECATED routeTableId "Use generic-lens or generic-optics with 'routeTableId' instead"  #-}

-- | [IPv4 traffic only] The ID of a carrier gateway.
--
-- /Note:/ Consider using 'carrierGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrCarrierGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.CarrierGatewayId)
rrCarrierGatewayId = Lens.field @"carrierGatewayId"
{-# INLINEABLE rrCarrierGatewayId #-}
{-# DEPRECATED carrierGatewayId "Use generic-lens or generic-optics with 'carrierGatewayId' instead"  #-}

-- | The IPv4 CIDR address block used for the destination match. The value that you provide must match the CIDR of an existing route in the table.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDestinationCidrBlock :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
rrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE rrDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The IPv6 CIDR address block used for the destination match. The value that you provide must match the CIDR of an existing route in the table.
--
-- /Note:/ Consider using 'destinationIpv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDestinationIpv6CidrBlock :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
rrDestinationIpv6CidrBlock = Lens.field @"destinationIpv6CidrBlock"
{-# INLINEABLE rrDestinationIpv6CidrBlock #-}
{-# DEPRECATED destinationIpv6CidrBlock "Use generic-lens or generic-optics with 'destinationIpv6CidrBlock' instead"  #-}

-- | The ID of the prefix list for the route.
--
-- /Note:/ Consider using 'destinationPrefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDestinationPrefixListId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.DestinationPrefixListId)
rrDestinationPrefixListId = Lens.field @"destinationPrefixListId"
{-# INLINEABLE rrDestinationPrefixListId #-}
{-# DEPRECATED destinationPrefixListId "Use generic-lens or generic-optics with 'destinationPrefixListId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDryRun :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Bool)
rrDryRun = Lens.field @"dryRun"
{-# INLINEABLE rrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | [IPv6 traffic only] The ID of an egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrEgressOnlyInternetGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.EgressOnlyInternetGatewayId)
rrEgressOnlyInternetGatewayId = Lens.field @"egressOnlyInternetGatewayId"
{-# INLINEABLE rrEgressOnlyInternetGatewayId #-}
{-# DEPRECATED egressOnlyInternetGatewayId "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayId' instead"  #-}

-- | The ID of an internet gateway or virtual private gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.GatewayId)
rrGatewayId = Lens.field @"gatewayId"
{-# INLINEABLE rrGatewayId #-}
{-# DEPRECATED gatewayId "Use generic-lens or generic-optics with 'gatewayId' instead"  #-}

-- | The ID of a NAT instance in your VPC.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrInstanceId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.InstanceId)
rrInstanceId = Lens.field @"instanceId"
{-# INLINEABLE rrInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrLocalGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.LocalGatewayId)
rrLocalGatewayId = Lens.field @"localGatewayId"
{-# INLINEABLE rrLocalGatewayId #-}
{-# DEPRECATED localGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead"  #-}

-- | Specifies whether to reset the local route to its default target (@local@ ).
--
-- /Note:/ Consider using 'localTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrLocalTarget :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Bool)
rrLocalTarget = Lens.field @"localTarget"
{-# INLINEABLE rrLocalTarget #-}
{-# DEPRECATED localTarget "Use generic-lens or generic-optics with 'localTarget' instead"  #-}

-- | [IPv4 traffic only] The ID of a NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrNatGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.NatGatewayId)
rrNatGatewayId = Lens.field @"natGatewayId"
{-# INLINEABLE rrNatGatewayId #-}
{-# DEPRECATED natGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead"  #-}

-- | The ID of a network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrNetworkInterfaceId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.NetworkInterfaceId)
rrNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE rrNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The ID of a transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrTransitGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.TransitGatewayId)
rrTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE rrTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

-- | The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints only.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrVpcEndpointId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.VpcEndpointId)
rrVpcEndpointId = Lens.field @"vpcEndpointId"
{-# INLINEABLE rrVpcEndpointId #-}
{-# DEPRECATED vpcEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead"  #-}

-- | The ID of a VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrVpcPeeringConnectionId :: Lens.Lens' ReplaceRoute (Core.Maybe Types.VpcPeeringConnectionId)
rrVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# INLINEABLE rrVpcPeeringConnectionId #-}
{-# DEPRECATED vpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead"  #-}

instance Core.ToQuery ReplaceRoute where
        toQuery ReplaceRoute{..}
          = Core.toQueryPair "Action" ("ReplaceRoute" :: Core.Text) Core.<>
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
              Core.maybe Core.mempty (Core.toQueryPair "LocalTarget") localTarget
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

instance Core.ToHeaders ReplaceRoute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ReplaceRoute where
        type Rs ReplaceRoute = ReplaceRouteResponse
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
        parseResponse = Response.receiveNull ReplaceRouteResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkReplaceRouteResponse' smart constructor.
data ReplaceRouteResponse = ReplaceRouteResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceRouteResponse' value with any optional fields omitted.
mkReplaceRouteResponse
    :: ReplaceRouteResponse
mkReplaceRouteResponse = ReplaceRouteResponse'
