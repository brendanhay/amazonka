{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Route
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Route
  ( Route (..),

    -- * Smart constructor
    mkRoute,

    -- * Lenses
    rCarrierGatewayId,
    rDestinationCidrBlock,
    rDestinationIpv6CidrBlock,
    rDestinationPrefixListId,
    rEgressOnlyInternetGatewayId,
    rGatewayId,
    rInstanceId,
    rInstanceOwnerId,
    rLocalGatewayId,
    rNatGatewayId,
    rNetworkInterfaceId,
    rOrigin,
    rState,
    rTransitGatewayId,
    rVpcPeeringConnectionId,
  )
where

import qualified Network.AWS.EC2.Types.CarrierGatewayId as Types
import qualified Network.AWS.EC2.Types.RouteOrigin as Types
import qualified Network.AWS.EC2.Types.RouteState as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a route in a route table.
--
-- /See:/ 'mkRoute' smart constructor.
data Route = Route'
  { -- | The ID of the carrier gateway.
    carrierGatewayId :: Core.Maybe Types.CarrierGatewayId,
    -- | The IPv4 CIDR block used for the destination match.
    destinationCidrBlock :: Core.Maybe Types.String,
    -- | The IPv6 CIDR block used for the destination match.
    destinationIpv6CidrBlock :: Core.Maybe Types.String,
    -- | The prefix of the AWS service.
    destinationPrefixListId :: Core.Maybe Types.String,
    -- | The ID of the egress-only internet gateway.
    egressOnlyInternetGatewayId :: Core.Maybe Types.String,
    -- | The ID of a gateway attached to your VPC.
    gatewayId :: Core.Maybe Types.String,
    -- | The ID of a NAT instance in your VPC.
    instanceId :: Core.Maybe Types.String,
    -- | The AWS account ID of the owner of the instance.
    instanceOwnerId :: Core.Maybe Types.String,
    -- | The ID of the local gateway.
    localGatewayId :: Core.Maybe Types.String,
    -- | The ID of a NAT gateway.
    natGatewayId :: Core.Maybe Types.String,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Types.String,
    -- | Describes how the route was created.
    --
    --
    --     * @CreateRouteTable@ - The route was automatically created when the route table was created.
    --
    --
    --     * @CreateRoute@ - The route was manually added to the route table.
    --
    --
    --     * @EnableVgwRoutePropagation@ - The route was propagated by route propagation.
    origin :: Core.Maybe Types.RouteOrigin,
    -- | The state of the route. The @blackhole@ state indicates that the route's target isn't available (for example, the specified gateway isn't attached to the VPC, or the specified NAT instance has been terminated).
    state :: Core.Maybe Types.RouteState,
    -- | The ID of a transit gateway.
    transitGatewayId :: Core.Maybe Types.String,
    -- | The ID of a VPC peering connection.
    vpcPeeringConnectionId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Route' value with any optional fields omitted.
mkRoute ::
  Route
mkRoute =
  Route'
    { carrierGatewayId = Core.Nothing,
      destinationCidrBlock = Core.Nothing,
      destinationIpv6CidrBlock = Core.Nothing,
      destinationPrefixListId = Core.Nothing,
      egressOnlyInternetGatewayId = Core.Nothing,
      gatewayId = Core.Nothing,
      instanceId = Core.Nothing,
      instanceOwnerId = Core.Nothing,
      localGatewayId = Core.Nothing,
      natGatewayId = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      origin = Core.Nothing,
      state = Core.Nothing,
      transitGatewayId = Core.Nothing,
      vpcPeeringConnectionId = Core.Nothing
    }

-- | The ID of the carrier gateway.
--
-- /Note:/ Consider using 'carrierGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCarrierGatewayId :: Lens.Lens' Route (Core.Maybe Types.CarrierGatewayId)
rCarrierGatewayId = Lens.field @"carrierGatewayId"
{-# DEPRECATED rCarrierGatewayId "Use generic-lens or generic-optics with 'carrierGatewayId' instead." #-}

-- | The IPv4 CIDR block used for the destination match.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDestinationCidrBlock :: Lens.Lens' Route (Core.Maybe Types.String)
rDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# DEPRECATED rDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The IPv6 CIDR block used for the destination match.
--
-- /Note:/ Consider using 'destinationIpv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDestinationIpv6CidrBlock :: Lens.Lens' Route (Core.Maybe Types.String)
rDestinationIpv6CidrBlock = Lens.field @"destinationIpv6CidrBlock"
{-# DEPRECATED rDestinationIpv6CidrBlock "Use generic-lens or generic-optics with 'destinationIpv6CidrBlock' instead." #-}

-- | The prefix of the AWS service.
--
-- /Note:/ Consider using 'destinationPrefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDestinationPrefixListId :: Lens.Lens' Route (Core.Maybe Types.String)
rDestinationPrefixListId = Lens.field @"destinationPrefixListId"
{-# DEPRECATED rDestinationPrefixListId "Use generic-lens or generic-optics with 'destinationPrefixListId' instead." #-}

-- | The ID of the egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEgressOnlyInternetGatewayId :: Lens.Lens' Route (Core.Maybe Types.String)
rEgressOnlyInternetGatewayId = Lens.field @"egressOnlyInternetGatewayId"
{-# DEPRECATED rEgressOnlyInternetGatewayId "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayId' instead." #-}

-- | The ID of a gateway attached to your VPC.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rGatewayId :: Lens.Lens' Route (Core.Maybe Types.String)
rGatewayId = Lens.field @"gatewayId"
{-# DEPRECATED rGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | The ID of a NAT instance in your VPC.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceId :: Lens.Lens' Route (Core.Maybe Types.String)
rInstanceId = Lens.field @"instanceId"
{-# DEPRECATED rInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The AWS account ID of the owner of the instance.
--
-- /Note:/ Consider using 'instanceOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceOwnerId :: Lens.Lens' Route (Core.Maybe Types.String)
rInstanceOwnerId = Lens.field @"instanceOwnerId"
{-# DEPRECATED rInstanceOwnerId "Use generic-lens or generic-optics with 'instanceOwnerId' instead." #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLocalGatewayId :: Lens.Lens' Route (Core.Maybe Types.String)
rLocalGatewayId = Lens.field @"localGatewayId"
{-# DEPRECATED rLocalGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead." #-}

-- | The ID of a NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNatGatewayId :: Lens.Lens' Route (Core.Maybe Types.String)
rNatGatewayId = Lens.field @"natGatewayId"
{-# DEPRECATED rNatGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNetworkInterfaceId :: Lens.Lens' Route (Core.Maybe Types.String)
rNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED rNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | Describes how the route was created.
--
--
--     * @CreateRouteTable@ - The route was automatically created when the route table was created.
--
--
--     * @CreateRoute@ - The route was manually added to the route table.
--
--
--     * @EnableVgwRoutePropagation@ - The route was propagated by route propagation.
--
--
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOrigin :: Lens.Lens' Route (Core.Maybe Types.RouteOrigin)
rOrigin = Lens.field @"origin"
{-# DEPRECATED rOrigin "Use generic-lens or generic-optics with 'origin' instead." #-}

-- | The state of the route. The @blackhole@ state indicates that the route's target isn't available (for example, the specified gateway isn't attached to the VPC, or the specified NAT instance has been terminated).
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rState :: Lens.Lens' Route (Core.Maybe Types.RouteState)
rState = Lens.field @"state"
{-# DEPRECATED rState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of a transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTransitGatewayId :: Lens.Lens' Route (Core.Maybe Types.String)
rTransitGatewayId = Lens.field @"transitGatewayId"
{-# DEPRECATED rTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The ID of a VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rVpcPeeringConnectionId :: Lens.Lens' Route (Core.Maybe Types.String)
rVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# DEPRECATED rVpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

instance Core.FromXML Route where
  parseXML x =
    Route'
      Core.<$> (x Core..@? "carrierGatewayId")
      Core.<*> (x Core..@? "destinationCidrBlock")
      Core.<*> (x Core..@? "destinationIpv6CidrBlock")
      Core.<*> (x Core..@? "destinationPrefixListId")
      Core.<*> (x Core..@? "egressOnlyInternetGatewayId")
      Core.<*> (x Core..@? "gatewayId")
      Core.<*> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "instanceOwnerId")
      Core.<*> (x Core..@? "localGatewayId")
      Core.<*> (x Core..@? "natGatewayId")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "origin")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "transitGatewayId")
      Core.<*> (x Core..@? "vpcPeeringConnectionId")
