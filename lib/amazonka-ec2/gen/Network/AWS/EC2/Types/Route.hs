{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Route
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Route
  ( Route (..)
  -- * Smart constructor
  , mkRoute
  -- * Lenses
  , rCarrierGatewayId
  , rDestinationCidrBlock
  , rDestinationIpv6CidrBlock
  , rDestinationPrefixListId
  , rEgressOnlyInternetGatewayId
  , rGatewayId
  , rInstanceId
  , rInstanceOwnerId
  , rLocalGatewayId
  , rNatGatewayId
  , rNetworkInterfaceId
  , rOrigin
  , rState
  , rTransitGatewayId
  , rVpcPeeringConnectionId
  ) where

import qualified Network.AWS.EC2.Types.CarrierGatewayId as Types
import qualified Network.AWS.EC2.Types.RouteOrigin as Types
import qualified Network.AWS.EC2.Types.RouteState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a route in a route table.
--
-- /See:/ 'mkRoute' smart constructor.
data Route = Route'
  { carrierGatewayId :: Core.Maybe Types.CarrierGatewayId
    -- ^ The ID of the carrier gateway.
  , destinationCidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv4 CIDR block used for the destination match.
  , destinationIpv6CidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR block used for the destination match.
  , destinationPrefixListId :: Core.Maybe Core.Text
    -- ^ The prefix of the AWS service.
  , egressOnlyInternetGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the egress-only internet gateway.
  , gatewayId :: Core.Maybe Core.Text
    -- ^ The ID of a gateway attached to your VPC.
  , instanceId :: Core.Maybe Core.Text
    -- ^ The ID of a NAT instance in your VPC.
  , instanceOwnerId :: Core.Maybe Core.Text
    -- ^ The AWS account ID of the owner of the instance.
  , localGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the local gateway.
  , natGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of a NAT gateway.
  , networkInterfaceId :: Core.Maybe Core.Text
    -- ^ The ID of the network interface.
  , origin :: Core.Maybe Types.RouteOrigin
    -- ^ Describes how the route was created.
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
  , state :: Core.Maybe Types.RouteState
    -- ^ The state of the route. The @blackhole@ state indicates that the route's target isn't available (for example, the specified gateway isn't attached to the VPC, or the specified NAT instance has been terminated).
  , transitGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of a transit gateway.
  , vpcPeeringConnectionId :: Core.Maybe Core.Text
    -- ^ The ID of a VPC peering connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Route' value with any optional fields omitted.
mkRoute
    :: Route
mkRoute
  = Route'{carrierGatewayId = Core.Nothing,
           destinationCidrBlock = Core.Nothing,
           destinationIpv6CidrBlock = Core.Nothing,
           destinationPrefixListId = Core.Nothing,
           egressOnlyInternetGatewayId = Core.Nothing,
           gatewayId = Core.Nothing, instanceId = Core.Nothing,
           instanceOwnerId = Core.Nothing, localGatewayId = Core.Nothing,
           natGatewayId = Core.Nothing, networkInterfaceId = Core.Nothing,
           origin = Core.Nothing, state = Core.Nothing,
           transitGatewayId = Core.Nothing,
           vpcPeeringConnectionId = Core.Nothing}

-- | The ID of the carrier gateway.
--
-- /Note:/ Consider using 'carrierGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCarrierGatewayId :: Lens.Lens' Route (Core.Maybe Types.CarrierGatewayId)
rCarrierGatewayId = Lens.field @"carrierGatewayId"
{-# INLINEABLE rCarrierGatewayId #-}
{-# DEPRECATED carrierGatewayId "Use generic-lens or generic-optics with 'carrierGatewayId' instead"  #-}

-- | The IPv4 CIDR block used for the destination match.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDestinationCidrBlock :: Lens.Lens' Route (Core.Maybe Core.Text)
rDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE rDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The IPv6 CIDR block used for the destination match.
--
-- /Note:/ Consider using 'destinationIpv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDestinationIpv6CidrBlock :: Lens.Lens' Route (Core.Maybe Core.Text)
rDestinationIpv6CidrBlock = Lens.field @"destinationIpv6CidrBlock"
{-# INLINEABLE rDestinationIpv6CidrBlock #-}
{-# DEPRECATED destinationIpv6CidrBlock "Use generic-lens or generic-optics with 'destinationIpv6CidrBlock' instead"  #-}

-- | The prefix of the AWS service.
--
-- /Note:/ Consider using 'destinationPrefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDestinationPrefixListId :: Lens.Lens' Route (Core.Maybe Core.Text)
rDestinationPrefixListId = Lens.field @"destinationPrefixListId"
{-# INLINEABLE rDestinationPrefixListId #-}
{-# DEPRECATED destinationPrefixListId "Use generic-lens or generic-optics with 'destinationPrefixListId' instead"  #-}

-- | The ID of the egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEgressOnlyInternetGatewayId :: Lens.Lens' Route (Core.Maybe Core.Text)
rEgressOnlyInternetGatewayId = Lens.field @"egressOnlyInternetGatewayId"
{-# INLINEABLE rEgressOnlyInternetGatewayId #-}
{-# DEPRECATED egressOnlyInternetGatewayId "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayId' instead"  #-}

-- | The ID of a gateway attached to your VPC.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rGatewayId :: Lens.Lens' Route (Core.Maybe Core.Text)
rGatewayId = Lens.field @"gatewayId"
{-# INLINEABLE rGatewayId #-}
{-# DEPRECATED gatewayId "Use generic-lens or generic-optics with 'gatewayId' instead"  #-}

-- | The ID of a NAT instance in your VPC.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceId :: Lens.Lens' Route (Core.Maybe Core.Text)
rInstanceId = Lens.field @"instanceId"
{-# INLINEABLE rInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The AWS account ID of the owner of the instance.
--
-- /Note:/ Consider using 'instanceOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceOwnerId :: Lens.Lens' Route (Core.Maybe Core.Text)
rInstanceOwnerId = Lens.field @"instanceOwnerId"
{-# INLINEABLE rInstanceOwnerId #-}
{-# DEPRECATED instanceOwnerId "Use generic-lens or generic-optics with 'instanceOwnerId' instead"  #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLocalGatewayId :: Lens.Lens' Route (Core.Maybe Core.Text)
rLocalGatewayId = Lens.field @"localGatewayId"
{-# INLINEABLE rLocalGatewayId #-}
{-# DEPRECATED localGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead"  #-}

-- | The ID of a NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNatGatewayId :: Lens.Lens' Route (Core.Maybe Core.Text)
rNatGatewayId = Lens.field @"natGatewayId"
{-# INLINEABLE rNatGatewayId #-}
{-# DEPRECATED natGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead"  #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNetworkInterfaceId :: Lens.Lens' Route (Core.Maybe Core.Text)
rNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE rNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

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
{-# INLINEABLE rOrigin #-}
{-# DEPRECATED origin "Use generic-lens or generic-optics with 'origin' instead"  #-}

-- | The state of the route. The @blackhole@ state indicates that the route's target isn't available (for example, the specified gateway isn't attached to the VPC, or the specified NAT instance has been terminated).
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rState :: Lens.Lens' Route (Core.Maybe Types.RouteState)
rState = Lens.field @"state"
{-# INLINEABLE rState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The ID of a transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTransitGatewayId :: Lens.Lens' Route (Core.Maybe Core.Text)
rTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE rTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

-- | The ID of a VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rVpcPeeringConnectionId :: Lens.Lens' Route (Core.Maybe Core.Text)
rVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# INLINEABLE rVpcPeeringConnectionId #-}
{-# DEPRECATED vpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead"  #-}

instance Core.FromXML Route where
        parseXML x
          = Route' Core.<$>
              (x Core..@? "carrierGatewayId") Core.<*>
                x Core..@? "destinationCidrBlock"
                Core.<*> x Core..@? "destinationIpv6CidrBlock"
                Core.<*> x Core..@? "destinationPrefixListId"
                Core.<*> x Core..@? "egressOnlyInternetGatewayId"
                Core.<*> x Core..@? "gatewayId"
                Core.<*> x Core..@? "instanceId"
                Core.<*> x Core..@? "instanceOwnerId"
                Core.<*> x Core..@? "localGatewayId"
                Core.<*> x Core..@? "natGatewayId"
                Core.<*> x Core..@? "networkInterfaceId"
                Core.<*> x Core..@? "origin"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "transitGatewayId"
                Core.<*> x Core..@? "vpcPeeringConnectionId"
