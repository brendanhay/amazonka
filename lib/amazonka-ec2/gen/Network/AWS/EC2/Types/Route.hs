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
    rVPCPeeringConnectionId,
    rInstanceId,
    rOrigin,
    rState,
    rEgressOnlyInternetGatewayId,
    rDestinationIPv6CidrBlock,
    rLocalGatewayId,
    rNatGatewayId,
    rNetworkInterfaceId,
    rTransitGatewayId,
    rGatewayId,
    rInstanceOwnerId,
    rDestinationPrefixListId,
    rCarrierGatewayId,
    rDestinationCidrBlock,
  )
where

import Network.AWS.EC2.Types.RouteOrigin
import Network.AWS.EC2.Types.RouteState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a route in a route table.
--
-- /See:/ 'mkRoute' smart constructor.
data Route = Route'
  { vpcPeeringConnectionId :: Lude.Maybe Lude.Text,
    instanceId :: Lude.Maybe Lude.Text,
    origin :: Lude.Maybe RouteOrigin,
    state :: Lude.Maybe RouteState,
    egressOnlyInternetGatewayId :: Lude.Maybe Lude.Text,
    destinationIPv6CidrBlock :: Lude.Maybe Lude.Text,
    localGatewayId :: Lude.Maybe Lude.Text,
    natGatewayId :: Lude.Maybe Lude.Text,
    networkInterfaceId :: Lude.Maybe Lude.Text,
    transitGatewayId :: Lude.Maybe Lude.Text,
    gatewayId :: Lude.Maybe Lude.Text,
    instanceOwnerId :: Lude.Maybe Lude.Text,
    destinationPrefixListId :: Lude.Maybe Lude.Text,
    carrierGatewayId :: Lude.Maybe Lude.Text,
    destinationCidrBlock :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Route' with the minimum fields required to make a request.
--
-- * 'carrierGatewayId' - The ID of the carrier gateway.
-- * 'destinationCidrBlock' - The IPv4 CIDR block used for the destination match.
-- * 'destinationIPv6CidrBlock' - The IPv6 CIDR block used for the destination match.
-- * 'destinationPrefixListId' - The prefix of the AWS service.
-- * 'egressOnlyInternetGatewayId' - The ID of the egress-only internet gateway.
-- * 'gatewayId' - The ID of a gateway attached to your VPC.
-- * 'instanceId' - The ID of a NAT instance in your VPC.
-- * 'instanceOwnerId' - The AWS account ID of the owner of the instance.
-- * 'localGatewayId' - The ID of the local gateway.
-- * 'natGatewayId' - The ID of a NAT gateway.
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'origin' - Describes how the route was created.
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
-- * 'state' - The state of the route. The @blackhole@ state indicates that the route's target isn't available (for example, the specified gateway isn't attached to the VPC, or the specified NAT instance has been terminated).
-- * 'transitGatewayId' - The ID of a transit gateway.
-- * 'vpcPeeringConnectionId' - The ID of a VPC peering connection.
mkRoute ::
  Route
mkRoute =
  Route'
    { vpcPeeringConnectionId = Lude.Nothing,
      instanceId = Lude.Nothing,
      origin = Lude.Nothing,
      state = Lude.Nothing,
      egressOnlyInternetGatewayId = Lude.Nothing,
      destinationIPv6CidrBlock = Lude.Nothing,
      localGatewayId = Lude.Nothing,
      natGatewayId = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      transitGatewayId = Lude.Nothing,
      gatewayId = Lude.Nothing,
      instanceOwnerId = Lude.Nothing,
      destinationPrefixListId = Lude.Nothing,
      carrierGatewayId = Lude.Nothing,
      destinationCidrBlock = Lude.Nothing
    }

-- | The ID of a VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rVPCPeeringConnectionId :: Lens.Lens' Route (Lude.Maybe Lude.Text)
rVPCPeeringConnectionId = Lens.lens (vpcPeeringConnectionId :: Route -> Lude.Maybe Lude.Text) (\s a -> s {vpcPeeringConnectionId = a} :: Route)
{-# DEPRECATED rVPCPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

-- | The ID of a NAT instance in your VPC.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceId :: Lens.Lens' Route (Lude.Maybe Lude.Text)
rInstanceId = Lens.lens (instanceId :: Route -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: Route)
{-# DEPRECATED rInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

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
rOrigin :: Lens.Lens' Route (Lude.Maybe RouteOrigin)
rOrigin = Lens.lens (origin :: Route -> Lude.Maybe RouteOrigin) (\s a -> s {origin = a} :: Route)
{-# DEPRECATED rOrigin "Use generic-lens or generic-optics with 'origin' instead." #-}

-- | The state of the route. The @blackhole@ state indicates that the route's target isn't available (for example, the specified gateway isn't attached to the VPC, or the specified NAT instance has been terminated).
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rState :: Lens.Lens' Route (Lude.Maybe RouteState)
rState = Lens.lens (state :: Route -> Lude.Maybe RouteState) (\s a -> s {state = a} :: Route)
{-# DEPRECATED rState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEgressOnlyInternetGatewayId :: Lens.Lens' Route (Lude.Maybe Lude.Text)
rEgressOnlyInternetGatewayId = Lens.lens (egressOnlyInternetGatewayId :: Route -> Lude.Maybe Lude.Text) (\s a -> s {egressOnlyInternetGatewayId = a} :: Route)
{-# DEPRECATED rEgressOnlyInternetGatewayId "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayId' instead." #-}

-- | The IPv6 CIDR block used for the destination match.
--
-- /Note:/ Consider using 'destinationIPv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDestinationIPv6CidrBlock :: Lens.Lens' Route (Lude.Maybe Lude.Text)
rDestinationIPv6CidrBlock = Lens.lens (destinationIPv6CidrBlock :: Route -> Lude.Maybe Lude.Text) (\s a -> s {destinationIPv6CidrBlock = a} :: Route)
{-# DEPRECATED rDestinationIPv6CidrBlock "Use generic-lens or generic-optics with 'destinationIPv6CidrBlock' instead." #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLocalGatewayId :: Lens.Lens' Route (Lude.Maybe Lude.Text)
rLocalGatewayId = Lens.lens (localGatewayId :: Route -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayId = a} :: Route)
{-# DEPRECATED rLocalGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead." #-}

-- | The ID of a NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNatGatewayId :: Lens.Lens' Route (Lude.Maybe Lude.Text)
rNatGatewayId = Lens.lens (natGatewayId :: Route -> Lude.Maybe Lude.Text) (\s a -> s {natGatewayId = a} :: Route)
{-# DEPRECATED rNatGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNetworkInterfaceId :: Lens.Lens' Route (Lude.Maybe Lude.Text)
rNetworkInterfaceId = Lens.lens (networkInterfaceId :: Route -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: Route)
{-# DEPRECATED rNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of a transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTransitGatewayId :: Lens.Lens' Route (Lude.Maybe Lude.Text)
rTransitGatewayId = Lens.lens (transitGatewayId :: Route -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayId = a} :: Route)
{-# DEPRECATED rTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The ID of a gateway attached to your VPC.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rGatewayId :: Lens.Lens' Route (Lude.Maybe Lude.Text)
rGatewayId = Lens.lens (gatewayId :: Route -> Lude.Maybe Lude.Text) (\s a -> s {gatewayId = a} :: Route)
{-# DEPRECATED rGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | The AWS account ID of the owner of the instance.
--
-- /Note:/ Consider using 'instanceOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstanceOwnerId :: Lens.Lens' Route (Lude.Maybe Lude.Text)
rInstanceOwnerId = Lens.lens (instanceOwnerId :: Route -> Lude.Maybe Lude.Text) (\s a -> s {instanceOwnerId = a} :: Route)
{-# DEPRECATED rInstanceOwnerId "Use generic-lens or generic-optics with 'instanceOwnerId' instead." #-}

-- | The prefix of the AWS service.
--
-- /Note:/ Consider using 'destinationPrefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDestinationPrefixListId :: Lens.Lens' Route (Lude.Maybe Lude.Text)
rDestinationPrefixListId = Lens.lens (destinationPrefixListId :: Route -> Lude.Maybe Lude.Text) (\s a -> s {destinationPrefixListId = a} :: Route)
{-# DEPRECATED rDestinationPrefixListId "Use generic-lens or generic-optics with 'destinationPrefixListId' instead." #-}

-- | The ID of the carrier gateway.
--
-- /Note:/ Consider using 'carrierGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCarrierGatewayId :: Lens.Lens' Route (Lude.Maybe Lude.Text)
rCarrierGatewayId = Lens.lens (carrierGatewayId :: Route -> Lude.Maybe Lude.Text) (\s a -> s {carrierGatewayId = a} :: Route)
{-# DEPRECATED rCarrierGatewayId "Use generic-lens or generic-optics with 'carrierGatewayId' instead." #-}

-- | The IPv4 CIDR block used for the destination match.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDestinationCidrBlock :: Lens.Lens' Route (Lude.Maybe Lude.Text)
rDestinationCidrBlock = Lens.lens (destinationCidrBlock :: Route -> Lude.Maybe Lude.Text) (\s a -> s {destinationCidrBlock = a} :: Route)
{-# DEPRECATED rDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

instance Lude.FromXML Route where
  parseXML x =
    Route'
      Lude.<$> (x Lude..@? "vpcPeeringConnectionId")
      Lude.<*> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "origin")
      Lude.<*> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "egressOnlyInternetGatewayId")
      Lude.<*> (x Lude..@? "destinationIpv6CidrBlock")
      Lude.<*> (x Lude..@? "localGatewayId")
      Lude.<*> (x Lude..@? "natGatewayId")
      Lude.<*> (x Lude..@? "networkInterfaceId")
      Lude.<*> (x Lude..@? "transitGatewayId")
      Lude.<*> (x Lude..@? "gatewayId")
      Lude.<*> (x Lude..@? "instanceOwnerId")
      Lude.<*> (x Lude..@? "destinationPrefixListId")
      Lude.<*> (x Lude..@? "carrierGatewayId")
      Lude.<*> (x Lude..@? "destinationCidrBlock")
