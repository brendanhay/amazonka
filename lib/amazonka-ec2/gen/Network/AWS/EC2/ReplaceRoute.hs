{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    rrVPCPeeringConnectionId,
    rrInstanceId,
    rrEgressOnlyInternetGatewayId,
    rrDestinationIPv6CidrBlock,
    rrLocalGatewayId,
    rrNatGatewayId,
    rrNetworkInterfaceId,
    rrLocalTarget,
    rrTransitGatewayId,
    rrGatewayId,
    rrVPCEndpointId,
    rrDestinationPrefixListId,
    rrDryRun,
    rrCarrierGatewayId,
    rrDestinationCidrBlock,
    rrRouteTableId,

    -- * Destructuring the response
    ReplaceRouteResponse (..),
    mkReplaceRouteResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkReplaceRoute' smart constructor.
data ReplaceRoute = ReplaceRoute'
  { vpcPeeringConnectionId ::
      Lude.Maybe Lude.Text,
    instanceId :: Lude.Maybe Lude.Text,
    egressOnlyInternetGatewayId :: Lude.Maybe Lude.Text,
    destinationIPv6CidrBlock :: Lude.Maybe Lude.Text,
    localGatewayId :: Lude.Maybe Lude.Text,
    natGatewayId :: Lude.Maybe Lude.Text,
    networkInterfaceId :: Lude.Maybe Lude.Text,
    localTarget :: Lude.Maybe Lude.Bool,
    transitGatewayId :: Lude.Maybe Lude.Text,
    gatewayId :: Lude.Maybe Lude.Text,
    vpcEndpointId :: Lude.Maybe Lude.Text,
    destinationPrefixListId :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    carrierGatewayId :: Lude.Maybe Lude.Text,
    destinationCidrBlock :: Lude.Maybe Lude.Text,
    routeTableId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceRoute' with the minimum fields required to make a request.
--
-- * 'carrierGatewayId' - [IPv4 traffic only] The ID of a carrier gateway.
-- * 'destinationCidrBlock' - The IPv4 CIDR address block used for the destination match. The value that you provide must match the CIDR of an existing route in the table.
-- * 'destinationIPv6CidrBlock' - The IPv6 CIDR address block used for the destination match. The value that you provide must match the CIDR of an existing route in the table.
-- * 'destinationPrefixListId' - The ID of the prefix list for the route.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'egressOnlyInternetGatewayId' - [IPv6 traffic only] The ID of an egress-only internet gateway.
-- * 'gatewayId' - The ID of an internet gateway or virtual private gateway.
-- * 'instanceId' - The ID of a NAT instance in your VPC.
-- * 'localGatewayId' - The ID of the local gateway.
-- * 'localTarget' - Specifies whether to reset the local route to its default target (@local@ ).
-- * 'natGatewayId' - [IPv4 traffic only] The ID of a NAT gateway.
-- * 'networkInterfaceId' - The ID of a network interface.
-- * 'routeTableId' - The ID of the route table.
-- * 'transitGatewayId' - The ID of a transit gateway.
-- * 'vpcEndpointId' - The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints only.
-- * 'vpcPeeringConnectionId' - The ID of a VPC peering connection.
mkReplaceRoute ::
  -- | 'routeTableId'
  Lude.Text ->
  ReplaceRoute
mkReplaceRoute pRouteTableId_ =
  ReplaceRoute'
    { vpcPeeringConnectionId = Lude.Nothing,
      instanceId = Lude.Nothing,
      egressOnlyInternetGatewayId = Lude.Nothing,
      destinationIPv6CidrBlock = Lude.Nothing,
      localGatewayId = Lude.Nothing,
      natGatewayId = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      localTarget = Lude.Nothing,
      transitGatewayId = Lude.Nothing,
      gatewayId = Lude.Nothing,
      vpcEndpointId = Lude.Nothing,
      destinationPrefixListId = Lude.Nothing,
      dryRun = Lude.Nothing,
      carrierGatewayId = Lude.Nothing,
      destinationCidrBlock = Lude.Nothing,
      routeTableId = pRouteTableId_
    }

-- | The ID of a VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrVPCPeeringConnectionId :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Text)
rrVPCPeeringConnectionId = Lens.lens (vpcPeeringConnectionId :: ReplaceRoute -> Lude.Maybe Lude.Text) (\s a -> s {vpcPeeringConnectionId = a} :: ReplaceRoute)
{-# DEPRECATED rrVPCPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

-- | The ID of a NAT instance in your VPC.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrInstanceId :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Text)
rrInstanceId = Lens.lens (instanceId :: ReplaceRoute -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: ReplaceRoute)
{-# DEPRECATED rrInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | [IPv6 traffic only] The ID of an egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrEgressOnlyInternetGatewayId :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Text)
rrEgressOnlyInternetGatewayId = Lens.lens (egressOnlyInternetGatewayId :: ReplaceRoute -> Lude.Maybe Lude.Text) (\s a -> s {egressOnlyInternetGatewayId = a} :: ReplaceRoute)
{-# DEPRECATED rrEgressOnlyInternetGatewayId "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayId' instead." #-}

-- | The IPv6 CIDR address block used for the destination match. The value that you provide must match the CIDR of an existing route in the table.
--
-- /Note:/ Consider using 'destinationIPv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDestinationIPv6CidrBlock :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Text)
rrDestinationIPv6CidrBlock = Lens.lens (destinationIPv6CidrBlock :: ReplaceRoute -> Lude.Maybe Lude.Text) (\s a -> s {destinationIPv6CidrBlock = a} :: ReplaceRoute)
{-# DEPRECATED rrDestinationIPv6CidrBlock "Use generic-lens or generic-optics with 'destinationIPv6CidrBlock' instead." #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrLocalGatewayId :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Text)
rrLocalGatewayId = Lens.lens (localGatewayId :: ReplaceRoute -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayId = a} :: ReplaceRoute)
{-# DEPRECATED rrLocalGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead." #-}

-- | [IPv4 traffic only] The ID of a NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrNatGatewayId :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Text)
rrNatGatewayId = Lens.lens (natGatewayId :: ReplaceRoute -> Lude.Maybe Lude.Text) (\s a -> s {natGatewayId = a} :: ReplaceRoute)
{-# DEPRECATED rrNatGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead." #-}

-- | The ID of a network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrNetworkInterfaceId :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Text)
rrNetworkInterfaceId = Lens.lens (networkInterfaceId :: ReplaceRoute -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: ReplaceRoute)
{-# DEPRECATED rrNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | Specifies whether to reset the local route to its default target (@local@ ).
--
-- /Note:/ Consider using 'localTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrLocalTarget :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Bool)
rrLocalTarget = Lens.lens (localTarget :: ReplaceRoute -> Lude.Maybe Lude.Bool) (\s a -> s {localTarget = a} :: ReplaceRoute)
{-# DEPRECATED rrLocalTarget "Use generic-lens or generic-optics with 'localTarget' instead." #-}

-- | The ID of a transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrTransitGatewayId :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Text)
rrTransitGatewayId = Lens.lens (transitGatewayId :: ReplaceRoute -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayId = a} :: ReplaceRoute)
{-# DEPRECATED rrTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The ID of an internet gateway or virtual private gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrGatewayId :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Text)
rrGatewayId = Lens.lens (gatewayId :: ReplaceRoute -> Lude.Maybe Lude.Text) (\s a -> s {gatewayId = a} :: ReplaceRoute)
{-# DEPRECATED rrGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints only.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrVPCEndpointId :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Text)
rrVPCEndpointId = Lens.lens (vpcEndpointId :: ReplaceRoute -> Lude.Maybe Lude.Text) (\s a -> s {vpcEndpointId = a} :: ReplaceRoute)
{-# DEPRECATED rrVPCEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead." #-}

-- | The ID of the prefix list for the route.
--
-- /Note:/ Consider using 'destinationPrefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDestinationPrefixListId :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Text)
rrDestinationPrefixListId = Lens.lens (destinationPrefixListId :: ReplaceRoute -> Lude.Maybe Lude.Text) (\s a -> s {destinationPrefixListId = a} :: ReplaceRoute)
{-# DEPRECATED rrDestinationPrefixListId "Use generic-lens or generic-optics with 'destinationPrefixListId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDryRun :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Bool)
rrDryRun = Lens.lens (dryRun :: ReplaceRoute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ReplaceRoute)
{-# DEPRECATED rrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | [IPv4 traffic only] The ID of a carrier gateway.
--
-- /Note:/ Consider using 'carrierGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrCarrierGatewayId :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Text)
rrCarrierGatewayId = Lens.lens (carrierGatewayId :: ReplaceRoute -> Lude.Maybe Lude.Text) (\s a -> s {carrierGatewayId = a} :: ReplaceRoute)
{-# DEPRECATED rrCarrierGatewayId "Use generic-lens or generic-optics with 'carrierGatewayId' instead." #-}

-- | The IPv4 CIDR address block used for the destination match. The value that you provide must match the CIDR of an existing route in the table.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDestinationCidrBlock :: Lens.Lens' ReplaceRoute (Lude.Maybe Lude.Text)
rrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: ReplaceRoute -> Lude.Maybe Lude.Text) (\s a -> s {destinationCidrBlock = a} :: ReplaceRoute)
{-# DEPRECATED rrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrRouteTableId :: Lens.Lens' ReplaceRoute Lude.Text
rrRouteTableId = Lens.lens (routeTableId :: ReplaceRoute -> Lude.Text) (\s a -> s {routeTableId = a} :: ReplaceRoute)
{-# DEPRECATED rrRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

instance Lude.AWSRequest ReplaceRoute where
  type Rs ReplaceRoute = ReplaceRouteResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ReplaceRouteResponse'

instance Lude.ToHeaders ReplaceRoute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ReplaceRoute where
  toPath = Lude.const "/"

instance Lude.ToQuery ReplaceRoute where
  toQuery ReplaceRoute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ReplaceRoute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpcPeeringConnectionId" Lude.=: vpcPeeringConnectionId,
        "InstanceId" Lude.=: instanceId,
        "EgressOnlyInternetGatewayId" Lude.=: egressOnlyInternetGatewayId,
        "DestinationIpv6CidrBlock" Lude.=: destinationIPv6CidrBlock,
        "LocalGatewayId" Lude.=: localGatewayId,
        "NatGatewayId" Lude.=: natGatewayId,
        "NetworkInterfaceId" Lude.=: networkInterfaceId,
        "LocalTarget" Lude.=: localTarget,
        "TransitGatewayId" Lude.=: transitGatewayId,
        "GatewayId" Lude.=: gatewayId,
        "VpcEndpointId" Lude.=: vpcEndpointId,
        "DestinationPrefixListId" Lude.=: destinationPrefixListId,
        "DryRun" Lude.=: dryRun,
        "CarrierGatewayId" Lude.=: carrierGatewayId,
        "DestinationCidrBlock" Lude.=: destinationCidrBlock,
        "RouteTableId" Lude.=: routeTableId
      ]

-- | /See:/ 'mkReplaceRouteResponse' smart constructor.
data ReplaceRouteResponse = ReplaceRouteResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceRouteResponse' with the minimum fields required to make a request.
mkReplaceRouteResponse ::
  ReplaceRouteResponse
mkReplaceRouteResponse = ReplaceRouteResponse'
