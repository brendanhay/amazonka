{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    crVPCPeeringConnectionId,
    crInstanceId,
    crEgressOnlyInternetGatewayId,
    crDestinationIPv6CidrBlock,
    crLocalGatewayId,
    crNatGatewayId,
    crNetworkInterfaceId,
    crTransitGatewayId,
    crGatewayId,
    crVPCEndpointId,
    crDestinationPrefixListId,
    crDryRun,
    crCarrierGatewayId,
    crDestinationCidrBlock,
    crRouteTableId,

    -- * Destructuring the response
    CreateRouteResponse (..),
    mkCreateRouteResponse,

    -- ** Response lenses
    crrsReturn,
    crrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateRoute' smart constructor.
data CreateRoute = CreateRoute'
  { vpcPeeringConnectionId ::
      Lude.Maybe Lude.Text,
    instanceId :: Lude.Maybe Lude.Text,
    egressOnlyInternetGatewayId :: Lude.Maybe Lude.Text,
    destinationIPv6CidrBlock :: Lude.Maybe Lude.Text,
    localGatewayId :: Lude.Maybe Lude.Text,
    natGatewayId :: Lude.Maybe Lude.Text,
    networkInterfaceId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateRoute' with the minimum fields required to make a request.
--
-- * 'carrierGatewayId' - The ID of the carrier gateway.
--
-- You can only use this option when the VPC contains a subnet which is associated with a Wavelength Zone.
-- * 'destinationCidrBlock' - The IPv4 CIDR address block used for the destination match. Routing decisions are based on the most specific match. We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
-- * 'destinationIPv6CidrBlock' - The IPv6 CIDR block used for the destination match. Routing decisions are based on the most specific match.
-- * 'destinationPrefixListId' - The ID of a prefix list used for the destination match.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'egressOnlyInternetGatewayId' - [IPv6 traffic only] The ID of an egress-only internet gateway.
-- * 'gatewayId' - The ID of an internet gateway or virtual private gateway attached to your VPC.
-- * 'instanceId' - The ID of a NAT instance in your VPC. The operation fails if you specify an instance ID unless exactly one network interface is attached.
-- * 'localGatewayId' - The ID of the local gateway.
-- * 'natGatewayId' - [IPv4 traffic only] The ID of a NAT gateway.
-- * 'networkInterfaceId' - The ID of a network interface.
-- * 'routeTableId' - The ID of the route table for the route.
-- * 'transitGatewayId' - The ID of a transit gateway.
-- * 'vpcEndpointId' - The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints only.
-- * 'vpcPeeringConnectionId' - The ID of a VPC peering connection.
mkCreateRoute ::
  -- | 'routeTableId'
  Lude.Text ->
  CreateRoute
mkCreateRoute pRouteTableId_ =
  CreateRoute'
    { vpcPeeringConnectionId = Lude.Nothing,
      instanceId = Lude.Nothing,
      egressOnlyInternetGatewayId = Lude.Nothing,
      destinationIPv6CidrBlock = Lude.Nothing,
      localGatewayId = Lude.Nothing,
      natGatewayId = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
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
crVPCPeeringConnectionId :: Lens.Lens' CreateRoute (Lude.Maybe Lude.Text)
crVPCPeeringConnectionId = Lens.lens (vpcPeeringConnectionId :: CreateRoute -> Lude.Maybe Lude.Text) (\s a -> s {vpcPeeringConnectionId = a} :: CreateRoute)
{-# DEPRECATED crVPCPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

-- | The ID of a NAT instance in your VPC. The operation fails if you specify an instance ID unless exactly one network interface is attached.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crInstanceId :: Lens.Lens' CreateRoute (Lude.Maybe Lude.Text)
crInstanceId = Lens.lens (instanceId :: CreateRoute -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: CreateRoute)
{-# DEPRECATED crInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | [IPv6 traffic only] The ID of an egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crEgressOnlyInternetGatewayId :: Lens.Lens' CreateRoute (Lude.Maybe Lude.Text)
crEgressOnlyInternetGatewayId = Lens.lens (egressOnlyInternetGatewayId :: CreateRoute -> Lude.Maybe Lude.Text) (\s a -> s {egressOnlyInternetGatewayId = a} :: CreateRoute)
{-# DEPRECATED crEgressOnlyInternetGatewayId "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayId' instead." #-}

-- | The IPv6 CIDR block used for the destination match. Routing decisions are based on the most specific match.
--
-- /Note:/ Consider using 'destinationIPv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDestinationIPv6CidrBlock :: Lens.Lens' CreateRoute (Lude.Maybe Lude.Text)
crDestinationIPv6CidrBlock = Lens.lens (destinationIPv6CidrBlock :: CreateRoute -> Lude.Maybe Lude.Text) (\s a -> s {destinationIPv6CidrBlock = a} :: CreateRoute)
{-# DEPRECATED crDestinationIPv6CidrBlock "Use generic-lens or generic-optics with 'destinationIPv6CidrBlock' instead." #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crLocalGatewayId :: Lens.Lens' CreateRoute (Lude.Maybe Lude.Text)
crLocalGatewayId = Lens.lens (localGatewayId :: CreateRoute -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayId = a} :: CreateRoute)
{-# DEPRECATED crLocalGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead." #-}

-- | [IPv4 traffic only] The ID of a NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crNatGatewayId :: Lens.Lens' CreateRoute (Lude.Maybe Lude.Text)
crNatGatewayId = Lens.lens (natGatewayId :: CreateRoute -> Lude.Maybe Lude.Text) (\s a -> s {natGatewayId = a} :: CreateRoute)
{-# DEPRECATED crNatGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead." #-}

-- | The ID of a network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crNetworkInterfaceId :: Lens.Lens' CreateRoute (Lude.Maybe Lude.Text)
crNetworkInterfaceId = Lens.lens (networkInterfaceId :: CreateRoute -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: CreateRoute)
{-# DEPRECATED crNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of a transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTransitGatewayId :: Lens.Lens' CreateRoute (Lude.Maybe Lude.Text)
crTransitGatewayId = Lens.lens (transitGatewayId :: CreateRoute -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayId = a} :: CreateRoute)
{-# DEPRECATED crTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The ID of an internet gateway or virtual private gateway attached to your VPC.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crGatewayId :: Lens.Lens' CreateRoute (Lude.Maybe Lude.Text)
crGatewayId = Lens.lens (gatewayId :: CreateRoute -> Lude.Maybe Lude.Text) (\s a -> s {gatewayId = a} :: CreateRoute)
{-# DEPRECATED crGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints only.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crVPCEndpointId :: Lens.Lens' CreateRoute (Lude.Maybe Lude.Text)
crVPCEndpointId = Lens.lens (vpcEndpointId :: CreateRoute -> Lude.Maybe Lude.Text) (\s a -> s {vpcEndpointId = a} :: CreateRoute)
{-# DEPRECATED crVPCEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead." #-}

-- | The ID of a prefix list used for the destination match.
--
-- /Note:/ Consider using 'destinationPrefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDestinationPrefixListId :: Lens.Lens' CreateRoute (Lude.Maybe Lude.Text)
crDestinationPrefixListId = Lens.lens (destinationPrefixListId :: CreateRoute -> Lude.Maybe Lude.Text) (\s a -> s {destinationPrefixListId = a} :: CreateRoute)
{-# DEPRECATED crDestinationPrefixListId "Use generic-lens or generic-optics with 'destinationPrefixListId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDryRun :: Lens.Lens' CreateRoute (Lude.Maybe Lude.Bool)
crDryRun = Lens.lens (dryRun :: CreateRoute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateRoute)
{-# DEPRECATED crDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the carrier gateway.
--
-- You can only use this option when the VPC contains a subnet which is associated with a Wavelength Zone.
--
-- /Note:/ Consider using 'carrierGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCarrierGatewayId :: Lens.Lens' CreateRoute (Lude.Maybe Lude.Text)
crCarrierGatewayId = Lens.lens (carrierGatewayId :: CreateRoute -> Lude.Maybe Lude.Text) (\s a -> s {carrierGatewayId = a} :: CreateRoute)
{-# DEPRECATED crCarrierGatewayId "Use generic-lens or generic-optics with 'carrierGatewayId' instead." #-}

-- | The IPv4 CIDR address block used for the destination match. Routing decisions are based on the most specific match. We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDestinationCidrBlock :: Lens.Lens' CreateRoute (Lude.Maybe Lude.Text)
crDestinationCidrBlock = Lens.lens (destinationCidrBlock :: CreateRoute -> Lude.Maybe Lude.Text) (\s a -> s {destinationCidrBlock = a} :: CreateRoute)
{-# DEPRECATED crDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

-- | The ID of the route table for the route.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRouteTableId :: Lens.Lens' CreateRoute Lude.Text
crRouteTableId = Lens.lens (routeTableId :: CreateRoute -> Lude.Text) (\s a -> s {routeTableId = a} :: CreateRoute)
{-# DEPRECATED crRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

instance Lude.AWSRequest CreateRoute where
  type Rs CreateRoute = CreateRouteResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateRouteResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRoute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateRoute where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRoute where
  toQuery CreateRoute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateRoute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpcPeeringConnectionId" Lude.=: vpcPeeringConnectionId,
        "InstanceId" Lude.=: instanceId,
        "EgressOnlyInternetGatewayId" Lude.=: egressOnlyInternetGatewayId,
        "DestinationIpv6CidrBlock" Lude.=: destinationIPv6CidrBlock,
        "LocalGatewayId" Lude.=: localGatewayId,
        "NatGatewayId" Lude.=: natGatewayId,
        "NetworkInterfaceId" Lude.=: networkInterfaceId,
        "TransitGatewayId" Lude.=: transitGatewayId,
        "GatewayId" Lude.=: gatewayId,
        "VpcEndpointId" Lude.=: vpcEndpointId,
        "DestinationPrefixListId" Lude.=: destinationPrefixListId,
        "DryRun" Lude.=: dryRun,
        "CarrierGatewayId" Lude.=: carrierGatewayId,
        "DestinationCidrBlock" Lude.=: destinationCidrBlock,
        "RouteTableId" Lude.=: routeTableId
      ]

-- | /See:/ 'mkCreateRouteResponse' smart constructor.
data CreateRouteResponse = CreateRouteResponse'
  { return ::
      Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRouteResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
mkCreateRouteResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRouteResponse
mkCreateRouteResponse pResponseStatus_ =
  CreateRouteResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsReturn :: Lens.Lens' CreateRouteResponse (Lude.Maybe Lude.Bool)
crrsReturn = Lens.lens (return :: CreateRouteResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: CreateRouteResponse)
{-# DEPRECATED crrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsResponseStatus :: Lens.Lens' CreateRouteResponse Lude.Int
crrsResponseStatus = Lens.lens (responseStatus :: CreateRouteResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRouteResponse)
{-# DEPRECATED crrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
