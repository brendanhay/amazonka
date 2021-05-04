{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route in a route table within a VPC.
--
-- You must specify one of the following targets: internet gateway or
-- virtual private gateway, NAT instance, NAT gateway, VPC peering
-- connection, network interface, egress-only internet gateway, or transit
-- gateway.
--
-- When determining how to route traffic, we use the route with the most
-- specific match. For example, traffic is destined for the IPv4 address
-- @192.0.2.3@, and the route table includes the following two IPv4 routes:
--
-- -   @192.0.2.0\/24@ (goes to some target A)
--
-- -   @192.0.2.0\/28@ (goes to some target B)
--
-- Both routes apply to the traffic destined for @192.0.2.3@. However, the
-- second route in the list covers a smaller number of IP addresses and is
-- therefore more specific, so we use that route to determine where to
-- target the traffic.
--
-- For more information about route tables, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.CreateRoute
  ( -- * Creating a Request
    CreateRoute (..),
    newCreateRoute,

    -- * Request Lenses
    createRoute_instanceId,
    createRoute_vpcPeeringConnectionId,
    createRoute_dryRun,
    createRoute_vpcEndpointId,
    createRoute_destinationPrefixListId,
    createRoute_destinationIpv6CidrBlock,
    createRoute_localGatewayId,
    createRoute_egressOnlyInternetGatewayId,
    createRoute_carrierGatewayId,
    createRoute_destinationCidrBlock,
    createRoute_networkInterfaceId,
    createRoute_natGatewayId,
    createRoute_gatewayId,
    createRoute_transitGatewayId,
    createRoute_routeTableId,

    -- * Destructuring the Response
    CreateRouteResponse (..),
    newCreateRouteResponse,

    -- * Response Lenses
    createRouteResponse_return,
    createRouteResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRoute' smart constructor.
data CreateRoute = CreateRoute'
  { -- | The ID of a NAT instance in your VPC. The operation fails if you specify
    -- an instance ID unless exactly one network interface is attached.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a VPC peering connection.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints
    -- only.
    vpcEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a prefix list used for the destination match.
    destinationPrefixListId :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR block used for the destination match. Routing decisions
    -- are based on the most specific match.
    destinationIpv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local gateway.
    localGatewayId :: Prelude.Maybe Prelude.Text,
    -- | [IPv6 traffic only] The ID of an egress-only internet gateway.
    egressOnlyInternetGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the carrier gateway.
    --
    -- You can only use this option when the VPC contains a subnet which is
    -- associated with a Wavelength Zone.
    carrierGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 CIDR address block used for the destination match. Routing
    -- decisions are based on the most specific match. We modify the specified
    -- CIDR block to its canonical form; for example, if you specify
    -- @100.68.0.18\/18@, we modify it to @100.68.0.0\/18@.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The ID of a network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | [IPv4 traffic only] The ID of a NAT gateway.
    natGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of an internet gateway or virtual private gateway attached to
    -- your VPC.
    gatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the route table for the route.
    routeTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'createRoute_instanceId' - The ID of a NAT instance in your VPC. The operation fails if you specify
-- an instance ID unless exactly one network interface is attached.
--
-- 'vpcPeeringConnectionId', 'createRoute_vpcPeeringConnectionId' - The ID of a VPC peering connection.
--
-- 'dryRun', 'createRoute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcEndpointId', 'createRoute_vpcEndpointId' - The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints
-- only.
--
-- 'destinationPrefixListId', 'createRoute_destinationPrefixListId' - The ID of a prefix list used for the destination match.
--
-- 'destinationIpv6CidrBlock', 'createRoute_destinationIpv6CidrBlock' - The IPv6 CIDR block used for the destination match. Routing decisions
-- are based on the most specific match.
--
-- 'localGatewayId', 'createRoute_localGatewayId' - The ID of the local gateway.
--
-- 'egressOnlyInternetGatewayId', 'createRoute_egressOnlyInternetGatewayId' - [IPv6 traffic only] The ID of an egress-only internet gateway.
--
-- 'carrierGatewayId', 'createRoute_carrierGatewayId' - The ID of the carrier gateway.
--
-- You can only use this option when the VPC contains a subnet which is
-- associated with a Wavelength Zone.
--
-- 'destinationCidrBlock', 'createRoute_destinationCidrBlock' - The IPv4 CIDR address block used for the destination match. Routing
-- decisions are based on the most specific match. We modify the specified
-- CIDR block to its canonical form; for example, if you specify
-- @100.68.0.18\/18@, we modify it to @100.68.0.0\/18@.
--
-- 'networkInterfaceId', 'createRoute_networkInterfaceId' - The ID of a network interface.
--
-- 'natGatewayId', 'createRoute_natGatewayId' - [IPv4 traffic only] The ID of a NAT gateway.
--
-- 'gatewayId', 'createRoute_gatewayId' - The ID of an internet gateway or virtual private gateway attached to
-- your VPC.
--
-- 'transitGatewayId', 'createRoute_transitGatewayId' - The ID of a transit gateway.
--
-- 'routeTableId', 'createRoute_routeTableId' - The ID of the route table for the route.
newCreateRoute ::
  -- | 'routeTableId'
  Prelude.Text ->
  CreateRoute
newCreateRoute pRouteTableId_ =
  CreateRoute'
    { instanceId = Prelude.Nothing,
      vpcPeeringConnectionId = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      vpcEndpointId = Prelude.Nothing,
      destinationPrefixListId = Prelude.Nothing,
      destinationIpv6CidrBlock = Prelude.Nothing,
      localGatewayId = Prelude.Nothing,
      egressOnlyInternetGatewayId = Prelude.Nothing,
      carrierGatewayId = Prelude.Nothing,
      destinationCidrBlock = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      natGatewayId = Prelude.Nothing,
      gatewayId = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      routeTableId = pRouteTableId_
    }

-- | The ID of a NAT instance in your VPC. The operation fails if you specify
-- an instance ID unless exactly one network interface is attached.
createRoute_instanceId :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_instanceId = Lens.lens (\CreateRoute' {instanceId} -> instanceId) (\s@CreateRoute' {} a -> s {instanceId = a} :: CreateRoute)

-- | The ID of a VPC peering connection.
createRoute_vpcPeeringConnectionId :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_vpcPeeringConnectionId = Lens.lens (\CreateRoute' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@CreateRoute' {} a -> s {vpcPeeringConnectionId = a} :: CreateRoute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createRoute_dryRun :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Bool)
createRoute_dryRun = Lens.lens (\CreateRoute' {dryRun} -> dryRun) (\s@CreateRoute' {} a -> s {dryRun = a} :: CreateRoute)

-- | The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints
-- only.
createRoute_vpcEndpointId :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_vpcEndpointId = Lens.lens (\CreateRoute' {vpcEndpointId} -> vpcEndpointId) (\s@CreateRoute' {} a -> s {vpcEndpointId = a} :: CreateRoute)

-- | The ID of a prefix list used for the destination match.
createRoute_destinationPrefixListId :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_destinationPrefixListId = Lens.lens (\CreateRoute' {destinationPrefixListId} -> destinationPrefixListId) (\s@CreateRoute' {} a -> s {destinationPrefixListId = a} :: CreateRoute)

-- | The IPv6 CIDR block used for the destination match. Routing decisions
-- are based on the most specific match.
createRoute_destinationIpv6CidrBlock :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_destinationIpv6CidrBlock = Lens.lens (\CreateRoute' {destinationIpv6CidrBlock} -> destinationIpv6CidrBlock) (\s@CreateRoute' {} a -> s {destinationIpv6CidrBlock = a} :: CreateRoute)

-- | The ID of the local gateway.
createRoute_localGatewayId :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_localGatewayId = Lens.lens (\CreateRoute' {localGatewayId} -> localGatewayId) (\s@CreateRoute' {} a -> s {localGatewayId = a} :: CreateRoute)

-- | [IPv6 traffic only] The ID of an egress-only internet gateway.
createRoute_egressOnlyInternetGatewayId :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_egressOnlyInternetGatewayId = Lens.lens (\CreateRoute' {egressOnlyInternetGatewayId} -> egressOnlyInternetGatewayId) (\s@CreateRoute' {} a -> s {egressOnlyInternetGatewayId = a} :: CreateRoute)

-- | The ID of the carrier gateway.
--
-- You can only use this option when the VPC contains a subnet which is
-- associated with a Wavelength Zone.
createRoute_carrierGatewayId :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_carrierGatewayId = Lens.lens (\CreateRoute' {carrierGatewayId} -> carrierGatewayId) (\s@CreateRoute' {} a -> s {carrierGatewayId = a} :: CreateRoute)

-- | The IPv4 CIDR address block used for the destination match. Routing
-- decisions are based on the most specific match. We modify the specified
-- CIDR block to its canonical form; for example, if you specify
-- @100.68.0.18\/18@, we modify it to @100.68.0.0\/18@.
createRoute_destinationCidrBlock :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_destinationCidrBlock = Lens.lens (\CreateRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@CreateRoute' {} a -> s {destinationCidrBlock = a} :: CreateRoute)

-- | The ID of a network interface.
createRoute_networkInterfaceId :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_networkInterfaceId = Lens.lens (\CreateRoute' {networkInterfaceId} -> networkInterfaceId) (\s@CreateRoute' {} a -> s {networkInterfaceId = a} :: CreateRoute)

-- | [IPv4 traffic only] The ID of a NAT gateway.
createRoute_natGatewayId :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_natGatewayId = Lens.lens (\CreateRoute' {natGatewayId} -> natGatewayId) (\s@CreateRoute' {} a -> s {natGatewayId = a} :: CreateRoute)

-- | The ID of an internet gateway or virtual private gateway attached to
-- your VPC.
createRoute_gatewayId :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_gatewayId = Lens.lens (\CreateRoute' {gatewayId} -> gatewayId) (\s@CreateRoute' {} a -> s {gatewayId = a} :: CreateRoute)

-- | The ID of a transit gateway.
createRoute_transitGatewayId :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_transitGatewayId = Lens.lens (\CreateRoute' {transitGatewayId} -> transitGatewayId) (\s@CreateRoute' {} a -> s {transitGatewayId = a} :: CreateRoute)

-- | The ID of the route table for the route.
createRoute_routeTableId :: Lens.Lens' CreateRoute Prelude.Text
createRoute_routeTableId = Lens.lens (\CreateRoute' {routeTableId} -> routeTableId) (\s@CreateRoute' {} a -> s {routeTableId = a} :: CreateRoute)

instance Prelude.AWSRequest CreateRoute where
  type Rs CreateRoute = CreateRouteResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateRouteResponse'
            Prelude.<$> (x Prelude..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRoute

instance Prelude.NFData CreateRoute

instance Prelude.ToHeaders CreateRoute where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateRoute where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateRoute where
  toQuery CreateRoute' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateRoute" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "InstanceId" Prelude.=: instanceId,
        "VpcPeeringConnectionId"
          Prelude.=: vpcPeeringConnectionId,
        "DryRun" Prelude.=: dryRun,
        "VpcEndpointId" Prelude.=: vpcEndpointId,
        "DestinationPrefixListId"
          Prelude.=: destinationPrefixListId,
        "DestinationIpv6CidrBlock"
          Prelude.=: destinationIpv6CidrBlock,
        "LocalGatewayId" Prelude.=: localGatewayId,
        "EgressOnlyInternetGatewayId"
          Prelude.=: egressOnlyInternetGatewayId,
        "CarrierGatewayId" Prelude.=: carrierGatewayId,
        "DestinationCidrBlock"
          Prelude.=: destinationCidrBlock,
        "NetworkInterfaceId" Prelude.=: networkInterfaceId,
        "NatGatewayId" Prelude.=: natGatewayId,
        "GatewayId" Prelude.=: gatewayId,
        "TransitGatewayId" Prelude.=: transitGatewayId,
        "RouteTableId" Prelude.=: routeTableId
      ]

-- | /See:/ 'newCreateRouteResponse' smart constructor.
data CreateRouteResponse = CreateRouteResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'createRouteResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'createRouteResponse_httpStatus' - The response's http status code.
newCreateRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRouteResponse
newCreateRouteResponse pHttpStatus_ =
  CreateRouteResponse'
    { return' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
createRouteResponse_return :: Lens.Lens' CreateRouteResponse (Prelude.Maybe Prelude.Bool)
createRouteResponse_return = Lens.lens (\CreateRouteResponse' {return'} -> return') (\s@CreateRouteResponse' {} a -> s {return' = a} :: CreateRouteResponse)

-- | The response's http status code.
createRouteResponse_httpStatus :: Lens.Lens' CreateRouteResponse Prelude.Int
createRouteResponse_httpStatus = Lens.lens (\CreateRouteResponse' {httpStatus} -> httpStatus) (\s@CreateRouteResponse' {} a -> s {httpStatus = a} :: CreateRouteResponse)

instance Prelude.NFData CreateRouteResponse
