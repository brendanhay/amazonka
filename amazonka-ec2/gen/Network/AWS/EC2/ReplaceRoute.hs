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
-- Module      : Network.AWS.EC2.ReplaceRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces an existing route within a route table in a VPC. You must
-- provide only one of the following: internet gateway, virtual private
-- gateway, NAT instance, NAT gateway, VPC peering connection, network
-- interface, egress-only internet gateway, or transit gateway.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.ReplaceRoute
  ( -- * Creating a Request
    ReplaceRoute (..),
    newReplaceRoute,

    -- * Request Lenses
    replaceRoute_instanceId,
    replaceRoute_localTarget,
    replaceRoute_vpcPeeringConnectionId,
    replaceRoute_dryRun,
    replaceRoute_vpcEndpointId,
    replaceRoute_destinationPrefixListId,
    replaceRoute_destinationIpv6CidrBlock,
    replaceRoute_localGatewayId,
    replaceRoute_egressOnlyInternetGatewayId,
    replaceRoute_carrierGatewayId,
    replaceRoute_destinationCidrBlock,
    replaceRoute_networkInterfaceId,
    replaceRoute_natGatewayId,
    replaceRoute_gatewayId,
    replaceRoute_transitGatewayId,
    replaceRoute_routeTableId,

    -- * Destructuring the Response
    ReplaceRouteResponse (..),
    newReplaceRouteResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newReplaceRoute' smart constructor.
data ReplaceRoute = ReplaceRoute'
  { -- | The ID of a NAT instance in your VPC.
    instanceId :: Core.Maybe Core.Text,
    -- | Specifies whether to reset the local route to its default target
    -- (@local@).
    localTarget :: Core.Maybe Core.Bool,
    -- | The ID of a VPC peering connection.
    vpcPeeringConnectionId :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints
    -- only.
    vpcEndpointId :: Core.Maybe Core.Text,
    -- | The ID of the prefix list for the route.
    destinationPrefixListId :: Core.Maybe Core.Text,
    -- | The IPv6 CIDR address block used for the destination match. The value
    -- that you provide must match the CIDR of an existing route in the table.
    destinationIpv6CidrBlock :: Core.Maybe Core.Text,
    -- | The ID of the local gateway.
    localGatewayId :: Core.Maybe Core.Text,
    -- | [IPv6 traffic only] The ID of an egress-only internet gateway.
    egressOnlyInternetGatewayId :: Core.Maybe Core.Text,
    -- | [IPv4 traffic only] The ID of a carrier gateway.
    carrierGatewayId :: Core.Maybe Core.Text,
    -- | The IPv4 CIDR address block used for the destination match. The value
    -- that you provide must match the CIDR of an existing route in the table.
    destinationCidrBlock :: Core.Maybe Core.Text,
    -- | The ID of a network interface.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | [IPv4 traffic only] The ID of a NAT gateway.
    natGatewayId :: Core.Maybe Core.Text,
    -- | The ID of an internet gateway or virtual private gateway.
    gatewayId :: Core.Maybe Core.Text,
    -- | The ID of a transit gateway.
    transitGatewayId :: Core.Maybe Core.Text,
    -- | The ID of the route table.
    routeTableId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplaceRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'replaceRoute_instanceId' - The ID of a NAT instance in your VPC.
--
-- 'localTarget', 'replaceRoute_localTarget' - Specifies whether to reset the local route to its default target
-- (@local@).
--
-- 'vpcPeeringConnectionId', 'replaceRoute_vpcPeeringConnectionId' - The ID of a VPC peering connection.
--
-- 'dryRun', 'replaceRoute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcEndpointId', 'replaceRoute_vpcEndpointId' - The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints
-- only.
--
-- 'destinationPrefixListId', 'replaceRoute_destinationPrefixListId' - The ID of the prefix list for the route.
--
-- 'destinationIpv6CidrBlock', 'replaceRoute_destinationIpv6CidrBlock' - The IPv6 CIDR address block used for the destination match. The value
-- that you provide must match the CIDR of an existing route in the table.
--
-- 'localGatewayId', 'replaceRoute_localGatewayId' - The ID of the local gateway.
--
-- 'egressOnlyInternetGatewayId', 'replaceRoute_egressOnlyInternetGatewayId' - [IPv6 traffic only] The ID of an egress-only internet gateway.
--
-- 'carrierGatewayId', 'replaceRoute_carrierGatewayId' - [IPv4 traffic only] The ID of a carrier gateway.
--
-- 'destinationCidrBlock', 'replaceRoute_destinationCidrBlock' - The IPv4 CIDR address block used for the destination match. The value
-- that you provide must match the CIDR of an existing route in the table.
--
-- 'networkInterfaceId', 'replaceRoute_networkInterfaceId' - The ID of a network interface.
--
-- 'natGatewayId', 'replaceRoute_natGatewayId' - [IPv4 traffic only] The ID of a NAT gateway.
--
-- 'gatewayId', 'replaceRoute_gatewayId' - The ID of an internet gateway or virtual private gateway.
--
-- 'transitGatewayId', 'replaceRoute_transitGatewayId' - The ID of a transit gateway.
--
-- 'routeTableId', 'replaceRoute_routeTableId' - The ID of the route table.
newReplaceRoute ::
  -- | 'routeTableId'
  Core.Text ->
  ReplaceRoute
newReplaceRoute pRouteTableId_ =
  ReplaceRoute'
    { instanceId = Core.Nothing,
      localTarget = Core.Nothing,
      vpcPeeringConnectionId = Core.Nothing,
      dryRun = Core.Nothing,
      vpcEndpointId = Core.Nothing,
      destinationPrefixListId = Core.Nothing,
      destinationIpv6CidrBlock = Core.Nothing,
      localGatewayId = Core.Nothing,
      egressOnlyInternetGatewayId = Core.Nothing,
      carrierGatewayId = Core.Nothing,
      destinationCidrBlock = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      natGatewayId = Core.Nothing,
      gatewayId = Core.Nothing,
      transitGatewayId = Core.Nothing,
      routeTableId = pRouteTableId_
    }

-- | The ID of a NAT instance in your VPC.
replaceRoute_instanceId :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
replaceRoute_instanceId = Lens.lens (\ReplaceRoute' {instanceId} -> instanceId) (\s@ReplaceRoute' {} a -> s {instanceId = a} :: ReplaceRoute)

-- | Specifies whether to reset the local route to its default target
-- (@local@).
replaceRoute_localTarget :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Bool)
replaceRoute_localTarget = Lens.lens (\ReplaceRoute' {localTarget} -> localTarget) (\s@ReplaceRoute' {} a -> s {localTarget = a} :: ReplaceRoute)

-- | The ID of a VPC peering connection.
replaceRoute_vpcPeeringConnectionId :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
replaceRoute_vpcPeeringConnectionId = Lens.lens (\ReplaceRoute' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@ReplaceRoute' {} a -> s {vpcPeeringConnectionId = a} :: ReplaceRoute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
replaceRoute_dryRun :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Bool)
replaceRoute_dryRun = Lens.lens (\ReplaceRoute' {dryRun} -> dryRun) (\s@ReplaceRoute' {} a -> s {dryRun = a} :: ReplaceRoute)

-- | The ID of a VPC endpoint. Supported for Gateway Load Balancer endpoints
-- only.
replaceRoute_vpcEndpointId :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
replaceRoute_vpcEndpointId = Lens.lens (\ReplaceRoute' {vpcEndpointId} -> vpcEndpointId) (\s@ReplaceRoute' {} a -> s {vpcEndpointId = a} :: ReplaceRoute)

-- | The ID of the prefix list for the route.
replaceRoute_destinationPrefixListId :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
replaceRoute_destinationPrefixListId = Lens.lens (\ReplaceRoute' {destinationPrefixListId} -> destinationPrefixListId) (\s@ReplaceRoute' {} a -> s {destinationPrefixListId = a} :: ReplaceRoute)

-- | The IPv6 CIDR address block used for the destination match. The value
-- that you provide must match the CIDR of an existing route in the table.
replaceRoute_destinationIpv6CidrBlock :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
replaceRoute_destinationIpv6CidrBlock = Lens.lens (\ReplaceRoute' {destinationIpv6CidrBlock} -> destinationIpv6CidrBlock) (\s@ReplaceRoute' {} a -> s {destinationIpv6CidrBlock = a} :: ReplaceRoute)

-- | The ID of the local gateway.
replaceRoute_localGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
replaceRoute_localGatewayId = Lens.lens (\ReplaceRoute' {localGatewayId} -> localGatewayId) (\s@ReplaceRoute' {} a -> s {localGatewayId = a} :: ReplaceRoute)

-- | [IPv6 traffic only] The ID of an egress-only internet gateway.
replaceRoute_egressOnlyInternetGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
replaceRoute_egressOnlyInternetGatewayId = Lens.lens (\ReplaceRoute' {egressOnlyInternetGatewayId} -> egressOnlyInternetGatewayId) (\s@ReplaceRoute' {} a -> s {egressOnlyInternetGatewayId = a} :: ReplaceRoute)

-- | [IPv4 traffic only] The ID of a carrier gateway.
replaceRoute_carrierGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
replaceRoute_carrierGatewayId = Lens.lens (\ReplaceRoute' {carrierGatewayId} -> carrierGatewayId) (\s@ReplaceRoute' {} a -> s {carrierGatewayId = a} :: ReplaceRoute)

-- | The IPv4 CIDR address block used for the destination match. The value
-- that you provide must match the CIDR of an existing route in the table.
replaceRoute_destinationCidrBlock :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
replaceRoute_destinationCidrBlock = Lens.lens (\ReplaceRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@ReplaceRoute' {} a -> s {destinationCidrBlock = a} :: ReplaceRoute)

-- | The ID of a network interface.
replaceRoute_networkInterfaceId :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
replaceRoute_networkInterfaceId = Lens.lens (\ReplaceRoute' {networkInterfaceId} -> networkInterfaceId) (\s@ReplaceRoute' {} a -> s {networkInterfaceId = a} :: ReplaceRoute)

-- | [IPv4 traffic only] The ID of a NAT gateway.
replaceRoute_natGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
replaceRoute_natGatewayId = Lens.lens (\ReplaceRoute' {natGatewayId} -> natGatewayId) (\s@ReplaceRoute' {} a -> s {natGatewayId = a} :: ReplaceRoute)

-- | The ID of an internet gateway or virtual private gateway.
replaceRoute_gatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
replaceRoute_gatewayId = Lens.lens (\ReplaceRoute' {gatewayId} -> gatewayId) (\s@ReplaceRoute' {} a -> s {gatewayId = a} :: ReplaceRoute)

-- | The ID of a transit gateway.
replaceRoute_transitGatewayId :: Lens.Lens' ReplaceRoute (Core.Maybe Core.Text)
replaceRoute_transitGatewayId = Lens.lens (\ReplaceRoute' {transitGatewayId} -> transitGatewayId) (\s@ReplaceRoute' {} a -> s {transitGatewayId = a} :: ReplaceRoute)

-- | The ID of the route table.
replaceRoute_routeTableId :: Lens.Lens' ReplaceRoute Core.Text
replaceRoute_routeTableId = Lens.lens (\ReplaceRoute' {routeTableId} -> routeTableId) (\s@ReplaceRoute' {} a -> s {routeTableId = a} :: ReplaceRoute)

instance Core.AWSRequest ReplaceRoute where
  type AWSResponse ReplaceRoute = ReplaceRouteResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull ReplaceRouteResponse'

instance Core.Hashable ReplaceRoute

instance Core.NFData ReplaceRoute

instance Core.ToHeaders ReplaceRoute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ReplaceRoute where
  toPath = Core.const "/"

instance Core.ToQuery ReplaceRoute where
  toQuery ReplaceRoute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ReplaceRoute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "InstanceId" Core.=: instanceId,
        "LocalTarget" Core.=: localTarget,
        "VpcPeeringConnectionId"
          Core.=: vpcPeeringConnectionId,
        "DryRun" Core.=: dryRun,
        "VpcEndpointId" Core.=: vpcEndpointId,
        "DestinationPrefixListId"
          Core.=: destinationPrefixListId,
        "DestinationIpv6CidrBlock"
          Core.=: destinationIpv6CidrBlock,
        "LocalGatewayId" Core.=: localGatewayId,
        "EgressOnlyInternetGatewayId"
          Core.=: egressOnlyInternetGatewayId,
        "CarrierGatewayId" Core.=: carrierGatewayId,
        "DestinationCidrBlock" Core.=: destinationCidrBlock,
        "NetworkInterfaceId" Core.=: networkInterfaceId,
        "NatGatewayId" Core.=: natGatewayId,
        "GatewayId" Core.=: gatewayId,
        "TransitGatewayId" Core.=: transitGatewayId,
        "RouteTableId" Core.=: routeTableId
      ]

-- | /See:/ 'newReplaceRouteResponse' smart constructor.
data ReplaceRouteResponse = ReplaceRouteResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplaceRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newReplaceRouteResponse ::
  ReplaceRouteResponse
newReplaceRouteResponse = ReplaceRouteResponse'

instance Core.NFData ReplaceRouteResponse
