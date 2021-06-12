{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Route
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Route where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.RouteOrigin
import Network.AWS.EC2.Types.RouteState
import qualified Network.AWS.Lens as Lens

-- | Describes a route in a route table.
--
-- /See:/ 'newRoute' smart constructor.
data Route = Route'
  { -- | The ID of a NAT instance in your VPC.
    instanceId :: Core.Maybe Core.Text,
    -- | Describes how the route was created.
    --
    -- -   @CreateRouteTable@ - The route was automatically created when the
    --     route table was created.
    --
    -- -   @CreateRoute@ - The route was manually added to the route table.
    --
    -- -   @EnableVgwRoutePropagation@ - The route was propagated by route
    --     propagation.
    origin :: Core.Maybe RouteOrigin,
    -- | The ID of a VPC peering connection.
    vpcPeeringConnectionId :: Core.Maybe Core.Text,
    -- | The prefix of the AWS service.
    destinationPrefixListId :: Core.Maybe Core.Text,
    -- | The IPv6 CIDR block used for the destination match.
    destinationIpv6CidrBlock :: Core.Maybe Core.Text,
    -- | The ID of the local gateway.
    localGatewayId :: Core.Maybe Core.Text,
    -- | The state of the route. The @blackhole@ state indicates that the
    -- route\'s target isn\'t available (for example, the specified gateway
    -- isn\'t attached to the VPC, or the specified NAT instance has been
    -- terminated).
    state :: Core.Maybe RouteState,
    -- | The ID of the egress-only internet gateway.
    egressOnlyInternetGatewayId :: Core.Maybe Core.Text,
    -- | The ID of the carrier gateway.
    carrierGatewayId :: Core.Maybe Core.Text,
    -- | The IPv4 CIDR block used for the destination match.
    destinationCidrBlock :: Core.Maybe Core.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The ID of a NAT gateway.
    natGatewayId :: Core.Maybe Core.Text,
    -- | The AWS account ID of the owner of the instance.
    instanceOwnerId :: Core.Maybe Core.Text,
    -- | The ID of a gateway attached to your VPC.
    gatewayId :: Core.Maybe Core.Text,
    -- | The ID of a transit gateway.
    transitGatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Route' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'route_instanceId' - The ID of a NAT instance in your VPC.
--
-- 'origin', 'route_origin' - Describes how the route was created.
--
-- -   @CreateRouteTable@ - The route was automatically created when the
--     route table was created.
--
-- -   @CreateRoute@ - The route was manually added to the route table.
--
-- -   @EnableVgwRoutePropagation@ - The route was propagated by route
--     propagation.
--
-- 'vpcPeeringConnectionId', 'route_vpcPeeringConnectionId' - The ID of a VPC peering connection.
--
-- 'destinationPrefixListId', 'route_destinationPrefixListId' - The prefix of the AWS service.
--
-- 'destinationIpv6CidrBlock', 'route_destinationIpv6CidrBlock' - The IPv6 CIDR block used for the destination match.
--
-- 'localGatewayId', 'route_localGatewayId' - The ID of the local gateway.
--
-- 'state', 'route_state' - The state of the route. The @blackhole@ state indicates that the
-- route\'s target isn\'t available (for example, the specified gateway
-- isn\'t attached to the VPC, or the specified NAT instance has been
-- terminated).
--
-- 'egressOnlyInternetGatewayId', 'route_egressOnlyInternetGatewayId' - The ID of the egress-only internet gateway.
--
-- 'carrierGatewayId', 'route_carrierGatewayId' - The ID of the carrier gateway.
--
-- 'destinationCidrBlock', 'route_destinationCidrBlock' - The IPv4 CIDR block used for the destination match.
--
-- 'networkInterfaceId', 'route_networkInterfaceId' - The ID of the network interface.
--
-- 'natGatewayId', 'route_natGatewayId' - The ID of a NAT gateway.
--
-- 'instanceOwnerId', 'route_instanceOwnerId' - The AWS account ID of the owner of the instance.
--
-- 'gatewayId', 'route_gatewayId' - The ID of a gateway attached to your VPC.
--
-- 'transitGatewayId', 'route_transitGatewayId' - The ID of a transit gateway.
newRoute ::
  Route
newRoute =
  Route'
    { instanceId = Core.Nothing,
      origin = Core.Nothing,
      vpcPeeringConnectionId = Core.Nothing,
      destinationPrefixListId = Core.Nothing,
      destinationIpv6CidrBlock = Core.Nothing,
      localGatewayId = Core.Nothing,
      state = Core.Nothing,
      egressOnlyInternetGatewayId = Core.Nothing,
      carrierGatewayId = Core.Nothing,
      destinationCidrBlock = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      natGatewayId = Core.Nothing,
      instanceOwnerId = Core.Nothing,
      gatewayId = Core.Nothing,
      transitGatewayId = Core.Nothing
    }

-- | The ID of a NAT instance in your VPC.
route_instanceId :: Lens.Lens' Route (Core.Maybe Core.Text)
route_instanceId = Lens.lens (\Route' {instanceId} -> instanceId) (\s@Route' {} a -> s {instanceId = a} :: Route)

-- | Describes how the route was created.
--
-- -   @CreateRouteTable@ - The route was automatically created when the
--     route table was created.
--
-- -   @CreateRoute@ - The route was manually added to the route table.
--
-- -   @EnableVgwRoutePropagation@ - The route was propagated by route
--     propagation.
route_origin :: Lens.Lens' Route (Core.Maybe RouteOrigin)
route_origin = Lens.lens (\Route' {origin} -> origin) (\s@Route' {} a -> s {origin = a} :: Route)

-- | The ID of a VPC peering connection.
route_vpcPeeringConnectionId :: Lens.Lens' Route (Core.Maybe Core.Text)
route_vpcPeeringConnectionId = Lens.lens (\Route' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@Route' {} a -> s {vpcPeeringConnectionId = a} :: Route)

-- | The prefix of the AWS service.
route_destinationPrefixListId :: Lens.Lens' Route (Core.Maybe Core.Text)
route_destinationPrefixListId = Lens.lens (\Route' {destinationPrefixListId} -> destinationPrefixListId) (\s@Route' {} a -> s {destinationPrefixListId = a} :: Route)

-- | The IPv6 CIDR block used for the destination match.
route_destinationIpv6CidrBlock :: Lens.Lens' Route (Core.Maybe Core.Text)
route_destinationIpv6CidrBlock = Lens.lens (\Route' {destinationIpv6CidrBlock} -> destinationIpv6CidrBlock) (\s@Route' {} a -> s {destinationIpv6CidrBlock = a} :: Route)

-- | The ID of the local gateway.
route_localGatewayId :: Lens.Lens' Route (Core.Maybe Core.Text)
route_localGatewayId = Lens.lens (\Route' {localGatewayId} -> localGatewayId) (\s@Route' {} a -> s {localGatewayId = a} :: Route)

-- | The state of the route. The @blackhole@ state indicates that the
-- route\'s target isn\'t available (for example, the specified gateway
-- isn\'t attached to the VPC, or the specified NAT instance has been
-- terminated).
route_state :: Lens.Lens' Route (Core.Maybe RouteState)
route_state = Lens.lens (\Route' {state} -> state) (\s@Route' {} a -> s {state = a} :: Route)

-- | The ID of the egress-only internet gateway.
route_egressOnlyInternetGatewayId :: Lens.Lens' Route (Core.Maybe Core.Text)
route_egressOnlyInternetGatewayId = Lens.lens (\Route' {egressOnlyInternetGatewayId} -> egressOnlyInternetGatewayId) (\s@Route' {} a -> s {egressOnlyInternetGatewayId = a} :: Route)

-- | The ID of the carrier gateway.
route_carrierGatewayId :: Lens.Lens' Route (Core.Maybe Core.Text)
route_carrierGatewayId = Lens.lens (\Route' {carrierGatewayId} -> carrierGatewayId) (\s@Route' {} a -> s {carrierGatewayId = a} :: Route)

-- | The IPv4 CIDR block used for the destination match.
route_destinationCidrBlock :: Lens.Lens' Route (Core.Maybe Core.Text)
route_destinationCidrBlock = Lens.lens (\Route' {destinationCidrBlock} -> destinationCidrBlock) (\s@Route' {} a -> s {destinationCidrBlock = a} :: Route)

-- | The ID of the network interface.
route_networkInterfaceId :: Lens.Lens' Route (Core.Maybe Core.Text)
route_networkInterfaceId = Lens.lens (\Route' {networkInterfaceId} -> networkInterfaceId) (\s@Route' {} a -> s {networkInterfaceId = a} :: Route)

-- | The ID of a NAT gateway.
route_natGatewayId :: Lens.Lens' Route (Core.Maybe Core.Text)
route_natGatewayId = Lens.lens (\Route' {natGatewayId} -> natGatewayId) (\s@Route' {} a -> s {natGatewayId = a} :: Route)

-- | The AWS account ID of the owner of the instance.
route_instanceOwnerId :: Lens.Lens' Route (Core.Maybe Core.Text)
route_instanceOwnerId = Lens.lens (\Route' {instanceOwnerId} -> instanceOwnerId) (\s@Route' {} a -> s {instanceOwnerId = a} :: Route)

-- | The ID of a gateway attached to your VPC.
route_gatewayId :: Lens.Lens' Route (Core.Maybe Core.Text)
route_gatewayId = Lens.lens (\Route' {gatewayId} -> gatewayId) (\s@Route' {} a -> s {gatewayId = a} :: Route)

-- | The ID of a transit gateway.
route_transitGatewayId :: Lens.Lens' Route (Core.Maybe Core.Text)
route_transitGatewayId = Lens.lens (\Route' {transitGatewayId} -> transitGatewayId) (\s@Route' {} a -> s {transitGatewayId = a} :: Route)

instance Core.FromXML Route where
  parseXML x =
    Route'
      Core.<$> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "origin")
      Core.<*> (x Core..@? "vpcPeeringConnectionId")
      Core.<*> (x Core..@? "destinationPrefixListId")
      Core.<*> (x Core..@? "destinationIpv6CidrBlock")
      Core.<*> (x Core..@? "localGatewayId")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "egressOnlyInternetGatewayId")
      Core.<*> (x Core..@? "carrierGatewayId")
      Core.<*> (x Core..@? "destinationCidrBlock")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "natGatewayId")
      Core.<*> (x Core..@? "instanceOwnerId")
      Core.<*> (x Core..@? "gatewayId")
      Core.<*> (x Core..@? "transitGatewayId")

instance Core.Hashable Route

instance Core.NFData Route
