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
-- Module      : Amazonka.EC2.Types.Route
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Route where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.RouteOrigin
import Amazonka.EC2.Types.RouteState
import qualified Amazonka.Prelude as Prelude

-- | Describes a route in a route table.
--
-- /See:/ 'newRoute' smart constructor.
data Route = Route'
  { -- | The ID of the carrier gateway.
    carrierGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the core network.
    coreNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 CIDR block used for the destination match.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR block used for the destination match.
    destinationIpv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The prefix of the Amazon Web Service.
    destinationPrefixListId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the egress-only internet gateway.
    egressOnlyInternetGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a gateway attached to your VPC.
    gatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a NAT instance in your VPC.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of Amazon Web Services account that owns the instance.
    instanceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local gateway.
    localGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a NAT gateway.
    natGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | Describes how the route was created.
    --
    -- -   @CreateRouteTable@ - The route was automatically created when the
    --     route table was created.
    --
    -- -   @CreateRoute@ - The route was manually added to the route table.
    --
    -- -   @EnableVgwRoutePropagation@ - The route was propagated by route
    --     propagation.
    origin :: Prelude.Maybe RouteOrigin,
    -- | The state of the route. The @blackhole@ state indicates that the
    -- route\'s target isn\'t available (for example, the specified gateway
    -- isn\'t attached to the VPC, or the specified NAT instance has been
    -- terminated).
    state :: Prelude.Maybe RouteState,
    -- | The ID of a transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a VPC peering connection.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Route' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'carrierGatewayId', 'route_carrierGatewayId' - The ID of the carrier gateway.
--
-- 'coreNetworkArn', 'route_coreNetworkArn' - The Amazon Resource Name (ARN) of the core network.
--
-- 'destinationCidrBlock', 'route_destinationCidrBlock' - The IPv4 CIDR block used for the destination match.
--
-- 'destinationIpv6CidrBlock', 'route_destinationIpv6CidrBlock' - The IPv6 CIDR block used for the destination match.
--
-- 'destinationPrefixListId', 'route_destinationPrefixListId' - The prefix of the Amazon Web Service.
--
-- 'egressOnlyInternetGatewayId', 'route_egressOnlyInternetGatewayId' - The ID of the egress-only internet gateway.
--
-- 'gatewayId', 'route_gatewayId' - The ID of a gateway attached to your VPC.
--
-- 'instanceId', 'route_instanceId' - The ID of a NAT instance in your VPC.
--
-- 'instanceOwnerId', 'route_instanceOwnerId' - The ID of Amazon Web Services account that owns the instance.
--
-- 'localGatewayId', 'route_localGatewayId' - The ID of the local gateway.
--
-- 'natGatewayId', 'route_natGatewayId' - The ID of a NAT gateway.
--
-- 'networkInterfaceId', 'route_networkInterfaceId' - The ID of the network interface.
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
-- 'state', 'route_state' - The state of the route. The @blackhole@ state indicates that the
-- route\'s target isn\'t available (for example, the specified gateway
-- isn\'t attached to the VPC, or the specified NAT instance has been
-- terminated).
--
-- 'transitGatewayId', 'route_transitGatewayId' - The ID of a transit gateway.
--
-- 'vpcPeeringConnectionId', 'route_vpcPeeringConnectionId' - The ID of a VPC peering connection.
newRoute ::
  Route
newRoute =
  Route'
    { carrierGatewayId = Prelude.Nothing,
      coreNetworkArn = Prelude.Nothing,
      destinationCidrBlock = Prelude.Nothing,
      destinationIpv6CidrBlock = Prelude.Nothing,
      destinationPrefixListId = Prelude.Nothing,
      egressOnlyInternetGatewayId = Prelude.Nothing,
      gatewayId = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      instanceOwnerId = Prelude.Nothing,
      localGatewayId = Prelude.Nothing,
      natGatewayId = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      origin = Prelude.Nothing,
      state = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      vpcPeeringConnectionId = Prelude.Nothing
    }

-- | The ID of the carrier gateway.
route_carrierGatewayId :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_carrierGatewayId = Lens.lens (\Route' {carrierGatewayId} -> carrierGatewayId) (\s@Route' {} a -> s {carrierGatewayId = a} :: Route)

-- | The Amazon Resource Name (ARN) of the core network.
route_coreNetworkArn :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_coreNetworkArn = Lens.lens (\Route' {coreNetworkArn} -> coreNetworkArn) (\s@Route' {} a -> s {coreNetworkArn = a} :: Route)

-- | The IPv4 CIDR block used for the destination match.
route_destinationCidrBlock :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_destinationCidrBlock = Lens.lens (\Route' {destinationCidrBlock} -> destinationCidrBlock) (\s@Route' {} a -> s {destinationCidrBlock = a} :: Route)

-- | The IPv6 CIDR block used for the destination match.
route_destinationIpv6CidrBlock :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_destinationIpv6CidrBlock = Lens.lens (\Route' {destinationIpv6CidrBlock} -> destinationIpv6CidrBlock) (\s@Route' {} a -> s {destinationIpv6CidrBlock = a} :: Route)

-- | The prefix of the Amazon Web Service.
route_destinationPrefixListId :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_destinationPrefixListId = Lens.lens (\Route' {destinationPrefixListId} -> destinationPrefixListId) (\s@Route' {} a -> s {destinationPrefixListId = a} :: Route)

-- | The ID of the egress-only internet gateway.
route_egressOnlyInternetGatewayId :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_egressOnlyInternetGatewayId = Lens.lens (\Route' {egressOnlyInternetGatewayId} -> egressOnlyInternetGatewayId) (\s@Route' {} a -> s {egressOnlyInternetGatewayId = a} :: Route)

-- | The ID of a gateway attached to your VPC.
route_gatewayId :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_gatewayId = Lens.lens (\Route' {gatewayId} -> gatewayId) (\s@Route' {} a -> s {gatewayId = a} :: Route)

-- | The ID of a NAT instance in your VPC.
route_instanceId :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_instanceId = Lens.lens (\Route' {instanceId} -> instanceId) (\s@Route' {} a -> s {instanceId = a} :: Route)

-- | The ID of Amazon Web Services account that owns the instance.
route_instanceOwnerId :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_instanceOwnerId = Lens.lens (\Route' {instanceOwnerId} -> instanceOwnerId) (\s@Route' {} a -> s {instanceOwnerId = a} :: Route)

-- | The ID of the local gateway.
route_localGatewayId :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_localGatewayId = Lens.lens (\Route' {localGatewayId} -> localGatewayId) (\s@Route' {} a -> s {localGatewayId = a} :: Route)

-- | The ID of a NAT gateway.
route_natGatewayId :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_natGatewayId = Lens.lens (\Route' {natGatewayId} -> natGatewayId) (\s@Route' {} a -> s {natGatewayId = a} :: Route)

-- | The ID of the network interface.
route_networkInterfaceId :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_networkInterfaceId = Lens.lens (\Route' {networkInterfaceId} -> networkInterfaceId) (\s@Route' {} a -> s {networkInterfaceId = a} :: Route)

-- | Describes how the route was created.
--
-- -   @CreateRouteTable@ - The route was automatically created when the
--     route table was created.
--
-- -   @CreateRoute@ - The route was manually added to the route table.
--
-- -   @EnableVgwRoutePropagation@ - The route was propagated by route
--     propagation.
route_origin :: Lens.Lens' Route (Prelude.Maybe RouteOrigin)
route_origin = Lens.lens (\Route' {origin} -> origin) (\s@Route' {} a -> s {origin = a} :: Route)

-- | The state of the route. The @blackhole@ state indicates that the
-- route\'s target isn\'t available (for example, the specified gateway
-- isn\'t attached to the VPC, or the specified NAT instance has been
-- terminated).
route_state :: Lens.Lens' Route (Prelude.Maybe RouteState)
route_state = Lens.lens (\Route' {state} -> state) (\s@Route' {} a -> s {state = a} :: Route)

-- | The ID of a transit gateway.
route_transitGatewayId :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_transitGatewayId = Lens.lens (\Route' {transitGatewayId} -> transitGatewayId) (\s@Route' {} a -> s {transitGatewayId = a} :: Route)

-- | The ID of a VPC peering connection.
route_vpcPeeringConnectionId :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_vpcPeeringConnectionId = Lens.lens (\Route' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@Route' {} a -> s {vpcPeeringConnectionId = a} :: Route)

instance Data.FromXML Route where
  parseXML x =
    Route'
      Prelude.<$> (x Data..@? "carrierGatewayId")
      Prelude.<*> (x Data..@? "coreNetworkArn")
      Prelude.<*> (x Data..@? "destinationCidrBlock")
      Prelude.<*> (x Data..@? "destinationIpv6CidrBlock")
      Prelude.<*> (x Data..@? "destinationPrefixListId")
      Prelude.<*> (x Data..@? "egressOnlyInternetGatewayId")
      Prelude.<*> (x Data..@? "gatewayId")
      Prelude.<*> (x Data..@? "instanceId")
      Prelude.<*> (x Data..@? "instanceOwnerId")
      Prelude.<*> (x Data..@? "localGatewayId")
      Prelude.<*> (x Data..@? "natGatewayId")
      Prelude.<*> (x Data..@? "networkInterfaceId")
      Prelude.<*> (x Data..@? "origin")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "transitGatewayId")
      Prelude.<*> (x Data..@? "vpcPeeringConnectionId")

instance Prelude.Hashable Route where
  hashWithSalt _salt Route' {..} =
    _salt
      `Prelude.hashWithSalt` carrierGatewayId
      `Prelude.hashWithSalt` coreNetworkArn
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` destinationIpv6CidrBlock
      `Prelude.hashWithSalt` destinationPrefixListId
      `Prelude.hashWithSalt` egressOnlyInternetGatewayId
      `Prelude.hashWithSalt` gatewayId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` instanceOwnerId
      `Prelude.hashWithSalt` localGatewayId
      `Prelude.hashWithSalt` natGatewayId
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` origin
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` transitGatewayId
      `Prelude.hashWithSalt` vpcPeeringConnectionId

instance Prelude.NFData Route where
  rnf Route' {..} =
    Prelude.rnf carrierGatewayId
      `Prelude.seq` Prelude.rnf coreNetworkArn
      `Prelude.seq` Prelude.rnf destinationCidrBlock
      `Prelude.seq` Prelude.rnf destinationIpv6CidrBlock
      `Prelude.seq` Prelude.rnf destinationPrefixListId
      `Prelude.seq` Prelude.rnf egressOnlyInternetGatewayId
      `Prelude.seq` Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf instanceOwnerId
      `Prelude.seq` Prelude.rnf localGatewayId
      `Prelude.seq` Prelude.rnf natGatewayId
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf origin
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf vpcPeeringConnectionId
