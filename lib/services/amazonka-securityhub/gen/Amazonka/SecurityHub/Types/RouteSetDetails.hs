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
-- Module      : Amazonka.SecurityHub.Types.RouteSetDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RouteSetDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the routes in the route table.
--
-- /See:/ 'newRouteSetDetails' smart constructor.
data RouteSetDetails = RouteSetDetails'
  { -- | The ID of the carrier gateway.
    carrierGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the core network.
    coreNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 CIDR block used for the destination match.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR block used for the destination match.
    destinationIpv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The prefix of the destination Amazon Web Service.
    destinationPrefixListId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the egress-only internet gateway.
    egressOnlyInternetGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a gateway attached to your VPC.
    gatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a NAT instance in your VPC.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the instance.
    instanceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local gateway.
    localGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a NAT gateway.
    natGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | Describes how the route was created.
    origin :: Prelude.Maybe Prelude.Text,
    -- | The state of the route.
    state :: Prelude.Maybe Prelude.Text,
    -- | The ID of a transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a VPC peering connection.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteSetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'carrierGatewayId', 'routeSetDetails_carrierGatewayId' - The ID of the carrier gateway.
--
-- 'coreNetworkArn', 'routeSetDetails_coreNetworkArn' - The Amazon Resource Name (ARN) of the core network.
--
-- 'destinationCidrBlock', 'routeSetDetails_destinationCidrBlock' - The IPv4 CIDR block used for the destination match.
--
-- 'destinationIpv6CidrBlock', 'routeSetDetails_destinationIpv6CidrBlock' - The IPv6 CIDR block used for the destination match.
--
-- 'destinationPrefixListId', 'routeSetDetails_destinationPrefixListId' - The prefix of the destination Amazon Web Service.
--
-- 'egressOnlyInternetGatewayId', 'routeSetDetails_egressOnlyInternetGatewayId' - The ID of the egress-only internet gateway.
--
-- 'gatewayId', 'routeSetDetails_gatewayId' - The ID of a gateway attached to your VPC.
--
-- 'instanceId', 'routeSetDetails_instanceId' - The ID of a NAT instance in your VPC.
--
-- 'instanceOwnerId', 'routeSetDetails_instanceOwnerId' - The ID of the Amazon Web Services account that owns the instance.
--
-- 'localGatewayId', 'routeSetDetails_localGatewayId' - The ID of the local gateway.
--
-- 'natGatewayId', 'routeSetDetails_natGatewayId' - The ID of a NAT gateway.
--
-- 'networkInterfaceId', 'routeSetDetails_networkInterfaceId' - The ID of the network interface.
--
-- 'origin', 'routeSetDetails_origin' - Describes how the route was created.
--
-- 'state', 'routeSetDetails_state' - The state of the route.
--
-- 'transitGatewayId', 'routeSetDetails_transitGatewayId' - The ID of a transit gateway.
--
-- 'vpcPeeringConnectionId', 'routeSetDetails_vpcPeeringConnectionId' - The ID of a VPC peering connection.
newRouteSetDetails ::
  RouteSetDetails
newRouteSetDetails =
  RouteSetDetails'
    { carrierGatewayId =
        Prelude.Nothing,
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
routeSetDetails_carrierGatewayId :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_carrierGatewayId = Lens.lens (\RouteSetDetails' {carrierGatewayId} -> carrierGatewayId) (\s@RouteSetDetails' {} a -> s {carrierGatewayId = a} :: RouteSetDetails)

-- | The Amazon Resource Name (ARN) of the core network.
routeSetDetails_coreNetworkArn :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_coreNetworkArn = Lens.lens (\RouteSetDetails' {coreNetworkArn} -> coreNetworkArn) (\s@RouteSetDetails' {} a -> s {coreNetworkArn = a} :: RouteSetDetails)

-- | The IPv4 CIDR block used for the destination match.
routeSetDetails_destinationCidrBlock :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_destinationCidrBlock = Lens.lens (\RouteSetDetails' {destinationCidrBlock} -> destinationCidrBlock) (\s@RouteSetDetails' {} a -> s {destinationCidrBlock = a} :: RouteSetDetails)

-- | The IPv6 CIDR block used for the destination match.
routeSetDetails_destinationIpv6CidrBlock :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_destinationIpv6CidrBlock = Lens.lens (\RouteSetDetails' {destinationIpv6CidrBlock} -> destinationIpv6CidrBlock) (\s@RouteSetDetails' {} a -> s {destinationIpv6CidrBlock = a} :: RouteSetDetails)

-- | The prefix of the destination Amazon Web Service.
routeSetDetails_destinationPrefixListId :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_destinationPrefixListId = Lens.lens (\RouteSetDetails' {destinationPrefixListId} -> destinationPrefixListId) (\s@RouteSetDetails' {} a -> s {destinationPrefixListId = a} :: RouteSetDetails)

-- | The ID of the egress-only internet gateway.
routeSetDetails_egressOnlyInternetGatewayId :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_egressOnlyInternetGatewayId = Lens.lens (\RouteSetDetails' {egressOnlyInternetGatewayId} -> egressOnlyInternetGatewayId) (\s@RouteSetDetails' {} a -> s {egressOnlyInternetGatewayId = a} :: RouteSetDetails)

-- | The ID of a gateway attached to your VPC.
routeSetDetails_gatewayId :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_gatewayId = Lens.lens (\RouteSetDetails' {gatewayId} -> gatewayId) (\s@RouteSetDetails' {} a -> s {gatewayId = a} :: RouteSetDetails)

-- | The ID of a NAT instance in your VPC.
routeSetDetails_instanceId :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_instanceId = Lens.lens (\RouteSetDetails' {instanceId} -> instanceId) (\s@RouteSetDetails' {} a -> s {instanceId = a} :: RouteSetDetails)

-- | The ID of the Amazon Web Services account that owns the instance.
routeSetDetails_instanceOwnerId :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_instanceOwnerId = Lens.lens (\RouteSetDetails' {instanceOwnerId} -> instanceOwnerId) (\s@RouteSetDetails' {} a -> s {instanceOwnerId = a} :: RouteSetDetails)

-- | The ID of the local gateway.
routeSetDetails_localGatewayId :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_localGatewayId = Lens.lens (\RouteSetDetails' {localGatewayId} -> localGatewayId) (\s@RouteSetDetails' {} a -> s {localGatewayId = a} :: RouteSetDetails)

-- | The ID of a NAT gateway.
routeSetDetails_natGatewayId :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_natGatewayId = Lens.lens (\RouteSetDetails' {natGatewayId} -> natGatewayId) (\s@RouteSetDetails' {} a -> s {natGatewayId = a} :: RouteSetDetails)

-- | The ID of the network interface.
routeSetDetails_networkInterfaceId :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_networkInterfaceId = Lens.lens (\RouteSetDetails' {networkInterfaceId} -> networkInterfaceId) (\s@RouteSetDetails' {} a -> s {networkInterfaceId = a} :: RouteSetDetails)

-- | Describes how the route was created.
routeSetDetails_origin :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_origin = Lens.lens (\RouteSetDetails' {origin} -> origin) (\s@RouteSetDetails' {} a -> s {origin = a} :: RouteSetDetails)

-- | The state of the route.
routeSetDetails_state :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_state = Lens.lens (\RouteSetDetails' {state} -> state) (\s@RouteSetDetails' {} a -> s {state = a} :: RouteSetDetails)

-- | The ID of a transit gateway.
routeSetDetails_transitGatewayId :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_transitGatewayId = Lens.lens (\RouteSetDetails' {transitGatewayId} -> transitGatewayId) (\s@RouteSetDetails' {} a -> s {transitGatewayId = a} :: RouteSetDetails)

-- | The ID of a VPC peering connection.
routeSetDetails_vpcPeeringConnectionId :: Lens.Lens' RouteSetDetails (Prelude.Maybe Prelude.Text)
routeSetDetails_vpcPeeringConnectionId = Lens.lens (\RouteSetDetails' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@RouteSetDetails' {} a -> s {vpcPeeringConnectionId = a} :: RouteSetDetails)

instance Data.FromJSON RouteSetDetails where
  parseJSON =
    Data.withObject
      "RouteSetDetails"
      ( \x ->
          RouteSetDetails'
            Prelude.<$> (x Data..:? "CarrierGatewayId")
            Prelude.<*> (x Data..:? "CoreNetworkArn")
            Prelude.<*> (x Data..:? "DestinationCidrBlock")
            Prelude.<*> (x Data..:? "DestinationIpv6CidrBlock")
            Prelude.<*> (x Data..:? "DestinationPrefixListId")
            Prelude.<*> (x Data..:? "EgressOnlyInternetGatewayId")
            Prelude.<*> (x Data..:? "GatewayId")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "InstanceOwnerId")
            Prelude.<*> (x Data..:? "LocalGatewayId")
            Prelude.<*> (x Data..:? "NatGatewayId")
            Prelude.<*> (x Data..:? "NetworkInterfaceId")
            Prelude.<*> (x Data..:? "Origin")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "TransitGatewayId")
            Prelude.<*> (x Data..:? "VpcPeeringConnectionId")
      )

instance Prelude.Hashable RouteSetDetails where
  hashWithSalt _salt RouteSetDetails' {..} =
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

instance Prelude.NFData RouteSetDetails where
  rnf RouteSetDetails' {..} =
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

instance Data.ToJSON RouteSetDetails where
  toJSON RouteSetDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CarrierGatewayId" Data..=)
              Prelude.<$> carrierGatewayId,
            ("CoreNetworkArn" Data..=)
              Prelude.<$> coreNetworkArn,
            ("DestinationCidrBlock" Data..=)
              Prelude.<$> destinationCidrBlock,
            ("DestinationIpv6CidrBlock" Data..=)
              Prelude.<$> destinationIpv6CidrBlock,
            ("DestinationPrefixListId" Data..=)
              Prelude.<$> destinationPrefixListId,
            ("EgressOnlyInternetGatewayId" Data..=)
              Prelude.<$> egressOnlyInternetGatewayId,
            ("GatewayId" Data..=) Prelude.<$> gatewayId,
            ("InstanceId" Data..=) Prelude.<$> instanceId,
            ("InstanceOwnerId" Data..=)
              Prelude.<$> instanceOwnerId,
            ("LocalGatewayId" Data..=)
              Prelude.<$> localGatewayId,
            ("NatGatewayId" Data..=) Prelude.<$> natGatewayId,
            ("NetworkInterfaceId" Data..=)
              Prelude.<$> networkInterfaceId,
            ("Origin" Data..=) Prelude.<$> origin,
            ("State" Data..=) Prelude.<$> state,
            ("TransitGatewayId" Data..=)
              Prelude.<$> transitGatewayId,
            ("VpcPeeringConnectionId" Data..=)
              Prelude.<$> vpcPeeringConnectionId
          ]
      )
