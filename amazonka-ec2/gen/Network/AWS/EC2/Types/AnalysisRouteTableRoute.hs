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
-- Module      : Network.AWS.EC2.Types.AnalysisRouteTableRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AnalysisRouteTableRoute where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a route table route.
--
-- /See:/ 'newAnalysisRouteTableRoute' smart constructor.
data AnalysisRouteTableRoute = AnalysisRouteTableRoute'
  { -- | The ID of the instance, such as a NAT instance.
    instanceId :: Core.Maybe Core.Text,
    -- | Describes how the route was created. The following are possible values:
    --
    -- -   @CreateRouteTable@ - The route was automatically created when the
    --     route table was created.
    --
    -- -   @CreateRoute@ - The route was manually added to the route table.
    --
    -- -   @EnableVgwRoutePropagation@ - The route was propagated by route
    --     propagation.
    origin :: Core.Maybe Core.Text,
    -- | The ID of a VPC peering connection.
    vpcPeeringConnectionId :: Core.Maybe Core.Text,
    -- | The prefix of the AWS service.
    destinationPrefixListId :: Core.Maybe Core.Text,
    -- | The destination IPv4 address, in CIDR notation.
    destinationCidr :: Core.Maybe Core.Text,
    -- | The ID of an egress-only internet gateway.
    egressOnlyInternetGatewayId :: Core.Maybe Core.Text,
    -- | The ID of a network interface.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The ID of a NAT gateway.
    natGatewayId :: Core.Maybe Core.Text,
    -- | The ID of the gateway, such as an internet gateway or virtual private
    -- gateway.
    gatewayId :: Core.Maybe Core.Text,
    -- | The ID of a transit gateway.
    transitGatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AnalysisRouteTableRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'analysisRouteTableRoute_instanceId' - The ID of the instance, such as a NAT instance.
--
-- 'origin', 'analysisRouteTableRoute_origin' - Describes how the route was created. The following are possible values:
--
-- -   @CreateRouteTable@ - The route was automatically created when the
--     route table was created.
--
-- -   @CreateRoute@ - The route was manually added to the route table.
--
-- -   @EnableVgwRoutePropagation@ - The route was propagated by route
--     propagation.
--
-- 'vpcPeeringConnectionId', 'analysisRouteTableRoute_vpcPeeringConnectionId' - The ID of a VPC peering connection.
--
-- 'destinationPrefixListId', 'analysisRouteTableRoute_destinationPrefixListId' - The prefix of the AWS service.
--
-- 'destinationCidr', 'analysisRouteTableRoute_destinationCidr' - The destination IPv4 address, in CIDR notation.
--
-- 'egressOnlyInternetGatewayId', 'analysisRouteTableRoute_egressOnlyInternetGatewayId' - The ID of an egress-only internet gateway.
--
-- 'networkInterfaceId', 'analysisRouteTableRoute_networkInterfaceId' - The ID of a network interface.
--
-- 'natGatewayId', 'analysisRouteTableRoute_natGatewayId' - The ID of a NAT gateway.
--
-- 'gatewayId', 'analysisRouteTableRoute_gatewayId' - The ID of the gateway, such as an internet gateway or virtual private
-- gateway.
--
-- 'transitGatewayId', 'analysisRouteTableRoute_transitGatewayId' - The ID of a transit gateway.
newAnalysisRouteTableRoute ::
  AnalysisRouteTableRoute
newAnalysisRouteTableRoute =
  AnalysisRouteTableRoute'
    { instanceId = Core.Nothing,
      origin = Core.Nothing,
      vpcPeeringConnectionId = Core.Nothing,
      destinationPrefixListId = Core.Nothing,
      destinationCidr = Core.Nothing,
      egressOnlyInternetGatewayId = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      natGatewayId = Core.Nothing,
      gatewayId = Core.Nothing,
      transitGatewayId = Core.Nothing
    }

-- | The ID of the instance, such as a NAT instance.
analysisRouteTableRoute_instanceId :: Lens.Lens' AnalysisRouteTableRoute (Core.Maybe Core.Text)
analysisRouteTableRoute_instanceId = Lens.lens (\AnalysisRouteTableRoute' {instanceId} -> instanceId) (\s@AnalysisRouteTableRoute' {} a -> s {instanceId = a} :: AnalysisRouteTableRoute)

-- | Describes how the route was created. The following are possible values:
--
-- -   @CreateRouteTable@ - The route was automatically created when the
--     route table was created.
--
-- -   @CreateRoute@ - The route was manually added to the route table.
--
-- -   @EnableVgwRoutePropagation@ - The route was propagated by route
--     propagation.
analysisRouteTableRoute_origin :: Lens.Lens' AnalysisRouteTableRoute (Core.Maybe Core.Text)
analysisRouteTableRoute_origin = Lens.lens (\AnalysisRouteTableRoute' {origin} -> origin) (\s@AnalysisRouteTableRoute' {} a -> s {origin = a} :: AnalysisRouteTableRoute)

-- | The ID of a VPC peering connection.
analysisRouteTableRoute_vpcPeeringConnectionId :: Lens.Lens' AnalysisRouteTableRoute (Core.Maybe Core.Text)
analysisRouteTableRoute_vpcPeeringConnectionId = Lens.lens (\AnalysisRouteTableRoute' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@AnalysisRouteTableRoute' {} a -> s {vpcPeeringConnectionId = a} :: AnalysisRouteTableRoute)

-- | The prefix of the AWS service.
analysisRouteTableRoute_destinationPrefixListId :: Lens.Lens' AnalysisRouteTableRoute (Core.Maybe Core.Text)
analysisRouteTableRoute_destinationPrefixListId = Lens.lens (\AnalysisRouteTableRoute' {destinationPrefixListId} -> destinationPrefixListId) (\s@AnalysisRouteTableRoute' {} a -> s {destinationPrefixListId = a} :: AnalysisRouteTableRoute)

-- | The destination IPv4 address, in CIDR notation.
analysisRouteTableRoute_destinationCidr :: Lens.Lens' AnalysisRouteTableRoute (Core.Maybe Core.Text)
analysisRouteTableRoute_destinationCidr = Lens.lens (\AnalysisRouteTableRoute' {destinationCidr} -> destinationCidr) (\s@AnalysisRouteTableRoute' {} a -> s {destinationCidr = a} :: AnalysisRouteTableRoute)

-- | The ID of an egress-only internet gateway.
analysisRouteTableRoute_egressOnlyInternetGatewayId :: Lens.Lens' AnalysisRouteTableRoute (Core.Maybe Core.Text)
analysisRouteTableRoute_egressOnlyInternetGatewayId = Lens.lens (\AnalysisRouteTableRoute' {egressOnlyInternetGatewayId} -> egressOnlyInternetGatewayId) (\s@AnalysisRouteTableRoute' {} a -> s {egressOnlyInternetGatewayId = a} :: AnalysisRouteTableRoute)

-- | The ID of a network interface.
analysisRouteTableRoute_networkInterfaceId :: Lens.Lens' AnalysisRouteTableRoute (Core.Maybe Core.Text)
analysisRouteTableRoute_networkInterfaceId = Lens.lens (\AnalysisRouteTableRoute' {networkInterfaceId} -> networkInterfaceId) (\s@AnalysisRouteTableRoute' {} a -> s {networkInterfaceId = a} :: AnalysisRouteTableRoute)

-- | The ID of a NAT gateway.
analysisRouteTableRoute_natGatewayId :: Lens.Lens' AnalysisRouteTableRoute (Core.Maybe Core.Text)
analysisRouteTableRoute_natGatewayId = Lens.lens (\AnalysisRouteTableRoute' {natGatewayId} -> natGatewayId) (\s@AnalysisRouteTableRoute' {} a -> s {natGatewayId = a} :: AnalysisRouteTableRoute)

-- | The ID of the gateway, such as an internet gateway or virtual private
-- gateway.
analysisRouteTableRoute_gatewayId :: Lens.Lens' AnalysisRouteTableRoute (Core.Maybe Core.Text)
analysisRouteTableRoute_gatewayId = Lens.lens (\AnalysisRouteTableRoute' {gatewayId} -> gatewayId) (\s@AnalysisRouteTableRoute' {} a -> s {gatewayId = a} :: AnalysisRouteTableRoute)

-- | The ID of a transit gateway.
analysisRouteTableRoute_transitGatewayId :: Lens.Lens' AnalysisRouteTableRoute (Core.Maybe Core.Text)
analysisRouteTableRoute_transitGatewayId = Lens.lens (\AnalysisRouteTableRoute' {transitGatewayId} -> transitGatewayId) (\s@AnalysisRouteTableRoute' {} a -> s {transitGatewayId = a} :: AnalysisRouteTableRoute)

instance Core.FromXML AnalysisRouteTableRoute where
  parseXML x =
    AnalysisRouteTableRoute'
      Core.<$> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "origin")
      Core.<*> (x Core..@? "vpcPeeringConnectionId")
      Core.<*> (x Core..@? "destinationPrefixListId")
      Core.<*> (x Core..@? "destinationCidr")
      Core.<*> (x Core..@? "egressOnlyInternetGatewayId")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "natGatewayId")
      Core.<*> (x Core..@? "gatewayId")
      Core.<*> (x Core..@? "transitGatewayId")

instance Core.Hashable AnalysisRouteTableRoute

instance Core.NFData AnalysisRouteTableRoute
