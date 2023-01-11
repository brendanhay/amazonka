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
-- Module      : Amazonka.FMS.Types.NetworkFirewallInternetTrafficNotInspectedViolation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.NetworkFirewallInternetTrafficNotInspectedViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.ExpectedRoute
import Amazonka.FMS.Types.Route
import qualified Amazonka.Prelude as Prelude

-- | Violation detail for the subnet for which internet traffic that hasn\'t
-- been inspected.
--
-- /See:/ 'newNetworkFirewallInternetTrafficNotInspectedViolation' smart constructor.
data NetworkFirewallInternetTrafficNotInspectedViolation = NetworkFirewallInternetTrafficNotInspectedViolation'
  { -- | The actual firewall subnet routes.
    actualFirewallSubnetRoutes :: Prelude.Maybe [Route],
    -- | The actual internet gateway routes.
    actualInternetGatewayRoutes :: Prelude.Maybe [Route],
    -- | Information about the subnet route table for the current firewall.
    currentFirewallSubnetRouteTable :: Prelude.Maybe Prelude.Text,
    -- | The current route table for the internet gateway.
    currentInternetGatewayRouteTable :: Prelude.Maybe Prelude.Text,
    -- | The expected endpoint for the current firewall.
    expectedFirewallEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The firewall subnet routes that are expected.
    expectedFirewallSubnetRoutes :: Prelude.Maybe [ExpectedRoute],
    -- | The internet gateway routes that are expected.
    expectedInternetGatewayRoutes :: Prelude.Maybe [ExpectedRoute],
    -- | The firewall subnet ID.
    firewallSubnetId :: Prelude.Maybe Prelude.Text,
    -- | The internet gateway ID.
    internetGatewayId :: Prelude.Maybe Prelude.Text,
    -- | Information about whether the route table is used in another
    -- Availability Zone.
    isRouteTableUsedInDifferentAZ :: Prelude.Maybe Prelude.Bool,
    -- | Information about the route table ID.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | The subnet Availability Zone.
    subnetAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The subnet ID.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The route or routes that are in violation.
    violatingRoutes :: Prelude.Maybe [Route],
    -- | Information about the VPC ID.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFirewallInternetTrafficNotInspectedViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actualFirewallSubnetRoutes', 'networkFirewallInternetTrafficNotInspectedViolation_actualFirewallSubnetRoutes' - The actual firewall subnet routes.
--
-- 'actualInternetGatewayRoutes', 'networkFirewallInternetTrafficNotInspectedViolation_actualInternetGatewayRoutes' - The actual internet gateway routes.
--
-- 'currentFirewallSubnetRouteTable', 'networkFirewallInternetTrafficNotInspectedViolation_currentFirewallSubnetRouteTable' - Information about the subnet route table for the current firewall.
--
-- 'currentInternetGatewayRouteTable', 'networkFirewallInternetTrafficNotInspectedViolation_currentInternetGatewayRouteTable' - The current route table for the internet gateway.
--
-- 'expectedFirewallEndpoint', 'networkFirewallInternetTrafficNotInspectedViolation_expectedFirewallEndpoint' - The expected endpoint for the current firewall.
--
-- 'expectedFirewallSubnetRoutes', 'networkFirewallInternetTrafficNotInspectedViolation_expectedFirewallSubnetRoutes' - The firewall subnet routes that are expected.
--
-- 'expectedInternetGatewayRoutes', 'networkFirewallInternetTrafficNotInspectedViolation_expectedInternetGatewayRoutes' - The internet gateway routes that are expected.
--
-- 'firewallSubnetId', 'networkFirewallInternetTrafficNotInspectedViolation_firewallSubnetId' - The firewall subnet ID.
--
-- 'internetGatewayId', 'networkFirewallInternetTrafficNotInspectedViolation_internetGatewayId' - The internet gateway ID.
--
-- 'isRouteTableUsedInDifferentAZ', 'networkFirewallInternetTrafficNotInspectedViolation_isRouteTableUsedInDifferentAZ' - Information about whether the route table is used in another
-- Availability Zone.
--
-- 'routeTableId', 'networkFirewallInternetTrafficNotInspectedViolation_routeTableId' - Information about the route table ID.
--
-- 'subnetAvailabilityZone', 'networkFirewallInternetTrafficNotInspectedViolation_subnetAvailabilityZone' - The subnet Availability Zone.
--
-- 'subnetId', 'networkFirewallInternetTrafficNotInspectedViolation_subnetId' - The subnet ID.
--
-- 'violatingRoutes', 'networkFirewallInternetTrafficNotInspectedViolation_violatingRoutes' - The route or routes that are in violation.
--
-- 'vpcId', 'networkFirewallInternetTrafficNotInspectedViolation_vpcId' - Information about the VPC ID.
newNetworkFirewallInternetTrafficNotInspectedViolation ::
  NetworkFirewallInternetTrafficNotInspectedViolation
newNetworkFirewallInternetTrafficNotInspectedViolation =
  NetworkFirewallInternetTrafficNotInspectedViolation'
    { actualFirewallSubnetRoutes =
        Prelude.Nothing,
      actualInternetGatewayRoutes =
        Prelude.Nothing,
      currentFirewallSubnetRouteTable =
        Prelude.Nothing,
      currentInternetGatewayRouteTable =
        Prelude.Nothing,
      expectedFirewallEndpoint =
        Prelude.Nothing,
      expectedFirewallSubnetRoutes =
        Prelude.Nothing,
      expectedInternetGatewayRoutes =
        Prelude.Nothing,
      firewallSubnetId =
        Prelude.Nothing,
      internetGatewayId =
        Prelude.Nothing,
      isRouteTableUsedInDifferentAZ =
        Prelude.Nothing,
      routeTableId =
        Prelude.Nothing,
      subnetAvailabilityZone =
        Prelude.Nothing,
      subnetId =
        Prelude.Nothing,
      violatingRoutes =
        Prelude.Nothing,
      vpcId =
        Prelude.Nothing
    }

-- | The actual firewall subnet routes.
networkFirewallInternetTrafficNotInspectedViolation_actualFirewallSubnetRoutes :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe [Route])
networkFirewallInternetTrafficNotInspectedViolation_actualFirewallSubnetRoutes = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {actualFirewallSubnetRoutes} -> actualFirewallSubnetRoutes) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {actualFirewallSubnetRoutes = a} :: NetworkFirewallInternetTrafficNotInspectedViolation) Prelude.. Lens.mapping Lens.coerced

-- | The actual internet gateway routes.
networkFirewallInternetTrafficNotInspectedViolation_actualInternetGatewayRoutes :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe [Route])
networkFirewallInternetTrafficNotInspectedViolation_actualInternetGatewayRoutes = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {actualInternetGatewayRoutes} -> actualInternetGatewayRoutes) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {actualInternetGatewayRoutes = a} :: NetworkFirewallInternetTrafficNotInspectedViolation) Prelude.. Lens.mapping Lens.coerced

-- | Information about the subnet route table for the current firewall.
networkFirewallInternetTrafficNotInspectedViolation_currentFirewallSubnetRouteTable :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe Prelude.Text)
networkFirewallInternetTrafficNotInspectedViolation_currentFirewallSubnetRouteTable = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {currentFirewallSubnetRouteTable} -> currentFirewallSubnetRouteTable) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {currentFirewallSubnetRouteTable = a} :: NetworkFirewallInternetTrafficNotInspectedViolation)

-- | The current route table for the internet gateway.
networkFirewallInternetTrafficNotInspectedViolation_currentInternetGatewayRouteTable :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe Prelude.Text)
networkFirewallInternetTrafficNotInspectedViolation_currentInternetGatewayRouteTable = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {currentInternetGatewayRouteTable} -> currentInternetGatewayRouteTable) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {currentInternetGatewayRouteTable = a} :: NetworkFirewallInternetTrafficNotInspectedViolation)

-- | The expected endpoint for the current firewall.
networkFirewallInternetTrafficNotInspectedViolation_expectedFirewallEndpoint :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe Prelude.Text)
networkFirewallInternetTrafficNotInspectedViolation_expectedFirewallEndpoint = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {expectedFirewallEndpoint} -> expectedFirewallEndpoint) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {expectedFirewallEndpoint = a} :: NetworkFirewallInternetTrafficNotInspectedViolation)

-- | The firewall subnet routes that are expected.
networkFirewallInternetTrafficNotInspectedViolation_expectedFirewallSubnetRoutes :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe [ExpectedRoute])
networkFirewallInternetTrafficNotInspectedViolation_expectedFirewallSubnetRoutes = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {expectedFirewallSubnetRoutes} -> expectedFirewallSubnetRoutes) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {expectedFirewallSubnetRoutes = a} :: NetworkFirewallInternetTrafficNotInspectedViolation) Prelude.. Lens.mapping Lens.coerced

-- | The internet gateway routes that are expected.
networkFirewallInternetTrafficNotInspectedViolation_expectedInternetGatewayRoutes :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe [ExpectedRoute])
networkFirewallInternetTrafficNotInspectedViolation_expectedInternetGatewayRoutes = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {expectedInternetGatewayRoutes} -> expectedInternetGatewayRoutes) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {expectedInternetGatewayRoutes = a} :: NetworkFirewallInternetTrafficNotInspectedViolation) Prelude.. Lens.mapping Lens.coerced

-- | The firewall subnet ID.
networkFirewallInternetTrafficNotInspectedViolation_firewallSubnetId :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe Prelude.Text)
networkFirewallInternetTrafficNotInspectedViolation_firewallSubnetId = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {firewallSubnetId} -> firewallSubnetId) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {firewallSubnetId = a} :: NetworkFirewallInternetTrafficNotInspectedViolation)

-- | The internet gateway ID.
networkFirewallInternetTrafficNotInspectedViolation_internetGatewayId :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe Prelude.Text)
networkFirewallInternetTrafficNotInspectedViolation_internetGatewayId = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {internetGatewayId} -> internetGatewayId) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {internetGatewayId = a} :: NetworkFirewallInternetTrafficNotInspectedViolation)

-- | Information about whether the route table is used in another
-- Availability Zone.
networkFirewallInternetTrafficNotInspectedViolation_isRouteTableUsedInDifferentAZ :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe Prelude.Bool)
networkFirewallInternetTrafficNotInspectedViolation_isRouteTableUsedInDifferentAZ = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {isRouteTableUsedInDifferentAZ} -> isRouteTableUsedInDifferentAZ) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {isRouteTableUsedInDifferentAZ = a} :: NetworkFirewallInternetTrafficNotInspectedViolation)

-- | Information about the route table ID.
networkFirewallInternetTrafficNotInspectedViolation_routeTableId :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe Prelude.Text)
networkFirewallInternetTrafficNotInspectedViolation_routeTableId = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {routeTableId} -> routeTableId) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {routeTableId = a} :: NetworkFirewallInternetTrafficNotInspectedViolation)

-- | The subnet Availability Zone.
networkFirewallInternetTrafficNotInspectedViolation_subnetAvailabilityZone :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe Prelude.Text)
networkFirewallInternetTrafficNotInspectedViolation_subnetAvailabilityZone = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {subnetAvailabilityZone} -> subnetAvailabilityZone) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {subnetAvailabilityZone = a} :: NetworkFirewallInternetTrafficNotInspectedViolation)

-- | The subnet ID.
networkFirewallInternetTrafficNotInspectedViolation_subnetId :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe Prelude.Text)
networkFirewallInternetTrafficNotInspectedViolation_subnetId = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {subnetId} -> subnetId) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {subnetId = a} :: NetworkFirewallInternetTrafficNotInspectedViolation)

-- | The route or routes that are in violation.
networkFirewallInternetTrafficNotInspectedViolation_violatingRoutes :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe [Route])
networkFirewallInternetTrafficNotInspectedViolation_violatingRoutes = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {violatingRoutes} -> violatingRoutes) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {violatingRoutes = a} :: NetworkFirewallInternetTrafficNotInspectedViolation) Prelude.. Lens.mapping Lens.coerced

-- | Information about the VPC ID.
networkFirewallInternetTrafficNotInspectedViolation_vpcId :: Lens.Lens' NetworkFirewallInternetTrafficNotInspectedViolation (Prelude.Maybe Prelude.Text)
networkFirewallInternetTrafficNotInspectedViolation_vpcId = Lens.lens (\NetworkFirewallInternetTrafficNotInspectedViolation' {vpcId} -> vpcId) (\s@NetworkFirewallInternetTrafficNotInspectedViolation' {} a -> s {vpcId = a} :: NetworkFirewallInternetTrafficNotInspectedViolation)

instance
  Data.FromJSON
    NetworkFirewallInternetTrafficNotInspectedViolation
  where
  parseJSON =
    Data.withObject
      "NetworkFirewallInternetTrafficNotInspectedViolation"
      ( \x ->
          NetworkFirewallInternetTrafficNotInspectedViolation'
            Prelude.<$> ( x Data..:? "ActualFirewallSubnetRoutes"
                            Data..!= Prelude.mempty
                        )
              Prelude.<*> ( x Data..:? "ActualInternetGatewayRoutes"
                              Data..!= Prelude.mempty
                          )
              Prelude.<*> (x Data..:? "CurrentFirewallSubnetRouteTable")
              Prelude.<*> (x Data..:? "CurrentInternetGatewayRouteTable")
              Prelude.<*> (x Data..:? "ExpectedFirewallEndpoint")
              Prelude.<*> ( x Data..:? "ExpectedFirewallSubnetRoutes"
                              Data..!= Prelude.mempty
                          )
              Prelude.<*> ( x Data..:? "ExpectedInternetGatewayRoutes"
                              Data..!= Prelude.mempty
                          )
              Prelude.<*> (x Data..:? "FirewallSubnetId")
              Prelude.<*> (x Data..:? "InternetGatewayId")
              Prelude.<*> (x Data..:? "IsRouteTableUsedInDifferentAZ")
              Prelude.<*> (x Data..:? "RouteTableId")
              Prelude.<*> (x Data..:? "SubnetAvailabilityZone")
              Prelude.<*> (x Data..:? "SubnetId")
              Prelude.<*> ( x Data..:? "ViolatingRoutes"
                              Data..!= Prelude.mempty
                          )
              Prelude.<*> (x Data..:? "VpcId")
      )

instance
  Prelude.Hashable
    NetworkFirewallInternetTrafficNotInspectedViolation
  where
  hashWithSalt
    _salt
    NetworkFirewallInternetTrafficNotInspectedViolation' {..} =
      _salt
        `Prelude.hashWithSalt` actualFirewallSubnetRoutes
        `Prelude.hashWithSalt` actualInternetGatewayRoutes
        `Prelude.hashWithSalt` currentFirewallSubnetRouteTable
        `Prelude.hashWithSalt` currentInternetGatewayRouteTable
        `Prelude.hashWithSalt` expectedFirewallEndpoint
        `Prelude.hashWithSalt` expectedFirewallSubnetRoutes
        `Prelude.hashWithSalt` expectedInternetGatewayRoutes
        `Prelude.hashWithSalt` firewallSubnetId
        `Prelude.hashWithSalt` internetGatewayId
        `Prelude.hashWithSalt` isRouteTableUsedInDifferentAZ
        `Prelude.hashWithSalt` routeTableId
        `Prelude.hashWithSalt` subnetAvailabilityZone
        `Prelude.hashWithSalt` subnetId
        `Prelude.hashWithSalt` violatingRoutes
        `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    NetworkFirewallInternetTrafficNotInspectedViolation
  where
  rnf
    NetworkFirewallInternetTrafficNotInspectedViolation' {..} =
      Prelude.rnf actualFirewallSubnetRoutes
        `Prelude.seq` Prelude.rnf actualInternetGatewayRoutes
        `Prelude.seq` Prelude.rnf currentFirewallSubnetRouteTable
        `Prelude.seq` Prelude.rnf currentInternetGatewayRouteTable
        `Prelude.seq` Prelude.rnf expectedFirewallEndpoint
        `Prelude.seq` Prelude.rnf expectedFirewallSubnetRoutes
        `Prelude.seq` Prelude.rnf expectedInternetGatewayRoutes
        `Prelude.seq` Prelude.rnf firewallSubnetId
        `Prelude.seq` Prelude.rnf internetGatewayId
        `Prelude.seq` Prelude.rnf isRouteTableUsedInDifferentAZ
        `Prelude.seq` Prelude.rnf routeTableId
        `Prelude.seq` Prelude.rnf subnetAvailabilityZone
        `Prelude.seq` Prelude.rnf subnetId
        `Prelude.seq` Prelude.rnf violatingRoutes
        `Prelude.seq` Prelude.rnf vpcId
