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
-- Module      : Amazonka.FMS.Types.NetworkFirewallInvalidRouteConfigurationViolation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.NetworkFirewallInvalidRouteConfigurationViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.ExpectedRoute
import Amazonka.FMS.Types.Route
import qualified Amazonka.Prelude as Prelude

-- | Violation detail for the improperly configured subnet route. It\'s
-- possible there is a missing route table route, or a configuration that
-- causes traffic to cross an Availability Zone boundary.
--
-- /See:/ 'newNetworkFirewallInvalidRouteConfigurationViolation' smart constructor.
data NetworkFirewallInvalidRouteConfigurationViolation = NetworkFirewallInvalidRouteConfigurationViolation'
  { -- | The actual internet gateway routes.
    actualInternetGatewayRoutes :: Prelude.Maybe [Route],
    -- | The subnet route table for the current firewall.
    currentFirewallSubnetRouteTable :: Prelude.Maybe Prelude.Text,
    -- | The route that\'s in violation.
    violatingRoute :: Prelude.Maybe Route,
    -- | The firewall endpoint that\'s expected.
    expectedFirewallEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The actual firewall subnet routes that are expected.
    actualFirewallSubnetRoutes :: Prelude.Maybe [Route],
    -- | The expected routes for the internet gateway.
    expectedInternetGatewayRoutes :: Prelude.Maybe [ExpectedRoute],
    -- | The firewall subnet routes that are expected.
    expectedFirewallSubnetRoutes :: Prelude.Maybe [ExpectedRoute],
    -- | The route table for the current internet gateway.
    currentInternetGatewayRouteTable :: Prelude.Maybe Prelude.Text,
    -- | The route table ID.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | The internet gateway ID.
    internetGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The actual firewall endpoint.
    actualFirewallEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The expected subnet ID for the firewall.
    expectedFirewallSubnetId :: Prelude.Maybe Prelude.Text,
    -- | Information about the VPC ID.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The subnets that are affected.
    affectedSubnets :: Prelude.Maybe [Prelude.Text],
    -- | The actual subnet ID for the firewall.
    actualFirewallSubnetId :: Prelude.Maybe Prelude.Text,
    -- | Information about whether the route table is used in another
    -- Availability Zone.
    isRouteTableUsedInDifferentAZ :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFirewallInvalidRouteConfigurationViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actualInternetGatewayRoutes', 'networkFirewallInvalidRouteConfigurationViolation_actualInternetGatewayRoutes' - The actual internet gateway routes.
--
-- 'currentFirewallSubnetRouteTable', 'networkFirewallInvalidRouteConfigurationViolation_currentFirewallSubnetRouteTable' - The subnet route table for the current firewall.
--
-- 'violatingRoute', 'networkFirewallInvalidRouteConfigurationViolation_violatingRoute' - The route that\'s in violation.
--
-- 'expectedFirewallEndpoint', 'networkFirewallInvalidRouteConfigurationViolation_expectedFirewallEndpoint' - The firewall endpoint that\'s expected.
--
-- 'actualFirewallSubnetRoutes', 'networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetRoutes' - The actual firewall subnet routes that are expected.
--
-- 'expectedInternetGatewayRoutes', 'networkFirewallInvalidRouteConfigurationViolation_expectedInternetGatewayRoutes' - The expected routes for the internet gateway.
--
-- 'expectedFirewallSubnetRoutes', 'networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetRoutes' - The firewall subnet routes that are expected.
--
-- 'currentInternetGatewayRouteTable', 'networkFirewallInvalidRouteConfigurationViolation_currentInternetGatewayRouteTable' - The route table for the current internet gateway.
--
-- 'routeTableId', 'networkFirewallInvalidRouteConfigurationViolation_routeTableId' - The route table ID.
--
-- 'internetGatewayId', 'networkFirewallInvalidRouteConfigurationViolation_internetGatewayId' - The internet gateway ID.
--
-- 'actualFirewallEndpoint', 'networkFirewallInvalidRouteConfigurationViolation_actualFirewallEndpoint' - The actual firewall endpoint.
--
-- 'expectedFirewallSubnetId', 'networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetId' - The expected subnet ID for the firewall.
--
-- 'vpcId', 'networkFirewallInvalidRouteConfigurationViolation_vpcId' - Information about the VPC ID.
--
-- 'affectedSubnets', 'networkFirewallInvalidRouteConfigurationViolation_affectedSubnets' - The subnets that are affected.
--
-- 'actualFirewallSubnetId', 'networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetId' - The actual subnet ID for the firewall.
--
-- 'isRouteTableUsedInDifferentAZ', 'networkFirewallInvalidRouteConfigurationViolation_isRouteTableUsedInDifferentAZ' - Information about whether the route table is used in another
-- Availability Zone.
newNetworkFirewallInvalidRouteConfigurationViolation ::
  NetworkFirewallInvalidRouteConfigurationViolation
newNetworkFirewallInvalidRouteConfigurationViolation =
  NetworkFirewallInvalidRouteConfigurationViolation'
    { actualInternetGatewayRoutes =
        Prelude.Nothing,
      currentFirewallSubnetRouteTable =
        Prelude.Nothing,
      violatingRoute =
        Prelude.Nothing,
      expectedFirewallEndpoint =
        Prelude.Nothing,
      actualFirewallSubnetRoutes =
        Prelude.Nothing,
      expectedInternetGatewayRoutes =
        Prelude.Nothing,
      expectedFirewallSubnetRoutes =
        Prelude.Nothing,
      currentInternetGatewayRouteTable =
        Prelude.Nothing,
      routeTableId =
        Prelude.Nothing,
      internetGatewayId =
        Prelude.Nothing,
      actualFirewallEndpoint =
        Prelude.Nothing,
      expectedFirewallSubnetId =
        Prelude.Nothing,
      vpcId = Prelude.Nothing,
      affectedSubnets =
        Prelude.Nothing,
      actualFirewallSubnetId =
        Prelude.Nothing,
      isRouteTableUsedInDifferentAZ =
        Prelude.Nothing
    }

-- | The actual internet gateway routes.
networkFirewallInvalidRouteConfigurationViolation_actualInternetGatewayRoutes :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe [Route])
networkFirewallInvalidRouteConfigurationViolation_actualInternetGatewayRoutes = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {actualInternetGatewayRoutes} -> actualInternetGatewayRoutes) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {actualInternetGatewayRoutes = a} :: NetworkFirewallInvalidRouteConfigurationViolation) Prelude.. Lens.mapping Lens.coerced

-- | The subnet route table for the current firewall.
networkFirewallInvalidRouteConfigurationViolation_currentFirewallSubnetRouteTable :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_currentFirewallSubnetRouteTable = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {currentFirewallSubnetRouteTable} -> currentFirewallSubnetRouteTable) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {currentFirewallSubnetRouteTable = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The route that\'s in violation.
networkFirewallInvalidRouteConfigurationViolation_violatingRoute :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Route)
networkFirewallInvalidRouteConfigurationViolation_violatingRoute = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {violatingRoute} -> violatingRoute) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {violatingRoute = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The firewall endpoint that\'s expected.
networkFirewallInvalidRouteConfigurationViolation_expectedFirewallEndpoint :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_expectedFirewallEndpoint = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {expectedFirewallEndpoint} -> expectedFirewallEndpoint) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {expectedFirewallEndpoint = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The actual firewall subnet routes that are expected.
networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetRoutes :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe [Route])
networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetRoutes = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {actualFirewallSubnetRoutes} -> actualFirewallSubnetRoutes) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {actualFirewallSubnetRoutes = a} :: NetworkFirewallInvalidRouteConfigurationViolation) Prelude.. Lens.mapping Lens.coerced

-- | The expected routes for the internet gateway.
networkFirewallInvalidRouteConfigurationViolation_expectedInternetGatewayRoutes :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe [ExpectedRoute])
networkFirewallInvalidRouteConfigurationViolation_expectedInternetGatewayRoutes = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {expectedInternetGatewayRoutes} -> expectedInternetGatewayRoutes) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {expectedInternetGatewayRoutes = a} :: NetworkFirewallInvalidRouteConfigurationViolation) Prelude.. Lens.mapping Lens.coerced

-- | The firewall subnet routes that are expected.
networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetRoutes :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe [ExpectedRoute])
networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetRoutes = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {expectedFirewallSubnetRoutes} -> expectedFirewallSubnetRoutes) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {expectedFirewallSubnetRoutes = a} :: NetworkFirewallInvalidRouteConfigurationViolation) Prelude.. Lens.mapping Lens.coerced

-- | The route table for the current internet gateway.
networkFirewallInvalidRouteConfigurationViolation_currentInternetGatewayRouteTable :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_currentInternetGatewayRouteTable = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {currentInternetGatewayRouteTable} -> currentInternetGatewayRouteTable) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {currentInternetGatewayRouteTable = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The route table ID.
networkFirewallInvalidRouteConfigurationViolation_routeTableId :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_routeTableId = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {routeTableId} -> routeTableId) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {routeTableId = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The internet gateway ID.
networkFirewallInvalidRouteConfigurationViolation_internetGatewayId :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_internetGatewayId = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {internetGatewayId} -> internetGatewayId) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {internetGatewayId = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The actual firewall endpoint.
networkFirewallInvalidRouteConfigurationViolation_actualFirewallEndpoint :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_actualFirewallEndpoint = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {actualFirewallEndpoint} -> actualFirewallEndpoint) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {actualFirewallEndpoint = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The expected subnet ID for the firewall.
networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetId :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetId = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {expectedFirewallSubnetId} -> expectedFirewallSubnetId) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {expectedFirewallSubnetId = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | Information about the VPC ID.
networkFirewallInvalidRouteConfigurationViolation_vpcId :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_vpcId = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {vpcId} -> vpcId) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {vpcId = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The subnets that are affected.
networkFirewallInvalidRouteConfigurationViolation_affectedSubnets :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe [Prelude.Text])
networkFirewallInvalidRouteConfigurationViolation_affectedSubnets = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {affectedSubnets} -> affectedSubnets) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {affectedSubnets = a} :: NetworkFirewallInvalidRouteConfigurationViolation) Prelude.. Lens.mapping Lens.coerced

-- | The actual subnet ID for the firewall.
networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetId :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetId = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {actualFirewallSubnetId} -> actualFirewallSubnetId) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {actualFirewallSubnetId = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | Information about whether the route table is used in another
-- Availability Zone.
networkFirewallInvalidRouteConfigurationViolation_isRouteTableUsedInDifferentAZ :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Bool)
networkFirewallInvalidRouteConfigurationViolation_isRouteTableUsedInDifferentAZ = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {isRouteTableUsedInDifferentAZ} -> isRouteTableUsedInDifferentAZ) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {isRouteTableUsedInDifferentAZ = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

instance
  Data.FromJSON
    NetworkFirewallInvalidRouteConfigurationViolation
  where
  parseJSON =
    Data.withObject
      "NetworkFirewallInvalidRouteConfigurationViolation"
      ( \x ->
          NetworkFirewallInvalidRouteConfigurationViolation'
            Prelude.<$> ( x Data..:? "ActualInternetGatewayRoutes"
                            Data..!= Prelude.mempty
                        )
              Prelude.<*> (x Data..:? "CurrentFirewallSubnetRouteTable")
              Prelude.<*> (x Data..:? "ViolatingRoute")
              Prelude.<*> (x Data..:? "ExpectedFirewallEndpoint")
              Prelude.<*> ( x Data..:? "ActualFirewallSubnetRoutes"
                              Data..!= Prelude.mempty
                          )
              Prelude.<*> ( x Data..:? "ExpectedInternetGatewayRoutes"
                              Data..!= Prelude.mempty
                          )
              Prelude.<*> ( x Data..:? "ExpectedFirewallSubnetRoutes"
                              Data..!= Prelude.mempty
                          )
              Prelude.<*> (x Data..:? "CurrentInternetGatewayRouteTable")
              Prelude.<*> (x Data..:? "RouteTableId")
              Prelude.<*> (x Data..:? "InternetGatewayId")
              Prelude.<*> (x Data..:? "ActualFirewallEndpoint")
              Prelude.<*> (x Data..:? "ExpectedFirewallSubnetId")
              Prelude.<*> (x Data..:? "VpcId")
              Prelude.<*> ( x Data..:? "AffectedSubnets"
                              Data..!= Prelude.mempty
                          )
              Prelude.<*> (x Data..:? "ActualFirewallSubnetId")
              Prelude.<*> (x Data..:? "IsRouteTableUsedInDifferentAZ")
      )

instance
  Prelude.Hashable
    NetworkFirewallInvalidRouteConfigurationViolation
  where
  hashWithSalt
    _salt
    NetworkFirewallInvalidRouteConfigurationViolation' {..} =
      _salt
        `Prelude.hashWithSalt` actualInternetGatewayRoutes
        `Prelude.hashWithSalt` currentFirewallSubnetRouteTable
        `Prelude.hashWithSalt` violatingRoute
        `Prelude.hashWithSalt` expectedFirewallEndpoint
        `Prelude.hashWithSalt` actualFirewallSubnetRoutes
        `Prelude.hashWithSalt` expectedInternetGatewayRoutes
        `Prelude.hashWithSalt` expectedFirewallSubnetRoutes
        `Prelude.hashWithSalt` currentInternetGatewayRouteTable
        `Prelude.hashWithSalt` routeTableId
        `Prelude.hashWithSalt` internetGatewayId
        `Prelude.hashWithSalt` actualFirewallEndpoint
        `Prelude.hashWithSalt` expectedFirewallSubnetId
        `Prelude.hashWithSalt` vpcId
        `Prelude.hashWithSalt` affectedSubnets
        `Prelude.hashWithSalt` actualFirewallSubnetId
        `Prelude.hashWithSalt` isRouteTableUsedInDifferentAZ

instance
  Prelude.NFData
    NetworkFirewallInvalidRouteConfigurationViolation
  where
  rnf
    NetworkFirewallInvalidRouteConfigurationViolation' {..} =
      Prelude.rnf actualInternetGatewayRoutes
        `Prelude.seq` Prelude.rnf currentFirewallSubnetRouteTable
        `Prelude.seq` Prelude.rnf violatingRoute
        `Prelude.seq` Prelude.rnf expectedFirewallEndpoint
        `Prelude.seq` Prelude.rnf actualFirewallSubnetRoutes
        `Prelude.seq` Prelude.rnf expectedInternetGatewayRoutes
        `Prelude.seq` Prelude.rnf expectedFirewallSubnetRoutes
        `Prelude.seq` Prelude.rnf currentInternetGatewayRouteTable
        `Prelude.seq` Prelude.rnf routeTableId
        `Prelude.seq` Prelude.rnf internetGatewayId
        `Prelude.seq` Prelude.rnf actualFirewallEndpoint
        `Prelude.seq` Prelude.rnf expectedFirewallSubnetId
        `Prelude.seq` Prelude.rnf vpcId
        `Prelude.seq` Prelude.rnf affectedSubnets
        `Prelude.seq` Prelude.rnf actualFirewallSubnetId
        `Prelude.seq` Prelude.rnf
          isRouteTableUsedInDifferentAZ
