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
-- Module      : Network.AWS.FMS.Types.NetworkFirewallInvalidRouteConfigurationViolation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.NetworkFirewallInvalidRouteConfigurationViolation where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.ExpectedRoute
import Network.AWS.FMS.Types.Route
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Violation detail for the improperly configured subnet route. It\'s
-- possible there is a missing route table route, or a configuration that
-- causes traffic to cross an Availability Zone boundary.
--
-- /See:/ 'newNetworkFirewallInvalidRouteConfigurationViolation' smart constructor.
data NetworkFirewallInvalidRouteConfigurationViolation = NetworkFirewallInvalidRouteConfigurationViolation'
  { -- | The expected routes for the internet gateway.
    expectedInternetGatewayRoutes :: Prelude.Maybe [ExpectedRoute],
    -- | The firewall subnet routes that are expected.
    expectedFirewallSubnetRoutes :: Prelude.Maybe [ExpectedRoute],
    -- | The actual subnet ID for the firewall.
    actualFirewallSubnetId :: Prelude.Maybe Prelude.Text,
    -- | Information about whether the route table is used in another
    -- Availability Zone.
    isRouteTableUsedInDifferentAZ :: Prelude.Maybe Prelude.Bool,
    -- | The actual firewall endpoint.
    actualFirewallEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The subnet route table for the current firewall.
    currentFirewallSubnetRouteTable :: Prelude.Maybe Prelude.Text,
    -- | The route table for the current internet gateway.
    currentInternetGatewayRouteTable :: Prelude.Maybe Prelude.Text,
    -- | The route table ID.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | The actual internet gateway routes.
    actualInternetGatewayRoutes :: Prelude.Maybe [Route],
    -- | The actual firewall subnet routes that are expected.
    actualFirewallSubnetRoutes :: Prelude.Maybe [Route],
    -- | The internet gateway ID.
    internetGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The expected subnet ID for the firewall.
    expectedFirewallSubnetId :: Prelude.Maybe Prelude.Text,
    -- | The firewall endpoint that\'s expected.
    expectedFirewallEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The route that\'s in violation.
    violatingRoute :: Prelude.Maybe Route,
    -- | Information about the VPC ID.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The subnets that are affected.
    affectedSubnets :: Prelude.Maybe [Prelude.Text]
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
-- 'expectedInternetGatewayRoutes', 'networkFirewallInvalidRouteConfigurationViolation_expectedInternetGatewayRoutes' - The expected routes for the internet gateway.
--
-- 'expectedFirewallSubnetRoutes', 'networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetRoutes' - The firewall subnet routes that are expected.
--
-- 'actualFirewallSubnetId', 'networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetId' - The actual subnet ID for the firewall.
--
-- 'isRouteTableUsedInDifferentAZ', 'networkFirewallInvalidRouteConfigurationViolation_isRouteTableUsedInDifferentAZ' - Information about whether the route table is used in another
-- Availability Zone.
--
-- 'actualFirewallEndpoint', 'networkFirewallInvalidRouteConfigurationViolation_actualFirewallEndpoint' - The actual firewall endpoint.
--
-- 'currentFirewallSubnetRouteTable', 'networkFirewallInvalidRouteConfigurationViolation_currentFirewallSubnetRouteTable' - The subnet route table for the current firewall.
--
-- 'currentInternetGatewayRouteTable', 'networkFirewallInvalidRouteConfigurationViolation_currentInternetGatewayRouteTable' - The route table for the current internet gateway.
--
-- 'routeTableId', 'networkFirewallInvalidRouteConfigurationViolation_routeTableId' - The route table ID.
--
-- 'actualInternetGatewayRoutes', 'networkFirewallInvalidRouteConfigurationViolation_actualInternetGatewayRoutes' - The actual internet gateway routes.
--
-- 'actualFirewallSubnetRoutes', 'networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetRoutes' - The actual firewall subnet routes that are expected.
--
-- 'internetGatewayId', 'networkFirewallInvalidRouteConfigurationViolation_internetGatewayId' - The internet gateway ID.
--
-- 'expectedFirewallSubnetId', 'networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetId' - The expected subnet ID for the firewall.
--
-- 'expectedFirewallEndpoint', 'networkFirewallInvalidRouteConfigurationViolation_expectedFirewallEndpoint' - The firewall endpoint that\'s expected.
--
-- 'violatingRoute', 'networkFirewallInvalidRouteConfigurationViolation_violatingRoute' - The route that\'s in violation.
--
-- 'vpcId', 'networkFirewallInvalidRouteConfigurationViolation_vpcId' - Information about the VPC ID.
--
-- 'affectedSubnets', 'networkFirewallInvalidRouteConfigurationViolation_affectedSubnets' - The subnets that are affected.
newNetworkFirewallInvalidRouteConfigurationViolation ::
  NetworkFirewallInvalidRouteConfigurationViolation
newNetworkFirewallInvalidRouteConfigurationViolation =
  NetworkFirewallInvalidRouteConfigurationViolation'
    { expectedInternetGatewayRoutes =
        Prelude.Nothing,
      expectedFirewallSubnetRoutes =
        Prelude.Nothing,
      actualFirewallSubnetId =
        Prelude.Nothing,
      isRouteTableUsedInDifferentAZ =
        Prelude.Nothing,
      actualFirewallEndpoint =
        Prelude.Nothing,
      currentFirewallSubnetRouteTable =
        Prelude.Nothing,
      currentInternetGatewayRouteTable =
        Prelude.Nothing,
      routeTableId =
        Prelude.Nothing,
      actualInternetGatewayRoutes =
        Prelude.Nothing,
      actualFirewallSubnetRoutes =
        Prelude.Nothing,
      internetGatewayId =
        Prelude.Nothing,
      expectedFirewallSubnetId =
        Prelude.Nothing,
      expectedFirewallEndpoint =
        Prelude.Nothing,
      violatingRoute =
        Prelude.Nothing,
      vpcId = Prelude.Nothing,
      affectedSubnets =
        Prelude.Nothing
    }

-- | The expected routes for the internet gateway.
networkFirewallInvalidRouteConfigurationViolation_expectedInternetGatewayRoutes :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe [ExpectedRoute])
networkFirewallInvalidRouteConfigurationViolation_expectedInternetGatewayRoutes = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {expectedInternetGatewayRoutes} -> expectedInternetGatewayRoutes) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {expectedInternetGatewayRoutes = a} :: NetworkFirewallInvalidRouteConfigurationViolation) Prelude.. Lens.mapping Lens._Coerce

-- | The firewall subnet routes that are expected.
networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetRoutes :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe [ExpectedRoute])
networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetRoutes = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {expectedFirewallSubnetRoutes} -> expectedFirewallSubnetRoutes) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {expectedFirewallSubnetRoutes = a} :: NetworkFirewallInvalidRouteConfigurationViolation) Prelude.. Lens.mapping Lens._Coerce

-- | The actual subnet ID for the firewall.
networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetId :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetId = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {actualFirewallSubnetId} -> actualFirewallSubnetId) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {actualFirewallSubnetId = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | Information about whether the route table is used in another
-- Availability Zone.
networkFirewallInvalidRouteConfigurationViolation_isRouteTableUsedInDifferentAZ :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Bool)
networkFirewallInvalidRouteConfigurationViolation_isRouteTableUsedInDifferentAZ = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {isRouteTableUsedInDifferentAZ} -> isRouteTableUsedInDifferentAZ) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {isRouteTableUsedInDifferentAZ = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The actual firewall endpoint.
networkFirewallInvalidRouteConfigurationViolation_actualFirewallEndpoint :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_actualFirewallEndpoint = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {actualFirewallEndpoint} -> actualFirewallEndpoint) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {actualFirewallEndpoint = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The subnet route table for the current firewall.
networkFirewallInvalidRouteConfigurationViolation_currentFirewallSubnetRouteTable :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_currentFirewallSubnetRouteTable = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {currentFirewallSubnetRouteTable} -> currentFirewallSubnetRouteTable) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {currentFirewallSubnetRouteTable = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The route table for the current internet gateway.
networkFirewallInvalidRouteConfigurationViolation_currentInternetGatewayRouteTable :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_currentInternetGatewayRouteTable = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {currentInternetGatewayRouteTable} -> currentInternetGatewayRouteTable) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {currentInternetGatewayRouteTable = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The route table ID.
networkFirewallInvalidRouteConfigurationViolation_routeTableId :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_routeTableId = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {routeTableId} -> routeTableId) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {routeTableId = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The actual internet gateway routes.
networkFirewallInvalidRouteConfigurationViolation_actualInternetGatewayRoutes :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe [Route])
networkFirewallInvalidRouteConfigurationViolation_actualInternetGatewayRoutes = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {actualInternetGatewayRoutes} -> actualInternetGatewayRoutes) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {actualInternetGatewayRoutes = a} :: NetworkFirewallInvalidRouteConfigurationViolation) Prelude.. Lens.mapping Lens._Coerce

-- | The actual firewall subnet routes that are expected.
networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetRoutes :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe [Route])
networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetRoutes = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {actualFirewallSubnetRoutes} -> actualFirewallSubnetRoutes) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {actualFirewallSubnetRoutes = a} :: NetworkFirewallInvalidRouteConfigurationViolation) Prelude.. Lens.mapping Lens._Coerce

-- | The internet gateway ID.
networkFirewallInvalidRouteConfigurationViolation_internetGatewayId :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_internetGatewayId = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {internetGatewayId} -> internetGatewayId) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {internetGatewayId = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The expected subnet ID for the firewall.
networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetId :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetId = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {expectedFirewallSubnetId} -> expectedFirewallSubnetId) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {expectedFirewallSubnetId = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The firewall endpoint that\'s expected.
networkFirewallInvalidRouteConfigurationViolation_expectedFirewallEndpoint :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_expectedFirewallEndpoint = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {expectedFirewallEndpoint} -> expectedFirewallEndpoint) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {expectedFirewallEndpoint = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The route that\'s in violation.
networkFirewallInvalidRouteConfigurationViolation_violatingRoute :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Route)
networkFirewallInvalidRouteConfigurationViolation_violatingRoute = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {violatingRoute} -> violatingRoute) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {violatingRoute = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | Information about the VPC ID.
networkFirewallInvalidRouteConfigurationViolation_vpcId :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe Prelude.Text)
networkFirewallInvalidRouteConfigurationViolation_vpcId = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {vpcId} -> vpcId) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {vpcId = a} :: NetworkFirewallInvalidRouteConfigurationViolation)

-- | The subnets that are affected.
networkFirewallInvalidRouteConfigurationViolation_affectedSubnets :: Lens.Lens' NetworkFirewallInvalidRouteConfigurationViolation (Prelude.Maybe [Prelude.Text])
networkFirewallInvalidRouteConfigurationViolation_affectedSubnets = Lens.lens (\NetworkFirewallInvalidRouteConfigurationViolation' {affectedSubnets} -> affectedSubnets) (\s@NetworkFirewallInvalidRouteConfigurationViolation' {} a -> s {affectedSubnets = a} :: NetworkFirewallInvalidRouteConfigurationViolation) Prelude.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    NetworkFirewallInvalidRouteConfigurationViolation
  where
  parseJSON =
    Core.withObject
      "NetworkFirewallInvalidRouteConfigurationViolation"
      ( \x ->
          NetworkFirewallInvalidRouteConfigurationViolation'
            Prelude.<$> ( x Core..:? "ExpectedInternetGatewayRoutes"
                            Core..!= Prelude.mempty
                        )
              Prelude.<*> ( x Core..:? "ExpectedFirewallSubnetRoutes"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> (x Core..:? "ActualFirewallSubnetId")
              Prelude.<*> (x Core..:? "IsRouteTableUsedInDifferentAZ")
              Prelude.<*> (x Core..:? "ActualFirewallEndpoint")
              Prelude.<*> (x Core..:? "CurrentFirewallSubnetRouteTable")
              Prelude.<*> (x Core..:? "CurrentInternetGatewayRouteTable")
              Prelude.<*> (x Core..:? "RouteTableId")
              Prelude.<*> ( x Core..:? "ActualInternetGatewayRoutes"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> ( x Core..:? "ActualFirewallSubnetRoutes"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> (x Core..:? "InternetGatewayId")
              Prelude.<*> (x Core..:? "ExpectedFirewallSubnetId")
              Prelude.<*> (x Core..:? "ExpectedFirewallEndpoint")
              Prelude.<*> (x Core..:? "ViolatingRoute")
              Prelude.<*> (x Core..:? "VpcId")
              Prelude.<*> ( x Core..:? "AffectedSubnets"
                              Core..!= Prelude.mempty
                          )
      )

instance
  Prelude.Hashable
    NetworkFirewallInvalidRouteConfigurationViolation

instance
  Prelude.NFData
    NetworkFirewallInvalidRouteConfigurationViolation
