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
-- Module      : Amazonka.FMS.Types.RouteHasOutOfScopeEndpointViolation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.RouteHasOutOfScopeEndpointViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.Route
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the route endpoint that violates the policy
-- scope.
--
-- /See:/ 'newRouteHasOutOfScopeEndpointViolation' smart constructor.
data RouteHasOutOfScopeEndpointViolation = RouteHasOutOfScopeEndpointViolation'
  { -- | The route table associated with the current firewall subnet.
    currentFirewallSubnetRouteTable :: Prelude.Maybe Prelude.Text,
    -- | The current route table associated with the Internet Gateway.
    currentInternetGatewayRouteTable :: Prelude.Maybe Prelude.Text,
    -- | The ID of the firewall subnet.
    firewallSubnetId :: Prelude.Maybe Prelude.Text,
    -- | The list of firewall subnet routes.
    firewallSubnetRoutes :: Prelude.Maybe [Route],
    -- | The ID of the Internet Gateway.
    internetGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The routes in the route table associated with the Internet Gateway.
    internetGatewayRoutes :: Prelude.Maybe [Route],
    -- | The ID of the route table.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | The subnet\'s Availability Zone.
    subnetAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet\'s Availability Zone.
    subnetAvailabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet associated with the route that violates the policy
    -- scope.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The list of routes that violate the route table.
    violatingRoutes :: Prelude.Maybe [Route],
    -- | The VPC ID of the route that violates the policy scope.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteHasOutOfScopeEndpointViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentFirewallSubnetRouteTable', 'routeHasOutOfScopeEndpointViolation_currentFirewallSubnetRouteTable' - The route table associated with the current firewall subnet.
--
-- 'currentInternetGatewayRouteTable', 'routeHasOutOfScopeEndpointViolation_currentInternetGatewayRouteTable' - The current route table associated with the Internet Gateway.
--
-- 'firewallSubnetId', 'routeHasOutOfScopeEndpointViolation_firewallSubnetId' - The ID of the firewall subnet.
--
-- 'firewallSubnetRoutes', 'routeHasOutOfScopeEndpointViolation_firewallSubnetRoutes' - The list of firewall subnet routes.
--
-- 'internetGatewayId', 'routeHasOutOfScopeEndpointViolation_internetGatewayId' - The ID of the Internet Gateway.
--
-- 'internetGatewayRoutes', 'routeHasOutOfScopeEndpointViolation_internetGatewayRoutes' - The routes in the route table associated with the Internet Gateway.
--
-- 'routeTableId', 'routeHasOutOfScopeEndpointViolation_routeTableId' - The ID of the route table.
--
-- 'subnetAvailabilityZone', 'routeHasOutOfScopeEndpointViolation_subnetAvailabilityZone' - The subnet\'s Availability Zone.
--
-- 'subnetAvailabilityZoneId', 'routeHasOutOfScopeEndpointViolation_subnetAvailabilityZoneId' - The ID of the subnet\'s Availability Zone.
--
-- 'subnetId', 'routeHasOutOfScopeEndpointViolation_subnetId' - The ID of the subnet associated with the route that violates the policy
-- scope.
--
-- 'violatingRoutes', 'routeHasOutOfScopeEndpointViolation_violatingRoutes' - The list of routes that violate the route table.
--
-- 'vpcId', 'routeHasOutOfScopeEndpointViolation_vpcId' - The VPC ID of the route that violates the policy scope.
newRouteHasOutOfScopeEndpointViolation ::
  RouteHasOutOfScopeEndpointViolation
newRouteHasOutOfScopeEndpointViolation =
  RouteHasOutOfScopeEndpointViolation'
    { currentFirewallSubnetRouteTable =
        Prelude.Nothing,
      currentInternetGatewayRouteTable =
        Prelude.Nothing,
      firewallSubnetId = Prelude.Nothing,
      firewallSubnetRoutes = Prelude.Nothing,
      internetGatewayId = Prelude.Nothing,
      internetGatewayRoutes =
        Prelude.Nothing,
      routeTableId = Prelude.Nothing,
      subnetAvailabilityZone =
        Prelude.Nothing,
      subnetAvailabilityZoneId =
        Prelude.Nothing,
      subnetId = Prelude.Nothing,
      violatingRoutes = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The route table associated with the current firewall subnet.
routeHasOutOfScopeEndpointViolation_currentFirewallSubnetRouteTable :: Lens.Lens' RouteHasOutOfScopeEndpointViolation (Prelude.Maybe Prelude.Text)
routeHasOutOfScopeEndpointViolation_currentFirewallSubnetRouteTable = Lens.lens (\RouteHasOutOfScopeEndpointViolation' {currentFirewallSubnetRouteTable} -> currentFirewallSubnetRouteTable) (\s@RouteHasOutOfScopeEndpointViolation' {} a -> s {currentFirewallSubnetRouteTable = a} :: RouteHasOutOfScopeEndpointViolation)

-- | The current route table associated with the Internet Gateway.
routeHasOutOfScopeEndpointViolation_currentInternetGatewayRouteTable :: Lens.Lens' RouteHasOutOfScopeEndpointViolation (Prelude.Maybe Prelude.Text)
routeHasOutOfScopeEndpointViolation_currentInternetGatewayRouteTable = Lens.lens (\RouteHasOutOfScopeEndpointViolation' {currentInternetGatewayRouteTable} -> currentInternetGatewayRouteTable) (\s@RouteHasOutOfScopeEndpointViolation' {} a -> s {currentInternetGatewayRouteTable = a} :: RouteHasOutOfScopeEndpointViolation)

-- | The ID of the firewall subnet.
routeHasOutOfScopeEndpointViolation_firewallSubnetId :: Lens.Lens' RouteHasOutOfScopeEndpointViolation (Prelude.Maybe Prelude.Text)
routeHasOutOfScopeEndpointViolation_firewallSubnetId = Lens.lens (\RouteHasOutOfScopeEndpointViolation' {firewallSubnetId} -> firewallSubnetId) (\s@RouteHasOutOfScopeEndpointViolation' {} a -> s {firewallSubnetId = a} :: RouteHasOutOfScopeEndpointViolation)

-- | The list of firewall subnet routes.
routeHasOutOfScopeEndpointViolation_firewallSubnetRoutes :: Lens.Lens' RouteHasOutOfScopeEndpointViolation (Prelude.Maybe [Route])
routeHasOutOfScopeEndpointViolation_firewallSubnetRoutes = Lens.lens (\RouteHasOutOfScopeEndpointViolation' {firewallSubnetRoutes} -> firewallSubnetRoutes) (\s@RouteHasOutOfScopeEndpointViolation' {} a -> s {firewallSubnetRoutes = a} :: RouteHasOutOfScopeEndpointViolation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Internet Gateway.
routeHasOutOfScopeEndpointViolation_internetGatewayId :: Lens.Lens' RouteHasOutOfScopeEndpointViolation (Prelude.Maybe Prelude.Text)
routeHasOutOfScopeEndpointViolation_internetGatewayId = Lens.lens (\RouteHasOutOfScopeEndpointViolation' {internetGatewayId} -> internetGatewayId) (\s@RouteHasOutOfScopeEndpointViolation' {} a -> s {internetGatewayId = a} :: RouteHasOutOfScopeEndpointViolation)

-- | The routes in the route table associated with the Internet Gateway.
routeHasOutOfScopeEndpointViolation_internetGatewayRoutes :: Lens.Lens' RouteHasOutOfScopeEndpointViolation (Prelude.Maybe [Route])
routeHasOutOfScopeEndpointViolation_internetGatewayRoutes = Lens.lens (\RouteHasOutOfScopeEndpointViolation' {internetGatewayRoutes} -> internetGatewayRoutes) (\s@RouteHasOutOfScopeEndpointViolation' {} a -> s {internetGatewayRoutes = a} :: RouteHasOutOfScopeEndpointViolation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the route table.
routeHasOutOfScopeEndpointViolation_routeTableId :: Lens.Lens' RouteHasOutOfScopeEndpointViolation (Prelude.Maybe Prelude.Text)
routeHasOutOfScopeEndpointViolation_routeTableId = Lens.lens (\RouteHasOutOfScopeEndpointViolation' {routeTableId} -> routeTableId) (\s@RouteHasOutOfScopeEndpointViolation' {} a -> s {routeTableId = a} :: RouteHasOutOfScopeEndpointViolation)

-- | The subnet\'s Availability Zone.
routeHasOutOfScopeEndpointViolation_subnetAvailabilityZone :: Lens.Lens' RouteHasOutOfScopeEndpointViolation (Prelude.Maybe Prelude.Text)
routeHasOutOfScopeEndpointViolation_subnetAvailabilityZone = Lens.lens (\RouteHasOutOfScopeEndpointViolation' {subnetAvailabilityZone} -> subnetAvailabilityZone) (\s@RouteHasOutOfScopeEndpointViolation' {} a -> s {subnetAvailabilityZone = a} :: RouteHasOutOfScopeEndpointViolation)

-- | The ID of the subnet\'s Availability Zone.
routeHasOutOfScopeEndpointViolation_subnetAvailabilityZoneId :: Lens.Lens' RouteHasOutOfScopeEndpointViolation (Prelude.Maybe Prelude.Text)
routeHasOutOfScopeEndpointViolation_subnetAvailabilityZoneId = Lens.lens (\RouteHasOutOfScopeEndpointViolation' {subnetAvailabilityZoneId} -> subnetAvailabilityZoneId) (\s@RouteHasOutOfScopeEndpointViolation' {} a -> s {subnetAvailabilityZoneId = a} :: RouteHasOutOfScopeEndpointViolation)

-- | The ID of the subnet associated with the route that violates the policy
-- scope.
routeHasOutOfScopeEndpointViolation_subnetId :: Lens.Lens' RouteHasOutOfScopeEndpointViolation (Prelude.Maybe Prelude.Text)
routeHasOutOfScopeEndpointViolation_subnetId = Lens.lens (\RouteHasOutOfScopeEndpointViolation' {subnetId} -> subnetId) (\s@RouteHasOutOfScopeEndpointViolation' {} a -> s {subnetId = a} :: RouteHasOutOfScopeEndpointViolation)

-- | The list of routes that violate the route table.
routeHasOutOfScopeEndpointViolation_violatingRoutes :: Lens.Lens' RouteHasOutOfScopeEndpointViolation (Prelude.Maybe [Route])
routeHasOutOfScopeEndpointViolation_violatingRoutes = Lens.lens (\RouteHasOutOfScopeEndpointViolation' {violatingRoutes} -> violatingRoutes) (\s@RouteHasOutOfScopeEndpointViolation' {} a -> s {violatingRoutes = a} :: RouteHasOutOfScopeEndpointViolation) Prelude.. Lens.mapping Lens.coerced

-- | The VPC ID of the route that violates the policy scope.
routeHasOutOfScopeEndpointViolation_vpcId :: Lens.Lens' RouteHasOutOfScopeEndpointViolation (Prelude.Maybe Prelude.Text)
routeHasOutOfScopeEndpointViolation_vpcId = Lens.lens (\RouteHasOutOfScopeEndpointViolation' {vpcId} -> vpcId) (\s@RouteHasOutOfScopeEndpointViolation' {} a -> s {vpcId = a} :: RouteHasOutOfScopeEndpointViolation)

instance
  Data.FromJSON
    RouteHasOutOfScopeEndpointViolation
  where
  parseJSON =
    Data.withObject
      "RouteHasOutOfScopeEndpointViolation"
      ( \x ->
          RouteHasOutOfScopeEndpointViolation'
            Prelude.<$> (x Data..:? "CurrentFirewallSubnetRouteTable")
            Prelude.<*> (x Data..:? "CurrentInternetGatewayRouteTable")
            Prelude.<*> (x Data..:? "FirewallSubnetId")
            Prelude.<*> ( x Data..:? "FirewallSubnetRoutes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "InternetGatewayId")
            Prelude.<*> ( x Data..:? "InternetGatewayRoutes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "RouteTableId")
            Prelude.<*> (x Data..:? "SubnetAvailabilityZone")
            Prelude.<*> (x Data..:? "SubnetAvailabilityZoneId")
            Prelude.<*> (x Data..:? "SubnetId")
            Prelude.<*> ( x Data..:? "ViolatingRoutes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "VpcId")
      )

instance
  Prelude.Hashable
    RouteHasOutOfScopeEndpointViolation
  where
  hashWithSalt
    _salt
    RouteHasOutOfScopeEndpointViolation' {..} =
      _salt
        `Prelude.hashWithSalt` currentFirewallSubnetRouteTable
        `Prelude.hashWithSalt` currentInternetGatewayRouteTable
        `Prelude.hashWithSalt` firewallSubnetId
        `Prelude.hashWithSalt` firewallSubnetRoutes
        `Prelude.hashWithSalt` internetGatewayId
        `Prelude.hashWithSalt` internetGatewayRoutes
        `Prelude.hashWithSalt` routeTableId
        `Prelude.hashWithSalt` subnetAvailabilityZone
        `Prelude.hashWithSalt` subnetAvailabilityZoneId
        `Prelude.hashWithSalt` subnetId
        `Prelude.hashWithSalt` violatingRoutes
        `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    RouteHasOutOfScopeEndpointViolation
  where
  rnf RouteHasOutOfScopeEndpointViolation' {..} =
    Prelude.rnf currentFirewallSubnetRouteTable
      `Prelude.seq` Prelude.rnf currentInternetGatewayRouteTable
      `Prelude.seq` Prelude.rnf firewallSubnetId
      `Prelude.seq` Prelude.rnf firewallSubnetRoutes
      `Prelude.seq` Prelude.rnf internetGatewayId
      `Prelude.seq` Prelude.rnf internetGatewayRoutes
      `Prelude.seq` Prelude.rnf routeTableId
      `Prelude.seq` Prelude.rnf subnetAvailabilityZone
      `Prelude.seq` Prelude.rnf subnetAvailabilityZoneId
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf violatingRoutes
      `Prelude.seq` Prelude.rnf vpcId
