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
-- Module      : Amazonka.FMS.Types.NetworkFirewallBlackHoleRouteDetectedViolation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.NetworkFirewallBlackHoleRouteDetectedViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.Route
import qualified Amazonka.Prelude as Prelude

-- | Violation detail for an internet gateway route with an inactive state in
-- the customer subnet route table or Network Firewall subnet route table.
--
-- /See:/ 'newNetworkFirewallBlackHoleRouteDetectedViolation' smart constructor.
data NetworkFirewallBlackHoleRouteDetectedViolation = NetworkFirewallBlackHoleRouteDetectedViolation'
  { -- | Information about the route table ID.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | Information about the route or routes that are in violation.
    violatingRoutes :: Prelude.Maybe [Route],
    -- | The subnet that has an inactive state.
    violationTarget :: Prelude.Maybe Prelude.Text,
    -- | Information about the VPC ID.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFirewallBlackHoleRouteDetectedViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routeTableId', 'networkFirewallBlackHoleRouteDetectedViolation_routeTableId' - Information about the route table ID.
--
-- 'violatingRoutes', 'networkFirewallBlackHoleRouteDetectedViolation_violatingRoutes' - Information about the route or routes that are in violation.
--
-- 'violationTarget', 'networkFirewallBlackHoleRouteDetectedViolation_violationTarget' - The subnet that has an inactive state.
--
-- 'vpcId', 'networkFirewallBlackHoleRouteDetectedViolation_vpcId' - Information about the VPC ID.
newNetworkFirewallBlackHoleRouteDetectedViolation ::
  NetworkFirewallBlackHoleRouteDetectedViolation
newNetworkFirewallBlackHoleRouteDetectedViolation =
  NetworkFirewallBlackHoleRouteDetectedViolation'
    { routeTableId =
        Prelude.Nothing,
      violatingRoutes =
        Prelude.Nothing,
      violationTarget =
        Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | Information about the route table ID.
networkFirewallBlackHoleRouteDetectedViolation_routeTableId :: Lens.Lens' NetworkFirewallBlackHoleRouteDetectedViolation (Prelude.Maybe Prelude.Text)
networkFirewallBlackHoleRouteDetectedViolation_routeTableId = Lens.lens (\NetworkFirewallBlackHoleRouteDetectedViolation' {routeTableId} -> routeTableId) (\s@NetworkFirewallBlackHoleRouteDetectedViolation' {} a -> s {routeTableId = a} :: NetworkFirewallBlackHoleRouteDetectedViolation)

-- | Information about the route or routes that are in violation.
networkFirewallBlackHoleRouteDetectedViolation_violatingRoutes :: Lens.Lens' NetworkFirewallBlackHoleRouteDetectedViolation (Prelude.Maybe [Route])
networkFirewallBlackHoleRouteDetectedViolation_violatingRoutes = Lens.lens (\NetworkFirewallBlackHoleRouteDetectedViolation' {violatingRoutes} -> violatingRoutes) (\s@NetworkFirewallBlackHoleRouteDetectedViolation' {} a -> s {violatingRoutes = a} :: NetworkFirewallBlackHoleRouteDetectedViolation) Prelude.. Lens.mapping Lens.coerced

-- | The subnet that has an inactive state.
networkFirewallBlackHoleRouteDetectedViolation_violationTarget :: Lens.Lens' NetworkFirewallBlackHoleRouteDetectedViolation (Prelude.Maybe Prelude.Text)
networkFirewallBlackHoleRouteDetectedViolation_violationTarget = Lens.lens (\NetworkFirewallBlackHoleRouteDetectedViolation' {violationTarget} -> violationTarget) (\s@NetworkFirewallBlackHoleRouteDetectedViolation' {} a -> s {violationTarget = a} :: NetworkFirewallBlackHoleRouteDetectedViolation)

-- | Information about the VPC ID.
networkFirewallBlackHoleRouteDetectedViolation_vpcId :: Lens.Lens' NetworkFirewallBlackHoleRouteDetectedViolation (Prelude.Maybe Prelude.Text)
networkFirewallBlackHoleRouteDetectedViolation_vpcId = Lens.lens (\NetworkFirewallBlackHoleRouteDetectedViolation' {vpcId} -> vpcId) (\s@NetworkFirewallBlackHoleRouteDetectedViolation' {} a -> s {vpcId = a} :: NetworkFirewallBlackHoleRouteDetectedViolation)

instance
  Data.FromJSON
    NetworkFirewallBlackHoleRouteDetectedViolation
  where
  parseJSON =
    Data.withObject
      "NetworkFirewallBlackHoleRouteDetectedViolation"
      ( \x ->
          NetworkFirewallBlackHoleRouteDetectedViolation'
            Prelude.<$> (x Data..:? "RouteTableId")
            Prelude.<*> ( x
                            Data..:? "ViolatingRoutes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ViolationTarget")
            Prelude.<*> (x Data..:? "VpcId")
      )

instance
  Prelude.Hashable
    NetworkFirewallBlackHoleRouteDetectedViolation
  where
  hashWithSalt
    _salt
    NetworkFirewallBlackHoleRouteDetectedViolation' {..} =
      _salt
        `Prelude.hashWithSalt` routeTableId
        `Prelude.hashWithSalt` violatingRoutes
        `Prelude.hashWithSalt` violationTarget
        `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    NetworkFirewallBlackHoleRouteDetectedViolation
  where
  rnf
    NetworkFirewallBlackHoleRouteDetectedViolation' {..} =
      Prelude.rnf routeTableId
        `Prelude.seq` Prelude.rnf violatingRoutes
        `Prelude.seq` Prelude.rnf violationTarget
        `Prelude.seq` Prelude.rnf vpcId
