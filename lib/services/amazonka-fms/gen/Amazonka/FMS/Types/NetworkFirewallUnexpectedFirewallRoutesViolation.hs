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
-- Module      : Amazonka.FMS.Types.NetworkFirewallUnexpectedFirewallRoutesViolation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.NetworkFirewallUnexpectedFirewallRoutesViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.Route
import qualified Amazonka.Prelude as Prelude

-- | Violation detail for an unexpected route that\'s present in a route
-- table.
--
-- /See:/ 'newNetworkFirewallUnexpectedFirewallRoutesViolation' smart constructor.
data NetworkFirewallUnexpectedFirewallRoutesViolation = NetworkFirewallUnexpectedFirewallRoutesViolation'
  { -- | The routes that are in violation.
    violatingRoutes :: Prelude.Maybe [Route],
    -- | The endpoint of the firewall.
    firewallEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The ID of the route table.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | The subnet ID for the firewall.
    firewallSubnetId :: Prelude.Maybe Prelude.Text,
    -- | Information about the VPC ID.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFirewallUnexpectedFirewallRoutesViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'violatingRoutes', 'networkFirewallUnexpectedFirewallRoutesViolation_violatingRoutes' - The routes that are in violation.
--
-- 'firewallEndpoint', 'networkFirewallUnexpectedFirewallRoutesViolation_firewallEndpoint' - The endpoint of the firewall.
--
-- 'routeTableId', 'networkFirewallUnexpectedFirewallRoutesViolation_routeTableId' - The ID of the route table.
--
-- 'firewallSubnetId', 'networkFirewallUnexpectedFirewallRoutesViolation_firewallSubnetId' - The subnet ID for the firewall.
--
-- 'vpcId', 'networkFirewallUnexpectedFirewallRoutesViolation_vpcId' - Information about the VPC ID.
newNetworkFirewallUnexpectedFirewallRoutesViolation ::
  NetworkFirewallUnexpectedFirewallRoutesViolation
newNetworkFirewallUnexpectedFirewallRoutesViolation =
  NetworkFirewallUnexpectedFirewallRoutesViolation'
    { violatingRoutes =
        Prelude.Nothing,
      firewallEndpoint =
        Prelude.Nothing,
      routeTableId =
        Prelude.Nothing,
      firewallSubnetId =
        Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The routes that are in violation.
networkFirewallUnexpectedFirewallRoutesViolation_violatingRoutes :: Lens.Lens' NetworkFirewallUnexpectedFirewallRoutesViolation (Prelude.Maybe [Route])
networkFirewallUnexpectedFirewallRoutesViolation_violatingRoutes = Lens.lens (\NetworkFirewallUnexpectedFirewallRoutesViolation' {violatingRoutes} -> violatingRoutes) (\s@NetworkFirewallUnexpectedFirewallRoutesViolation' {} a -> s {violatingRoutes = a} :: NetworkFirewallUnexpectedFirewallRoutesViolation) Prelude.. Lens.mapping Lens.coerced

-- | The endpoint of the firewall.
networkFirewallUnexpectedFirewallRoutesViolation_firewallEndpoint :: Lens.Lens' NetworkFirewallUnexpectedFirewallRoutesViolation (Prelude.Maybe Prelude.Text)
networkFirewallUnexpectedFirewallRoutesViolation_firewallEndpoint = Lens.lens (\NetworkFirewallUnexpectedFirewallRoutesViolation' {firewallEndpoint} -> firewallEndpoint) (\s@NetworkFirewallUnexpectedFirewallRoutesViolation' {} a -> s {firewallEndpoint = a} :: NetworkFirewallUnexpectedFirewallRoutesViolation)

-- | The ID of the route table.
networkFirewallUnexpectedFirewallRoutesViolation_routeTableId :: Lens.Lens' NetworkFirewallUnexpectedFirewallRoutesViolation (Prelude.Maybe Prelude.Text)
networkFirewallUnexpectedFirewallRoutesViolation_routeTableId = Lens.lens (\NetworkFirewallUnexpectedFirewallRoutesViolation' {routeTableId} -> routeTableId) (\s@NetworkFirewallUnexpectedFirewallRoutesViolation' {} a -> s {routeTableId = a} :: NetworkFirewallUnexpectedFirewallRoutesViolation)

-- | The subnet ID for the firewall.
networkFirewallUnexpectedFirewallRoutesViolation_firewallSubnetId :: Lens.Lens' NetworkFirewallUnexpectedFirewallRoutesViolation (Prelude.Maybe Prelude.Text)
networkFirewallUnexpectedFirewallRoutesViolation_firewallSubnetId = Lens.lens (\NetworkFirewallUnexpectedFirewallRoutesViolation' {firewallSubnetId} -> firewallSubnetId) (\s@NetworkFirewallUnexpectedFirewallRoutesViolation' {} a -> s {firewallSubnetId = a} :: NetworkFirewallUnexpectedFirewallRoutesViolation)

-- | Information about the VPC ID.
networkFirewallUnexpectedFirewallRoutesViolation_vpcId :: Lens.Lens' NetworkFirewallUnexpectedFirewallRoutesViolation (Prelude.Maybe Prelude.Text)
networkFirewallUnexpectedFirewallRoutesViolation_vpcId = Lens.lens (\NetworkFirewallUnexpectedFirewallRoutesViolation' {vpcId} -> vpcId) (\s@NetworkFirewallUnexpectedFirewallRoutesViolation' {} a -> s {vpcId = a} :: NetworkFirewallUnexpectedFirewallRoutesViolation)

instance
  Data.FromJSON
    NetworkFirewallUnexpectedFirewallRoutesViolation
  where
  parseJSON =
    Data.withObject
      "NetworkFirewallUnexpectedFirewallRoutesViolation"
      ( \x ->
          NetworkFirewallUnexpectedFirewallRoutesViolation'
            Prelude.<$> ( x Data..:? "ViolatingRoutes"
                            Data..!= Prelude.mempty
                        )
              Prelude.<*> (x Data..:? "FirewallEndpoint")
              Prelude.<*> (x Data..:? "RouteTableId")
              Prelude.<*> (x Data..:? "FirewallSubnetId")
              Prelude.<*> (x Data..:? "VpcId")
      )

instance
  Prelude.Hashable
    NetworkFirewallUnexpectedFirewallRoutesViolation
  where
  hashWithSalt
    _salt
    NetworkFirewallUnexpectedFirewallRoutesViolation' {..} =
      _salt `Prelude.hashWithSalt` violatingRoutes
        `Prelude.hashWithSalt` firewallEndpoint
        `Prelude.hashWithSalt` routeTableId
        `Prelude.hashWithSalt` firewallSubnetId
        `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    NetworkFirewallUnexpectedFirewallRoutesViolation
  where
  rnf
    NetworkFirewallUnexpectedFirewallRoutesViolation' {..} =
      Prelude.rnf violatingRoutes
        `Prelude.seq` Prelude.rnf firewallEndpoint
        `Prelude.seq` Prelude.rnf routeTableId
        `Prelude.seq` Prelude.rnf firewallSubnetId
        `Prelude.seq` Prelude.rnf vpcId
