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
-- Module      : Network.AWS.FMS.Types.NetworkFirewallUnexpectedFirewallRoutesViolation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.NetworkFirewallUnexpectedFirewallRoutesViolation where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.Route
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Violation detail for an unexpected route that\'s present in a route
-- table.
--
-- /See:/ 'newNetworkFirewallUnexpectedFirewallRoutesViolation' smart constructor.
data NetworkFirewallUnexpectedFirewallRoutesViolation = NetworkFirewallUnexpectedFirewallRoutesViolation'
  { -- | The endpoint of the firewall.
    firewallEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The ID of the route table.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | The routes that are in violation.
    violatingRoutes :: Prelude.Maybe [Route],
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
-- 'firewallEndpoint', 'networkFirewallUnexpectedFirewallRoutesViolation_firewallEndpoint' - The endpoint of the firewall.
--
-- 'routeTableId', 'networkFirewallUnexpectedFirewallRoutesViolation_routeTableId' - The ID of the route table.
--
-- 'violatingRoutes', 'networkFirewallUnexpectedFirewallRoutesViolation_violatingRoutes' - The routes that are in violation.
--
-- 'firewallSubnetId', 'networkFirewallUnexpectedFirewallRoutesViolation_firewallSubnetId' - The subnet ID for the firewall.
--
-- 'vpcId', 'networkFirewallUnexpectedFirewallRoutesViolation_vpcId' - Information about the VPC ID.
newNetworkFirewallUnexpectedFirewallRoutesViolation ::
  NetworkFirewallUnexpectedFirewallRoutesViolation
newNetworkFirewallUnexpectedFirewallRoutesViolation =
  NetworkFirewallUnexpectedFirewallRoutesViolation'
    { firewallEndpoint =
        Prelude.Nothing,
      routeTableId =
        Prelude.Nothing,
      violatingRoutes =
        Prelude.Nothing,
      firewallSubnetId =
        Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The endpoint of the firewall.
networkFirewallUnexpectedFirewallRoutesViolation_firewallEndpoint :: Lens.Lens' NetworkFirewallUnexpectedFirewallRoutesViolation (Prelude.Maybe Prelude.Text)
networkFirewallUnexpectedFirewallRoutesViolation_firewallEndpoint = Lens.lens (\NetworkFirewallUnexpectedFirewallRoutesViolation' {firewallEndpoint} -> firewallEndpoint) (\s@NetworkFirewallUnexpectedFirewallRoutesViolation' {} a -> s {firewallEndpoint = a} :: NetworkFirewallUnexpectedFirewallRoutesViolation)

-- | The ID of the route table.
networkFirewallUnexpectedFirewallRoutesViolation_routeTableId :: Lens.Lens' NetworkFirewallUnexpectedFirewallRoutesViolation (Prelude.Maybe Prelude.Text)
networkFirewallUnexpectedFirewallRoutesViolation_routeTableId = Lens.lens (\NetworkFirewallUnexpectedFirewallRoutesViolation' {routeTableId} -> routeTableId) (\s@NetworkFirewallUnexpectedFirewallRoutesViolation' {} a -> s {routeTableId = a} :: NetworkFirewallUnexpectedFirewallRoutesViolation)

-- | The routes that are in violation.
networkFirewallUnexpectedFirewallRoutesViolation_violatingRoutes :: Lens.Lens' NetworkFirewallUnexpectedFirewallRoutesViolation (Prelude.Maybe [Route])
networkFirewallUnexpectedFirewallRoutesViolation_violatingRoutes = Lens.lens (\NetworkFirewallUnexpectedFirewallRoutesViolation' {violatingRoutes} -> violatingRoutes) (\s@NetworkFirewallUnexpectedFirewallRoutesViolation' {} a -> s {violatingRoutes = a} :: NetworkFirewallUnexpectedFirewallRoutesViolation) Prelude.. Lens.mapping Lens._Coerce

-- | The subnet ID for the firewall.
networkFirewallUnexpectedFirewallRoutesViolation_firewallSubnetId :: Lens.Lens' NetworkFirewallUnexpectedFirewallRoutesViolation (Prelude.Maybe Prelude.Text)
networkFirewallUnexpectedFirewallRoutesViolation_firewallSubnetId = Lens.lens (\NetworkFirewallUnexpectedFirewallRoutesViolation' {firewallSubnetId} -> firewallSubnetId) (\s@NetworkFirewallUnexpectedFirewallRoutesViolation' {} a -> s {firewallSubnetId = a} :: NetworkFirewallUnexpectedFirewallRoutesViolation)

-- | Information about the VPC ID.
networkFirewallUnexpectedFirewallRoutesViolation_vpcId :: Lens.Lens' NetworkFirewallUnexpectedFirewallRoutesViolation (Prelude.Maybe Prelude.Text)
networkFirewallUnexpectedFirewallRoutesViolation_vpcId = Lens.lens (\NetworkFirewallUnexpectedFirewallRoutesViolation' {vpcId} -> vpcId) (\s@NetworkFirewallUnexpectedFirewallRoutesViolation' {} a -> s {vpcId = a} :: NetworkFirewallUnexpectedFirewallRoutesViolation)

instance
  Core.FromJSON
    NetworkFirewallUnexpectedFirewallRoutesViolation
  where
  parseJSON =
    Core.withObject
      "NetworkFirewallUnexpectedFirewallRoutesViolation"
      ( \x ->
          NetworkFirewallUnexpectedFirewallRoutesViolation'
            Prelude.<$> (x Core..:? "FirewallEndpoint")
              Prelude.<*> (x Core..:? "RouteTableId")
              Prelude.<*> ( x Core..:? "ViolatingRoutes"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> (x Core..:? "FirewallSubnetId")
              Prelude.<*> (x Core..:? "VpcId")
      )

instance
  Prelude.Hashable
    NetworkFirewallUnexpectedFirewallRoutesViolation

instance
  Prelude.NFData
    NetworkFirewallUnexpectedFirewallRoutesViolation
