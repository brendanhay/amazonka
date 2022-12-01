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
-- Module      : Amazonka.FMS.Types.NetworkFirewallUnexpectedGatewayRoutesViolation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.NetworkFirewallUnexpectedGatewayRoutesViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FMS.Types.Route
import qualified Amazonka.Prelude as Prelude

-- | Violation detail for an unexpected gateway route that’s present in a
-- route table.
--
-- /See:/ 'newNetworkFirewallUnexpectedGatewayRoutesViolation' smart constructor.
data NetworkFirewallUnexpectedGatewayRoutesViolation = NetworkFirewallUnexpectedGatewayRoutesViolation'
  { -- | The routes that are in violation.
    violatingRoutes :: Prelude.Maybe [Route],
    -- | Information about the route table.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | Information about the gateway ID.
    gatewayId :: Prelude.Maybe Prelude.Text,
    -- | Information about the VPC ID.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFirewallUnexpectedGatewayRoutesViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'violatingRoutes', 'networkFirewallUnexpectedGatewayRoutesViolation_violatingRoutes' - The routes that are in violation.
--
-- 'routeTableId', 'networkFirewallUnexpectedGatewayRoutesViolation_routeTableId' - Information about the route table.
--
-- 'gatewayId', 'networkFirewallUnexpectedGatewayRoutesViolation_gatewayId' - Information about the gateway ID.
--
-- 'vpcId', 'networkFirewallUnexpectedGatewayRoutesViolation_vpcId' - Information about the VPC ID.
newNetworkFirewallUnexpectedGatewayRoutesViolation ::
  NetworkFirewallUnexpectedGatewayRoutesViolation
newNetworkFirewallUnexpectedGatewayRoutesViolation =
  NetworkFirewallUnexpectedGatewayRoutesViolation'
    { violatingRoutes =
        Prelude.Nothing,
      routeTableId =
        Prelude.Nothing,
      gatewayId =
        Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The routes that are in violation.
networkFirewallUnexpectedGatewayRoutesViolation_violatingRoutes :: Lens.Lens' NetworkFirewallUnexpectedGatewayRoutesViolation (Prelude.Maybe [Route])
networkFirewallUnexpectedGatewayRoutesViolation_violatingRoutes = Lens.lens (\NetworkFirewallUnexpectedGatewayRoutesViolation' {violatingRoutes} -> violatingRoutes) (\s@NetworkFirewallUnexpectedGatewayRoutesViolation' {} a -> s {violatingRoutes = a} :: NetworkFirewallUnexpectedGatewayRoutesViolation) Prelude.. Lens.mapping Lens.coerced

-- | Information about the route table.
networkFirewallUnexpectedGatewayRoutesViolation_routeTableId :: Lens.Lens' NetworkFirewallUnexpectedGatewayRoutesViolation (Prelude.Maybe Prelude.Text)
networkFirewallUnexpectedGatewayRoutesViolation_routeTableId = Lens.lens (\NetworkFirewallUnexpectedGatewayRoutesViolation' {routeTableId} -> routeTableId) (\s@NetworkFirewallUnexpectedGatewayRoutesViolation' {} a -> s {routeTableId = a} :: NetworkFirewallUnexpectedGatewayRoutesViolation)

-- | Information about the gateway ID.
networkFirewallUnexpectedGatewayRoutesViolation_gatewayId :: Lens.Lens' NetworkFirewallUnexpectedGatewayRoutesViolation (Prelude.Maybe Prelude.Text)
networkFirewallUnexpectedGatewayRoutesViolation_gatewayId = Lens.lens (\NetworkFirewallUnexpectedGatewayRoutesViolation' {gatewayId} -> gatewayId) (\s@NetworkFirewallUnexpectedGatewayRoutesViolation' {} a -> s {gatewayId = a} :: NetworkFirewallUnexpectedGatewayRoutesViolation)

-- | Information about the VPC ID.
networkFirewallUnexpectedGatewayRoutesViolation_vpcId :: Lens.Lens' NetworkFirewallUnexpectedGatewayRoutesViolation (Prelude.Maybe Prelude.Text)
networkFirewallUnexpectedGatewayRoutesViolation_vpcId = Lens.lens (\NetworkFirewallUnexpectedGatewayRoutesViolation' {vpcId} -> vpcId) (\s@NetworkFirewallUnexpectedGatewayRoutesViolation' {} a -> s {vpcId = a} :: NetworkFirewallUnexpectedGatewayRoutesViolation)

instance
  Core.FromJSON
    NetworkFirewallUnexpectedGatewayRoutesViolation
  where
  parseJSON =
    Core.withObject
      "NetworkFirewallUnexpectedGatewayRoutesViolation"
      ( \x ->
          NetworkFirewallUnexpectedGatewayRoutesViolation'
            Prelude.<$> ( x Core..:? "ViolatingRoutes"
                            Core..!= Prelude.mempty
                        )
              Prelude.<*> (x Core..:? "RouteTableId")
              Prelude.<*> (x Core..:? "GatewayId")
              Prelude.<*> (x Core..:? "VpcId")
      )

instance
  Prelude.Hashable
    NetworkFirewallUnexpectedGatewayRoutesViolation
  where
  hashWithSalt
    _salt
    NetworkFirewallUnexpectedGatewayRoutesViolation' {..} =
      _salt `Prelude.hashWithSalt` violatingRoutes
        `Prelude.hashWithSalt` routeTableId
        `Prelude.hashWithSalt` gatewayId
        `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    NetworkFirewallUnexpectedGatewayRoutesViolation
  where
  rnf
    NetworkFirewallUnexpectedGatewayRoutesViolation' {..} =
      Prelude.rnf violatingRoutes
        `Prelude.seq` Prelude.rnf routeTableId
        `Prelude.seq` Prelude.rnf gatewayId
        `Prelude.seq` Prelude.rnf vpcId
