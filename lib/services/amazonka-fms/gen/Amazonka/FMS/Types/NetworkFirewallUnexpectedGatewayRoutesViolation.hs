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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.NetworkFirewallUnexpectedGatewayRoutesViolation where

import qualified Amazonka.Core as Core
import Amazonka.FMS.Types.Route
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Violation detail for an unexpected gateway route that’s present in a
-- route table.
--
-- /See:/ 'newNetworkFirewallUnexpectedGatewayRoutesViolation' smart constructor.
data NetworkFirewallUnexpectedGatewayRoutesViolation = NetworkFirewallUnexpectedGatewayRoutesViolation'
  { -- | Information about the route table.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | Information about the VPC ID.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The routes that are in violation.
    violatingRoutes :: Prelude.Maybe [Route],
    -- | Information about the gateway ID.
    gatewayId :: Prelude.Maybe Prelude.Text
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
-- 'routeTableId', 'networkFirewallUnexpectedGatewayRoutesViolation_routeTableId' - Information about the route table.
--
-- 'vpcId', 'networkFirewallUnexpectedGatewayRoutesViolation_vpcId' - Information about the VPC ID.
--
-- 'violatingRoutes', 'networkFirewallUnexpectedGatewayRoutesViolation_violatingRoutes' - The routes that are in violation.
--
-- 'gatewayId', 'networkFirewallUnexpectedGatewayRoutesViolation_gatewayId' - Information about the gateway ID.
newNetworkFirewallUnexpectedGatewayRoutesViolation ::
  NetworkFirewallUnexpectedGatewayRoutesViolation
newNetworkFirewallUnexpectedGatewayRoutesViolation =
  NetworkFirewallUnexpectedGatewayRoutesViolation'
    { routeTableId =
        Prelude.Nothing,
      vpcId = Prelude.Nothing,
      violatingRoutes =
        Prelude.Nothing,
      gatewayId =
        Prelude.Nothing
    }

-- | Information about the route table.
networkFirewallUnexpectedGatewayRoutesViolation_routeTableId :: Lens.Lens' NetworkFirewallUnexpectedGatewayRoutesViolation (Prelude.Maybe Prelude.Text)
networkFirewallUnexpectedGatewayRoutesViolation_routeTableId = Lens.lens (\NetworkFirewallUnexpectedGatewayRoutesViolation' {routeTableId} -> routeTableId) (\s@NetworkFirewallUnexpectedGatewayRoutesViolation' {} a -> s {routeTableId = a} :: NetworkFirewallUnexpectedGatewayRoutesViolation)

-- | Information about the VPC ID.
networkFirewallUnexpectedGatewayRoutesViolation_vpcId :: Lens.Lens' NetworkFirewallUnexpectedGatewayRoutesViolation (Prelude.Maybe Prelude.Text)
networkFirewallUnexpectedGatewayRoutesViolation_vpcId = Lens.lens (\NetworkFirewallUnexpectedGatewayRoutesViolation' {vpcId} -> vpcId) (\s@NetworkFirewallUnexpectedGatewayRoutesViolation' {} a -> s {vpcId = a} :: NetworkFirewallUnexpectedGatewayRoutesViolation)

-- | The routes that are in violation.
networkFirewallUnexpectedGatewayRoutesViolation_violatingRoutes :: Lens.Lens' NetworkFirewallUnexpectedGatewayRoutesViolation (Prelude.Maybe [Route])
networkFirewallUnexpectedGatewayRoutesViolation_violatingRoutes = Lens.lens (\NetworkFirewallUnexpectedGatewayRoutesViolation' {violatingRoutes} -> violatingRoutes) (\s@NetworkFirewallUnexpectedGatewayRoutesViolation' {} a -> s {violatingRoutes = a} :: NetworkFirewallUnexpectedGatewayRoutesViolation) Prelude.. Lens.mapping Lens.coerced

-- | Information about the gateway ID.
networkFirewallUnexpectedGatewayRoutesViolation_gatewayId :: Lens.Lens' NetworkFirewallUnexpectedGatewayRoutesViolation (Prelude.Maybe Prelude.Text)
networkFirewallUnexpectedGatewayRoutesViolation_gatewayId = Lens.lens (\NetworkFirewallUnexpectedGatewayRoutesViolation' {gatewayId} -> gatewayId) (\s@NetworkFirewallUnexpectedGatewayRoutesViolation' {} a -> s {gatewayId = a} :: NetworkFirewallUnexpectedGatewayRoutesViolation)

instance
  Core.FromJSON
    NetworkFirewallUnexpectedGatewayRoutesViolation
  where
  parseJSON =
    Core.withObject
      "NetworkFirewallUnexpectedGatewayRoutesViolation"
      ( \x ->
          NetworkFirewallUnexpectedGatewayRoutesViolation'
            Prelude.<$> (x Core..:? "RouteTableId")
              Prelude.<*> (x Core..:? "VpcId")
              Prelude.<*> ( x Core..:? "ViolatingRoutes"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> (x Core..:? "GatewayId")
      )

instance
  Prelude.Hashable
    NetworkFirewallUnexpectedGatewayRoutesViolation
  where
  hashWithSalt
    salt'
    NetworkFirewallUnexpectedGatewayRoutesViolation' {..} =
      salt' `Prelude.hashWithSalt` gatewayId
        `Prelude.hashWithSalt` violatingRoutes
        `Prelude.hashWithSalt` vpcId
        `Prelude.hashWithSalt` routeTableId

instance
  Prelude.NFData
    NetworkFirewallUnexpectedGatewayRoutesViolation
  where
  rnf
    NetworkFirewallUnexpectedGatewayRoutesViolation' {..} =
      Prelude.rnf routeTableId
        `Prelude.seq` Prelude.rnf gatewayId
        `Prelude.seq` Prelude.rnf violatingRoutes
        `Prelude.seq` Prelude.rnf vpcId
