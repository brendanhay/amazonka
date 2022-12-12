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
-- Module      : Amazonka.FMS.Types.NetworkFirewallMissingExpectedRTViolation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.NetworkFirewallMissingExpectedRTViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Violation detail for Network Firewall for a subnet that\'s not
-- associated to the expected Firewall Manager managed route table.
--
-- /See:/ 'newNetworkFirewallMissingExpectedRTViolation' smart constructor.
data NetworkFirewallMissingExpectedRTViolation = NetworkFirewallMissingExpectedRTViolation'
  { -- | The Availability Zone of a violating subnet.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The resource ID of the current route table that\'s associated with the
    -- subnet, if one is available.
    currentRouteTable :: Prelude.Maybe Prelude.Text,
    -- | The resource ID of the route table that should be associated with the
    -- subnet.
    expectedRouteTable :: Prelude.Maybe Prelude.Text,
    -- | The resource ID of the VPC associated with a violating subnet.
    vpc :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Network Firewall or VPC resource that\'s in violation.
    violationTarget :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFirewallMissingExpectedRTViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'networkFirewallMissingExpectedRTViolation_availabilityZone' - The Availability Zone of a violating subnet.
--
-- 'currentRouteTable', 'networkFirewallMissingExpectedRTViolation_currentRouteTable' - The resource ID of the current route table that\'s associated with the
-- subnet, if one is available.
--
-- 'expectedRouteTable', 'networkFirewallMissingExpectedRTViolation_expectedRouteTable' - The resource ID of the route table that should be associated with the
-- subnet.
--
-- 'vpc', 'networkFirewallMissingExpectedRTViolation_vpc' - The resource ID of the VPC associated with a violating subnet.
--
-- 'violationTarget', 'networkFirewallMissingExpectedRTViolation_violationTarget' - The ID of the Network Firewall or VPC resource that\'s in violation.
newNetworkFirewallMissingExpectedRTViolation ::
  NetworkFirewallMissingExpectedRTViolation
newNetworkFirewallMissingExpectedRTViolation =
  NetworkFirewallMissingExpectedRTViolation'
    { availabilityZone =
        Prelude.Nothing,
      currentRouteTable =
        Prelude.Nothing,
      expectedRouteTable =
        Prelude.Nothing,
      vpc = Prelude.Nothing,
      violationTarget =
        Prelude.Nothing
    }

-- | The Availability Zone of a violating subnet.
networkFirewallMissingExpectedRTViolation_availabilityZone :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingExpectedRTViolation_availabilityZone = Lens.lens (\NetworkFirewallMissingExpectedRTViolation' {availabilityZone} -> availabilityZone) (\s@NetworkFirewallMissingExpectedRTViolation' {} a -> s {availabilityZone = a} :: NetworkFirewallMissingExpectedRTViolation)

-- | The resource ID of the current route table that\'s associated with the
-- subnet, if one is available.
networkFirewallMissingExpectedRTViolation_currentRouteTable :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingExpectedRTViolation_currentRouteTable = Lens.lens (\NetworkFirewallMissingExpectedRTViolation' {currentRouteTable} -> currentRouteTable) (\s@NetworkFirewallMissingExpectedRTViolation' {} a -> s {currentRouteTable = a} :: NetworkFirewallMissingExpectedRTViolation)

-- | The resource ID of the route table that should be associated with the
-- subnet.
networkFirewallMissingExpectedRTViolation_expectedRouteTable :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingExpectedRTViolation_expectedRouteTable = Lens.lens (\NetworkFirewallMissingExpectedRTViolation' {expectedRouteTable} -> expectedRouteTable) (\s@NetworkFirewallMissingExpectedRTViolation' {} a -> s {expectedRouteTable = a} :: NetworkFirewallMissingExpectedRTViolation)

-- | The resource ID of the VPC associated with a violating subnet.
networkFirewallMissingExpectedRTViolation_vpc :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingExpectedRTViolation_vpc = Lens.lens (\NetworkFirewallMissingExpectedRTViolation' {vpc} -> vpc) (\s@NetworkFirewallMissingExpectedRTViolation' {} a -> s {vpc = a} :: NetworkFirewallMissingExpectedRTViolation)

-- | The ID of the Network Firewall or VPC resource that\'s in violation.
networkFirewallMissingExpectedRTViolation_violationTarget :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingExpectedRTViolation_violationTarget = Lens.lens (\NetworkFirewallMissingExpectedRTViolation' {violationTarget} -> violationTarget) (\s@NetworkFirewallMissingExpectedRTViolation' {} a -> s {violationTarget = a} :: NetworkFirewallMissingExpectedRTViolation)

instance
  Data.FromJSON
    NetworkFirewallMissingExpectedRTViolation
  where
  parseJSON =
    Data.withObject
      "NetworkFirewallMissingExpectedRTViolation"
      ( \x ->
          NetworkFirewallMissingExpectedRTViolation'
            Prelude.<$> (x Data..:? "AvailabilityZone")
              Prelude.<*> (x Data..:? "CurrentRouteTable")
              Prelude.<*> (x Data..:? "ExpectedRouteTable")
              Prelude.<*> (x Data..:? "VPC")
              Prelude.<*> (x Data..:? "ViolationTarget")
      )

instance
  Prelude.Hashable
    NetworkFirewallMissingExpectedRTViolation
  where
  hashWithSalt
    _salt
    NetworkFirewallMissingExpectedRTViolation' {..} =
      _salt `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` currentRouteTable
        `Prelude.hashWithSalt` expectedRouteTable
        `Prelude.hashWithSalt` vpc
        `Prelude.hashWithSalt` violationTarget

instance
  Prelude.NFData
    NetworkFirewallMissingExpectedRTViolation
  where
  rnf NetworkFirewallMissingExpectedRTViolation' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf currentRouteTable
      `Prelude.seq` Prelude.rnf expectedRouteTable
      `Prelude.seq` Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf violationTarget
