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
-- Module      : Amazonka.FMS.Types.NetworkFirewallMissingSubnetViolation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.NetworkFirewallMissingSubnetViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Violation detail for Network Firewall for an Availability Zone that\'s
-- missing the expected Firewall Manager managed subnet.
--
-- /See:/ 'newNetworkFirewallMissingSubnetViolation' smart constructor.
data NetworkFirewallMissingSubnetViolation = NetworkFirewallMissingSubnetViolation'
  { -- | The Availability Zone of a violating subnet.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The reason the resource has this violation, if one is available.
    targetViolationReason :: Prelude.Maybe Prelude.Text,
    -- | The resource ID of the VPC associated with a violating subnet.
    vpc :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Network Firewall or VPC resource that\'s in violation.
    violationTarget :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFirewallMissingSubnetViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'networkFirewallMissingSubnetViolation_availabilityZone' - The Availability Zone of a violating subnet.
--
-- 'targetViolationReason', 'networkFirewallMissingSubnetViolation_targetViolationReason' - The reason the resource has this violation, if one is available.
--
-- 'vpc', 'networkFirewallMissingSubnetViolation_vpc' - The resource ID of the VPC associated with a violating subnet.
--
-- 'violationTarget', 'networkFirewallMissingSubnetViolation_violationTarget' - The ID of the Network Firewall or VPC resource that\'s in violation.
newNetworkFirewallMissingSubnetViolation ::
  NetworkFirewallMissingSubnetViolation
newNetworkFirewallMissingSubnetViolation =
  NetworkFirewallMissingSubnetViolation'
    { availabilityZone =
        Prelude.Nothing,
      targetViolationReason =
        Prelude.Nothing,
      vpc = Prelude.Nothing,
      violationTarget = Prelude.Nothing
    }

-- | The Availability Zone of a violating subnet.
networkFirewallMissingSubnetViolation_availabilityZone :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingSubnetViolation_availabilityZone = Lens.lens (\NetworkFirewallMissingSubnetViolation' {availabilityZone} -> availabilityZone) (\s@NetworkFirewallMissingSubnetViolation' {} a -> s {availabilityZone = a} :: NetworkFirewallMissingSubnetViolation)

-- | The reason the resource has this violation, if one is available.
networkFirewallMissingSubnetViolation_targetViolationReason :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingSubnetViolation_targetViolationReason = Lens.lens (\NetworkFirewallMissingSubnetViolation' {targetViolationReason} -> targetViolationReason) (\s@NetworkFirewallMissingSubnetViolation' {} a -> s {targetViolationReason = a} :: NetworkFirewallMissingSubnetViolation)

-- | The resource ID of the VPC associated with a violating subnet.
networkFirewallMissingSubnetViolation_vpc :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingSubnetViolation_vpc = Lens.lens (\NetworkFirewallMissingSubnetViolation' {vpc} -> vpc) (\s@NetworkFirewallMissingSubnetViolation' {} a -> s {vpc = a} :: NetworkFirewallMissingSubnetViolation)

-- | The ID of the Network Firewall or VPC resource that\'s in violation.
networkFirewallMissingSubnetViolation_violationTarget :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingSubnetViolation_violationTarget = Lens.lens (\NetworkFirewallMissingSubnetViolation' {violationTarget} -> violationTarget) (\s@NetworkFirewallMissingSubnetViolation' {} a -> s {violationTarget = a} :: NetworkFirewallMissingSubnetViolation)

instance
  Data.FromJSON
    NetworkFirewallMissingSubnetViolation
  where
  parseJSON =
    Data.withObject
      "NetworkFirewallMissingSubnetViolation"
      ( \x ->
          NetworkFirewallMissingSubnetViolation'
            Prelude.<$> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "TargetViolationReason")
            Prelude.<*> (x Data..:? "VPC")
            Prelude.<*> (x Data..:? "ViolationTarget")
      )

instance
  Prelude.Hashable
    NetworkFirewallMissingSubnetViolation
  where
  hashWithSalt
    _salt
    NetworkFirewallMissingSubnetViolation' {..} =
      _salt `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` targetViolationReason
        `Prelude.hashWithSalt` vpc
        `Prelude.hashWithSalt` violationTarget

instance
  Prelude.NFData
    NetworkFirewallMissingSubnetViolation
  where
  rnf NetworkFirewallMissingSubnetViolation' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf targetViolationReason
      `Prelude.seq` Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf violationTarget
