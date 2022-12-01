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
-- Module      : Amazonka.FMS.Types.NetworkFirewallMissingFirewallViolation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.NetworkFirewallMissingFirewallViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Violation detail for Network Firewall for a subnet that doesn\'t have a
-- Firewall Manager managed firewall in its VPC.
--
-- /See:/ 'newNetworkFirewallMissingFirewallViolation' smart constructor.
data NetworkFirewallMissingFirewallViolation = NetworkFirewallMissingFirewallViolation'
  { -- | The resource ID of the VPC associated with a violating subnet.
    vpc :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Network Firewall or VPC resource that\'s in violation.
    violationTarget :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone of a violating subnet.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The reason the resource has this violation, if one is available.
    targetViolationReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFirewallMissingFirewallViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpc', 'networkFirewallMissingFirewallViolation_vpc' - The resource ID of the VPC associated with a violating subnet.
--
-- 'violationTarget', 'networkFirewallMissingFirewallViolation_violationTarget' - The ID of the Network Firewall or VPC resource that\'s in violation.
--
-- 'availabilityZone', 'networkFirewallMissingFirewallViolation_availabilityZone' - The Availability Zone of a violating subnet.
--
-- 'targetViolationReason', 'networkFirewallMissingFirewallViolation_targetViolationReason' - The reason the resource has this violation, if one is available.
newNetworkFirewallMissingFirewallViolation ::
  NetworkFirewallMissingFirewallViolation
newNetworkFirewallMissingFirewallViolation =
  NetworkFirewallMissingFirewallViolation'
    { vpc =
        Prelude.Nothing,
      violationTarget = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      targetViolationReason =
        Prelude.Nothing
    }

-- | The resource ID of the VPC associated with a violating subnet.
networkFirewallMissingFirewallViolation_vpc :: Lens.Lens' NetworkFirewallMissingFirewallViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingFirewallViolation_vpc = Lens.lens (\NetworkFirewallMissingFirewallViolation' {vpc} -> vpc) (\s@NetworkFirewallMissingFirewallViolation' {} a -> s {vpc = a} :: NetworkFirewallMissingFirewallViolation)

-- | The ID of the Network Firewall or VPC resource that\'s in violation.
networkFirewallMissingFirewallViolation_violationTarget :: Lens.Lens' NetworkFirewallMissingFirewallViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingFirewallViolation_violationTarget = Lens.lens (\NetworkFirewallMissingFirewallViolation' {violationTarget} -> violationTarget) (\s@NetworkFirewallMissingFirewallViolation' {} a -> s {violationTarget = a} :: NetworkFirewallMissingFirewallViolation)

-- | The Availability Zone of a violating subnet.
networkFirewallMissingFirewallViolation_availabilityZone :: Lens.Lens' NetworkFirewallMissingFirewallViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingFirewallViolation_availabilityZone = Lens.lens (\NetworkFirewallMissingFirewallViolation' {availabilityZone} -> availabilityZone) (\s@NetworkFirewallMissingFirewallViolation' {} a -> s {availabilityZone = a} :: NetworkFirewallMissingFirewallViolation)

-- | The reason the resource has this violation, if one is available.
networkFirewallMissingFirewallViolation_targetViolationReason :: Lens.Lens' NetworkFirewallMissingFirewallViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingFirewallViolation_targetViolationReason = Lens.lens (\NetworkFirewallMissingFirewallViolation' {targetViolationReason} -> targetViolationReason) (\s@NetworkFirewallMissingFirewallViolation' {} a -> s {targetViolationReason = a} :: NetworkFirewallMissingFirewallViolation)

instance
  Core.FromJSON
    NetworkFirewallMissingFirewallViolation
  where
  parseJSON =
    Core.withObject
      "NetworkFirewallMissingFirewallViolation"
      ( \x ->
          NetworkFirewallMissingFirewallViolation'
            Prelude.<$> (x Core..:? "VPC")
            Prelude.<*> (x Core..:? "ViolationTarget")
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> (x Core..:? "TargetViolationReason")
      )

instance
  Prelude.Hashable
    NetworkFirewallMissingFirewallViolation
  where
  hashWithSalt
    _salt
    NetworkFirewallMissingFirewallViolation' {..} =
      _salt `Prelude.hashWithSalt` vpc
        `Prelude.hashWithSalt` violationTarget
        `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` targetViolationReason

instance
  Prelude.NFData
    NetworkFirewallMissingFirewallViolation
  where
  rnf NetworkFirewallMissingFirewallViolation' {..} =
    Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf violationTarget
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf targetViolationReason
