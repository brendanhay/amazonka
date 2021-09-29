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
-- Module      : Network.AWS.FMS.Types.NetworkFirewallMissingSubnetViolation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.NetworkFirewallMissingSubnetViolation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Violation detail for Network Firewall for an Availability Zone that\'s
-- missing the expected Firewall Manager managed subnet.
--
-- /See:/ 'newNetworkFirewallMissingSubnetViolation' smart constructor.
data NetworkFirewallMissingSubnetViolation = NetworkFirewallMissingSubnetViolation'
  { -- | The reason the resource has this violation, if one is available.
    targetViolationReason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Network Firewall or VPC resource that\'s in violation.
    violationTarget :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone of a violating subnet.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The resource ID of the VPC associated with a violating subnet.
    vpc :: Prelude.Maybe Prelude.Text
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
-- 'targetViolationReason', 'networkFirewallMissingSubnetViolation_targetViolationReason' - The reason the resource has this violation, if one is available.
--
-- 'violationTarget', 'networkFirewallMissingSubnetViolation_violationTarget' - The ID of the Network Firewall or VPC resource that\'s in violation.
--
-- 'availabilityZone', 'networkFirewallMissingSubnetViolation_availabilityZone' - The Availability Zone of a violating subnet.
--
-- 'vpc', 'networkFirewallMissingSubnetViolation_vpc' - The resource ID of the VPC associated with a violating subnet.
newNetworkFirewallMissingSubnetViolation ::
  NetworkFirewallMissingSubnetViolation
newNetworkFirewallMissingSubnetViolation =
  NetworkFirewallMissingSubnetViolation'
    { targetViolationReason =
        Prelude.Nothing,
      violationTarget = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      vpc = Prelude.Nothing
    }

-- | The reason the resource has this violation, if one is available.
networkFirewallMissingSubnetViolation_targetViolationReason :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingSubnetViolation_targetViolationReason = Lens.lens (\NetworkFirewallMissingSubnetViolation' {targetViolationReason} -> targetViolationReason) (\s@NetworkFirewallMissingSubnetViolation' {} a -> s {targetViolationReason = a} :: NetworkFirewallMissingSubnetViolation)

-- | The ID of the Network Firewall or VPC resource that\'s in violation.
networkFirewallMissingSubnetViolation_violationTarget :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingSubnetViolation_violationTarget = Lens.lens (\NetworkFirewallMissingSubnetViolation' {violationTarget} -> violationTarget) (\s@NetworkFirewallMissingSubnetViolation' {} a -> s {violationTarget = a} :: NetworkFirewallMissingSubnetViolation)

-- | The Availability Zone of a violating subnet.
networkFirewallMissingSubnetViolation_availabilityZone :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingSubnetViolation_availabilityZone = Lens.lens (\NetworkFirewallMissingSubnetViolation' {availabilityZone} -> availabilityZone) (\s@NetworkFirewallMissingSubnetViolation' {} a -> s {availabilityZone = a} :: NetworkFirewallMissingSubnetViolation)

-- | The resource ID of the VPC associated with a violating subnet.
networkFirewallMissingSubnetViolation_vpc :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Prelude.Maybe Prelude.Text)
networkFirewallMissingSubnetViolation_vpc = Lens.lens (\NetworkFirewallMissingSubnetViolation' {vpc} -> vpc) (\s@NetworkFirewallMissingSubnetViolation' {} a -> s {vpc = a} :: NetworkFirewallMissingSubnetViolation)

instance
  Core.FromJSON
    NetworkFirewallMissingSubnetViolation
  where
  parseJSON =
    Core.withObject
      "NetworkFirewallMissingSubnetViolation"
      ( \x ->
          NetworkFirewallMissingSubnetViolation'
            Prelude.<$> (x Core..:? "TargetViolationReason")
            Prelude.<*> (x Core..:? "ViolationTarget")
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> (x Core..:? "VPC")
      )

instance
  Prelude.Hashable
    NetworkFirewallMissingSubnetViolation

instance
  Prelude.NFData
    NetworkFirewallMissingSubnetViolation
