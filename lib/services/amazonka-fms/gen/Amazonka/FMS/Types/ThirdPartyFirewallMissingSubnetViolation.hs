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
-- Module      : Amazonka.FMS.Types.ThirdPartyFirewallMissingSubnetViolation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ThirdPartyFirewallMissingSubnetViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The violation details for a third-party firewall for an Availability
-- Zone that\'s missing the Firewall Manager managed subnet.
--
-- /See:/ 'newThirdPartyFirewallMissingSubnetViolation' smart constructor.
data ThirdPartyFirewallMissingSubnetViolation = ThirdPartyFirewallMissingSubnetViolation'
  { -- | The resource ID of the VPC associated with a subnet that\'s causing the
    -- violation.
    vpc :: Prelude.Maybe Prelude.Text,
    -- | The ID of the third-party firewall or VPC resource that\'s causing the
    -- violation.
    violationTarget :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone of a subnet that\'s causing the violation.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The reason the resource is causing the violation, if a reason is
    -- available.
    targetViolationReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThirdPartyFirewallMissingSubnetViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpc', 'thirdPartyFirewallMissingSubnetViolation_vpc' - The resource ID of the VPC associated with a subnet that\'s causing the
-- violation.
--
-- 'violationTarget', 'thirdPartyFirewallMissingSubnetViolation_violationTarget' - The ID of the third-party firewall or VPC resource that\'s causing the
-- violation.
--
-- 'availabilityZone', 'thirdPartyFirewallMissingSubnetViolation_availabilityZone' - The Availability Zone of a subnet that\'s causing the violation.
--
-- 'targetViolationReason', 'thirdPartyFirewallMissingSubnetViolation_targetViolationReason' - The reason the resource is causing the violation, if a reason is
-- available.
newThirdPartyFirewallMissingSubnetViolation ::
  ThirdPartyFirewallMissingSubnetViolation
newThirdPartyFirewallMissingSubnetViolation =
  ThirdPartyFirewallMissingSubnetViolation'
    { vpc =
        Prelude.Nothing,
      violationTarget = Prelude.Nothing,
      availabilityZone =
        Prelude.Nothing,
      targetViolationReason =
        Prelude.Nothing
    }

-- | The resource ID of the VPC associated with a subnet that\'s causing the
-- violation.
thirdPartyFirewallMissingSubnetViolation_vpc :: Lens.Lens' ThirdPartyFirewallMissingSubnetViolation (Prelude.Maybe Prelude.Text)
thirdPartyFirewallMissingSubnetViolation_vpc = Lens.lens (\ThirdPartyFirewallMissingSubnetViolation' {vpc} -> vpc) (\s@ThirdPartyFirewallMissingSubnetViolation' {} a -> s {vpc = a} :: ThirdPartyFirewallMissingSubnetViolation)

-- | The ID of the third-party firewall or VPC resource that\'s causing the
-- violation.
thirdPartyFirewallMissingSubnetViolation_violationTarget :: Lens.Lens' ThirdPartyFirewallMissingSubnetViolation (Prelude.Maybe Prelude.Text)
thirdPartyFirewallMissingSubnetViolation_violationTarget = Lens.lens (\ThirdPartyFirewallMissingSubnetViolation' {violationTarget} -> violationTarget) (\s@ThirdPartyFirewallMissingSubnetViolation' {} a -> s {violationTarget = a} :: ThirdPartyFirewallMissingSubnetViolation)

-- | The Availability Zone of a subnet that\'s causing the violation.
thirdPartyFirewallMissingSubnetViolation_availabilityZone :: Lens.Lens' ThirdPartyFirewallMissingSubnetViolation (Prelude.Maybe Prelude.Text)
thirdPartyFirewallMissingSubnetViolation_availabilityZone = Lens.lens (\ThirdPartyFirewallMissingSubnetViolation' {availabilityZone} -> availabilityZone) (\s@ThirdPartyFirewallMissingSubnetViolation' {} a -> s {availabilityZone = a} :: ThirdPartyFirewallMissingSubnetViolation)

-- | The reason the resource is causing the violation, if a reason is
-- available.
thirdPartyFirewallMissingSubnetViolation_targetViolationReason :: Lens.Lens' ThirdPartyFirewallMissingSubnetViolation (Prelude.Maybe Prelude.Text)
thirdPartyFirewallMissingSubnetViolation_targetViolationReason = Lens.lens (\ThirdPartyFirewallMissingSubnetViolation' {targetViolationReason} -> targetViolationReason) (\s@ThirdPartyFirewallMissingSubnetViolation' {} a -> s {targetViolationReason = a} :: ThirdPartyFirewallMissingSubnetViolation)

instance
  Data.FromJSON
    ThirdPartyFirewallMissingSubnetViolation
  where
  parseJSON =
    Data.withObject
      "ThirdPartyFirewallMissingSubnetViolation"
      ( \x ->
          ThirdPartyFirewallMissingSubnetViolation'
            Prelude.<$> (x Data..:? "VPC")
            Prelude.<*> (x Data..:? "ViolationTarget")
            Prelude.<*> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "TargetViolationReason")
      )

instance
  Prelude.Hashable
    ThirdPartyFirewallMissingSubnetViolation
  where
  hashWithSalt
    _salt
    ThirdPartyFirewallMissingSubnetViolation' {..} =
      _salt `Prelude.hashWithSalt` vpc
        `Prelude.hashWithSalt` violationTarget
        `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` targetViolationReason

instance
  Prelude.NFData
    ThirdPartyFirewallMissingSubnetViolation
  where
  rnf ThirdPartyFirewallMissingSubnetViolation' {..} =
    Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf violationTarget
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf targetViolationReason
