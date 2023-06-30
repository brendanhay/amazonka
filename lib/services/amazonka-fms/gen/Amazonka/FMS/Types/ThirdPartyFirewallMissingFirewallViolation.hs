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
-- Module      : Amazonka.FMS.Types.ThirdPartyFirewallMissingFirewallViolation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ThirdPartyFirewallMissingFirewallViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The violation details about a third-party firewall\'s subnet that
-- doesn\'t have a Firewall Manager managed firewall in its VPC.
--
-- /See:/ 'newThirdPartyFirewallMissingFirewallViolation' smart constructor.
data ThirdPartyFirewallMissingFirewallViolation = ThirdPartyFirewallMissingFirewallViolation'
  { -- | The Availability Zone of the third-party firewall that\'s causing the
    -- violation.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The reason the resource is causing this violation, if a reason is
    -- available.
    targetViolationReason :: Prelude.Maybe Prelude.Text,
    -- | The resource ID of the VPC associated with a third-party firewall.
    vpc :: Prelude.Maybe Prelude.Text,
    -- | The ID of the third-party firewall that\'s causing the violation.
    violationTarget :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThirdPartyFirewallMissingFirewallViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'thirdPartyFirewallMissingFirewallViolation_availabilityZone' - The Availability Zone of the third-party firewall that\'s causing the
-- violation.
--
-- 'targetViolationReason', 'thirdPartyFirewallMissingFirewallViolation_targetViolationReason' - The reason the resource is causing this violation, if a reason is
-- available.
--
-- 'vpc', 'thirdPartyFirewallMissingFirewallViolation_vpc' - The resource ID of the VPC associated with a third-party firewall.
--
-- 'violationTarget', 'thirdPartyFirewallMissingFirewallViolation_violationTarget' - The ID of the third-party firewall that\'s causing the violation.
newThirdPartyFirewallMissingFirewallViolation ::
  ThirdPartyFirewallMissingFirewallViolation
newThirdPartyFirewallMissingFirewallViolation =
  ThirdPartyFirewallMissingFirewallViolation'
    { availabilityZone =
        Prelude.Nothing,
      targetViolationReason =
        Prelude.Nothing,
      vpc = Prelude.Nothing,
      violationTarget =
        Prelude.Nothing
    }

-- | The Availability Zone of the third-party firewall that\'s causing the
-- violation.
thirdPartyFirewallMissingFirewallViolation_availabilityZone :: Lens.Lens' ThirdPartyFirewallMissingFirewallViolation (Prelude.Maybe Prelude.Text)
thirdPartyFirewallMissingFirewallViolation_availabilityZone = Lens.lens (\ThirdPartyFirewallMissingFirewallViolation' {availabilityZone} -> availabilityZone) (\s@ThirdPartyFirewallMissingFirewallViolation' {} a -> s {availabilityZone = a} :: ThirdPartyFirewallMissingFirewallViolation)

-- | The reason the resource is causing this violation, if a reason is
-- available.
thirdPartyFirewallMissingFirewallViolation_targetViolationReason :: Lens.Lens' ThirdPartyFirewallMissingFirewallViolation (Prelude.Maybe Prelude.Text)
thirdPartyFirewallMissingFirewallViolation_targetViolationReason = Lens.lens (\ThirdPartyFirewallMissingFirewallViolation' {targetViolationReason} -> targetViolationReason) (\s@ThirdPartyFirewallMissingFirewallViolation' {} a -> s {targetViolationReason = a} :: ThirdPartyFirewallMissingFirewallViolation)

-- | The resource ID of the VPC associated with a third-party firewall.
thirdPartyFirewallMissingFirewallViolation_vpc :: Lens.Lens' ThirdPartyFirewallMissingFirewallViolation (Prelude.Maybe Prelude.Text)
thirdPartyFirewallMissingFirewallViolation_vpc = Lens.lens (\ThirdPartyFirewallMissingFirewallViolation' {vpc} -> vpc) (\s@ThirdPartyFirewallMissingFirewallViolation' {} a -> s {vpc = a} :: ThirdPartyFirewallMissingFirewallViolation)

-- | The ID of the third-party firewall that\'s causing the violation.
thirdPartyFirewallMissingFirewallViolation_violationTarget :: Lens.Lens' ThirdPartyFirewallMissingFirewallViolation (Prelude.Maybe Prelude.Text)
thirdPartyFirewallMissingFirewallViolation_violationTarget = Lens.lens (\ThirdPartyFirewallMissingFirewallViolation' {violationTarget} -> violationTarget) (\s@ThirdPartyFirewallMissingFirewallViolation' {} a -> s {violationTarget = a} :: ThirdPartyFirewallMissingFirewallViolation)

instance
  Data.FromJSON
    ThirdPartyFirewallMissingFirewallViolation
  where
  parseJSON =
    Data.withObject
      "ThirdPartyFirewallMissingFirewallViolation"
      ( \x ->
          ThirdPartyFirewallMissingFirewallViolation'
            Prelude.<$> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "TargetViolationReason")
            Prelude.<*> (x Data..:? "VPC")
            Prelude.<*> (x Data..:? "ViolationTarget")
      )

instance
  Prelude.Hashable
    ThirdPartyFirewallMissingFirewallViolation
  where
  hashWithSalt
    _salt
    ThirdPartyFirewallMissingFirewallViolation' {..} =
      _salt
        `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` targetViolationReason
        `Prelude.hashWithSalt` vpc
        `Prelude.hashWithSalt` violationTarget

instance
  Prelude.NFData
    ThirdPartyFirewallMissingFirewallViolation
  where
  rnf ThirdPartyFirewallMissingFirewallViolation' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf targetViolationReason
      `Prelude.seq` Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf violationTarget
