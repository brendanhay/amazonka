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
-- Module      : Amazonka.FMS.Types.NetworkFirewallPolicyModifiedViolation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.NetworkFirewallPolicyModifiedViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FMS.Types.NetworkFirewallPolicyDescription
import qualified Amazonka.Prelude as Prelude

-- | Violation detail for Network Firewall for a firewall policy that has a
-- different NetworkFirewallPolicyDescription than is required by the
-- Firewall Manager policy.
--
-- /See:/ 'newNetworkFirewallPolicyModifiedViolation' smart constructor.
data NetworkFirewallPolicyModifiedViolation = NetworkFirewallPolicyModifiedViolation'
  { -- | The ID of the Network Firewall or VPC resource that\'s in violation.
    violationTarget :: Prelude.Maybe Prelude.Text,
    -- | The policy that\'s currently in use in the individual account.
    currentPolicyDescription :: Prelude.Maybe NetworkFirewallPolicyDescription,
    -- | The policy that should be in use in the individual account in order to
    -- be compliant.
    expectedPolicyDescription :: Prelude.Maybe NetworkFirewallPolicyDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFirewallPolicyModifiedViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'violationTarget', 'networkFirewallPolicyModifiedViolation_violationTarget' - The ID of the Network Firewall or VPC resource that\'s in violation.
--
-- 'currentPolicyDescription', 'networkFirewallPolicyModifiedViolation_currentPolicyDescription' - The policy that\'s currently in use in the individual account.
--
-- 'expectedPolicyDescription', 'networkFirewallPolicyModifiedViolation_expectedPolicyDescription' - The policy that should be in use in the individual account in order to
-- be compliant.
newNetworkFirewallPolicyModifiedViolation ::
  NetworkFirewallPolicyModifiedViolation
newNetworkFirewallPolicyModifiedViolation =
  NetworkFirewallPolicyModifiedViolation'
    { violationTarget =
        Prelude.Nothing,
      currentPolicyDescription =
        Prelude.Nothing,
      expectedPolicyDescription =
        Prelude.Nothing
    }

-- | The ID of the Network Firewall or VPC resource that\'s in violation.
networkFirewallPolicyModifiedViolation_violationTarget :: Lens.Lens' NetworkFirewallPolicyModifiedViolation (Prelude.Maybe Prelude.Text)
networkFirewallPolicyModifiedViolation_violationTarget = Lens.lens (\NetworkFirewallPolicyModifiedViolation' {violationTarget} -> violationTarget) (\s@NetworkFirewallPolicyModifiedViolation' {} a -> s {violationTarget = a} :: NetworkFirewallPolicyModifiedViolation)

-- | The policy that\'s currently in use in the individual account.
networkFirewallPolicyModifiedViolation_currentPolicyDescription :: Lens.Lens' NetworkFirewallPolicyModifiedViolation (Prelude.Maybe NetworkFirewallPolicyDescription)
networkFirewallPolicyModifiedViolation_currentPolicyDescription = Lens.lens (\NetworkFirewallPolicyModifiedViolation' {currentPolicyDescription} -> currentPolicyDescription) (\s@NetworkFirewallPolicyModifiedViolation' {} a -> s {currentPolicyDescription = a} :: NetworkFirewallPolicyModifiedViolation)

-- | The policy that should be in use in the individual account in order to
-- be compliant.
networkFirewallPolicyModifiedViolation_expectedPolicyDescription :: Lens.Lens' NetworkFirewallPolicyModifiedViolation (Prelude.Maybe NetworkFirewallPolicyDescription)
networkFirewallPolicyModifiedViolation_expectedPolicyDescription = Lens.lens (\NetworkFirewallPolicyModifiedViolation' {expectedPolicyDescription} -> expectedPolicyDescription) (\s@NetworkFirewallPolicyModifiedViolation' {} a -> s {expectedPolicyDescription = a} :: NetworkFirewallPolicyModifiedViolation)

instance
  Core.FromJSON
    NetworkFirewallPolicyModifiedViolation
  where
  parseJSON =
    Core.withObject
      "NetworkFirewallPolicyModifiedViolation"
      ( \x ->
          NetworkFirewallPolicyModifiedViolation'
            Prelude.<$> (x Core..:? "ViolationTarget")
            Prelude.<*> (x Core..:? "CurrentPolicyDescription")
            Prelude.<*> (x Core..:? "ExpectedPolicyDescription")
      )

instance
  Prelude.Hashable
    NetworkFirewallPolicyModifiedViolation
  where
  hashWithSalt
    _salt
    NetworkFirewallPolicyModifiedViolation' {..} =
      _salt `Prelude.hashWithSalt` violationTarget
        `Prelude.hashWithSalt` currentPolicyDescription
        `Prelude.hashWithSalt` expectedPolicyDescription

instance
  Prelude.NFData
    NetworkFirewallPolicyModifiedViolation
  where
  rnf NetworkFirewallPolicyModifiedViolation' {..} =
    Prelude.rnf violationTarget
      `Prelude.seq` Prelude.rnf currentPolicyDescription
      `Prelude.seq` Prelude.rnf expectedPolicyDescription
