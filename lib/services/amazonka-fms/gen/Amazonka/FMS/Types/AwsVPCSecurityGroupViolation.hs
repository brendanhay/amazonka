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
-- Module      : Amazonka.FMS.Types.AwsVPCSecurityGroupViolation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.AwsVPCSecurityGroupViolation where

import qualified Amazonka.Core as Core
import Amazonka.FMS.Types.PartialMatch
import Amazonka.FMS.Types.SecurityGroupRemediationAction
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Violation detail for the rule violation in a security group when
-- compared to the primary security group of the Firewall Manager policy.
--
-- /See:/ 'newAwsVPCSecurityGroupViolation' smart constructor.
data AwsVPCSecurityGroupViolation = AwsVPCSecurityGroupViolation'
  { -- | A description of the security group that violates the policy.
    violationTargetDescription :: Prelude.Maybe Prelude.Text,
    -- | Remediation options for the rule specified in the @ViolationTarget@.
    possibleSecurityGroupRemediationActions :: Prelude.Maybe [SecurityGroupRemediationAction],
    -- | The security group rule that is being evaluated.
    violationTarget :: Prelude.Maybe Prelude.Text,
    -- | List of rules specified in the security group of the Firewall Manager
    -- policy that partially match the @ViolationTarget@ rule.
    partialMatches :: Prelude.Maybe [PartialMatch]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsVPCSecurityGroupViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'violationTargetDescription', 'awsVPCSecurityGroupViolation_violationTargetDescription' - A description of the security group that violates the policy.
--
-- 'possibleSecurityGroupRemediationActions', 'awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions' - Remediation options for the rule specified in the @ViolationTarget@.
--
-- 'violationTarget', 'awsVPCSecurityGroupViolation_violationTarget' - The security group rule that is being evaluated.
--
-- 'partialMatches', 'awsVPCSecurityGroupViolation_partialMatches' - List of rules specified in the security group of the Firewall Manager
-- policy that partially match the @ViolationTarget@ rule.
newAwsVPCSecurityGroupViolation ::
  AwsVPCSecurityGroupViolation
newAwsVPCSecurityGroupViolation =
  AwsVPCSecurityGroupViolation'
    { violationTargetDescription =
        Prelude.Nothing,
      possibleSecurityGroupRemediationActions =
        Prelude.Nothing,
      violationTarget = Prelude.Nothing,
      partialMatches = Prelude.Nothing
    }

-- | A description of the security group that violates the policy.
awsVPCSecurityGroupViolation_violationTargetDescription :: Lens.Lens' AwsVPCSecurityGroupViolation (Prelude.Maybe Prelude.Text)
awsVPCSecurityGroupViolation_violationTargetDescription = Lens.lens (\AwsVPCSecurityGroupViolation' {violationTargetDescription} -> violationTargetDescription) (\s@AwsVPCSecurityGroupViolation' {} a -> s {violationTargetDescription = a} :: AwsVPCSecurityGroupViolation)

-- | Remediation options for the rule specified in the @ViolationTarget@.
awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions :: Lens.Lens' AwsVPCSecurityGroupViolation (Prelude.Maybe [SecurityGroupRemediationAction])
awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions = Lens.lens (\AwsVPCSecurityGroupViolation' {possibleSecurityGroupRemediationActions} -> possibleSecurityGroupRemediationActions) (\s@AwsVPCSecurityGroupViolation' {} a -> s {possibleSecurityGroupRemediationActions = a} :: AwsVPCSecurityGroupViolation) Prelude.. Lens.mapping Lens.coerced

-- | The security group rule that is being evaluated.
awsVPCSecurityGroupViolation_violationTarget :: Lens.Lens' AwsVPCSecurityGroupViolation (Prelude.Maybe Prelude.Text)
awsVPCSecurityGroupViolation_violationTarget = Lens.lens (\AwsVPCSecurityGroupViolation' {violationTarget} -> violationTarget) (\s@AwsVPCSecurityGroupViolation' {} a -> s {violationTarget = a} :: AwsVPCSecurityGroupViolation)

-- | List of rules specified in the security group of the Firewall Manager
-- policy that partially match the @ViolationTarget@ rule.
awsVPCSecurityGroupViolation_partialMatches :: Lens.Lens' AwsVPCSecurityGroupViolation (Prelude.Maybe [PartialMatch])
awsVPCSecurityGroupViolation_partialMatches = Lens.lens (\AwsVPCSecurityGroupViolation' {partialMatches} -> partialMatches) (\s@AwsVPCSecurityGroupViolation' {} a -> s {partialMatches = a} :: AwsVPCSecurityGroupViolation) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsVPCSecurityGroupViolation where
  parseJSON =
    Core.withObject
      "AwsVPCSecurityGroupViolation"
      ( \x ->
          AwsVPCSecurityGroupViolation'
            Prelude.<$> (x Core..:? "ViolationTargetDescription")
            Prelude.<*> ( x Core..:? "PossibleSecurityGroupRemediationActions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ViolationTarget")
            Prelude.<*> ( x Core..:? "PartialMatches"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AwsVPCSecurityGroupViolation
  where
  hashWithSalt _salt AwsVPCSecurityGroupViolation' {..} =
    _salt
      `Prelude.hashWithSalt` violationTargetDescription
      `Prelude.hashWithSalt` possibleSecurityGroupRemediationActions
      `Prelude.hashWithSalt` violationTarget
      `Prelude.hashWithSalt` partialMatches

instance Prelude.NFData AwsVPCSecurityGroupViolation where
  rnf AwsVPCSecurityGroupViolation' {..} =
    Prelude.rnf violationTargetDescription
      `Prelude.seq` Prelude.rnf possibleSecurityGroupRemediationActions
      `Prelude.seq` Prelude.rnf violationTarget
      `Prelude.seq` Prelude.rnf partialMatches
