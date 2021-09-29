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
-- Module      : Network.AWS.FMS.Types.AwsVPCSecurityGroupViolation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AwsVPCSecurityGroupViolation where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.PartialMatch
import Network.AWS.FMS.Types.SecurityGroupRemediationAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Violation detail for the rule violation in a security group when
-- compared to the primary security group of the Firewall Manager policy.
--
-- /See:/ 'newAwsVPCSecurityGroupViolation' smart constructor.
data AwsVPCSecurityGroupViolation = AwsVPCSecurityGroupViolation'
  { -- | Remediation options for the rule specified in the @ViolationTarget@.
    possibleSecurityGroupRemediationActions :: Prelude.Maybe [SecurityGroupRemediationAction],
    -- | List of rules specified in the security group of the Firewall Manager
    -- policy that partially match the @ViolationTarget@ rule.
    partialMatches :: Prelude.Maybe [PartialMatch],
    -- | The security group rule that is being evaluated.
    violationTarget :: Prelude.Maybe Prelude.Text,
    -- | A description of the security group that violates the policy.
    violationTargetDescription :: Prelude.Maybe Prelude.Text
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
-- 'possibleSecurityGroupRemediationActions', 'awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions' - Remediation options for the rule specified in the @ViolationTarget@.
--
-- 'partialMatches', 'awsVPCSecurityGroupViolation_partialMatches' - List of rules specified in the security group of the Firewall Manager
-- policy that partially match the @ViolationTarget@ rule.
--
-- 'violationTarget', 'awsVPCSecurityGroupViolation_violationTarget' - The security group rule that is being evaluated.
--
-- 'violationTargetDescription', 'awsVPCSecurityGroupViolation_violationTargetDescription' - A description of the security group that violates the policy.
newAwsVPCSecurityGroupViolation ::
  AwsVPCSecurityGroupViolation
newAwsVPCSecurityGroupViolation =
  AwsVPCSecurityGroupViolation'
    { possibleSecurityGroupRemediationActions =
        Prelude.Nothing,
      partialMatches = Prelude.Nothing,
      violationTarget = Prelude.Nothing,
      violationTargetDescription = Prelude.Nothing
    }

-- | Remediation options for the rule specified in the @ViolationTarget@.
awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions :: Lens.Lens' AwsVPCSecurityGroupViolation (Prelude.Maybe [SecurityGroupRemediationAction])
awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions = Lens.lens (\AwsVPCSecurityGroupViolation' {possibleSecurityGroupRemediationActions} -> possibleSecurityGroupRemediationActions) (\s@AwsVPCSecurityGroupViolation' {} a -> s {possibleSecurityGroupRemediationActions = a} :: AwsVPCSecurityGroupViolation) Prelude.. Lens.mapping Lens._Coerce

-- | List of rules specified in the security group of the Firewall Manager
-- policy that partially match the @ViolationTarget@ rule.
awsVPCSecurityGroupViolation_partialMatches :: Lens.Lens' AwsVPCSecurityGroupViolation (Prelude.Maybe [PartialMatch])
awsVPCSecurityGroupViolation_partialMatches = Lens.lens (\AwsVPCSecurityGroupViolation' {partialMatches} -> partialMatches) (\s@AwsVPCSecurityGroupViolation' {} a -> s {partialMatches = a} :: AwsVPCSecurityGroupViolation) Prelude.. Lens.mapping Lens._Coerce

-- | The security group rule that is being evaluated.
awsVPCSecurityGroupViolation_violationTarget :: Lens.Lens' AwsVPCSecurityGroupViolation (Prelude.Maybe Prelude.Text)
awsVPCSecurityGroupViolation_violationTarget = Lens.lens (\AwsVPCSecurityGroupViolation' {violationTarget} -> violationTarget) (\s@AwsVPCSecurityGroupViolation' {} a -> s {violationTarget = a} :: AwsVPCSecurityGroupViolation)

-- | A description of the security group that violates the policy.
awsVPCSecurityGroupViolation_violationTargetDescription :: Lens.Lens' AwsVPCSecurityGroupViolation (Prelude.Maybe Prelude.Text)
awsVPCSecurityGroupViolation_violationTargetDescription = Lens.lens (\AwsVPCSecurityGroupViolation' {violationTargetDescription} -> violationTargetDescription) (\s@AwsVPCSecurityGroupViolation' {} a -> s {violationTargetDescription = a} :: AwsVPCSecurityGroupViolation)

instance Core.FromJSON AwsVPCSecurityGroupViolation where
  parseJSON =
    Core.withObject
      "AwsVPCSecurityGroupViolation"
      ( \x ->
          AwsVPCSecurityGroupViolation'
            Prelude.<$> ( x Core..:? "PossibleSecurityGroupRemediationActions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "PartialMatches" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ViolationTarget")
            Prelude.<*> (x Core..:? "ViolationTargetDescription")
      )

instance
  Prelude.Hashable
    AwsVPCSecurityGroupViolation

instance Prelude.NFData AwsVPCSecurityGroupViolation
