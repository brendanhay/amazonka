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

-- | Details of the rule violation in a security group when compared to the
-- master security group of the AWS Firewall Manager policy.
--
-- /See:/ 'newAwsVPCSecurityGroupViolation' smart constructor.
data AwsVPCSecurityGroupViolation = AwsVPCSecurityGroupViolation'
  { -- | List of rules specified in the security group of the AWS Firewall
    -- Manager policy that partially match the @ViolationTarget@ rule.
    partialMatches :: Core.Maybe [PartialMatch],
    -- | Remediation options for the rule specified in the @ViolationTarget@.
    possibleSecurityGroupRemediationActions :: Core.Maybe [SecurityGroupRemediationAction],
    -- | The security group rule that is being evaluated.
    violationTarget :: Core.Maybe Core.Text,
    -- | A description of the security group that violates the policy.
    violationTargetDescription :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AwsVPCSecurityGroupViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partialMatches', 'awsVPCSecurityGroupViolation_partialMatches' - List of rules specified in the security group of the AWS Firewall
-- Manager policy that partially match the @ViolationTarget@ rule.
--
-- 'possibleSecurityGroupRemediationActions', 'awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions' - Remediation options for the rule specified in the @ViolationTarget@.
--
-- 'violationTarget', 'awsVPCSecurityGroupViolation_violationTarget' - The security group rule that is being evaluated.
--
-- 'violationTargetDescription', 'awsVPCSecurityGroupViolation_violationTargetDescription' - A description of the security group that violates the policy.
newAwsVPCSecurityGroupViolation ::
  AwsVPCSecurityGroupViolation
newAwsVPCSecurityGroupViolation =
  AwsVPCSecurityGroupViolation'
    { partialMatches =
        Core.Nothing,
      possibleSecurityGroupRemediationActions =
        Core.Nothing,
      violationTarget = Core.Nothing,
      violationTargetDescription = Core.Nothing
    }

-- | List of rules specified in the security group of the AWS Firewall
-- Manager policy that partially match the @ViolationTarget@ rule.
awsVPCSecurityGroupViolation_partialMatches :: Lens.Lens' AwsVPCSecurityGroupViolation (Core.Maybe [PartialMatch])
awsVPCSecurityGroupViolation_partialMatches = Lens.lens (\AwsVPCSecurityGroupViolation' {partialMatches} -> partialMatches) (\s@AwsVPCSecurityGroupViolation' {} a -> s {partialMatches = a} :: AwsVPCSecurityGroupViolation) Core.. Lens.mapping Lens._Coerce

-- | Remediation options for the rule specified in the @ViolationTarget@.
awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions :: Lens.Lens' AwsVPCSecurityGroupViolation (Core.Maybe [SecurityGroupRemediationAction])
awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions = Lens.lens (\AwsVPCSecurityGroupViolation' {possibleSecurityGroupRemediationActions} -> possibleSecurityGroupRemediationActions) (\s@AwsVPCSecurityGroupViolation' {} a -> s {possibleSecurityGroupRemediationActions = a} :: AwsVPCSecurityGroupViolation) Core.. Lens.mapping Lens._Coerce

-- | The security group rule that is being evaluated.
awsVPCSecurityGroupViolation_violationTarget :: Lens.Lens' AwsVPCSecurityGroupViolation (Core.Maybe Core.Text)
awsVPCSecurityGroupViolation_violationTarget = Lens.lens (\AwsVPCSecurityGroupViolation' {violationTarget} -> violationTarget) (\s@AwsVPCSecurityGroupViolation' {} a -> s {violationTarget = a} :: AwsVPCSecurityGroupViolation)

-- | A description of the security group that violates the policy.
awsVPCSecurityGroupViolation_violationTargetDescription :: Lens.Lens' AwsVPCSecurityGroupViolation (Core.Maybe Core.Text)
awsVPCSecurityGroupViolation_violationTargetDescription = Lens.lens (\AwsVPCSecurityGroupViolation' {violationTargetDescription} -> violationTargetDescription) (\s@AwsVPCSecurityGroupViolation' {} a -> s {violationTargetDescription = a} :: AwsVPCSecurityGroupViolation)

instance Core.FromJSON AwsVPCSecurityGroupViolation where
  parseJSON =
    Core.withObject
      "AwsVPCSecurityGroupViolation"
      ( \x ->
          AwsVPCSecurityGroupViolation'
            Core.<$> (x Core..:? "PartialMatches" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "PossibleSecurityGroupRemediationActions"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "ViolationTarget")
            Core.<*> (x Core..:? "ViolationTargetDescription")
      )

instance Core.Hashable AwsVPCSecurityGroupViolation

instance Core.NFData AwsVPCSecurityGroupViolation
