{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.FMS.Types.PartialMatch
import Network.AWS.FMS.Types.SecurityGroupRemediationAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details of the rule violation in a security group when compared to the
-- master security group of the AWS Firewall Manager policy.
--
-- /See:/ 'newAwsVPCSecurityGroupViolation' smart constructor.
data AwsVPCSecurityGroupViolation = AwsVPCSecurityGroupViolation'
  { -- | List of rules specified in the security group of the AWS Firewall
    -- Manager policy that partially match the @ViolationTarget@ rule.
    partialMatches :: Prelude.Maybe [PartialMatch],
    -- | Remediation options for the rule specified in the @ViolationTarget@.
    possibleSecurityGroupRemediationActions :: Prelude.Maybe [SecurityGroupRemediationAction],
    -- | The security group rule that is being evaluated.
    violationTarget :: Prelude.Maybe Prelude.Text,
    -- | A description of the security group that violates the policy.
    violationTargetDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      possibleSecurityGroupRemediationActions =
        Prelude.Nothing,
      violationTarget = Prelude.Nothing,
      violationTargetDescription = Prelude.Nothing
    }

-- | List of rules specified in the security group of the AWS Firewall
-- Manager policy that partially match the @ViolationTarget@ rule.
awsVPCSecurityGroupViolation_partialMatches :: Lens.Lens' AwsVPCSecurityGroupViolation (Prelude.Maybe [PartialMatch])
awsVPCSecurityGroupViolation_partialMatches = Lens.lens (\AwsVPCSecurityGroupViolation' {partialMatches} -> partialMatches) (\s@AwsVPCSecurityGroupViolation' {} a -> s {partialMatches = a} :: AwsVPCSecurityGroupViolation) Prelude.. Lens.mapping Prelude._Coerce

-- | Remediation options for the rule specified in the @ViolationTarget@.
awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions :: Lens.Lens' AwsVPCSecurityGroupViolation (Prelude.Maybe [SecurityGroupRemediationAction])
awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions = Lens.lens (\AwsVPCSecurityGroupViolation' {possibleSecurityGroupRemediationActions} -> possibleSecurityGroupRemediationActions) (\s@AwsVPCSecurityGroupViolation' {} a -> s {possibleSecurityGroupRemediationActions = a} :: AwsVPCSecurityGroupViolation) Prelude.. Lens.mapping Prelude._Coerce

-- | The security group rule that is being evaluated.
awsVPCSecurityGroupViolation_violationTarget :: Lens.Lens' AwsVPCSecurityGroupViolation (Prelude.Maybe Prelude.Text)
awsVPCSecurityGroupViolation_violationTarget = Lens.lens (\AwsVPCSecurityGroupViolation' {violationTarget} -> violationTarget) (\s@AwsVPCSecurityGroupViolation' {} a -> s {violationTarget = a} :: AwsVPCSecurityGroupViolation)

-- | A description of the security group that violates the policy.
awsVPCSecurityGroupViolation_violationTargetDescription :: Lens.Lens' AwsVPCSecurityGroupViolation (Prelude.Maybe Prelude.Text)
awsVPCSecurityGroupViolation_violationTargetDescription = Lens.lens (\AwsVPCSecurityGroupViolation' {violationTargetDescription} -> violationTargetDescription) (\s@AwsVPCSecurityGroupViolation' {} a -> s {violationTargetDescription = a} :: AwsVPCSecurityGroupViolation)

instance
  Prelude.FromJSON
    AwsVPCSecurityGroupViolation
  where
  parseJSON =
    Prelude.withObject
      "AwsVPCSecurityGroupViolation"
      ( \x ->
          AwsVPCSecurityGroupViolation'
            Prelude.<$> ( x Prelude..:? "PartialMatches"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Prelude..:? "PossibleSecurityGroupRemediationActions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ViolationTarget")
            Prelude.<*> (x Prelude..:? "ViolationTargetDescription")
      )

instance
  Prelude.Hashable
    AwsVPCSecurityGroupViolation

instance Prelude.NFData AwsVPCSecurityGroupViolation
