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
-- Module      : Network.AWS.FMS.Types.PolicyComplianceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.PolicyComplianceStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.DependentServiceName
import Network.AWS.FMS.Types.EvaluationResult
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Indicates whether the account is compliant with the specified policy. An
-- account is considered noncompliant if it includes resources that are not
-- protected by the policy, for AWS WAF and Shield Advanced policies, or
-- that are noncompliant with the policy, for security group policies.
--
-- /See:/ 'newPolicyComplianceStatus' smart constructor.
data PolicyComplianceStatus = PolicyComplianceStatus'
  { -- | The name of the AWS Firewall Manager policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The AWS account that created the AWS Firewall Manager policy.
    policyOwner :: Prelude.Maybe Prelude.Text,
    -- | The member account ID.
    memberAccount :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of the last update to the @EvaluationResult@ objects.
    lastUpdated :: Prelude.Maybe Core.POSIX,
    -- | An array of @EvaluationResult@ objects.
    evaluationResults :: Prelude.Maybe [EvaluationResult],
    -- | Details about problems with dependent services, such as AWS WAF or AWS
    -- Config, that are causing a resource to be noncompliant. The details
    -- include the name of the dependent service and the error message received
    -- that indicates the problem with the service.
    issueInfoMap :: Prelude.Maybe (Prelude.HashMap DependentServiceName Prelude.Text),
    -- | The ID of the AWS Firewall Manager policy.
    policyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyComplianceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'policyComplianceStatus_policyName' - The name of the AWS Firewall Manager policy.
--
-- 'policyOwner', 'policyComplianceStatus_policyOwner' - The AWS account that created the AWS Firewall Manager policy.
--
-- 'memberAccount', 'policyComplianceStatus_memberAccount' - The member account ID.
--
-- 'lastUpdated', 'policyComplianceStatus_lastUpdated' - Timestamp of the last update to the @EvaluationResult@ objects.
--
-- 'evaluationResults', 'policyComplianceStatus_evaluationResults' - An array of @EvaluationResult@ objects.
--
-- 'issueInfoMap', 'policyComplianceStatus_issueInfoMap' - Details about problems with dependent services, such as AWS WAF or AWS
-- Config, that are causing a resource to be noncompliant. The details
-- include the name of the dependent service and the error message received
-- that indicates the problem with the service.
--
-- 'policyId', 'policyComplianceStatus_policyId' - The ID of the AWS Firewall Manager policy.
newPolicyComplianceStatus ::
  PolicyComplianceStatus
newPolicyComplianceStatus =
  PolicyComplianceStatus'
    { policyName =
        Prelude.Nothing,
      policyOwner = Prelude.Nothing,
      memberAccount = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      evaluationResults = Prelude.Nothing,
      issueInfoMap = Prelude.Nothing,
      policyId = Prelude.Nothing
    }

-- | The name of the AWS Firewall Manager policy.
policyComplianceStatus_policyName :: Lens.Lens' PolicyComplianceStatus (Prelude.Maybe Prelude.Text)
policyComplianceStatus_policyName = Lens.lens (\PolicyComplianceStatus' {policyName} -> policyName) (\s@PolicyComplianceStatus' {} a -> s {policyName = a} :: PolicyComplianceStatus)

-- | The AWS account that created the AWS Firewall Manager policy.
policyComplianceStatus_policyOwner :: Lens.Lens' PolicyComplianceStatus (Prelude.Maybe Prelude.Text)
policyComplianceStatus_policyOwner = Lens.lens (\PolicyComplianceStatus' {policyOwner} -> policyOwner) (\s@PolicyComplianceStatus' {} a -> s {policyOwner = a} :: PolicyComplianceStatus)

-- | The member account ID.
policyComplianceStatus_memberAccount :: Lens.Lens' PolicyComplianceStatus (Prelude.Maybe Prelude.Text)
policyComplianceStatus_memberAccount = Lens.lens (\PolicyComplianceStatus' {memberAccount} -> memberAccount) (\s@PolicyComplianceStatus' {} a -> s {memberAccount = a} :: PolicyComplianceStatus)

-- | Timestamp of the last update to the @EvaluationResult@ objects.
policyComplianceStatus_lastUpdated :: Lens.Lens' PolicyComplianceStatus (Prelude.Maybe Prelude.UTCTime)
policyComplianceStatus_lastUpdated = Lens.lens (\PolicyComplianceStatus' {lastUpdated} -> lastUpdated) (\s@PolicyComplianceStatus' {} a -> s {lastUpdated = a} :: PolicyComplianceStatus) Prelude.. Lens.mapping Core._Time

-- | An array of @EvaluationResult@ objects.
policyComplianceStatus_evaluationResults :: Lens.Lens' PolicyComplianceStatus (Prelude.Maybe [EvaluationResult])
policyComplianceStatus_evaluationResults = Lens.lens (\PolicyComplianceStatus' {evaluationResults} -> evaluationResults) (\s@PolicyComplianceStatus' {} a -> s {evaluationResults = a} :: PolicyComplianceStatus) Prelude.. Lens.mapping Lens._Coerce

-- | Details about problems with dependent services, such as AWS WAF or AWS
-- Config, that are causing a resource to be noncompliant. The details
-- include the name of the dependent service and the error message received
-- that indicates the problem with the service.
policyComplianceStatus_issueInfoMap :: Lens.Lens' PolicyComplianceStatus (Prelude.Maybe (Prelude.HashMap DependentServiceName Prelude.Text))
policyComplianceStatus_issueInfoMap = Lens.lens (\PolicyComplianceStatus' {issueInfoMap} -> issueInfoMap) (\s@PolicyComplianceStatus' {} a -> s {issueInfoMap = a} :: PolicyComplianceStatus) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the AWS Firewall Manager policy.
policyComplianceStatus_policyId :: Lens.Lens' PolicyComplianceStatus (Prelude.Maybe Prelude.Text)
policyComplianceStatus_policyId = Lens.lens (\PolicyComplianceStatus' {policyId} -> policyId) (\s@PolicyComplianceStatus' {} a -> s {policyId = a} :: PolicyComplianceStatus)

instance Core.FromJSON PolicyComplianceStatus where
  parseJSON =
    Core.withObject
      "PolicyComplianceStatus"
      ( \x ->
          PolicyComplianceStatus'
            Prelude.<$> (x Core..:? "PolicyName")
            Prelude.<*> (x Core..:? "PolicyOwner")
            Prelude.<*> (x Core..:? "MemberAccount")
            Prelude.<*> (x Core..:? "LastUpdated")
            Prelude.<*> ( x Core..:? "EvaluationResults"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "IssueInfoMap" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "PolicyId")
      )

instance Prelude.Hashable PolicyComplianceStatus

instance Prelude.NFData PolicyComplianceStatus
