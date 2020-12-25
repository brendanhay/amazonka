{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.PolicyComplianceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.PolicyComplianceStatus
  ( PolicyComplianceStatus (..),

    -- * Smart constructor
    mkPolicyComplianceStatus,

    -- * Lenses
    pcsEvaluationResults,
    pcsIssueInfoMap,
    pcsLastUpdated,
    pcsMemberAccount,
    pcsPolicyId,
    pcsPolicyName,
    pcsPolicyOwner,
  )
where

import qualified Network.AWS.FMS.Types.AWSAccountId as Types
import qualified Network.AWS.FMS.Types.DependentServiceName as Types
import qualified Network.AWS.FMS.Types.DetailedInfo as Types
import qualified Network.AWS.FMS.Types.EvaluationResult as Types
import qualified Network.AWS.FMS.Types.PolicyId as Types
import qualified Network.AWS.FMS.Types.ResourceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether the account is compliant with the specified policy. An account is considered noncompliant if it includes resources that are not protected by the policy, for AWS WAF and Shield Advanced policies, or that are noncompliant with the policy, for security group policies.
--
-- /See:/ 'mkPolicyComplianceStatus' smart constructor.
data PolicyComplianceStatus = PolicyComplianceStatus'
  { -- | An array of @EvaluationResult@ objects.
    evaluationResults :: Core.Maybe [Types.EvaluationResult],
    -- | Details about problems with dependent services, such as AWS WAF or AWS Config, that are causing a resource to be noncompliant. The details include the name of the dependent service and the error message received that indicates the problem with the service.
    issueInfoMap :: Core.Maybe (Core.HashMap Types.DependentServiceName Types.DetailedInfo),
    -- | Timestamp of the last update to the @EvaluationResult@ objects.
    lastUpdated :: Core.Maybe Core.NominalDiffTime,
    -- | The member account ID.
    memberAccount :: Core.Maybe Types.AWSAccountId,
    -- | The ID of the AWS Firewall Manager policy.
    policyId :: Core.Maybe Types.PolicyId,
    -- | The name of the AWS Firewall Manager policy.
    policyName :: Core.Maybe Types.ResourceName,
    -- | The AWS account that created the AWS Firewall Manager policy.
    policyOwner :: Core.Maybe Types.AWSAccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PolicyComplianceStatus' value with any optional fields omitted.
mkPolicyComplianceStatus ::
  PolicyComplianceStatus
mkPolicyComplianceStatus =
  PolicyComplianceStatus'
    { evaluationResults = Core.Nothing,
      issueInfoMap = Core.Nothing,
      lastUpdated = Core.Nothing,
      memberAccount = Core.Nothing,
      policyId = Core.Nothing,
      policyName = Core.Nothing,
      policyOwner = Core.Nothing
    }

-- | An array of @EvaluationResult@ objects.
--
-- /Note:/ Consider using 'evaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsEvaluationResults :: Lens.Lens' PolicyComplianceStatus (Core.Maybe [Types.EvaluationResult])
pcsEvaluationResults = Lens.field @"evaluationResults"
{-# DEPRECATED pcsEvaluationResults "Use generic-lens or generic-optics with 'evaluationResults' instead." #-}

-- | Details about problems with dependent services, such as AWS WAF or AWS Config, that are causing a resource to be noncompliant. The details include the name of the dependent service and the error message received that indicates the problem with the service.
--
-- /Note:/ Consider using 'issueInfoMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsIssueInfoMap :: Lens.Lens' PolicyComplianceStatus (Core.Maybe (Core.HashMap Types.DependentServiceName Types.DetailedInfo))
pcsIssueInfoMap = Lens.field @"issueInfoMap"
{-# DEPRECATED pcsIssueInfoMap "Use generic-lens or generic-optics with 'issueInfoMap' instead." #-}

-- | Timestamp of the last update to the @EvaluationResult@ objects.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsLastUpdated :: Lens.Lens' PolicyComplianceStatus (Core.Maybe Core.NominalDiffTime)
pcsLastUpdated = Lens.field @"lastUpdated"
{-# DEPRECATED pcsLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | The member account ID.
--
-- /Note:/ Consider using 'memberAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsMemberAccount :: Lens.Lens' PolicyComplianceStatus (Core.Maybe Types.AWSAccountId)
pcsMemberAccount = Lens.field @"memberAccount"
{-# DEPRECATED pcsMemberAccount "Use generic-lens or generic-optics with 'memberAccount' instead." #-}

-- | The ID of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsPolicyId :: Lens.Lens' PolicyComplianceStatus (Core.Maybe Types.PolicyId)
pcsPolicyId = Lens.field @"policyId"
{-# DEPRECATED pcsPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | The name of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsPolicyName :: Lens.Lens' PolicyComplianceStatus (Core.Maybe Types.ResourceName)
pcsPolicyName = Lens.field @"policyName"
{-# DEPRECATED pcsPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The AWS account that created the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policyOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsPolicyOwner :: Lens.Lens' PolicyComplianceStatus (Core.Maybe Types.AWSAccountId)
pcsPolicyOwner = Lens.field @"policyOwner"
{-# DEPRECATED pcsPolicyOwner "Use generic-lens or generic-optics with 'policyOwner' instead." #-}

instance Core.FromJSON PolicyComplianceStatus where
  parseJSON =
    Core.withObject "PolicyComplianceStatus" Core.$
      \x ->
        PolicyComplianceStatus'
          Core.<$> (x Core..:? "EvaluationResults")
          Core.<*> (x Core..:? "IssueInfoMap")
          Core.<*> (x Core..:? "LastUpdated")
          Core.<*> (x Core..:? "MemberAccount")
          Core.<*> (x Core..:? "PolicyId")
          Core.<*> (x Core..:? "PolicyName")
          Core.<*> (x Core..:? "PolicyOwner")
