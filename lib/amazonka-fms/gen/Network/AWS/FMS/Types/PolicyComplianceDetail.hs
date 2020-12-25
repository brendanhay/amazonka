{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.PolicyComplianceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.PolicyComplianceDetail
  ( PolicyComplianceDetail (..),

    -- * Smart constructor
    mkPolicyComplianceDetail,

    -- * Lenses
    pcdEvaluationLimitExceeded,
    pcdExpiredAt,
    pcdIssueInfoMap,
    pcdMemberAccount,
    pcdPolicyId,
    pcdPolicyOwner,
    pcdViolators,
  )
where

import qualified Network.AWS.FMS.Types.ComplianceViolator as Types
import qualified Network.AWS.FMS.Types.DependentServiceName as Types
import qualified Network.AWS.FMS.Types.DetailedInfo as Types
import qualified Network.AWS.FMS.Types.MemberAccount as Types
import qualified Network.AWS.FMS.Types.PolicyId as Types
import qualified Network.AWS.FMS.Types.PolicyOwner as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the noncompliant resources in a member account for a specific AWS Firewall Manager policy. A maximum of 100 entries are displayed. If more than 100 resources are noncompliant, @EvaluationLimitExceeded@ is set to @True@ .
--
-- /See:/ 'mkPolicyComplianceDetail' smart constructor.
data PolicyComplianceDetail = PolicyComplianceDetail'
  { -- | Indicates if over 100 resources are noncompliant with the AWS Firewall Manager policy.
    evaluationLimitExceeded :: Core.Maybe Core.Bool,
    -- | A timestamp that indicates when the returned information should be considered out of date.
    expiredAt :: Core.Maybe Core.NominalDiffTime,
    -- | Details about problems with dependent services, such as AWS WAF or AWS Config, that are causing a resource to be noncompliant. The details include the name of the dependent service and the error message received that indicates the problem with the service.
    issueInfoMap :: Core.Maybe (Core.HashMap Types.DependentServiceName Types.DetailedInfo),
    -- | The AWS account ID.
    memberAccount :: Core.Maybe Types.MemberAccount,
    -- | The ID of the AWS Firewall Manager policy.
    policyId :: Core.Maybe Types.PolicyId,
    -- | The AWS account that created the AWS Firewall Manager policy.
    policyOwner :: Core.Maybe Types.PolicyOwner,
    -- | An array of resources that aren't protected by the AWS WAF or Shield Advanced policy or that aren't in compliance with the security group policy.
    violators :: Core.Maybe [Types.ComplianceViolator]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PolicyComplianceDetail' value with any optional fields omitted.
mkPolicyComplianceDetail ::
  PolicyComplianceDetail
mkPolicyComplianceDetail =
  PolicyComplianceDetail'
    { evaluationLimitExceeded = Core.Nothing,
      expiredAt = Core.Nothing,
      issueInfoMap = Core.Nothing,
      memberAccount = Core.Nothing,
      policyId = Core.Nothing,
      policyOwner = Core.Nothing,
      violators = Core.Nothing
    }

-- | Indicates if over 100 resources are noncompliant with the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'evaluationLimitExceeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdEvaluationLimitExceeded :: Lens.Lens' PolicyComplianceDetail (Core.Maybe Core.Bool)
pcdEvaluationLimitExceeded = Lens.field @"evaluationLimitExceeded"
{-# DEPRECATED pcdEvaluationLimitExceeded "Use generic-lens or generic-optics with 'evaluationLimitExceeded' instead." #-}

-- | A timestamp that indicates when the returned information should be considered out of date.
--
-- /Note:/ Consider using 'expiredAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdExpiredAt :: Lens.Lens' PolicyComplianceDetail (Core.Maybe Core.NominalDiffTime)
pcdExpiredAt = Lens.field @"expiredAt"
{-# DEPRECATED pcdExpiredAt "Use generic-lens or generic-optics with 'expiredAt' instead." #-}

-- | Details about problems with dependent services, such as AWS WAF or AWS Config, that are causing a resource to be noncompliant. The details include the name of the dependent service and the error message received that indicates the problem with the service.
--
-- /Note:/ Consider using 'issueInfoMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdIssueInfoMap :: Lens.Lens' PolicyComplianceDetail (Core.Maybe (Core.HashMap Types.DependentServiceName Types.DetailedInfo))
pcdIssueInfoMap = Lens.field @"issueInfoMap"
{-# DEPRECATED pcdIssueInfoMap "Use generic-lens or generic-optics with 'issueInfoMap' instead." #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'memberAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdMemberAccount :: Lens.Lens' PolicyComplianceDetail (Core.Maybe Types.MemberAccount)
pcdMemberAccount = Lens.field @"memberAccount"
{-# DEPRECATED pcdMemberAccount "Use generic-lens or generic-optics with 'memberAccount' instead." #-}

-- | The ID of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdPolicyId :: Lens.Lens' PolicyComplianceDetail (Core.Maybe Types.PolicyId)
pcdPolicyId = Lens.field @"policyId"
{-# DEPRECATED pcdPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | The AWS account that created the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policyOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdPolicyOwner :: Lens.Lens' PolicyComplianceDetail (Core.Maybe Types.PolicyOwner)
pcdPolicyOwner = Lens.field @"policyOwner"
{-# DEPRECATED pcdPolicyOwner "Use generic-lens or generic-optics with 'policyOwner' instead." #-}

-- | An array of resources that aren't protected by the AWS WAF or Shield Advanced policy or that aren't in compliance with the security group policy.
--
-- /Note:/ Consider using 'violators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdViolators :: Lens.Lens' PolicyComplianceDetail (Core.Maybe [Types.ComplianceViolator])
pcdViolators = Lens.field @"violators"
{-# DEPRECATED pcdViolators "Use generic-lens or generic-optics with 'violators' instead." #-}

instance Core.FromJSON PolicyComplianceDetail where
  parseJSON =
    Core.withObject "PolicyComplianceDetail" Core.$
      \x ->
        PolicyComplianceDetail'
          Core.<$> (x Core..:? "EvaluationLimitExceeded")
          Core.<*> (x Core..:? "ExpiredAt")
          Core.<*> (x Core..:? "IssueInfoMap")
          Core.<*> (x Core..:? "MemberAccount")
          Core.<*> (x Core..:? "PolicyId")
          Core.<*> (x Core..:? "PolicyOwner")
          Core.<*> (x Core..:? "Violators")
