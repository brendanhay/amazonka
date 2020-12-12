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
    pcdExpiredAt,
    pcdPolicyId,
    pcdViolators,
    pcdEvaluationLimitExceeded,
    pcdIssueInfoMap,
    pcdPolicyOwner,
    pcdMemberAccount,
  )
where

import Network.AWS.FMS.Types.ComplianceViolator
import Network.AWS.FMS.Types.DependentServiceName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the noncompliant resources in a member account for a specific AWS Firewall Manager policy. A maximum of 100 entries are displayed. If more than 100 resources are noncompliant, @EvaluationLimitExceeded@ is set to @True@ .
--
-- /See:/ 'mkPolicyComplianceDetail' smart constructor.
data PolicyComplianceDetail = PolicyComplianceDetail'
  { expiredAt ::
      Lude.Maybe Lude.Timestamp,
    policyId :: Lude.Maybe Lude.Text,
    violators :: Lude.Maybe [ComplianceViolator],
    evaluationLimitExceeded ::
      Lude.Maybe Lude.Bool,
    issueInfoMap ::
      Lude.Maybe
        ( Lude.HashMap
            DependentServiceName
            (Lude.Text)
        ),
    policyOwner :: Lude.Maybe Lude.Text,
    memberAccount :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyComplianceDetail' with the minimum fields required to make a request.
--
-- * 'evaluationLimitExceeded' - Indicates if over 100 resources are noncompliant with the AWS Firewall Manager policy.
-- * 'expiredAt' - A timestamp that indicates when the returned information should be considered out of date.
-- * 'issueInfoMap' - Details about problems with dependent services, such as AWS WAF or AWS Config, that are causing a resource to be noncompliant. The details include the name of the dependent service and the error message received that indicates the problem with the service.
-- * 'memberAccount' - The AWS account ID.
-- * 'policyId' - The ID of the AWS Firewall Manager policy.
-- * 'policyOwner' - The AWS account that created the AWS Firewall Manager policy.
-- * 'violators' - An array of resources that aren't protected by the AWS WAF or Shield Advanced policy or that aren't in compliance with the security group policy.
mkPolicyComplianceDetail ::
  PolicyComplianceDetail
mkPolicyComplianceDetail =
  PolicyComplianceDetail'
    { expiredAt = Lude.Nothing,
      policyId = Lude.Nothing,
      violators = Lude.Nothing,
      evaluationLimitExceeded = Lude.Nothing,
      issueInfoMap = Lude.Nothing,
      policyOwner = Lude.Nothing,
      memberAccount = Lude.Nothing
    }

-- | A timestamp that indicates when the returned information should be considered out of date.
--
-- /Note:/ Consider using 'expiredAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdExpiredAt :: Lens.Lens' PolicyComplianceDetail (Lude.Maybe Lude.Timestamp)
pcdExpiredAt = Lens.lens (expiredAt :: PolicyComplianceDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {expiredAt = a} :: PolicyComplianceDetail)
{-# DEPRECATED pcdExpiredAt "Use generic-lens or generic-optics with 'expiredAt' instead." #-}

-- | The ID of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdPolicyId :: Lens.Lens' PolicyComplianceDetail (Lude.Maybe Lude.Text)
pcdPolicyId = Lens.lens (policyId :: PolicyComplianceDetail -> Lude.Maybe Lude.Text) (\s a -> s {policyId = a} :: PolicyComplianceDetail)
{-# DEPRECATED pcdPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | An array of resources that aren't protected by the AWS WAF or Shield Advanced policy or that aren't in compliance with the security group policy.
--
-- /Note:/ Consider using 'violators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdViolators :: Lens.Lens' PolicyComplianceDetail (Lude.Maybe [ComplianceViolator])
pcdViolators = Lens.lens (violators :: PolicyComplianceDetail -> Lude.Maybe [ComplianceViolator]) (\s a -> s {violators = a} :: PolicyComplianceDetail)
{-# DEPRECATED pcdViolators "Use generic-lens or generic-optics with 'violators' instead." #-}

-- | Indicates if over 100 resources are noncompliant with the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'evaluationLimitExceeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdEvaluationLimitExceeded :: Lens.Lens' PolicyComplianceDetail (Lude.Maybe Lude.Bool)
pcdEvaluationLimitExceeded = Lens.lens (evaluationLimitExceeded :: PolicyComplianceDetail -> Lude.Maybe Lude.Bool) (\s a -> s {evaluationLimitExceeded = a} :: PolicyComplianceDetail)
{-# DEPRECATED pcdEvaluationLimitExceeded "Use generic-lens or generic-optics with 'evaluationLimitExceeded' instead." #-}

-- | Details about problems with dependent services, such as AWS WAF or AWS Config, that are causing a resource to be noncompliant. The details include the name of the dependent service and the error message received that indicates the problem with the service.
--
-- /Note:/ Consider using 'issueInfoMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdIssueInfoMap :: Lens.Lens' PolicyComplianceDetail (Lude.Maybe (Lude.HashMap DependentServiceName (Lude.Text)))
pcdIssueInfoMap = Lens.lens (issueInfoMap :: PolicyComplianceDetail -> Lude.Maybe (Lude.HashMap DependentServiceName (Lude.Text))) (\s a -> s {issueInfoMap = a} :: PolicyComplianceDetail)
{-# DEPRECATED pcdIssueInfoMap "Use generic-lens or generic-optics with 'issueInfoMap' instead." #-}

-- | The AWS account that created the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policyOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdPolicyOwner :: Lens.Lens' PolicyComplianceDetail (Lude.Maybe Lude.Text)
pcdPolicyOwner = Lens.lens (policyOwner :: PolicyComplianceDetail -> Lude.Maybe Lude.Text) (\s a -> s {policyOwner = a} :: PolicyComplianceDetail)
{-# DEPRECATED pcdPolicyOwner "Use generic-lens or generic-optics with 'policyOwner' instead." #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'memberAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdMemberAccount :: Lens.Lens' PolicyComplianceDetail (Lude.Maybe Lude.Text)
pcdMemberAccount = Lens.lens (memberAccount :: PolicyComplianceDetail -> Lude.Maybe Lude.Text) (\s a -> s {memberAccount = a} :: PolicyComplianceDetail)
{-# DEPRECATED pcdMemberAccount "Use generic-lens or generic-optics with 'memberAccount' instead." #-}

instance Lude.FromJSON PolicyComplianceDetail where
  parseJSON =
    Lude.withObject
      "PolicyComplianceDetail"
      ( \x ->
          PolicyComplianceDetail'
            Lude.<$> (x Lude..:? "ExpiredAt")
            Lude.<*> (x Lude..:? "PolicyId")
            Lude.<*> (x Lude..:? "Violators" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EvaluationLimitExceeded")
            Lude.<*> (x Lude..:? "IssueInfoMap" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "PolicyOwner")
            Lude.<*> (x Lude..:? "MemberAccount")
      )
