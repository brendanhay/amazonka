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
    pcsLastUpdated,
    pcsPolicyName,
    pcsPolicyId,
    pcsIssueInfoMap,
    pcsPolicyOwner,
    pcsMemberAccount,
  )
where

import Network.AWS.FMS.Types.DependentServiceName
import Network.AWS.FMS.Types.EvaluationResult
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether the account is compliant with the specified policy. An account is considered noncompliant if it includes resources that are not protected by the policy, for AWS WAF and Shield Advanced policies, or that are noncompliant with the policy, for security group policies.
--
-- /See:/ 'mkPolicyComplianceStatus' smart constructor.
data PolicyComplianceStatus = PolicyComplianceStatus'
  { evaluationResults ::
      Lude.Maybe [EvaluationResult],
    lastUpdated :: Lude.Maybe Lude.Timestamp,
    policyName :: Lude.Maybe Lude.Text,
    policyId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'PolicyComplianceStatus' with the minimum fields required to make a request.
--
-- * 'evaluationResults' - An array of @EvaluationResult@ objects.
-- * 'issueInfoMap' - Details about problems with dependent services, such as AWS WAF or AWS Config, that are causing a resource to be noncompliant. The details include the name of the dependent service and the error message received that indicates the problem with the service.
-- * 'lastUpdated' - Timestamp of the last update to the @EvaluationResult@ objects.
-- * 'memberAccount' - The member account ID.
-- * 'policyId' - The ID of the AWS Firewall Manager policy.
-- * 'policyName' - The name of the AWS Firewall Manager policy.
-- * 'policyOwner' - The AWS account that created the AWS Firewall Manager policy.
mkPolicyComplianceStatus ::
  PolicyComplianceStatus
mkPolicyComplianceStatus =
  PolicyComplianceStatus'
    { evaluationResults = Lude.Nothing,
      lastUpdated = Lude.Nothing,
      policyName = Lude.Nothing,
      policyId = Lude.Nothing,
      issueInfoMap = Lude.Nothing,
      policyOwner = Lude.Nothing,
      memberAccount = Lude.Nothing
    }

-- | An array of @EvaluationResult@ objects.
--
-- /Note:/ Consider using 'evaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsEvaluationResults :: Lens.Lens' PolicyComplianceStatus (Lude.Maybe [EvaluationResult])
pcsEvaluationResults = Lens.lens (evaluationResults :: PolicyComplianceStatus -> Lude.Maybe [EvaluationResult]) (\s a -> s {evaluationResults = a} :: PolicyComplianceStatus)
{-# DEPRECATED pcsEvaluationResults "Use generic-lens or generic-optics with 'evaluationResults' instead." #-}

-- | Timestamp of the last update to the @EvaluationResult@ objects.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsLastUpdated :: Lens.Lens' PolicyComplianceStatus (Lude.Maybe Lude.Timestamp)
pcsLastUpdated = Lens.lens (lastUpdated :: PolicyComplianceStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdated = a} :: PolicyComplianceStatus)
{-# DEPRECATED pcsLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | The name of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsPolicyName :: Lens.Lens' PolicyComplianceStatus (Lude.Maybe Lude.Text)
pcsPolicyName = Lens.lens (policyName :: PolicyComplianceStatus -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: PolicyComplianceStatus)
{-# DEPRECATED pcsPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The ID of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsPolicyId :: Lens.Lens' PolicyComplianceStatus (Lude.Maybe Lude.Text)
pcsPolicyId = Lens.lens (policyId :: PolicyComplianceStatus -> Lude.Maybe Lude.Text) (\s a -> s {policyId = a} :: PolicyComplianceStatus)
{-# DEPRECATED pcsPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | Details about problems with dependent services, such as AWS WAF or AWS Config, that are causing a resource to be noncompliant. The details include the name of the dependent service and the error message received that indicates the problem with the service.
--
-- /Note:/ Consider using 'issueInfoMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsIssueInfoMap :: Lens.Lens' PolicyComplianceStatus (Lude.Maybe (Lude.HashMap DependentServiceName (Lude.Text)))
pcsIssueInfoMap = Lens.lens (issueInfoMap :: PolicyComplianceStatus -> Lude.Maybe (Lude.HashMap DependentServiceName (Lude.Text))) (\s a -> s {issueInfoMap = a} :: PolicyComplianceStatus)
{-# DEPRECATED pcsIssueInfoMap "Use generic-lens or generic-optics with 'issueInfoMap' instead." #-}

-- | The AWS account that created the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policyOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsPolicyOwner :: Lens.Lens' PolicyComplianceStatus (Lude.Maybe Lude.Text)
pcsPolicyOwner = Lens.lens (policyOwner :: PolicyComplianceStatus -> Lude.Maybe Lude.Text) (\s a -> s {policyOwner = a} :: PolicyComplianceStatus)
{-# DEPRECATED pcsPolicyOwner "Use generic-lens or generic-optics with 'policyOwner' instead." #-}

-- | The member account ID.
--
-- /Note:/ Consider using 'memberAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsMemberAccount :: Lens.Lens' PolicyComplianceStatus (Lude.Maybe Lude.Text)
pcsMemberAccount = Lens.lens (memberAccount :: PolicyComplianceStatus -> Lude.Maybe Lude.Text) (\s a -> s {memberAccount = a} :: PolicyComplianceStatus)
{-# DEPRECATED pcsMemberAccount "Use generic-lens or generic-optics with 'memberAccount' instead." #-}

instance Lude.FromJSON PolicyComplianceStatus where
  parseJSON =
    Lude.withObject
      "PolicyComplianceStatus"
      ( \x ->
          PolicyComplianceStatus'
            Lude.<$> (x Lude..:? "EvaluationResults" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LastUpdated")
            Lude.<*> (x Lude..:? "PolicyName")
            Lude.<*> (x Lude..:? "PolicyId")
            Lude.<*> (x Lude..:? "IssueInfoMap" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "PolicyOwner")
            Lude.<*> (x Lude..:? "MemberAccount")
      )
