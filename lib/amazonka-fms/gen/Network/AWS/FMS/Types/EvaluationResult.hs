-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.EvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.EvaluationResult
  ( EvaluationResult (..),

    -- * Smart constructor
    mkEvaluationResult,

    -- * Lenses
    erViolatorCount,
    erComplianceStatus,
    erEvaluationLimitExceeded,
  )
where

import Network.AWS.FMS.Types.PolicyComplianceStatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the compliance status for the account. An account is considered noncompliant if it includes resources that are not protected by the specified policy or that don't comply with the policy.
--
-- /See:/ 'mkEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { violatorCount ::
      Lude.Maybe Lude.Natural,
    complianceStatus :: Lude.Maybe PolicyComplianceStatusType,
    evaluationLimitExceeded :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EvaluationResult' with the minimum fields required to make a request.
--
-- * 'complianceStatus' - Describes an AWS account's compliance with the AWS Firewall Manager policy.
-- * 'evaluationLimitExceeded' - Indicates that over 100 resources are noncompliant with the AWS Firewall Manager policy.
-- * 'violatorCount' - The number of resources that are noncompliant with the specified policy. For AWS WAF and Shield Advanced policies, a resource is considered noncompliant if it is not associated with the policy. For security group policies, a resource is considered noncompliant if it doesn't comply with the rules of the policy and remediation is disabled or not possible.
mkEvaluationResult ::
  EvaluationResult
mkEvaluationResult =
  EvaluationResult'
    { violatorCount = Lude.Nothing,
      complianceStatus = Lude.Nothing,
      evaluationLimitExceeded = Lude.Nothing
    }

-- | The number of resources that are noncompliant with the specified policy. For AWS WAF and Shield Advanced policies, a resource is considered noncompliant if it is not associated with the policy. For security group policies, a resource is considered noncompliant if it doesn't comply with the rules of the policy and remediation is disabled or not possible.
--
-- /Note:/ Consider using 'violatorCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erViolatorCount :: Lens.Lens' EvaluationResult (Lude.Maybe Lude.Natural)
erViolatorCount = Lens.lens (violatorCount :: EvaluationResult -> Lude.Maybe Lude.Natural) (\s a -> s {violatorCount = a} :: EvaluationResult)
{-# DEPRECATED erViolatorCount "Use generic-lens or generic-optics with 'violatorCount' instead." #-}

-- | Describes an AWS account's compliance with the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'complianceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erComplianceStatus :: Lens.Lens' EvaluationResult (Lude.Maybe PolicyComplianceStatusType)
erComplianceStatus = Lens.lens (complianceStatus :: EvaluationResult -> Lude.Maybe PolicyComplianceStatusType) (\s a -> s {complianceStatus = a} :: EvaluationResult)
{-# DEPRECATED erComplianceStatus "Use generic-lens or generic-optics with 'complianceStatus' instead." #-}

-- | Indicates that over 100 resources are noncompliant with the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'evaluationLimitExceeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEvaluationLimitExceeded :: Lens.Lens' EvaluationResult (Lude.Maybe Lude.Bool)
erEvaluationLimitExceeded = Lens.lens (evaluationLimitExceeded :: EvaluationResult -> Lude.Maybe Lude.Bool) (\s a -> s {evaluationLimitExceeded = a} :: EvaluationResult)
{-# DEPRECATED erEvaluationLimitExceeded "Use generic-lens or generic-optics with 'evaluationLimitExceeded' instead." #-}

instance Lude.FromJSON EvaluationResult where
  parseJSON =
    Lude.withObject
      "EvaluationResult"
      ( \x ->
          EvaluationResult'
            Lude.<$> (x Lude..:? "ViolatorCount")
            Lude.<*> (x Lude..:? "ComplianceStatus")
            Lude.<*> (x Lude..:? "EvaluationLimitExceeded")
      )
