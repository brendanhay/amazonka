{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.EvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.EvaluationResult
  ( EvaluationResult (..)
  -- * Smart constructor
  , mkEvaluationResult
  -- * Lenses
  , erComplianceStatus
  , erEvaluationLimitExceeded
  , erViolatorCount
  ) where

import qualified Network.AWS.FMS.Types.PolicyComplianceStatusType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the compliance status for the account. An account is considered noncompliant if it includes resources that are not protected by the specified policy or that don't comply with the policy.
--
-- /See:/ 'mkEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { complianceStatus :: Core.Maybe Types.PolicyComplianceStatusType
    -- ^ Describes an AWS account's compliance with the AWS Firewall Manager policy.
  , evaluationLimitExceeded :: Core.Maybe Core.Bool
    -- ^ Indicates that over 100 resources are noncompliant with the AWS Firewall Manager policy.
  , violatorCount :: Core.Maybe Core.Natural
    -- ^ The number of resources that are noncompliant with the specified policy. For AWS WAF and Shield Advanced policies, a resource is considered noncompliant if it is not associated with the policy. For security group policies, a resource is considered noncompliant if it doesn't comply with the rules of the policy and remediation is disabled or not possible.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EvaluationResult' value with any optional fields omitted.
mkEvaluationResult
    :: EvaluationResult
mkEvaluationResult
  = EvaluationResult'{complianceStatus = Core.Nothing,
                      evaluationLimitExceeded = Core.Nothing,
                      violatorCount = Core.Nothing}

-- | Describes an AWS account's compliance with the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'complianceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erComplianceStatus :: Lens.Lens' EvaluationResult (Core.Maybe Types.PolicyComplianceStatusType)
erComplianceStatus = Lens.field @"complianceStatus"
{-# INLINEABLE erComplianceStatus #-}
{-# DEPRECATED complianceStatus "Use generic-lens or generic-optics with 'complianceStatus' instead"  #-}

-- | Indicates that over 100 resources are noncompliant with the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'evaluationLimitExceeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEvaluationLimitExceeded :: Lens.Lens' EvaluationResult (Core.Maybe Core.Bool)
erEvaluationLimitExceeded = Lens.field @"evaluationLimitExceeded"
{-# INLINEABLE erEvaluationLimitExceeded #-}
{-# DEPRECATED evaluationLimitExceeded "Use generic-lens or generic-optics with 'evaluationLimitExceeded' instead"  #-}

-- | The number of resources that are noncompliant with the specified policy. For AWS WAF and Shield Advanced policies, a resource is considered noncompliant if it is not associated with the policy. For security group policies, a resource is considered noncompliant if it doesn't comply with the rules of the policy and remediation is disabled or not possible.
--
-- /Note:/ Consider using 'violatorCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erViolatorCount :: Lens.Lens' EvaluationResult (Core.Maybe Core.Natural)
erViolatorCount = Lens.field @"violatorCount"
{-# INLINEABLE erViolatorCount #-}
{-# DEPRECATED violatorCount "Use generic-lens or generic-optics with 'violatorCount' instead"  #-}

instance Core.FromJSON EvaluationResult where
        parseJSON
          = Core.withObject "EvaluationResult" Core.$
              \ x ->
                EvaluationResult' Core.<$>
                  (x Core..:? "ComplianceStatus") Core.<*>
                    x Core..:? "EvaluationLimitExceeded"
                    Core.<*> x Core..:? "ViolatorCount"
