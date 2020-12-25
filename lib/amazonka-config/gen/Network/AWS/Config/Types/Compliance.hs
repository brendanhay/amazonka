{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Compliance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Compliance
  ( Compliance (..),

    -- * Smart constructor
    mkCompliance,

    -- * Lenses
    cComplianceContributorCount,
    cComplianceType,
  )
where

import qualified Network.AWS.Config.Types.ComplianceContributorCount as Types
import qualified Network.AWS.Config.Types.ComplianceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether an AWS resource or AWS Config rule is compliant and provides the number of contributors that affect the compliance.
--
-- /See:/ 'mkCompliance' smart constructor.
data Compliance = Compliance'
  { -- | The number of AWS resources or AWS Config rules that cause a result of @NON_COMPLIANT@ , up to a maximum number.
    complianceContributorCount :: Core.Maybe Types.ComplianceContributorCount,
    -- | Indicates whether an AWS resource or AWS Config rule is compliant.
    --
    -- A resource is compliant if it complies with all of the AWS Config rules that evaluate it. A resource is noncompliant if it does not comply with one or more of these rules.
    -- A rule is compliant if all of the resources that the rule evaluates comply with it. A rule is noncompliant if any of these resources do not comply.
    -- AWS Config returns the @INSUFFICIENT_DATA@ value when no evaluation results are available for the AWS resource or AWS Config rule.
    -- For the @Compliance@ data type, AWS Config supports only @COMPLIANT@ , @NON_COMPLIANT@ , and @INSUFFICIENT_DATA@ values. AWS Config does not support the @NOT_APPLICABLE@ value for the @Compliance@ data type.
    complianceType :: Core.Maybe Types.ComplianceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Compliance' value with any optional fields omitted.
mkCompliance ::
  Compliance
mkCompliance =
  Compliance'
    { complianceContributorCount = Core.Nothing,
      complianceType = Core.Nothing
    }

-- | The number of AWS resources or AWS Config rules that cause a result of @NON_COMPLIANT@ , up to a maximum number.
--
-- /Note:/ Consider using 'complianceContributorCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComplianceContributorCount :: Lens.Lens' Compliance (Core.Maybe Types.ComplianceContributorCount)
cComplianceContributorCount = Lens.field @"complianceContributorCount"
{-# DEPRECATED cComplianceContributorCount "Use generic-lens or generic-optics with 'complianceContributorCount' instead." #-}

-- | Indicates whether an AWS resource or AWS Config rule is compliant.
--
-- A resource is compliant if it complies with all of the AWS Config rules that evaluate it. A resource is noncompliant if it does not comply with one or more of these rules.
-- A rule is compliant if all of the resources that the rule evaluates comply with it. A rule is noncompliant if any of these resources do not comply.
-- AWS Config returns the @INSUFFICIENT_DATA@ value when no evaluation results are available for the AWS resource or AWS Config rule.
-- For the @Compliance@ data type, AWS Config supports only @COMPLIANT@ , @NON_COMPLIANT@ , and @INSUFFICIENT_DATA@ values. AWS Config does not support the @NOT_APPLICABLE@ value for the @Compliance@ data type.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComplianceType :: Lens.Lens' Compliance (Core.Maybe Types.ComplianceType)
cComplianceType = Lens.field @"complianceType"
{-# DEPRECATED cComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

instance Core.FromJSON Compliance where
  parseJSON =
    Core.withObject "Compliance" Core.$
      \x ->
        Compliance'
          Core.<$> (x Core..:? "ComplianceContributorCount")
          Core.<*> (x Core..:? "ComplianceType")
