{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ComplianceSummary
  ( ComplianceSummary (..)
  -- * Smart constructor
  , mkComplianceSummary
  -- * Lenses
  , csComplianceSummaryTimestamp
  , csCompliantResourceCount
  , csNonCompliantResourceCount
  ) where

import qualified Network.AWS.Config.Types.ComplianceContributorCount as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The number of AWS Config rules or AWS resources that are compliant and noncompliant.
--
-- /See:/ 'mkComplianceSummary' smart constructor.
data ComplianceSummary = ComplianceSummary'
  { complianceSummaryTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that AWS Config created the compliance summary.
  , compliantResourceCount :: Core.Maybe Types.ComplianceContributorCount
    -- ^ The number of AWS Config rules or AWS resources that are compliant, up to a maximum of 25 for rules and 100 for resources.
  , nonCompliantResourceCount :: Core.Maybe Types.ComplianceContributorCount
    -- ^ The number of AWS Config rules or AWS resources that are noncompliant, up to a maximum of 25 for rules and 100 for resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ComplianceSummary' value with any optional fields omitted.
mkComplianceSummary
    :: ComplianceSummary
mkComplianceSummary
  = ComplianceSummary'{complianceSummaryTimestamp = Core.Nothing,
                       compliantResourceCount = Core.Nothing,
                       nonCompliantResourceCount = Core.Nothing}

-- | The time that AWS Config created the compliance summary.
--
-- /Note:/ Consider using 'complianceSummaryTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csComplianceSummaryTimestamp :: Lens.Lens' ComplianceSummary (Core.Maybe Core.NominalDiffTime)
csComplianceSummaryTimestamp = Lens.field @"complianceSummaryTimestamp"
{-# INLINEABLE csComplianceSummaryTimestamp #-}
{-# DEPRECATED complianceSummaryTimestamp "Use generic-lens or generic-optics with 'complianceSummaryTimestamp' instead"  #-}

-- | The number of AWS Config rules or AWS resources that are compliant, up to a maximum of 25 for rules and 100 for resources.
--
-- /Note:/ Consider using 'compliantResourceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCompliantResourceCount :: Lens.Lens' ComplianceSummary (Core.Maybe Types.ComplianceContributorCount)
csCompliantResourceCount = Lens.field @"compliantResourceCount"
{-# INLINEABLE csCompliantResourceCount #-}
{-# DEPRECATED compliantResourceCount "Use generic-lens or generic-optics with 'compliantResourceCount' instead"  #-}

-- | The number of AWS Config rules or AWS resources that are noncompliant, up to a maximum of 25 for rules and 100 for resources.
--
-- /Note:/ Consider using 'nonCompliantResourceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNonCompliantResourceCount :: Lens.Lens' ComplianceSummary (Core.Maybe Types.ComplianceContributorCount)
csNonCompliantResourceCount = Lens.field @"nonCompliantResourceCount"
{-# INLINEABLE csNonCompliantResourceCount #-}
{-# DEPRECATED nonCompliantResourceCount "Use generic-lens or generic-optics with 'nonCompliantResourceCount' instead"  #-}

instance Core.FromJSON ComplianceSummary where
        parseJSON
          = Core.withObject "ComplianceSummary" Core.$
              \ x ->
                ComplianceSummary' Core.<$>
                  (x Core..:? "ComplianceSummaryTimestamp") Core.<*>
                    x Core..:? "CompliantResourceCount"
                    Core.<*> x Core..:? "NonCompliantResourceCount"
