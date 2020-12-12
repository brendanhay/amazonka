{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceSummary
  ( ComplianceSummary (..),

    -- * Smart constructor
    mkComplianceSummary,

    -- * Lenses
    csComplianceSummaryTimestamp,
    csCompliantResourceCount,
    csNonCompliantResourceCount,
  )
where

import Network.AWS.Config.Types.ComplianceContributorCount
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The number of AWS Config rules or AWS resources that are compliant and noncompliant.
--
-- /See:/ 'mkComplianceSummary' smart constructor.
data ComplianceSummary = ComplianceSummary'
  { complianceSummaryTimestamp ::
      Lude.Maybe Lude.Timestamp,
    compliantResourceCount ::
      Lude.Maybe ComplianceContributorCount,
    nonCompliantResourceCount ::
      Lude.Maybe ComplianceContributorCount
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComplianceSummary' with the minimum fields required to make a request.
--
-- * 'complianceSummaryTimestamp' - The time that AWS Config created the compliance summary.
-- * 'compliantResourceCount' - The number of AWS Config rules or AWS resources that are compliant, up to a maximum of 25 for rules and 100 for resources.
-- * 'nonCompliantResourceCount' - The number of AWS Config rules or AWS resources that are noncompliant, up to a maximum of 25 for rules and 100 for resources.
mkComplianceSummary ::
  ComplianceSummary
mkComplianceSummary =
  ComplianceSummary'
    { complianceSummaryTimestamp = Lude.Nothing,
      compliantResourceCount = Lude.Nothing,
      nonCompliantResourceCount = Lude.Nothing
    }

-- | The time that AWS Config created the compliance summary.
--
-- /Note:/ Consider using 'complianceSummaryTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csComplianceSummaryTimestamp :: Lens.Lens' ComplianceSummary (Lude.Maybe Lude.Timestamp)
csComplianceSummaryTimestamp = Lens.lens (complianceSummaryTimestamp :: ComplianceSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {complianceSummaryTimestamp = a} :: ComplianceSummary)
{-# DEPRECATED csComplianceSummaryTimestamp "Use generic-lens or generic-optics with 'complianceSummaryTimestamp' instead." #-}

-- | The number of AWS Config rules or AWS resources that are compliant, up to a maximum of 25 for rules and 100 for resources.
--
-- /Note:/ Consider using 'compliantResourceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCompliantResourceCount :: Lens.Lens' ComplianceSummary (Lude.Maybe ComplianceContributorCount)
csCompliantResourceCount = Lens.lens (compliantResourceCount :: ComplianceSummary -> Lude.Maybe ComplianceContributorCount) (\s a -> s {compliantResourceCount = a} :: ComplianceSummary)
{-# DEPRECATED csCompliantResourceCount "Use generic-lens or generic-optics with 'compliantResourceCount' instead." #-}

-- | The number of AWS Config rules or AWS resources that are noncompliant, up to a maximum of 25 for rules and 100 for resources.
--
-- /Note:/ Consider using 'nonCompliantResourceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNonCompliantResourceCount :: Lens.Lens' ComplianceSummary (Lude.Maybe ComplianceContributorCount)
csNonCompliantResourceCount = Lens.lens (nonCompliantResourceCount :: ComplianceSummary -> Lude.Maybe ComplianceContributorCount) (\s a -> s {nonCompliantResourceCount = a} :: ComplianceSummary)
{-# DEPRECATED csNonCompliantResourceCount "Use generic-lens or generic-optics with 'nonCompliantResourceCount' instead." #-}

instance Lude.FromJSON ComplianceSummary where
  parseJSON =
    Lude.withObject
      "ComplianceSummary"
      ( \x ->
          ComplianceSummary'
            Lude.<$> (x Lude..:? "ComplianceSummaryTimestamp")
            Lude.<*> (x Lude..:? "CompliantResourceCount")
            Lude.<*> (x Lude..:? "NonCompliantResourceCount")
      )
