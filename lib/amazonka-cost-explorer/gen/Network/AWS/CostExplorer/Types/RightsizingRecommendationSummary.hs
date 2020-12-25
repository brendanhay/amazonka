{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary
  ( RightsizingRecommendationSummary (..),

    -- * Smart constructor
    mkRightsizingRecommendationSummary,

    -- * Lenses
    rrsEstimatedTotalMonthlySavingsAmount,
    rrsSavingsCurrencyCode,
    rrsSavingsPercentage,
    rrsTotalRecommendationCount,
  )
where

import qualified Network.AWS.CostExplorer.Types.EstimatedTotalMonthlySavingsAmount as Types
import qualified Network.AWS.CostExplorer.Types.SavingsCurrencyCode as Types
import qualified Network.AWS.CostExplorer.Types.SavingsPercentage as Types
import qualified Network.AWS.CostExplorer.Types.TotalRecommendationCount as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summary of rightsizing recommendations
--
-- /See:/ 'mkRightsizingRecommendationSummary' smart constructor.
data RightsizingRecommendationSummary = RightsizingRecommendationSummary'
  { -- | Estimated total savings resulting from modifications, on a monthly basis.
    estimatedTotalMonthlySavingsAmount :: Core.Maybe Types.EstimatedTotalMonthlySavingsAmount,
    -- | The currency code that AWS used to calculate the savings.
    savingsCurrencyCode :: Core.Maybe Types.SavingsCurrencyCode,
    -- | Savings percentage based on the recommended modifications, relative to the total On-Demand costs associated with these instances.
    savingsPercentage :: Core.Maybe Types.SavingsPercentage,
    -- | Total number of instance recommendations.
    totalRecommendationCount :: Core.Maybe Types.TotalRecommendationCount
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RightsizingRecommendationSummary' value with any optional fields omitted.
mkRightsizingRecommendationSummary ::
  RightsizingRecommendationSummary
mkRightsizingRecommendationSummary =
  RightsizingRecommendationSummary'
    { estimatedTotalMonthlySavingsAmount =
        Core.Nothing,
      savingsCurrencyCode = Core.Nothing,
      savingsPercentage = Core.Nothing,
      totalRecommendationCount = Core.Nothing
    }

-- | Estimated total savings resulting from modifications, on a monthly basis.
--
-- /Note:/ Consider using 'estimatedTotalMonthlySavingsAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsEstimatedTotalMonthlySavingsAmount :: Lens.Lens' RightsizingRecommendationSummary (Core.Maybe Types.EstimatedTotalMonthlySavingsAmount)
rrsEstimatedTotalMonthlySavingsAmount = Lens.field @"estimatedTotalMonthlySavingsAmount"
{-# DEPRECATED rrsEstimatedTotalMonthlySavingsAmount "Use generic-lens or generic-optics with 'estimatedTotalMonthlySavingsAmount' instead." #-}

-- | The currency code that AWS used to calculate the savings.
--
-- /Note:/ Consider using 'savingsCurrencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsSavingsCurrencyCode :: Lens.Lens' RightsizingRecommendationSummary (Core.Maybe Types.SavingsCurrencyCode)
rrsSavingsCurrencyCode = Lens.field @"savingsCurrencyCode"
{-# DEPRECATED rrsSavingsCurrencyCode "Use generic-lens or generic-optics with 'savingsCurrencyCode' instead." #-}

-- | Savings percentage based on the recommended modifications, relative to the total On-Demand costs associated with these instances.
--
-- /Note:/ Consider using 'savingsPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsSavingsPercentage :: Lens.Lens' RightsizingRecommendationSummary (Core.Maybe Types.SavingsPercentage)
rrsSavingsPercentage = Lens.field @"savingsPercentage"
{-# DEPRECATED rrsSavingsPercentage "Use generic-lens or generic-optics with 'savingsPercentage' instead." #-}

-- | Total number of instance recommendations.
--
-- /Note:/ Consider using 'totalRecommendationCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsTotalRecommendationCount :: Lens.Lens' RightsizingRecommendationSummary (Core.Maybe Types.TotalRecommendationCount)
rrsTotalRecommendationCount = Lens.field @"totalRecommendationCount"
{-# DEPRECATED rrsTotalRecommendationCount "Use generic-lens or generic-optics with 'totalRecommendationCount' instead." #-}

instance Core.FromJSON RightsizingRecommendationSummary where
  parseJSON =
    Core.withObject "RightsizingRecommendationSummary" Core.$
      \x ->
        RightsizingRecommendationSummary'
          Core.<$> (x Core..:? "EstimatedTotalMonthlySavingsAmount")
          Core.<*> (x Core..:? "SavingsCurrencyCode")
          Core.<*> (x Core..:? "SavingsPercentage")
          Core.<*> (x Core..:? "TotalRecommendationCount")
