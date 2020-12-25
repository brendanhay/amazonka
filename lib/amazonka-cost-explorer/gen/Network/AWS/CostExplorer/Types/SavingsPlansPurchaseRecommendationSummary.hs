{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
  ( SavingsPlansPurchaseRecommendationSummary (..),

    -- * Smart constructor
    mkSavingsPlansPurchaseRecommendationSummary,

    -- * Lenses
    spprsCurrencyCode,
    spprsCurrentOnDemandSpend,
    spprsDailyCommitmentToPurchase,
    spprsEstimatedMonthlySavingsAmount,
    spprsEstimatedOnDemandCostWithCurrentCommitment,
    spprsEstimatedROI,
    spprsEstimatedSavingsAmount,
    spprsEstimatedSavingsPercentage,
    spprsEstimatedTotalCost,
    spprsHourlyCommitmentToPurchase,
    spprsTotalRecommendationCount,
  )
where

import qualified Network.AWS.CostExplorer.Types.GenericString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summary metrics for your Savings Plans Purchase Recommendations.
--
-- /See:/ 'mkSavingsPlansPurchaseRecommendationSummary' smart constructor.
data SavingsPlansPurchaseRecommendationSummary = SavingsPlansPurchaseRecommendationSummary'
  { -- | The currency code AWS used to generate the recommendations and present potential savings.
    currencyCode :: Core.Maybe Types.GenericString,
    -- | The current total on demand spend of the applicable usage types over the lookback period.
    currentOnDemandSpend :: Core.Maybe Types.GenericString,
    -- | The recommended Savings Plans cost on a daily (24 hourly) basis.
    dailyCommitmentToPurchase :: Core.Maybe Types.GenericString,
    -- | The estimated monthly savings amount, based on the recommended Savings Plans purchase.
    estimatedMonthlySavingsAmount :: Core.Maybe Types.GenericString,
    -- | The estimated On-Demand costs you would expect with no additional commitment, based on your usage of the selected time period and the Savings Plans you own.
    estimatedOnDemandCostWithCurrentCommitment :: Core.Maybe Types.GenericString,
    -- | The estimated return on investment based on the recommended Savings Plans and estimated savings.
    estimatedROI :: Core.Maybe Types.GenericString,
    -- | The estimated total savings over the lookback period, based on the purchase of the recommended Savings Plans.
    estimatedSavingsAmount :: Core.Maybe Types.GenericString,
    -- | The estimated savings relative to the total cost of On-Demand usage, over the lookback period. This is calculated as @estimatedSavingsAmount@ / @CurrentOnDemandSpend@ *100.
    estimatedSavingsPercentage :: Core.Maybe Types.GenericString,
    -- | The estimated total cost of the usage after purchasing the recommended Savings Plans. This is a sum of the cost of Savings Plans during this term, and the remaining On-Demand usage.
    estimatedTotalCost :: Core.Maybe Types.GenericString,
    -- | The recommended hourly commitment based on the recommendation parameters.
    hourlyCommitmentToPurchase :: Core.Maybe Types.GenericString,
    -- | The aggregate number of Savings Plans recommendations that exist for your account.
    totalRecommendationCount :: Core.Maybe Types.GenericString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SavingsPlansPurchaseRecommendationSummary' value with any optional fields omitted.
mkSavingsPlansPurchaseRecommendationSummary ::
  SavingsPlansPurchaseRecommendationSummary
mkSavingsPlansPurchaseRecommendationSummary =
  SavingsPlansPurchaseRecommendationSummary'
    { currencyCode =
        Core.Nothing,
      currentOnDemandSpend = Core.Nothing,
      dailyCommitmentToPurchase = Core.Nothing,
      estimatedMonthlySavingsAmount = Core.Nothing,
      estimatedOnDemandCostWithCurrentCommitment =
        Core.Nothing,
      estimatedROI = Core.Nothing,
      estimatedSavingsAmount = Core.Nothing,
      estimatedSavingsPercentage = Core.Nothing,
      estimatedTotalCost = Core.Nothing,
      hourlyCommitmentToPurchase = Core.Nothing,
      totalRecommendationCount = Core.Nothing
    }

-- | The currency code AWS used to generate the recommendations and present potential savings.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsCurrencyCode :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Types.GenericString)
spprsCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED spprsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The current total on demand spend of the applicable usage types over the lookback period.
--
-- /Note:/ Consider using 'currentOnDemandSpend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsCurrentOnDemandSpend :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Types.GenericString)
spprsCurrentOnDemandSpend = Lens.field @"currentOnDemandSpend"
{-# DEPRECATED spprsCurrentOnDemandSpend "Use generic-lens or generic-optics with 'currentOnDemandSpend' instead." #-}

-- | The recommended Savings Plans cost on a daily (24 hourly) basis.
--
-- /Note:/ Consider using 'dailyCommitmentToPurchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsDailyCommitmentToPurchase :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Types.GenericString)
spprsDailyCommitmentToPurchase = Lens.field @"dailyCommitmentToPurchase"
{-# DEPRECATED spprsDailyCommitmentToPurchase "Use generic-lens or generic-optics with 'dailyCommitmentToPurchase' instead." #-}

-- | The estimated monthly savings amount, based on the recommended Savings Plans purchase.
--
-- /Note:/ Consider using 'estimatedMonthlySavingsAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsEstimatedMonthlySavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Types.GenericString)
spprsEstimatedMonthlySavingsAmount = Lens.field @"estimatedMonthlySavingsAmount"
{-# DEPRECATED spprsEstimatedMonthlySavingsAmount "Use generic-lens or generic-optics with 'estimatedMonthlySavingsAmount' instead." #-}

-- | The estimated On-Demand costs you would expect with no additional commitment, based on your usage of the selected time period and the Savings Plans you own.
--
-- /Note:/ Consider using 'estimatedOnDemandCostWithCurrentCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsEstimatedOnDemandCostWithCurrentCommitment :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Types.GenericString)
spprsEstimatedOnDemandCostWithCurrentCommitment = Lens.field @"estimatedOnDemandCostWithCurrentCommitment"
{-# DEPRECATED spprsEstimatedOnDemandCostWithCurrentCommitment "Use generic-lens or generic-optics with 'estimatedOnDemandCostWithCurrentCommitment' instead." #-}

-- | The estimated return on investment based on the recommended Savings Plans and estimated savings.
--
-- /Note:/ Consider using 'estimatedROI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsEstimatedROI :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Types.GenericString)
spprsEstimatedROI = Lens.field @"estimatedROI"
{-# DEPRECATED spprsEstimatedROI "Use generic-lens or generic-optics with 'estimatedROI' instead." #-}

-- | The estimated total savings over the lookback period, based on the purchase of the recommended Savings Plans.
--
-- /Note:/ Consider using 'estimatedSavingsAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsEstimatedSavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Types.GenericString)
spprsEstimatedSavingsAmount = Lens.field @"estimatedSavingsAmount"
{-# DEPRECATED spprsEstimatedSavingsAmount "Use generic-lens or generic-optics with 'estimatedSavingsAmount' instead." #-}

-- | The estimated savings relative to the total cost of On-Demand usage, over the lookback period. This is calculated as @estimatedSavingsAmount@ / @CurrentOnDemandSpend@ *100.
--
-- /Note:/ Consider using 'estimatedSavingsPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsEstimatedSavingsPercentage :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Types.GenericString)
spprsEstimatedSavingsPercentage = Lens.field @"estimatedSavingsPercentage"
{-# DEPRECATED spprsEstimatedSavingsPercentage "Use generic-lens or generic-optics with 'estimatedSavingsPercentage' instead." #-}

-- | The estimated total cost of the usage after purchasing the recommended Savings Plans. This is a sum of the cost of Savings Plans during this term, and the remaining On-Demand usage.
--
-- /Note:/ Consider using 'estimatedTotalCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsEstimatedTotalCost :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Types.GenericString)
spprsEstimatedTotalCost = Lens.field @"estimatedTotalCost"
{-# DEPRECATED spprsEstimatedTotalCost "Use generic-lens or generic-optics with 'estimatedTotalCost' instead." #-}

-- | The recommended hourly commitment based on the recommendation parameters.
--
-- /Note:/ Consider using 'hourlyCommitmentToPurchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsHourlyCommitmentToPurchase :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Types.GenericString)
spprsHourlyCommitmentToPurchase = Lens.field @"hourlyCommitmentToPurchase"
{-# DEPRECATED spprsHourlyCommitmentToPurchase "Use generic-lens or generic-optics with 'hourlyCommitmentToPurchase' instead." #-}

-- | The aggregate number of Savings Plans recommendations that exist for your account.
--
-- /Note:/ Consider using 'totalRecommendationCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsTotalRecommendationCount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Types.GenericString)
spprsTotalRecommendationCount = Lens.field @"totalRecommendationCount"
{-# DEPRECATED spprsTotalRecommendationCount "Use generic-lens or generic-optics with 'totalRecommendationCount' instead." #-}

instance Core.FromJSON SavingsPlansPurchaseRecommendationSummary where
  parseJSON =
    Core.withObject "SavingsPlansPurchaseRecommendationSummary" Core.$
      \x ->
        SavingsPlansPurchaseRecommendationSummary'
          Core.<$> (x Core..:? "CurrencyCode")
          Core.<*> (x Core..:? "CurrentOnDemandSpend")
          Core.<*> (x Core..:? "DailyCommitmentToPurchase")
          Core.<*> (x Core..:? "EstimatedMonthlySavingsAmount")
          Core.<*> (x Core..:? "EstimatedOnDemandCostWithCurrentCommitment")
          Core.<*> (x Core..:? "EstimatedROI")
          Core.<*> (x Core..:? "EstimatedSavingsAmount")
          Core.<*> (x Core..:? "EstimatedSavingsPercentage")
          Core.<*> (x Core..:? "EstimatedTotalCost")
          Core.<*> (x Core..:? "HourlyCommitmentToPurchase")
          Core.<*> (x Core..:? "TotalRecommendationCount")
