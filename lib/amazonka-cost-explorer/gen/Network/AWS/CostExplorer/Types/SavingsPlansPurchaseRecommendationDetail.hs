{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
  ( SavingsPlansPurchaseRecommendationDetail (..)
  -- * Smart constructor
  , mkSavingsPlansPurchaseRecommendationDetail
  -- * Lenses
  , spprdAccountId
  , spprdCurrencyCode
  , spprdCurrentAverageHourlyOnDemandSpend
  , spprdCurrentMaximumHourlyOnDemandSpend
  , spprdCurrentMinimumHourlyOnDemandSpend
  , spprdEstimatedAverageUtilization
  , spprdEstimatedMonthlySavingsAmount
  , spprdEstimatedOnDemandCost
  , spprdEstimatedOnDemandCostWithCurrentCommitment
  , spprdEstimatedROI
  , spprdEstimatedSPCost
  , spprdEstimatedSavingsAmount
  , spprdEstimatedSavingsPercentage
  , spprdHourlyCommitmentToPurchase
  , spprdSavingsPlansDetails
  , spprdUpfrontCost
  ) where

import qualified Network.AWS.CostExplorer.Types.AccountId as Types
import qualified Network.AWS.CostExplorer.Types.CurrencyCode as Types
import qualified Network.AWS.CostExplorer.Types.CurrentAverageHourlyOnDemandSpend as Types
import qualified Network.AWS.CostExplorer.Types.CurrentMaximumHourlyOnDemandSpend as Types
import qualified Network.AWS.CostExplorer.Types.CurrentMinimumHourlyOnDemandSpend as Types
import qualified Network.AWS.CostExplorer.Types.EstimatedAverageUtilization as Types
import qualified Network.AWS.CostExplorer.Types.EstimatedMonthlySavingsAmount as Types
import qualified Network.AWS.CostExplorer.Types.EstimatedOnDemandCost as Types
import qualified Network.AWS.CostExplorer.Types.EstimatedOnDemandCostWithCurrentCommitment as Types
import qualified Network.AWS.CostExplorer.Types.EstimatedROI as Types
import qualified Network.AWS.CostExplorer.Types.EstimatedSPCost as Types
import qualified Network.AWS.CostExplorer.Types.EstimatedSavingsAmount as Types
import qualified Network.AWS.CostExplorer.Types.EstimatedSavingsPercentage as Types
import qualified Network.AWS.CostExplorer.Types.HourlyCommitmentToPurchase as Types
import qualified Network.AWS.CostExplorer.Types.SavingsPlansDetails as Types
import qualified Network.AWS.CostExplorer.Types.UpfrontCost as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details for your recommended Savings Plans.
--
-- /See:/ 'mkSavingsPlansPurchaseRecommendationDetail' smart constructor.
data SavingsPlansPurchaseRecommendationDetail = SavingsPlansPurchaseRecommendationDetail'
  { accountId :: Core.Maybe Types.AccountId
    -- ^ The @AccountID@ the recommendation is generated for.
  , currencyCode :: Core.Maybe Types.CurrencyCode
    -- ^ The currency code AWS used to generate the recommendations and present potential savings.
  , currentAverageHourlyOnDemandSpend :: Core.Maybe Types.CurrentAverageHourlyOnDemandSpend
    -- ^ The average value of hourly On-Demand spend over the lookback period of the applicable usage type.
  , currentMaximumHourlyOnDemandSpend :: Core.Maybe Types.CurrentMaximumHourlyOnDemandSpend
    -- ^ The highest value of hourly On-Demand spend over the lookback period of the applicable usage type.
  , currentMinimumHourlyOnDemandSpend :: Core.Maybe Types.CurrentMinimumHourlyOnDemandSpend
    -- ^ The lowest value of hourly On-Demand spend over the lookback period of the applicable usage type.
  , estimatedAverageUtilization :: Core.Maybe Types.EstimatedAverageUtilization
    -- ^ The estimated utilization of the recommended Savings Plans.
  , estimatedMonthlySavingsAmount :: Core.Maybe Types.EstimatedMonthlySavingsAmount
    -- ^ The estimated monthly savings amount, based on the recommended Savings Plans.
  , estimatedOnDemandCost :: Core.Maybe Types.EstimatedOnDemandCost
    -- ^ The remaining On-Demand cost estimated to not be covered by the recommended Savings Plans, over the length of the lookback period.
  , estimatedOnDemandCostWithCurrentCommitment :: Core.Maybe Types.EstimatedOnDemandCostWithCurrentCommitment
    -- ^ The estimated On-Demand costs you would expect with no additional commitment, based on your usage of the selected time period and the Savings Plans you own. 
  , estimatedROI :: Core.Maybe Types.EstimatedROI
    -- ^ The estimated return on investment based on the recommended Savings Plans purchased. This is calculated as @estimatedSavingsAmount@ / @estimatedSPCost@ *100.
  , estimatedSPCost :: Core.Maybe Types.EstimatedSPCost
    -- ^ The cost of the recommended Savings Plans over the length of the lookback period.
  , estimatedSavingsAmount :: Core.Maybe Types.EstimatedSavingsAmount
    -- ^ The estimated savings amount based on the recommended Savings Plans over the length of the lookback period.
  , estimatedSavingsPercentage :: Core.Maybe Types.EstimatedSavingsPercentage
    -- ^ The estimated savings percentage relative to the total cost of applicable On-Demand usage over the lookback period.
  , hourlyCommitmentToPurchase :: Core.Maybe Types.HourlyCommitmentToPurchase
    -- ^ The recommended hourly commitment level for the Savings Plans type, and configuration based on the usage during the lookback period.
  , savingsPlansDetails :: Core.Maybe Types.SavingsPlansDetails
    -- ^ Details for your recommended Savings Plans.
  , upfrontCost :: Core.Maybe Types.UpfrontCost
    -- ^ The upfront cost of the recommended Savings Plans, based on the selected payment option.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SavingsPlansPurchaseRecommendationDetail' value with any optional fields omitted.
mkSavingsPlansPurchaseRecommendationDetail
    :: SavingsPlansPurchaseRecommendationDetail
mkSavingsPlansPurchaseRecommendationDetail
  = SavingsPlansPurchaseRecommendationDetail'{accountId =
                                                Core.Nothing,
                                              currencyCode = Core.Nothing,
                                              currentAverageHourlyOnDemandSpend = Core.Nothing,
                                              currentMaximumHourlyOnDemandSpend = Core.Nothing,
                                              currentMinimumHourlyOnDemandSpend = Core.Nothing,
                                              estimatedAverageUtilization = Core.Nothing,
                                              estimatedMonthlySavingsAmount = Core.Nothing,
                                              estimatedOnDemandCost = Core.Nothing,
                                              estimatedOnDemandCostWithCurrentCommitment =
                                                Core.Nothing,
                                              estimatedROI = Core.Nothing,
                                              estimatedSPCost = Core.Nothing,
                                              estimatedSavingsAmount = Core.Nothing,
                                              estimatedSavingsPercentage = Core.Nothing,
                                              hourlyCommitmentToPurchase = Core.Nothing,
                                              savingsPlansDetails = Core.Nothing,
                                              upfrontCost = Core.Nothing}

-- | The @AccountID@ the recommendation is generated for.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdAccountId :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.AccountId)
spprdAccountId = Lens.field @"accountId"
{-# INLINEABLE spprdAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The currency code AWS used to generate the recommendations and present potential savings.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdCurrencyCode :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.CurrencyCode)
spprdCurrencyCode = Lens.field @"currencyCode"
{-# INLINEABLE spprdCurrencyCode #-}
{-# DEPRECATED currencyCode "Use generic-lens or generic-optics with 'currencyCode' instead"  #-}

-- | The average value of hourly On-Demand spend over the lookback period of the applicable usage type.
--
-- /Note:/ Consider using 'currentAverageHourlyOnDemandSpend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdCurrentAverageHourlyOnDemandSpend :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.CurrentAverageHourlyOnDemandSpend)
spprdCurrentAverageHourlyOnDemandSpend = Lens.field @"currentAverageHourlyOnDemandSpend"
{-# INLINEABLE spprdCurrentAverageHourlyOnDemandSpend #-}
{-# DEPRECATED currentAverageHourlyOnDemandSpend "Use generic-lens or generic-optics with 'currentAverageHourlyOnDemandSpend' instead"  #-}

-- | The highest value of hourly On-Demand spend over the lookback period of the applicable usage type.
--
-- /Note:/ Consider using 'currentMaximumHourlyOnDemandSpend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdCurrentMaximumHourlyOnDemandSpend :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.CurrentMaximumHourlyOnDemandSpend)
spprdCurrentMaximumHourlyOnDemandSpend = Lens.field @"currentMaximumHourlyOnDemandSpend"
{-# INLINEABLE spprdCurrentMaximumHourlyOnDemandSpend #-}
{-# DEPRECATED currentMaximumHourlyOnDemandSpend "Use generic-lens or generic-optics with 'currentMaximumHourlyOnDemandSpend' instead"  #-}

-- | The lowest value of hourly On-Demand spend over the lookback period of the applicable usage type.
--
-- /Note:/ Consider using 'currentMinimumHourlyOnDemandSpend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdCurrentMinimumHourlyOnDemandSpend :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.CurrentMinimumHourlyOnDemandSpend)
spprdCurrentMinimumHourlyOnDemandSpend = Lens.field @"currentMinimumHourlyOnDemandSpend"
{-# INLINEABLE spprdCurrentMinimumHourlyOnDemandSpend #-}
{-# DEPRECATED currentMinimumHourlyOnDemandSpend "Use generic-lens or generic-optics with 'currentMinimumHourlyOnDemandSpend' instead"  #-}

-- | The estimated utilization of the recommended Savings Plans.
--
-- /Note:/ Consider using 'estimatedAverageUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedAverageUtilization :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.EstimatedAverageUtilization)
spprdEstimatedAverageUtilization = Lens.field @"estimatedAverageUtilization"
{-# INLINEABLE spprdEstimatedAverageUtilization #-}
{-# DEPRECATED estimatedAverageUtilization "Use generic-lens or generic-optics with 'estimatedAverageUtilization' instead"  #-}

-- | The estimated monthly savings amount, based on the recommended Savings Plans.
--
-- /Note:/ Consider using 'estimatedMonthlySavingsAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedMonthlySavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.EstimatedMonthlySavingsAmount)
spprdEstimatedMonthlySavingsAmount = Lens.field @"estimatedMonthlySavingsAmount"
{-# INLINEABLE spprdEstimatedMonthlySavingsAmount #-}
{-# DEPRECATED estimatedMonthlySavingsAmount "Use generic-lens or generic-optics with 'estimatedMonthlySavingsAmount' instead"  #-}

-- | The remaining On-Demand cost estimated to not be covered by the recommended Savings Plans, over the length of the lookback period.
--
-- /Note:/ Consider using 'estimatedOnDemandCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedOnDemandCost :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.EstimatedOnDemandCost)
spprdEstimatedOnDemandCost = Lens.field @"estimatedOnDemandCost"
{-# INLINEABLE spprdEstimatedOnDemandCost #-}
{-# DEPRECATED estimatedOnDemandCost "Use generic-lens or generic-optics with 'estimatedOnDemandCost' instead"  #-}

-- | The estimated On-Demand costs you would expect with no additional commitment, based on your usage of the selected time period and the Savings Plans you own. 
--
-- /Note:/ Consider using 'estimatedOnDemandCostWithCurrentCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedOnDemandCostWithCurrentCommitment :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.EstimatedOnDemandCostWithCurrentCommitment)
spprdEstimatedOnDemandCostWithCurrentCommitment = Lens.field @"estimatedOnDemandCostWithCurrentCommitment"
{-# INLINEABLE spprdEstimatedOnDemandCostWithCurrentCommitment #-}
{-# DEPRECATED estimatedOnDemandCostWithCurrentCommitment "Use generic-lens or generic-optics with 'estimatedOnDemandCostWithCurrentCommitment' instead"  #-}

-- | The estimated return on investment based on the recommended Savings Plans purchased. This is calculated as @estimatedSavingsAmount@ / @estimatedSPCost@ *100.
--
-- /Note:/ Consider using 'estimatedROI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedROI :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.EstimatedROI)
spprdEstimatedROI = Lens.field @"estimatedROI"
{-# INLINEABLE spprdEstimatedROI #-}
{-# DEPRECATED estimatedROI "Use generic-lens or generic-optics with 'estimatedROI' instead"  #-}

-- | The cost of the recommended Savings Plans over the length of the lookback period.
--
-- /Note:/ Consider using 'estimatedSPCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedSPCost :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.EstimatedSPCost)
spprdEstimatedSPCost = Lens.field @"estimatedSPCost"
{-# INLINEABLE spprdEstimatedSPCost #-}
{-# DEPRECATED estimatedSPCost "Use generic-lens or generic-optics with 'estimatedSPCost' instead"  #-}

-- | The estimated savings amount based on the recommended Savings Plans over the length of the lookback period.
--
-- /Note:/ Consider using 'estimatedSavingsAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedSavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.EstimatedSavingsAmount)
spprdEstimatedSavingsAmount = Lens.field @"estimatedSavingsAmount"
{-# INLINEABLE spprdEstimatedSavingsAmount #-}
{-# DEPRECATED estimatedSavingsAmount "Use generic-lens or generic-optics with 'estimatedSavingsAmount' instead"  #-}

-- | The estimated savings percentage relative to the total cost of applicable On-Demand usage over the lookback period.
--
-- /Note:/ Consider using 'estimatedSavingsPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedSavingsPercentage :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.EstimatedSavingsPercentage)
spprdEstimatedSavingsPercentage = Lens.field @"estimatedSavingsPercentage"
{-# INLINEABLE spprdEstimatedSavingsPercentage #-}
{-# DEPRECATED estimatedSavingsPercentage "Use generic-lens or generic-optics with 'estimatedSavingsPercentage' instead"  #-}

-- | The recommended hourly commitment level for the Savings Plans type, and configuration based on the usage during the lookback period.
--
-- /Note:/ Consider using 'hourlyCommitmentToPurchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdHourlyCommitmentToPurchase :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.HourlyCommitmentToPurchase)
spprdHourlyCommitmentToPurchase = Lens.field @"hourlyCommitmentToPurchase"
{-# INLINEABLE spprdHourlyCommitmentToPurchase #-}
{-# DEPRECATED hourlyCommitmentToPurchase "Use generic-lens or generic-optics with 'hourlyCommitmentToPurchase' instead"  #-}

-- | Details for your recommended Savings Plans.
--
-- /Note:/ Consider using 'savingsPlansDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdSavingsPlansDetails :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.SavingsPlansDetails)
spprdSavingsPlansDetails = Lens.field @"savingsPlansDetails"
{-# INLINEABLE spprdSavingsPlansDetails #-}
{-# DEPRECATED savingsPlansDetails "Use generic-lens or generic-optics with 'savingsPlansDetails' instead"  #-}

-- | The upfront cost of the recommended Savings Plans, based on the selected payment option.
--
-- /Note:/ Consider using 'upfrontCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdUpfrontCost :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Types.UpfrontCost)
spprdUpfrontCost = Lens.field @"upfrontCost"
{-# INLINEABLE spprdUpfrontCost #-}
{-# DEPRECATED upfrontCost "Use generic-lens or generic-optics with 'upfrontCost' instead"  #-}

instance Core.FromJSON SavingsPlansPurchaseRecommendationDetail
         where
        parseJSON
          = Core.withObject "SavingsPlansPurchaseRecommendationDetail" Core.$
              \ x ->
                SavingsPlansPurchaseRecommendationDetail' Core.<$>
                  (x Core..:? "AccountId") Core.<*> x Core..:? "CurrencyCode"
                    Core.<*> x Core..:? "CurrentAverageHourlyOnDemandSpend"
                    Core.<*> x Core..:? "CurrentMaximumHourlyOnDemandSpend"
                    Core.<*> x Core..:? "CurrentMinimumHourlyOnDemandSpend"
                    Core.<*> x Core..:? "EstimatedAverageUtilization"
                    Core.<*> x Core..:? "EstimatedMonthlySavingsAmount"
                    Core.<*> x Core..:? "EstimatedOnDemandCost"
                    Core.<*> x Core..:? "EstimatedOnDemandCostWithCurrentCommitment"
                    Core.<*> x Core..:? "EstimatedROI"
                    Core.<*> x Core..:? "EstimatedSPCost"
                    Core.<*> x Core..:? "EstimatedSavingsAmount"
                    Core.<*> x Core..:? "EstimatedSavingsPercentage"
                    Core.<*> x Core..:? "HourlyCommitmentToPurchase"
                    Core.<*> x Core..:? "SavingsPlansDetails"
                    Core.<*> x Core..:? "UpfrontCost"
