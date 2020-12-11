-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
  ( SavingsPlansPurchaseRecommendationDetail (..),

    -- * Smart constructor
    mkSavingsPlansPurchaseRecommendationDetail,

    -- * Lenses
    spprdCurrencyCode,
    spprdCurrentAverageHourlyOnDemandSpend,
    spprdSavingsPlansDetails,
    spprdCurrentMinimumHourlyOnDemandSpend,
    spprdEstimatedROI,
    spprdCurrentMaximumHourlyOnDemandSpend,
    spprdEstimatedSavingsAmount,
    spprdAccountId,
    spprdEstimatedMonthlySavingsAmount,
    spprdEstimatedOnDemandCost,
    spprdEstimatedOnDemandCostWithCurrentCommitment,
    spprdUpfrontCost,
    spprdEstimatedSPCost,
    spprdEstimatedSavingsPercentage,
    spprdEstimatedAverageUtilization,
    spprdHourlyCommitmentToPurchase,
  )
where

import Network.AWS.CostExplorer.Types.SavingsPlansDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details for your recommended Savings Plans.
--
-- /See:/ 'mkSavingsPlansPurchaseRecommendationDetail' smart constructor.
data SavingsPlansPurchaseRecommendationDetail = SavingsPlansPurchaseRecommendationDetail'
  { currencyCode ::
      Lude.Maybe
        Lude.Text,
    currentAverageHourlyOnDemandSpend ::
      Lude.Maybe
        Lude.Text,
    savingsPlansDetails ::
      Lude.Maybe
        SavingsPlansDetails,
    currentMinimumHourlyOnDemandSpend ::
      Lude.Maybe
        Lude.Text,
    estimatedROI ::
      Lude.Maybe
        Lude.Text,
    currentMaximumHourlyOnDemandSpend ::
      Lude.Maybe
        Lude.Text,
    estimatedSavingsAmount ::
      Lude.Maybe
        Lude.Text,
    accountId ::
      Lude.Maybe
        Lude.Text,
    estimatedMonthlySavingsAmount ::
      Lude.Maybe
        Lude.Text,
    estimatedOnDemandCost ::
      Lude.Maybe
        Lude.Text,
    estimatedOnDemandCostWithCurrentCommitment ::
      Lude.Maybe
        Lude.Text,
    upfrontCost ::
      Lude.Maybe
        Lude.Text,
    estimatedSPCost ::
      Lude.Maybe
        Lude.Text,
    estimatedSavingsPercentage ::
      Lude.Maybe
        Lude.Text,
    estimatedAverageUtilization ::
      Lude.Maybe
        Lude.Text,
    hourlyCommitmentToPurchase ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SavingsPlansPurchaseRecommendationDetail' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountID@ the recommendation is generated for.
-- * 'currencyCode' - The currency code AWS used to generate the recommendations and present potential savings.
-- * 'currentAverageHourlyOnDemandSpend' - The average value of hourly On-Demand spend over the lookback period of the applicable usage type.
-- * 'currentMaximumHourlyOnDemandSpend' - The highest value of hourly On-Demand spend over the lookback period of the applicable usage type.
-- * 'currentMinimumHourlyOnDemandSpend' - The lowest value of hourly On-Demand spend over the lookback period of the applicable usage type.
-- * 'estimatedAverageUtilization' - The estimated utilization of the recommended Savings Plans.
-- * 'estimatedMonthlySavingsAmount' - The estimated monthly savings amount, based on the recommended Savings Plans.
-- * 'estimatedOnDemandCost' - The remaining On-Demand cost estimated to not be covered by the recommended Savings Plans, over the length of the lookback period.
-- * 'estimatedOnDemandCostWithCurrentCommitment' - The estimated On-Demand costs you would expect with no additional commitment, based on your usage of the selected time period and the Savings Plans you own.
-- * 'estimatedROI' - The estimated return on investment based on the recommended Savings Plans purchased. This is calculated as @estimatedSavingsAmount@ / @estimatedSPCost@ *100.
-- * 'estimatedSPCost' - The cost of the recommended Savings Plans over the length of the lookback period.
-- * 'estimatedSavingsAmount' - The estimated savings amount based on the recommended Savings Plans over the length of the lookback period.
-- * 'estimatedSavingsPercentage' - The estimated savings percentage relative to the total cost of applicable On-Demand usage over the lookback period.
-- * 'hourlyCommitmentToPurchase' - The recommended hourly commitment level for the Savings Plans type, and configuration based on the usage during the lookback period.
-- * 'savingsPlansDetails' - Details for your recommended Savings Plans.
-- * 'upfrontCost' - The upfront cost of the recommended Savings Plans, based on the selected payment option.
mkSavingsPlansPurchaseRecommendationDetail ::
  SavingsPlansPurchaseRecommendationDetail
mkSavingsPlansPurchaseRecommendationDetail =
  SavingsPlansPurchaseRecommendationDetail'
    { currencyCode =
        Lude.Nothing,
      currentAverageHourlyOnDemandSpend = Lude.Nothing,
      savingsPlansDetails = Lude.Nothing,
      currentMinimumHourlyOnDemandSpend = Lude.Nothing,
      estimatedROI = Lude.Nothing,
      currentMaximumHourlyOnDemandSpend = Lude.Nothing,
      estimatedSavingsAmount = Lude.Nothing,
      accountId = Lude.Nothing,
      estimatedMonthlySavingsAmount = Lude.Nothing,
      estimatedOnDemandCost = Lude.Nothing,
      estimatedOnDemandCostWithCurrentCommitment =
        Lude.Nothing,
      upfrontCost = Lude.Nothing,
      estimatedSPCost = Lude.Nothing,
      estimatedSavingsPercentage = Lude.Nothing,
      estimatedAverageUtilization = Lude.Nothing,
      hourlyCommitmentToPurchase = Lude.Nothing
    }

-- | The currency code AWS used to generate the recommendations and present potential savings.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdCurrencyCode :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdCurrencyCode = Lens.lens (currencyCode :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The average value of hourly On-Demand spend over the lookback period of the applicable usage type.
--
-- /Note:/ Consider using 'currentAverageHourlyOnDemandSpend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdCurrentAverageHourlyOnDemandSpend :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdCurrentAverageHourlyOnDemandSpend = Lens.lens (currentAverageHourlyOnDemandSpend :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {currentAverageHourlyOnDemandSpend = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdCurrentAverageHourlyOnDemandSpend "Use generic-lens or generic-optics with 'currentAverageHourlyOnDemandSpend' instead." #-}

-- | Details for your recommended Savings Plans.
--
-- /Note:/ Consider using 'savingsPlansDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdSavingsPlansDetails :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe SavingsPlansDetails)
spprdSavingsPlansDetails = Lens.lens (savingsPlansDetails :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe SavingsPlansDetails) (\s a -> s {savingsPlansDetails = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdSavingsPlansDetails "Use generic-lens or generic-optics with 'savingsPlansDetails' instead." #-}

-- | The lowest value of hourly On-Demand spend over the lookback period of the applicable usage type.
--
-- /Note:/ Consider using 'currentMinimumHourlyOnDemandSpend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdCurrentMinimumHourlyOnDemandSpend :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdCurrentMinimumHourlyOnDemandSpend = Lens.lens (currentMinimumHourlyOnDemandSpend :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {currentMinimumHourlyOnDemandSpend = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdCurrentMinimumHourlyOnDemandSpend "Use generic-lens or generic-optics with 'currentMinimumHourlyOnDemandSpend' instead." #-}

-- | The estimated return on investment based on the recommended Savings Plans purchased. This is calculated as @estimatedSavingsAmount@ / @estimatedSPCost@ *100.
--
-- /Note:/ Consider using 'estimatedROI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedROI :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdEstimatedROI = Lens.lens (estimatedROI :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {estimatedROI = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdEstimatedROI "Use generic-lens or generic-optics with 'estimatedROI' instead." #-}

-- | The highest value of hourly On-Demand spend over the lookback period of the applicable usage type.
--
-- /Note:/ Consider using 'currentMaximumHourlyOnDemandSpend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdCurrentMaximumHourlyOnDemandSpend :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdCurrentMaximumHourlyOnDemandSpend = Lens.lens (currentMaximumHourlyOnDemandSpend :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {currentMaximumHourlyOnDemandSpend = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdCurrentMaximumHourlyOnDemandSpend "Use generic-lens or generic-optics with 'currentMaximumHourlyOnDemandSpend' instead." #-}

-- | The estimated savings amount based on the recommended Savings Plans over the length of the lookback period.
--
-- /Note:/ Consider using 'estimatedSavingsAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedSavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdEstimatedSavingsAmount = Lens.lens (estimatedSavingsAmount :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {estimatedSavingsAmount = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdEstimatedSavingsAmount "Use generic-lens or generic-optics with 'estimatedSavingsAmount' instead." #-}

-- | The @AccountID@ the recommendation is generated for.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdAccountId :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdAccountId = Lens.lens (accountId :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The estimated monthly savings amount, based on the recommended Savings Plans.
--
-- /Note:/ Consider using 'estimatedMonthlySavingsAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedMonthlySavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdEstimatedMonthlySavingsAmount = Lens.lens (estimatedMonthlySavingsAmount :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {estimatedMonthlySavingsAmount = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdEstimatedMonthlySavingsAmount "Use generic-lens or generic-optics with 'estimatedMonthlySavingsAmount' instead." #-}

-- | The remaining On-Demand cost estimated to not be covered by the recommended Savings Plans, over the length of the lookback period.
--
-- /Note:/ Consider using 'estimatedOnDemandCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedOnDemandCost :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdEstimatedOnDemandCost = Lens.lens (estimatedOnDemandCost :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {estimatedOnDemandCost = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdEstimatedOnDemandCost "Use generic-lens or generic-optics with 'estimatedOnDemandCost' instead." #-}

-- | The estimated On-Demand costs you would expect with no additional commitment, based on your usage of the selected time period and the Savings Plans you own.
--
-- /Note:/ Consider using 'estimatedOnDemandCostWithCurrentCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedOnDemandCostWithCurrentCommitment :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdEstimatedOnDemandCostWithCurrentCommitment = Lens.lens (estimatedOnDemandCostWithCurrentCommitment :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {estimatedOnDemandCostWithCurrentCommitment = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdEstimatedOnDemandCostWithCurrentCommitment "Use generic-lens or generic-optics with 'estimatedOnDemandCostWithCurrentCommitment' instead." #-}

-- | The upfront cost of the recommended Savings Plans, based on the selected payment option.
--
-- /Note:/ Consider using 'upfrontCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdUpfrontCost :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdUpfrontCost = Lens.lens (upfrontCost :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {upfrontCost = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdUpfrontCost "Use generic-lens or generic-optics with 'upfrontCost' instead." #-}

-- | The cost of the recommended Savings Plans over the length of the lookback period.
--
-- /Note:/ Consider using 'estimatedSPCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedSPCost :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdEstimatedSPCost = Lens.lens (estimatedSPCost :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {estimatedSPCost = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdEstimatedSPCost "Use generic-lens or generic-optics with 'estimatedSPCost' instead." #-}

-- | The estimated savings percentage relative to the total cost of applicable On-Demand usage over the lookback period.
--
-- /Note:/ Consider using 'estimatedSavingsPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedSavingsPercentage :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdEstimatedSavingsPercentage = Lens.lens (estimatedSavingsPercentage :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {estimatedSavingsPercentage = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdEstimatedSavingsPercentage "Use generic-lens or generic-optics with 'estimatedSavingsPercentage' instead." #-}

-- | The estimated utilization of the recommended Savings Plans.
--
-- /Note:/ Consider using 'estimatedAverageUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdEstimatedAverageUtilization :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdEstimatedAverageUtilization = Lens.lens (estimatedAverageUtilization :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {estimatedAverageUtilization = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdEstimatedAverageUtilization "Use generic-lens or generic-optics with 'estimatedAverageUtilization' instead." #-}

-- | The recommended hourly commitment level for the Savings Plans type, and configuration based on the usage during the lookback period.
--
-- /Note:/ Consider using 'hourlyCommitmentToPurchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprdHourlyCommitmentToPurchase :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Lude.Maybe Lude.Text)
spprdHourlyCommitmentToPurchase = Lens.lens (hourlyCommitmentToPurchase :: SavingsPlansPurchaseRecommendationDetail -> Lude.Maybe Lude.Text) (\s a -> s {hourlyCommitmentToPurchase = a} :: SavingsPlansPurchaseRecommendationDetail)
{-# DEPRECATED spprdHourlyCommitmentToPurchase "Use generic-lens or generic-optics with 'hourlyCommitmentToPurchase' instead." #-}

instance Lude.FromJSON SavingsPlansPurchaseRecommendationDetail where
  parseJSON =
    Lude.withObject
      "SavingsPlansPurchaseRecommendationDetail"
      ( \x ->
          SavingsPlansPurchaseRecommendationDetail'
            Lude.<$> (x Lude..:? "CurrencyCode")
            Lude.<*> (x Lude..:? "CurrentAverageHourlyOnDemandSpend")
            Lude.<*> (x Lude..:? "SavingsPlansDetails")
            Lude.<*> (x Lude..:? "CurrentMinimumHourlyOnDemandSpend")
            Lude.<*> (x Lude..:? "EstimatedROI")
            Lude.<*> (x Lude..:? "CurrentMaximumHourlyOnDemandSpend")
            Lude.<*> (x Lude..:? "EstimatedSavingsAmount")
            Lude.<*> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..:? "EstimatedMonthlySavingsAmount")
            Lude.<*> (x Lude..:? "EstimatedOnDemandCost")
            Lude.<*> (x Lude..:? "EstimatedOnDemandCostWithCurrentCommitment")
            Lude.<*> (x Lude..:? "UpfrontCost")
            Lude.<*> (x Lude..:? "EstimatedSPCost")
            Lude.<*> (x Lude..:? "EstimatedSavingsPercentage")
            Lude.<*> (x Lude..:? "EstimatedAverageUtilization")
            Lude.<*> (x Lude..:? "HourlyCommitmentToPurchase")
      )
