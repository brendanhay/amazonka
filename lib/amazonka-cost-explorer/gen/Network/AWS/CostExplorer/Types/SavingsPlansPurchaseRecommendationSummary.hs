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
    spprsDailyCommitmentToPurchase,
    spprsEstimatedTotalCost,
    spprsEstimatedROI,
    spprsEstimatedSavingsAmount,
    spprsEstimatedMonthlySavingsAmount,
    spprsEstimatedOnDemandCostWithCurrentCommitment,
    spprsEstimatedSavingsPercentage,
    spprsTotalRecommendationCount,
    spprsCurrentOnDemandSpend,
    spprsHourlyCommitmentToPurchase,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary metrics for your Savings Plans Purchase Recommendations.
--
-- /See:/ 'mkSavingsPlansPurchaseRecommendationSummary' smart constructor.
data SavingsPlansPurchaseRecommendationSummary = SavingsPlansPurchaseRecommendationSummary'
  { currencyCode ::
      Lude.Maybe
        Lude.Text,
    dailyCommitmentToPurchase ::
      Lude.Maybe
        Lude.Text,
    estimatedTotalCost ::
      Lude.Maybe
        Lude.Text,
    estimatedROI ::
      Lude.Maybe
        Lude.Text,
    estimatedSavingsAmount ::
      Lude.Maybe
        Lude.Text,
    estimatedMonthlySavingsAmount ::
      Lude.Maybe
        Lude.Text,
    estimatedOnDemandCostWithCurrentCommitment ::
      Lude.Maybe
        Lude.Text,
    estimatedSavingsPercentage ::
      Lude.Maybe
        Lude.Text,
    totalRecommendationCount ::
      Lude.Maybe
        Lude.Text,
    currentOnDemandSpend ::
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

-- | Creates a value of 'SavingsPlansPurchaseRecommendationSummary' with the minimum fields required to make a request.
--
-- * 'currencyCode' - The currency code AWS used to generate the recommendations and present potential savings.
-- * 'currentOnDemandSpend' - The current total on demand spend of the applicable usage types over the lookback period.
-- * 'dailyCommitmentToPurchase' - The recommended Savings Plans cost on a daily (24 hourly) basis.
-- * 'estimatedMonthlySavingsAmount' - The estimated monthly savings amount, based on the recommended Savings Plans purchase.
-- * 'estimatedOnDemandCostWithCurrentCommitment' - The estimated On-Demand costs you would expect with no additional commitment, based on your usage of the selected time period and the Savings Plans you own.
-- * 'estimatedROI' - The estimated return on investment based on the recommended Savings Plans and estimated savings.
-- * 'estimatedSavingsAmount' - The estimated total savings over the lookback period, based on the purchase of the recommended Savings Plans.
-- * 'estimatedSavingsPercentage' - The estimated savings relative to the total cost of On-Demand usage, over the lookback period. This is calculated as @estimatedSavingsAmount@ / @CurrentOnDemandSpend@ *100.
-- * 'estimatedTotalCost' - The estimated total cost of the usage after purchasing the recommended Savings Plans. This is a sum of the cost of Savings Plans during this term, and the remaining On-Demand usage.
-- * 'hourlyCommitmentToPurchase' - The recommended hourly commitment based on the recommendation parameters.
-- * 'totalRecommendationCount' - The aggregate number of Savings Plans recommendations that exist for your account.
mkSavingsPlansPurchaseRecommendationSummary ::
  SavingsPlansPurchaseRecommendationSummary
mkSavingsPlansPurchaseRecommendationSummary =
  SavingsPlansPurchaseRecommendationSummary'
    { currencyCode =
        Lude.Nothing,
      dailyCommitmentToPurchase = Lude.Nothing,
      estimatedTotalCost = Lude.Nothing,
      estimatedROI = Lude.Nothing,
      estimatedSavingsAmount = Lude.Nothing,
      estimatedMonthlySavingsAmount = Lude.Nothing,
      estimatedOnDemandCostWithCurrentCommitment =
        Lude.Nothing,
      estimatedSavingsPercentage = Lude.Nothing,
      totalRecommendationCount = Lude.Nothing,
      currentOnDemandSpend = Lude.Nothing,
      hourlyCommitmentToPurchase = Lude.Nothing
    }

-- | The currency code AWS used to generate the recommendations and present potential savings.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsCurrencyCode :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Lude.Maybe Lude.Text)
spprsCurrencyCode = Lens.lens (currencyCode :: SavingsPlansPurchaseRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: SavingsPlansPurchaseRecommendationSummary)
{-# DEPRECATED spprsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The recommended Savings Plans cost on a daily (24 hourly) basis.
--
-- /Note:/ Consider using 'dailyCommitmentToPurchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsDailyCommitmentToPurchase :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Lude.Maybe Lude.Text)
spprsDailyCommitmentToPurchase = Lens.lens (dailyCommitmentToPurchase :: SavingsPlansPurchaseRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {dailyCommitmentToPurchase = a} :: SavingsPlansPurchaseRecommendationSummary)
{-# DEPRECATED spprsDailyCommitmentToPurchase "Use generic-lens or generic-optics with 'dailyCommitmentToPurchase' instead." #-}

-- | The estimated total cost of the usage after purchasing the recommended Savings Plans. This is a sum of the cost of Savings Plans during this term, and the remaining On-Demand usage.
--
-- /Note:/ Consider using 'estimatedTotalCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsEstimatedTotalCost :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Lude.Maybe Lude.Text)
spprsEstimatedTotalCost = Lens.lens (estimatedTotalCost :: SavingsPlansPurchaseRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {estimatedTotalCost = a} :: SavingsPlansPurchaseRecommendationSummary)
{-# DEPRECATED spprsEstimatedTotalCost "Use generic-lens or generic-optics with 'estimatedTotalCost' instead." #-}

-- | The estimated return on investment based on the recommended Savings Plans and estimated savings.
--
-- /Note:/ Consider using 'estimatedROI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsEstimatedROI :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Lude.Maybe Lude.Text)
spprsEstimatedROI = Lens.lens (estimatedROI :: SavingsPlansPurchaseRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {estimatedROI = a} :: SavingsPlansPurchaseRecommendationSummary)
{-# DEPRECATED spprsEstimatedROI "Use generic-lens or generic-optics with 'estimatedROI' instead." #-}

-- | The estimated total savings over the lookback period, based on the purchase of the recommended Savings Plans.
--
-- /Note:/ Consider using 'estimatedSavingsAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsEstimatedSavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Lude.Maybe Lude.Text)
spprsEstimatedSavingsAmount = Lens.lens (estimatedSavingsAmount :: SavingsPlansPurchaseRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {estimatedSavingsAmount = a} :: SavingsPlansPurchaseRecommendationSummary)
{-# DEPRECATED spprsEstimatedSavingsAmount "Use generic-lens or generic-optics with 'estimatedSavingsAmount' instead." #-}

-- | The estimated monthly savings amount, based on the recommended Savings Plans purchase.
--
-- /Note:/ Consider using 'estimatedMonthlySavingsAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsEstimatedMonthlySavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Lude.Maybe Lude.Text)
spprsEstimatedMonthlySavingsAmount = Lens.lens (estimatedMonthlySavingsAmount :: SavingsPlansPurchaseRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {estimatedMonthlySavingsAmount = a} :: SavingsPlansPurchaseRecommendationSummary)
{-# DEPRECATED spprsEstimatedMonthlySavingsAmount "Use generic-lens or generic-optics with 'estimatedMonthlySavingsAmount' instead." #-}

-- | The estimated On-Demand costs you would expect with no additional commitment, based on your usage of the selected time period and the Savings Plans you own.
--
-- /Note:/ Consider using 'estimatedOnDemandCostWithCurrentCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsEstimatedOnDemandCostWithCurrentCommitment :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Lude.Maybe Lude.Text)
spprsEstimatedOnDemandCostWithCurrentCommitment = Lens.lens (estimatedOnDemandCostWithCurrentCommitment :: SavingsPlansPurchaseRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {estimatedOnDemandCostWithCurrentCommitment = a} :: SavingsPlansPurchaseRecommendationSummary)
{-# DEPRECATED spprsEstimatedOnDemandCostWithCurrentCommitment "Use generic-lens or generic-optics with 'estimatedOnDemandCostWithCurrentCommitment' instead." #-}

-- | The estimated savings relative to the total cost of On-Demand usage, over the lookback period. This is calculated as @estimatedSavingsAmount@ / @CurrentOnDemandSpend@ *100.
--
-- /Note:/ Consider using 'estimatedSavingsPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsEstimatedSavingsPercentage :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Lude.Maybe Lude.Text)
spprsEstimatedSavingsPercentage = Lens.lens (estimatedSavingsPercentage :: SavingsPlansPurchaseRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {estimatedSavingsPercentage = a} :: SavingsPlansPurchaseRecommendationSummary)
{-# DEPRECATED spprsEstimatedSavingsPercentage "Use generic-lens or generic-optics with 'estimatedSavingsPercentage' instead." #-}

-- | The aggregate number of Savings Plans recommendations that exist for your account.
--
-- /Note:/ Consider using 'totalRecommendationCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsTotalRecommendationCount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Lude.Maybe Lude.Text)
spprsTotalRecommendationCount = Lens.lens (totalRecommendationCount :: SavingsPlansPurchaseRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {totalRecommendationCount = a} :: SavingsPlansPurchaseRecommendationSummary)
{-# DEPRECATED spprsTotalRecommendationCount "Use generic-lens or generic-optics with 'totalRecommendationCount' instead." #-}

-- | The current total on demand spend of the applicable usage types over the lookback period.
--
-- /Note:/ Consider using 'currentOnDemandSpend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsCurrentOnDemandSpend :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Lude.Maybe Lude.Text)
spprsCurrentOnDemandSpend = Lens.lens (currentOnDemandSpend :: SavingsPlansPurchaseRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {currentOnDemandSpend = a} :: SavingsPlansPurchaseRecommendationSummary)
{-# DEPRECATED spprsCurrentOnDemandSpend "Use generic-lens or generic-optics with 'currentOnDemandSpend' instead." #-}

-- | The recommended hourly commitment based on the recommendation parameters.
--
-- /Note:/ Consider using 'hourlyCommitmentToPurchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsHourlyCommitmentToPurchase :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Lude.Maybe Lude.Text)
spprsHourlyCommitmentToPurchase = Lens.lens (hourlyCommitmentToPurchase :: SavingsPlansPurchaseRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {hourlyCommitmentToPurchase = a} :: SavingsPlansPurchaseRecommendationSummary)
{-# DEPRECATED spprsHourlyCommitmentToPurchase "Use generic-lens or generic-optics with 'hourlyCommitmentToPurchase' instead." #-}

instance Lude.FromJSON SavingsPlansPurchaseRecommendationSummary where
  parseJSON =
    Lude.withObject
      "SavingsPlansPurchaseRecommendationSummary"
      ( \x ->
          SavingsPlansPurchaseRecommendationSummary'
            Lude.<$> (x Lude..:? "CurrencyCode")
            Lude.<*> (x Lude..:? "DailyCommitmentToPurchase")
            Lude.<*> (x Lude..:? "EstimatedTotalCost")
            Lude.<*> (x Lude..:? "EstimatedROI")
            Lude.<*> (x Lude..:? "EstimatedSavingsAmount")
            Lude.<*> (x Lude..:? "EstimatedMonthlySavingsAmount")
            Lude.<*> (x Lude..:? "EstimatedOnDemandCostWithCurrentCommitment")
            Lude.<*> (x Lude..:? "EstimatedSavingsPercentage")
            Lude.<*> (x Lude..:? "TotalRecommendationCount")
            Lude.<*> (x Lude..:? "CurrentOnDemandSpend")
            Lude.<*> (x Lude..:? "HourlyCommitmentToPurchase")
      )
