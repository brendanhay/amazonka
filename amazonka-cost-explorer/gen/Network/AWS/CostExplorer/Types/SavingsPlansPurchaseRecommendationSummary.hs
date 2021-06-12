{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Summary metrics for your Savings Plans Purchase Recommendations.
--
-- /See:/ 'newSavingsPlansPurchaseRecommendationSummary' smart constructor.
data SavingsPlansPurchaseRecommendationSummary = SavingsPlansPurchaseRecommendationSummary'
  { -- | The estimated monthly savings amount, based on the recommended Savings
    -- Plans purchase.
    estimatedMonthlySavingsAmount :: Core.Maybe Core.Text,
    -- | The estimated total savings over the lookback period, based on the
    -- purchase of the recommended Savings Plans.
    estimatedSavingsAmount :: Core.Maybe Core.Text,
    -- | The recommended hourly commitment based on the recommendation
    -- parameters.
    hourlyCommitmentToPurchase :: Core.Maybe Core.Text,
    -- | The estimated total cost of the usage after purchasing the recommended
    -- Savings Plans. This is a sum of the cost of Savings Plans during this
    -- term, and the remaining On-Demand usage.
    estimatedTotalCost :: Core.Maybe Core.Text,
    -- | The estimated savings relative to the total cost of On-Demand usage,
    -- over the lookback period. This is calculated as
    -- @estimatedSavingsAmount@\/ @CurrentOnDemandSpend@*100.
    estimatedSavingsPercentage :: Core.Maybe Core.Text,
    -- | The currency code AWS used to generate the recommendations and present
    -- potential savings.
    currencyCode :: Core.Maybe Core.Text,
    -- | The estimated On-Demand costs you would expect with no additional
    -- commitment, based on your usage of the selected time period and the
    -- Savings Plans you own.
    estimatedOnDemandCostWithCurrentCommitment :: Core.Maybe Core.Text,
    -- | The estimated return on investment based on the recommended Savings
    -- Plans and estimated savings.
    estimatedROI :: Core.Maybe Core.Text,
    -- | The current total on demand spend of the applicable usage types over the
    -- lookback period.
    currentOnDemandSpend :: Core.Maybe Core.Text,
    -- | The aggregate number of Savings Plans recommendations that exist for
    -- your account.
    totalRecommendationCount :: Core.Maybe Core.Text,
    -- | The recommended Savings Plans cost on a daily (24 hourly) basis.
    dailyCommitmentToPurchase :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SavingsPlansPurchaseRecommendationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedMonthlySavingsAmount', 'savingsPlansPurchaseRecommendationSummary_estimatedMonthlySavingsAmount' - The estimated monthly savings amount, based on the recommended Savings
-- Plans purchase.
--
-- 'estimatedSavingsAmount', 'savingsPlansPurchaseRecommendationSummary_estimatedSavingsAmount' - The estimated total savings over the lookback period, based on the
-- purchase of the recommended Savings Plans.
--
-- 'hourlyCommitmentToPurchase', 'savingsPlansPurchaseRecommendationSummary_hourlyCommitmentToPurchase' - The recommended hourly commitment based on the recommendation
-- parameters.
--
-- 'estimatedTotalCost', 'savingsPlansPurchaseRecommendationSummary_estimatedTotalCost' - The estimated total cost of the usage after purchasing the recommended
-- Savings Plans. This is a sum of the cost of Savings Plans during this
-- term, and the remaining On-Demand usage.
--
-- 'estimatedSavingsPercentage', 'savingsPlansPurchaseRecommendationSummary_estimatedSavingsPercentage' - The estimated savings relative to the total cost of On-Demand usage,
-- over the lookback period. This is calculated as
-- @estimatedSavingsAmount@\/ @CurrentOnDemandSpend@*100.
--
-- 'currencyCode', 'savingsPlansPurchaseRecommendationSummary_currencyCode' - The currency code AWS used to generate the recommendations and present
-- potential savings.
--
-- 'estimatedOnDemandCostWithCurrentCommitment', 'savingsPlansPurchaseRecommendationSummary_estimatedOnDemandCostWithCurrentCommitment' - The estimated On-Demand costs you would expect with no additional
-- commitment, based on your usage of the selected time period and the
-- Savings Plans you own.
--
-- 'estimatedROI', 'savingsPlansPurchaseRecommendationSummary_estimatedROI' - The estimated return on investment based on the recommended Savings
-- Plans and estimated savings.
--
-- 'currentOnDemandSpend', 'savingsPlansPurchaseRecommendationSummary_currentOnDemandSpend' - The current total on demand spend of the applicable usage types over the
-- lookback period.
--
-- 'totalRecommendationCount', 'savingsPlansPurchaseRecommendationSummary_totalRecommendationCount' - The aggregate number of Savings Plans recommendations that exist for
-- your account.
--
-- 'dailyCommitmentToPurchase', 'savingsPlansPurchaseRecommendationSummary_dailyCommitmentToPurchase' - The recommended Savings Plans cost on a daily (24 hourly) basis.
newSavingsPlansPurchaseRecommendationSummary ::
  SavingsPlansPurchaseRecommendationSummary
newSavingsPlansPurchaseRecommendationSummary =
  SavingsPlansPurchaseRecommendationSummary'
    { estimatedMonthlySavingsAmount =
        Core.Nothing,
      estimatedSavingsAmount =
        Core.Nothing,
      hourlyCommitmentToPurchase =
        Core.Nothing,
      estimatedTotalCost =
        Core.Nothing,
      estimatedSavingsPercentage =
        Core.Nothing,
      currencyCode = Core.Nothing,
      estimatedOnDemandCostWithCurrentCommitment =
        Core.Nothing,
      estimatedROI = Core.Nothing,
      currentOnDemandSpend =
        Core.Nothing,
      totalRecommendationCount =
        Core.Nothing,
      dailyCommitmentToPurchase =
        Core.Nothing
    }

-- | The estimated monthly savings amount, based on the recommended Savings
-- Plans purchase.
savingsPlansPurchaseRecommendationSummary_estimatedMonthlySavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationSummary_estimatedMonthlySavingsAmount = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedMonthlySavingsAmount} -> estimatedMonthlySavingsAmount) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedMonthlySavingsAmount = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated total savings over the lookback period, based on the
-- purchase of the recommended Savings Plans.
savingsPlansPurchaseRecommendationSummary_estimatedSavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationSummary_estimatedSavingsAmount = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedSavingsAmount} -> estimatedSavingsAmount) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedSavingsAmount = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The recommended hourly commitment based on the recommendation
-- parameters.
savingsPlansPurchaseRecommendationSummary_hourlyCommitmentToPurchase :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationSummary_hourlyCommitmentToPurchase = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {hourlyCommitmentToPurchase} -> hourlyCommitmentToPurchase) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {hourlyCommitmentToPurchase = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated total cost of the usage after purchasing the recommended
-- Savings Plans. This is a sum of the cost of Savings Plans during this
-- term, and the remaining On-Demand usage.
savingsPlansPurchaseRecommendationSummary_estimatedTotalCost :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationSummary_estimatedTotalCost = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedTotalCost} -> estimatedTotalCost) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedTotalCost = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated savings relative to the total cost of On-Demand usage,
-- over the lookback period. This is calculated as
-- @estimatedSavingsAmount@\/ @CurrentOnDemandSpend@*100.
savingsPlansPurchaseRecommendationSummary_estimatedSavingsPercentage :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationSummary_estimatedSavingsPercentage = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedSavingsPercentage} -> estimatedSavingsPercentage) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedSavingsPercentage = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The currency code AWS used to generate the recommendations and present
-- potential savings.
savingsPlansPurchaseRecommendationSummary_currencyCode :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationSummary_currencyCode = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {currencyCode} -> currencyCode) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {currencyCode = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated On-Demand costs you would expect with no additional
-- commitment, based on your usage of the selected time period and the
-- Savings Plans you own.
savingsPlansPurchaseRecommendationSummary_estimatedOnDemandCostWithCurrentCommitment :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationSummary_estimatedOnDemandCostWithCurrentCommitment = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedOnDemandCostWithCurrentCommitment} -> estimatedOnDemandCostWithCurrentCommitment) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedOnDemandCostWithCurrentCommitment = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated return on investment based on the recommended Savings
-- Plans and estimated savings.
savingsPlansPurchaseRecommendationSummary_estimatedROI :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationSummary_estimatedROI = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedROI} -> estimatedROI) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedROI = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The current total on demand spend of the applicable usage types over the
-- lookback period.
savingsPlansPurchaseRecommendationSummary_currentOnDemandSpend :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationSummary_currentOnDemandSpend = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {currentOnDemandSpend} -> currentOnDemandSpend) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {currentOnDemandSpend = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The aggregate number of Savings Plans recommendations that exist for
-- your account.
savingsPlansPurchaseRecommendationSummary_totalRecommendationCount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationSummary_totalRecommendationCount = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {totalRecommendationCount} -> totalRecommendationCount) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {totalRecommendationCount = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The recommended Savings Plans cost on a daily (24 hourly) basis.
savingsPlansPurchaseRecommendationSummary_dailyCommitmentToPurchase :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationSummary_dailyCommitmentToPurchase = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {dailyCommitmentToPurchase} -> dailyCommitmentToPurchase) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {dailyCommitmentToPurchase = a} :: SavingsPlansPurchaseRecommendationSummary)

instance
  Core.FromJSON
    SavingsPlansPurchaseRecommendationSummary
  where
  parseJSON =
    Core.withObject
      "SavingsPlansPurchaseRecommendationSummary"
      ( \x ->
          SavingsPlansPurchaseRecommendationSummary'
            Core.<$> (x Core..:? "EstimatedMonthlySavingsAmount")
            Core.<*> (x Core..:? "EstimatedSavingsAmount")
            Core.<*> (x Core..:? "HourlyCommitmentToPurchase")
            Core.<*> (x Core..:? "EstimatedTotalCost")
            Core.<*> (x Core..:? "EstimatedSavingsPercentage")
            Core.<*> (x Core..:? "CurrencyCode")
            Core.<*> ( x
                         Core..:? "EstimatedOnDemandCostWithCurrentCommitment"
                     )
            Core.<*> (x Core..:? "EstimatedROI")
            Core.<*> (x Core..:? "CurrentOnDemandSpend")
            Core.<*> (x Core..:? "TotalRecommendationCount")
            Core.<*> (x Core..:? "DailyCommitmentToPurchase")
      )

instance
  Core.Hashable
    SavingsPlansPurchaseRecommendationSummary

instance
  Core.NFData
    SavingsPlansPurchaseRecommendationSummary
