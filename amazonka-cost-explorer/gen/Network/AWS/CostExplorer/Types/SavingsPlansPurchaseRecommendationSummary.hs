{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summary metrics for your Savings Plans Purchase Recommendations.
--
-- /See:/ 'newSavingsPlansPurchaseRecommendationSummary' smart constructor.
data SavingsPlansPurchaseRecommendationSummary = SavingsPlansPurchaseRecommendationSummary'
  { -- | The estimated monthly savings amount, based on the recommended Savings
    -- Plans purchase.
    estimatedMonthlySavingsAmount :: Prelude.Maybe Prelude.Text,
    -- | The estimated total savings over the lookback period, based on the
    -- purchase of the recommended Savings Plans.
    estimatedSavingsAmount :: Prelude.Maybe Prelude.Text,
    -- | The recommended hourly commitment based on the recommendation
    -- parameters.
    hourlyCommitmentToPurchase :: Prelude.Maybe Prelude.Text,
    -- | The estimated total cost of the usage after purchasing the recommended
    -- Savings Plans. This is a sum of the cost of Savings Plans during this
    -- term, and the remaining On-Demand usage.
    estimatedTotalCost :: Prelude.Maybe Prelude.Text,
    -- | The estimated savings relative to the total cost of On-Demand usage,
    -- over the lookback period. This is calculated as
    -- @estimatedSavingsAmount@\/ @CurrentOnDemandSpend@*100.
    estimatedSavingsPercentage :: Prelude.Maybe Prelude.Text,
    -- | The currency code AWS used to generate the recommendations and present
    -- potential savings.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The estimated On-Demand costs you would expect with no additional
    -- commitment, based on your usage of the selected time period and the
    -- Savings Plans you own.
    estimatedOnDemandCostWithCurrentCommitment :: Prelude.Maybe Prelude.Text,
    -- | The estimated return on investment based on the recommended Savings
    -- Plans and estimated savings.
    estimatedROI :: Prelude.Maybe Prelude.Text,
    -- | The current total on demand spend of the applicable usage types over the
    -- lookback period.
    currentOnDemandSpend :: Prelude.Maybe Prelude.Text,
    -- | The aggregate number of Savings Plans recommendations that exist for
    -- your account.
    totalRecommendationCount :: Prelude.Maybe Prelude.Text,
    -- | The recommended Savings Plans cost on a daily (24 hourly) basis.
    dailyCommitmentToPurchase :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      estimatedSavingsAmount =
        Prelude.Nothing,
      hourlyCommitmentToPurchase =
        Prelude.Nothing,
      estimatedTotalCost =
        Prelude.Nothing,
      estimatedSavingsPercentage =
        Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      estimatedOnDemandCostWithCurrentCommitment =
        Prelude.Nothing,
      estimatedROI = Prelude.Nothing,
      currentOnDemandSpend =
        Prelude.Nothing,
      totalRecommendationCount =
        Prelude.Nothing,
      dailyCommitmentToPurchase =
        Prelude.Nothing
    }

-- | The estimated monthly savings amount, based on the recommended Savings
-- Plans purchase.
savingsPlansPurchaseRecommendationSummary_estimatedMonthlySavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_estimatedMonthlySavingsAmount = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedMonthlySavingsAmount} -> estimatedMonthlySavingsAmount) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedMonthlySavingsAmount = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated total savings over the lookback period, based on the
-- purchase of the recommended Savings Plans.
savingsPlansPurchaseRecommendationSummary_estimatedSavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_estimatedSavingsAmount = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedSavingsAmount} -> estimatedSavingsAmount) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedSavingsAmount = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The recommended hourly commitment based on the recommendation
-- parameters.
savingsPlansPurchaseRecommendationSummary_hourlyCommitmentToPurchase :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_hourlyCommitmentToPurchase = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {hourlyCommitmentToPurchase} -> hourlyCommitmentToPurchase) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {hourlyCommitmentToPurchase = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated total cost of the usage after purchasing the recommended
-- Savings Plans. This is a sum of the cost of Savings Plans during this
-- term, and the remaining On-Demand usage.
savingsPlansPurchaseRecommendationSummary_estimatedTotalCost :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_estimatedTotalCost = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedTotalCost} -> estimatedTotalCost) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedTotalCost = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated savings relative to the total cost of On-Demand usage,
-- over the lookback period. This is calculated as
-- @estimatedSavingsAmount@\/ @CurrentOnDemandSpend@*100.
savingsPlansPurchaseRecommendationSummary_estimatedSavingsPercentage :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_estimatedSavingsPercentage = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedSavingsPercentage} -> estimatedSavingsPercentage) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedSavingsPercentage = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The currency code AWS used to generate the recommendations and present
-- potential savings.
savingsPlansPurchaseRecommendationSummary_currencyCode :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_currencyCode = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {currencyCode} -> currencyCode) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {currencyCode = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated On-Demand costs you would expect with no additional
-- commitment, based on your usage of the selected time period and the
-- Savings Plans you own.
savingsPlansPurchaseRecommendationSummary_estimatedOnDemandCostWithCurrentCommitment :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_estimatedOnDemandCostWithCurrentCommitment = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedOnDemandCostWithCurrentCommitment} -> estimatedOnDemandCostWithCurrentCommitment) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedOnDemandCostWithCurrentCommitment = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated return on investment based on the recommended Savings
-- Plans and estimated savings.
savingsPlansPurchaseRecommendationSummary_estimatedROI :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_estimatedROI = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedROI} -> estimatedROI) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedROI = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The current total on demand spend of the applicable usage types over the
-- lookback period.
savingsPlansPurchaseRecommendationSummary_currentOnDemandSpend :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_currentOnDemandSpend = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {currentOnDemandSpend} -> currentOnDemandSpend) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {currentOnDemandSpend = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The aggregate number of Savings Plans recommendations that exist for
-- your account.
savingsPlansPurchaseRecommendationSummary_totalRecommendationCount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_totalRecommendationCount = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {totalRecommendationCount} -> totalRecommendationCount) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {totalRecommendationCount = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The recommended Savings Plans cost on a daily (24 hourly) basis.
savingsPlansPurchaseRecommendationSummary_dailyCommitmentToPurchase :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_dailyCommitmentToPurchase = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {dailyCommitmentToPurchase} -> dailyCommitmentToPurchase) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {dailyCommitmentToPurchase = a} :: SavingsPlansPurchaseRecommendationSummary)

instance
  Prelude.FromJSON
    SavingsPlansPurchaseRecommendationSummary
  where
  parseJSON =
    Prelude.withObject
      "SavingsPlansPurchaseRecommendationSummary"
      ( \x ->
          SavingsPlansPurchaseRecommendationSummary'
            Prelude.<$> (x Prelude..:? "EstimatedMonthlySavingsAmount")
              Prelude.<*> (x Prelude..:? "EstimatedSavingsAmount")
              Prelude.<*> (x Prelude..:? "HourlyCommitmentToPurchase")
              Prelude.<*> (x Prelude..:? "EstimatedTotalCost")
              Prelude.<*> (x Prelude..:? "EstimatedSavingsPercentage")
              Prelude.<*> (x Prelude..:? "CurrencyCode")
              Prelude.<*> ( x
                              Prelude..:? "EstimatedOnDemandCostWithCurrentCommitment"
                          )
              Prelude.<*> (x Prelude..:? "EstimatedROI")
              Prelude.<*> (x Prelude..:? "CurrentOnDemandSpend")
              Prelude.<*> (x Prelude..:? "TotalRecommendationCount")
              Prelude.<*> (x Prelude..:? "DailyCommitmentToPurchase")
      )

instance
  Prelude.Hashable
    SavingsPlansPurchaseRecommendationSummary

instance
  Prelude.NFData
    SavingsPlansPurchaseRecommendationSummary
