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
-- Module      : Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary metrics for your Savings Plans Purchase Recommendations.
--
-- /See:/ 'newSavingsPlansPurchaseRecommendationSummary' smart constructor.
data SavingsPlansPurchaseRecommendationSummary = SavingsPlansPurchaseRecommendationSummary'
  { -- | The currency code that Amazon Web Services used to generate the
    -- recommendations and present potential savings.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The current total on demand spend of the applicable usage types over the
    -- lookback period.
    currentOnDemandSpend :: Prelude.Maybe Prelude.Text,
    -- | The recommended Savings Plans cost on a daily (24 hourly) basis.
    dailyCommitmentToPurchase :: Prelude.Maybe Prelude.Text,
    -- | The estimated monthly savings amount that\'s based on the recommended
    -- Savings Plans purchase.
    estimatedMonthlySavingsAmount :: Prelude.Maybe Prelude.Text,
    -- | The estimated On-Demand costs you expect with no additional commitment.
    -- It\'s based on your usage of the selected time period and the Savings
    -- Plans you own.
    estimatedOnDemandCostWithCurrentCommitment :: Prelude.Maybe Prelude.Text,
    -- | The estimated return on investment that\'s based on the recommended
    -- Savings Plans and estimated savings.
    estimatedROI :: Prelude.Maybe Prelude.Text,
    -- | The estimated total savings over the lookback period, based on the
    -- purchase of the recommended Savings Plans.
    estimatedSavingsAmount :: Prelude.Maybe Prelude.Text,
    -- | The estimated savings relative to the total cost of On-Demand usage,
    -- over the lookback period. This is calculated as
    -- @estimatedSavingsAmount@\/ @CurrentOnDemandSpend@*100.
    estimatedSavingsPercentage :: Prelude.Maybe Prelude.Text,
    -- | The estimated total cost of the usage after purchasing the recommended
    -- Savings Plans. This is a sum of the cost of Savings Plans during this
    -- term, and the remaining On-Demand usage.
    estimatedTotalCost :: Prelude.Maybe Prelude.Text,
    -- | The recommended hourly commitment that\'s based on the recommendation
    -- parameters.
    hourlyCommitmentToPurchase :: Prelude.Maybe Prelude.Text,
    -- | The aggregate number of Savings Plans recommendations that exist for
    -- your account.
    totalRecommendationCount :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansPurchaseRecommendationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currencyCode', 'savingsPlansPurchaseRecommendationSummary_currencyCode' - The currency code that Amazon Web Services used to generate the
-- recommendations and present potential savings.
--
-- 'currentOnDemandSpend', 'savingsPlansPurchaseRecommendationSummary_currentOnDemandSpend' - The current total on demand spend of the applicable usage types over the
-- lookback period.
--
-- 'dailyCommitmentToPurchase', 'savingsPlansPurchaseRecommendationSummary_dailyCommitmentToPurchase' - The recommended Savings Plans cost on a daily (24 hourly) basis.
--
-- 'estimatedMonthlySavingsAmount', 'savingsPlansPurchaseRecommendationSummary_estimatedMonthlySavingsAmount' - The estimated monthly savings amount that\'s based on the recommended
-- Savings Plans purchase.
--
-- 'estimatedOnDemandCostWithCurrentCommitment', 'savingsPlansPurchaseRecommendationSummary_estimatedOnDemandCostWithCurrentCommitment' - The estimated On-Demand costs you expect with no additional commitment.
-- It\'s based on your usage of the selected time period and the Savings
-- Plans you own.
--
-- 'estimatedROI', 'savingsPlansPurchaseRecommendationSummary_estimatedROI' - The estimated return on investment that\'s based on the recommended
-- Savings Plans and estimated savings.
--
-- 'estimatedSavingsAmount', 'savingsPlansPurchaseRecommendationSummary_estimatedSavingsAmount' - The estimated total savings over the lookback period, based on the
-- purchase of the recommended Savings Plans.
--
-- 'estimatedSavingsPercentage', 'savingsPlansPurchaseRecommendationSummary_estimatedSavingsPercentage' - The estimated savings relative to the total cost of On-Demand usage,
-- over the lookback period. This is calculated as
-- @estimatedSavingsAmount@\/ @CurrentOnDemandSpend@*100.
--
-- 'estimatedTotalCost', 'savingsPlansPurchaseRecommendationSummary_estimatedTotalCost' - The estimated total cost of the usage after purchasing the recommended
-- Savings Plans. This is a sum of the cost of Savings Plans during this
-- term, and the remaining On-Demand usage.
--
-- 'hourlyCommitmentToPurchase', 'savingsPlansPurchaseRecommendationSummary_hourlyCommitmentToPurchase' - The recommended hourly commitment that\'s based on the recommendation
-- parameters.
--
-- 'totalRecommendationCount', 'savingsPlansPurchaseRecommendationSummary_totalRecommendationCount' - The aggregate number of Savings Plans recommendations that exist for
-- your account.
newSavingsPlansPurchaseRecommendationSummary ::
  SavingsPlansPurchaseRecommendationSummary
newSavingsPlansPurchaseRecommendationSummary =
  SavingsPlansPurchaseRecommendationSummary'
    { currencyCode =
        Prelude.Nothing,
      currentOnDemandSpend =
        Prelude.Nothing,
      dailyCommitmentToPurchase =
        Prelude.Nothing,
      estimatedMonthlySavingsAmount =
        Prelude.Nothing,
      estimatedOnDemandCostWithCurrentCommitment =
        Prelude.Nothing,
      estimatedROI = Prelude.Nothing,
      estimatedSavingsAmount =
        Prelude.Nothing,
      estimatedSavingsPercentage =
        Prelude.Nothing,
      estimatedTotalCost =
        Prelude.Nothing,
      hourlyCommitmentToPurchase =
        Prelude.Nothing,
      totalRecommendationCount =
        Prelude.Nothing
    }

-- | The currency code that Amazon Web Services used to generate the
-- recommendations and present potential savings.
savingsPlansPurchaseRecommendationSummary_currencyCode :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_currencyCode = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {currencyCode} -> currencyCode) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {currencyCode = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The current total on demand spend of the applicable usage types over the
-- lookback period.
savingsPlansPurchaseRecommendationSummary_currentOnDemandSpend :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_currentOnDemandSpend = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {currentOnDemandSpend} -> currentOnDemandSpend) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {currentOnDemandSpend = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The recommended Savings Plans cost on a daily (24 hourly) basis.
savingsPlansPurchaseRecommendationSummary_dailyCommitmentToPurchase :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_dailyCommitmentToPurchase = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {dailyCommitmentToPurchase} -> dailyCommitmentToPurchase) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {dailyCommitmentToPurchase = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated monthly savings amount that\'s based on the recommended
-- Savings Plans purchase.
savingsPlansPurchaseRecommendationSummary_estimatedMonthlySavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_estimatedMonthlySavingsAmount = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedMonthlySavingsAmount} -> estimatedMonthlySavingsAmount) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedMonthlySavingsAmount = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated On-Demand costs you expect with no additional commitment.
-- It\'s based on your usage of the selected time period and the Savings
-- Plans you own.
savingsPlansPurchaseRecommendationSummary_estimatedOnDemandCostWithCurrentCommitment :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_estimatedOnDemandCostWithCurrentCommitment = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedOnDemandCostWithCurrentCommitment} -> estimatedOnDemandCostWithCurrentCommitment) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedOnDemandCostWithCurrentCommitment = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated return on investment that\'s based on the recommended
-- Savings Plans and estimated savings.
savingsPlansPurchaseRecommendationSummary_estimatedROI :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_estimatedROI = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedROI} -> estimatedROI) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedROI = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated total savings over the lookback period, based on the
-- purchase of the recommended Savings Plans.
savingsPlansPurchaseRecommendationSummary_estimatedSavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_estimatedSavingsAmount = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedSavingsAmount} -> estimatedSavingsAmount) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedSavingsAmount = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated savings relative to the total cost of On-Demand usage,
-- over the lookback period. This is calculated as
-- @estimatedSavingsAmount@\/ @CurrentOnDemandSpend@*100.
savingsPlansPurchaseRecommendationSummary_estimatedSavingsPercentage :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_estimatedSavingsPercentage = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedSavingsPercentage} -> estimatedSavingsPercentage) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedSavingsPercentage = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The estimated total cost of the usage after purchasing the recommended
-- Savings Plans. This is a sum of the cost of Savings Plans during this
-- term, and the remaining On-Demand usage.
savingsPlansPurchaseRecommendationSummary_estimatedTotalCost :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_estimatedTotalCost = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {estimatedTotalCost} -> estimatedTotalCost) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {estimatedTotalCost = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The recommended hourly commitment that\'s based on the recommendation
-- parameters.
savingsPlansPurchaseRecommendationSummary_hourlyCommitmentToPurchase :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_hourlyCommitmentToPurchase = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {hourlyCommitmentToPurchase} -> hourlyCommitmentToPurchase) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {hourlyCommitmentToPurchase = a} :: SavingsPlansPurchaseRecommendationSummary)

-- | The aggregate number of Savings Plans recommendations that exist for
-- your account.
savingsPlansPurchaseRecommendationSummary_totalRecommendationCount :: Lens.Lens' SavingsPlansPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
savingsPlansPurchaseRecommendationSummary_totalRecommendationCount = Lens.lens (\SavingsPlansPurchaseRecommendationSummary' {totalRecommendationCount} -> totalRecommendationCount) (\s@SavingsPlansPurchaseRecommendationSummary' {} a -> s {totalRecommendationCount = a} :: SavingsPlansPurchaseRecommendationSummary)

instance
  Data.FromJSON
    SavingsPlansPurchaseRecommendationSummary
  where
  parseJSON =
    Data.withObject
      "SavingsPlansPurchaseRecommendationSummary"
      ( \x ->
          SavingsPlansPurchaseRecommendationSummary'
            Prelude.<$> (x Data..:? "CurrencyCode")
              Prelude.<*> (x Data..:? "CurrentOnDemandSpend")
              Prelude.<*> (x Data..:? "DailyCommitmentToPurchase")
              Prelude.<*> (x Data..:? "EstimatedMonthlySavingsAmount")
              Prelude.<*> ( x
                              Data..:? "EstimatedOnDemandCostWithCurrentCommitment"
                          )
              Prelude.<*> (x Data..:? "EstimatedROI")
              Prelude.<*> (x Data..:? "EstimatedSavingsAmount")
              Prelude.<*> (x Data..:? "EstimatedSavingsPercentage")
              Prelude.<*> (x Data..:? "EstimatedTotalCost")
              Prelude.<*> (x Data..:? "HourlyCommitmentToPurchase")
              Prelude.<*> (x Data..:? "TotalRecommendationCount")
      )

instance
  Prelude.Hashable
    SavingsPlansPurchaseRecommendationSummary
  where
  hashWithSalt
    _salt
    SavingsPlansPurchaseRecommendationSummary' {..} =
      _salt `Prelude.hashWithSalt` currencyCode
        `Prelude.hashWithSalt` currentOnDemandSpend
        `Prelude.hashWithSalt` dailyCommitmentToPurchase
        `Prelude.hashWithSalt` estimatedMonthlySavingsAmount
        `Prelude.hashWithSalt` estimatedOnDemandCostWithCurrentCommitment
        `Prelude.hashWithSalt` estimatedROI
        `Prelude.hashWithSalt` estimatedSavingsAmount
        `Prelude.hashWithSalt` estimatedSavingsPercentage
        `Prelude.hashWithSalt` estimatedTotalCost
        `Prelude.hashWithSalt` hourlyCommitmentToPurchase
        `Prelude.hashWithSalt` totalRecommendationCount

instance
  Prelude.NFData
    SavingsPlansPurchaseRecommendationSummary
  where
  rnf SavingsPlansPurchaseRecommendationSummary' {..} =
    Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf currentOnDemandSpend
      `Prelude.seq` Prelude.rnf dailyCommitmentToPurchase
      `Prelude.seq` Prelude.rnf estimatedMonthlySavingsAmount
      `Prelude.seq` Prelude.rnf
        estimatedOnDemandCostWithCurrentCommitment
      `Prelude.seq` Prelude.rnf estimatedROI
      `Prelude.seq` Prelude.rnf estimatedSavingsAmount
      `Prelude.seq` Prelude.rnf estimatedSavingsPercentage
      `Prelude.seq` Prelude.rnf estimatedTotalCost
      `Prelude.seq` Prelude.rnf hourlyCommitmentToPurchase
      `Prelude.seq` Prelude.rnf totalRecommendationCount
