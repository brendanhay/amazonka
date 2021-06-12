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
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.SavingsPlansDetails
import qualified Network.AWS.Lens as Lens

-- | Details for your recommended Savings Plans.
--
-- /See:/ 'newSavingsPlansPurchaseRecommendationDetail' smart constructor.
data SavingsPlansPurchaseRecommendationDetail = SavingsPlansPurchaseRecommendationDetail'
  { -- | The upfront cost of the recommended Savings Plans, based on the selected
    -- payment option.
    upfrontCost :: Core.Maybe Core.Text,
    -- | The remaining On-Demand cost estimated to not be covered by the
    -- recommended Savings Plans, over the length of the lookback period.
    estimatedOnDemandCost :: Core.Maybe Core.Text,
    -- | The @AccountID@ the recommendation is generated for.
    accountId :: Core.Maybe Core.Text,
    -- | The estimated monthly savings amount, based on the recommended Savings
    -- Plans.
    estimatedMonthlySavingsAmount :: Core.Maybe Core.Text,
    -- | The estimated savings amount based on the recommended Savings Plans over
    -- the length of the lookback period.
    estimatedSavingsAmount :: Core.Maybe Core.Text,
    -- | The highest value of hourly On-Demand spend over the lookback period of
    -- the applicable usage type.
    currentMaximumHourlyOnDemandSpend :: Core.Maybe Core.Text,
    -- | The recommended hourly commitment level for the Savings Plans type, and
    -- configuration based on the usage during the lookback period.
    hourlyCommitmentToPurchase :: Core.Maybe Core.Text,
    -- | The estimated utilization of the recommended Savings Plans.
    estimatedAverageUtilization :: Core.Maybe Core.Text,
    -- | The average value of hourly On-Demand spend over the lookback period of
    -- the applicable usage type.
    currentAverageHourlyOnDemandSpend :: Core.Maybe Core.Text,
    -- | The estimated savings percentage relative to the total cost of
    -- applicable On-Demand usage over the lookback period.
    estimatedSavingsPercentage :: Core.Maybe Core.Text,
    -- | Details for your recommended Savings Plans.
    savingsPlansDetails :: Core.Maybe SavingsPlansDetails,
    -- | The currency code AWS used to generate the recommendations and present
    -- potential savings.
    currencyCode :: Core.Maybe Core.Text,
    -- | The cost of the recommended Savings Plans over the length of the
    -- lookback period.
    estimatedSPCost :: Core.Maybe Core.Text,
    -- | The estimated On-Demand costs you would expect with no additional
    -- commitment, based on your usage of the selected time period and the
    -- Savings Plans you own.
    estimatedOnDemandCostWithCurrentCommitment :: Core.Maybe Core.Text,
    -- | The estimated return on investment based on the recommended Savings
    -- Plans purchased. This is calculated as @estimatedSavingsAmount@\/
    -- @estimatedSPCost@*100.
    estimatedROI :: Core.Maybe Core.Text,
    -- | The lowest value of hourly On-Demand spend over the lookback period of
    -- the applicable usage type.
    currentMinimumHourlyOnDemandSpend :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SavingsPlansPurchaseRecommendationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'upfrontCost', 'savingsPlansPurchaseRecommendationDetail_upfrontCost' - The upfront cost of the recommended Savings Plans, based on the selected
-- payment option.
--
-- 'estimatedOnDemandCost', 'savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCost' - The remaining On-Demand cost estimated to not be covered by the
-- recommended Savings Plans, over the length of the lookback period.
--
-- 'accountId', 'savingsPlansPurchaseRecommendationDetail_accountId' - The @AccountID@ the recommendation is generated for.
--
-- 'estimatedMonthlySavingsAmount', 'savingsPlansPurchaseRecommendationDetail_estimatedMonthlySavingsAmount' - The estimated monthly savings amount, based on the recommended Savings
-- Plans.
--
-- 'estimatedSavingsAmount', 'savingsPlansPurchaseRecommendationDetail_estimatedSavingsAmount' - The estimated savings amount based on the recommended Savings Plans over
-- the length of the lookback period.
--
-- 'currentMaximumHourlyOnDemandSpend', 'savingsPlansPurchaseRecommendationDetail_currentMaximumHourlyOnDemandSpend' - The highest value of hourly On-Demand spend over the lookback period of
-- the applicable usage type.
--
-- 'hourlyCommitmentToPurchase', 'savingsPlansPurchaseRecommendationDetail_hourlyCommitmentToPurchase' - The recommended hourly commitment level for the Savings Plans type, and
-- configuration based on the usage during the lookback period.
--
-- 'estimatedAverageUtilization', 'savingsPlansPurchaseRecommendationDetail_estimatedAverageUtilization' - The estimated utilization of the recommended Savings Plans.
--
-- 'currentAverageHourlyOnDemandSpend', 'savingsPlansPurchaseRecommendationDetail_currentAverageHourlyOnDemandSpend' - The average value of hourly On-Demand spend over the lookback period of
-- the applicable usage type.
--
-- 'estimatedSavingsPercentage', 'savingsPlansPurchaseRecommendationDetail_estimatedSavingsPercentage' - The estimated savings percentage relative to the total cost of
-- applicable On-Demand usage over the lookback period.
--
-- 'savingsPlansDetails', 'savingsPlansPurchaseRecommendationDetail_savingsPlansDetails' - Details for your recommended Savings Plans.
--
-- 'currencyCode', 'savingsPlansPurchaseRecommendationDetail_currencyCode' - The currency code AWS used to generate the recommendations and present
-- potential savings.
--
-- 'estimatedSPCost', 'savingsPlansPurchaseRecommendationDetail_estimatedSPCost' - The cost of the recommended Savings Plans over the length of the
-- lookback period.
--
-- 'estimatedOnDemandCostWithCurrentCommitment', 'savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCostWithCurrentCommitment' - The estimated On-Demand costs you would expect with no additional
-- commitment, based on your usage of the selected time period and the
-- Savings Plans you own.
--
-- 'estimatedROI', 'savingsPlansPurchaseRecommendationDetail_estimatedROI' - The estimated return on investment based on the recommended Savings
-- Plans purchased. This is calculated as @estimatedSavingsAmount@\/
-- @estimatedSPCost@*100.
--
-- 'currentMinimumHourlyOnDemandSpend', 'savingsPlansPurchaseRecommendationDetail_currentMinimumHourlyOnDemandSpend' - The lowest value of hourly On-Demand spend over the lookback period of
-- the applicable usage type.
newSavingsPlansPurchaseRecommendationDetail ::
  SavingsPlansPurchaseRecommendationDetail
newSavingsPlansPurchaseRecommendationDetail =
  SavingsPlansPurchaseRecommendationDetail'
    { upfrontCost =
        Core.Nothing,
      estimatedOnDemandCost =
        Core.Nothing,
      accountId = Core.Nothing,
      estimatedMonthlySavingsAmount =
        Core.Nothing,
      estimatedSavingsAmount =
        Core.Nothing,
      currentMaximumHourlyOnDemandSpend =
        Core.Nothing,
      hourlyCommitmentToPurchase =
        Core.Nothing,
      estimatedAverageUtilization =
        Core.Nothing,
      currentAverageHourlyOnDemandSpend =
        Core.Nothing,
      estimatedSavingsPercentage =
        Core.Nothing,
      savingsPlansDetails =
        Core.Nothing,
      currencyCode = Core.Nothing,
      estimatedSPCost = Core.Nothing,
      estimatedOnDemandCostWithCurrentCommitment =
        Core.Nothing,
      estimatedROI = Core.Nothing,
      currentMinimumHourlyOnDemandSpend =
        Core.Nothing
    }

-- | The upfront cost of the recommended Savings Plans, based on the selected
-- payment option.
savingsPlansPurchaseRecommendationDetail_upfrontCost :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_upfrontCost = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {upfrontCost} -> upfrontCost) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {upfrontCost = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | The remaining On-Demand cost estimated to not be covered by the
-- recommended Savings Plans, over the length of the lookback period.
savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCost :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCost = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {estimatedOnDemandCost} -> estimatedOnDemandCost) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {estimatedOnDemandCost = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | The @AccountID@ the recommendation is generated for.
savingsPlansPurchaseRecommendationDetail_accountId :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_accountId = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {accountId} -> accountId) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {accountId = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | The estimated monthly savings amount, based on the recommended Savings
-- Plans.
savingsPlansPurchaseRecommendationDetail_estimatedMonthlySavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_estimatedMonthlySavingsAmount = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {estimatedMonthlySavingsAmount} -> estimatedMonthlySavingsAmount) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {estimatedMonthlySavingsAmount = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | The estimated savings amount based on the recommended Savings Plans over
-- the length of the lookback period.
savingsPlansPurchaseRecommendationDetail_estimatedSavingsAmount :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_estimatedSavingsAmount = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {estimatedSavingsAmount} -> estimatedSavingsAmount) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {estimatedSavingsAmount = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | The highest value of hourly On-Demand spend over the lookback period of
-- the applicable usage type.
savingsPlansPurchaseRecommendationDetail_currentMaximumHourlyOnDemandSpend :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_currentMaximumHourlyOnDemandSpend = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {currentMaximumHourlyOnDemandSpend} -> currentMaximumHourlyOnDemandSpend) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {currentMaximumHourlyOnDemandSpend = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | The recommended hourly commitment level for the Savings Plans type, and
-- configuration based on the usage during the lookback period.
savingsPlansPurchaseRecommendationDetail_hourlyCommitmentToPurchase :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_hourlyCommitmentToPurchase = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {hourlyCommitmentToPurchase} -> hourlyCommitmentToPurchase) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {hourlyCommitmentToPurchase = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | The estimated utilization of the recommended Savings Plans.
savingsPlansPurchaseRecommendationDetail_estimatedAverageUtilization :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_estimatedAverageUtilization = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {estimatedAverageUtilization} -> estimatedAverageUtilization) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {estimatedAverageUtilization = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | The average value of hourly On-Demand spend over the lookback period of
-- the applicable usage type.
savingsPlansPurchaseRecommendationDetail_currentAverageHourlyOnDemandSpend :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_currentAverageHourlyOnDemandSpend = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {currentAverageHourlyOnDemandSpend} -> currentAverageHourlyOnDemandSpend) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {currentAverageHourlyOnDemandSpend = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | The estimated savings percentage relative to the total cost of
-- applicable On-Demand usage over the lookback period.
savingsPlansPurchaseRecommendationDetail_estimatedSavingsPercentage :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_estimatedSavingsPercentage = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {estimatedSavingsPercentage} -> estimatedSavingsPercentage) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {estimatedSavingsPercentage = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | Details for your recommended Savings Plans.
savingsPlansPurchaseRecommendationDetail_savingsPlansDetails :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe SavingsPlansDetails)
savingsPlansPurchaseRecommendationDetail_savingsPlansDetails = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {savingsPlansDetails} -> savingsPlansDetails) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {savingsPlansDetails = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | The currency code AWS used to generate the recommendations and present
-- potential savings.
savingsPlansPurchaseRecommendationDetail_currencyCode :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_currencyCode = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {currencyCode} -> currencyCode) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {currencyCode = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | The cost of the recommended Savings Plans over the length of the
-- lookback period.
savingsPlansPurchaseRecommendationDetail_estimatedSPCost :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_estimatedSPCost = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {estimatedSPCost} -> estimatedSPCost) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {estimatedSPCost = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | The estimated On-Demand costs you would expect with no additional
-- commitment, based on your usage of the selected time period and the
-- Savings Plans you own.
savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCostWithCurrentCommitment :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCostWithCurrentCommitment = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {estimatedOnDemandCostWithCurrentCommitment} -> estimatedOnDemandCostWithCurrentCommitment) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {estimatedOnDemandCostWithCurrentCommitment = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | The estimated return on investment based on the recommended Savings
-- Plans purchased. This is calculated as @estimatedSavingsAmount@\/
-- @estimatedSPCost@*100.
savingsPlansPurchaseRecommendationDetail_estimatedROI :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_estimatedROI = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {estimatedROI} -> estimatedROI) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {estimatedROI = a} :: SavingsPlansPurchaseRecommendationDetail)

-- | The lowest value of hourly On-Demand spend over the lookback period of
-- the applicable usage type.
savingsPlansPurchaseRecommendationDetail_currentMinimumHourlyOnDemandSpend :: Lens.Lens' SavingsPlansPurchaseRecommendationDetail (Core.Maybe Core.Text)
savingsPlansPurchaseRecommendationDetail_currentMinimumHourlyOnDemandSpend = Lens.lens (\SavingsPlansPurchaseRecommendationDetail' {currentMinimumHourlyOnDemandSpend} -> currentMinimumHourlyOnDemandSpend) (\s@SavingsPlansPurchaseRecommendationDetail' {} a -> s {currentMinimumHourlyOnDemandSpend = a} :: SavingsPlansPurchaseRecommendationDetail)

instance
  Core.FromJSON
    SavingsPlansPurchaseRecommendationDetail
  where
  parseJSON =
    Core.withObject
      "SavingsPlansPurchaseRecommendationDetail"
      ( \x ->
          SavingsPlansPurchaseRecommendationDetail'
            Core.<$> (x Core..:? "UpfrontCost")
            Core.<*> (x Core..:? "EstimatedOnDemandCost")
            Core.<*> (x Core..:? "AccountId")
            Core.<*> (x Core..:? "EstimatedMonthlySavingsAmount")
            Core.<*> (x Core..:? "EstimatedSavingsAmount")
            Core.<*> (x Core..:? "CurrentMaximumHourlyOnDemandSpend")
            Core.<*> (x Core..:? "HourlyCommitmentToPurchase")
            Core.<*> (x Core..:? "EstimatedAverageUtilization")
            Core.<*> (x Core..:? "CurrentAverageHourlyOnDemandSpend")
            Core.<*> (x Core..:? "EstimatedSavingsPercentage")
            Core.<*> (x Core..:? "SavingsPlansDetails")
            Core.<*> (x Core..:? "CurrencyCode")
            Core.<*> (x Core..:? "EstimatedSPCost")
            Core.<*> ( x
                         Core..:? "EstimatedOnDemandCostWithCurrentCommitment"
                     )
            Core.<*> (x Core..:? "EstimatedROI")
            Core.<*> (x Core..:? "CurrentMinimumHourlyOnDemandSpend")
      )

instance
  Core.Hashable
    SavingsPlansPurchaseRecommendationDetail

instance
  Core.NFData
    SavingsPlansPurchaseRecommendationDetail
