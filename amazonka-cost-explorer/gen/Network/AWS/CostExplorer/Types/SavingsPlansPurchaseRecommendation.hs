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
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendation where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.AccountScope
import Network.AWS.CostExplorer.Types.LookbackPeriodInDays
import Network.AWS.CostExplorer.Types.PaymentOption
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
import Network.AWS.CostExplorer.Types.SupportedSavingsPlansType
import Network.AWS.CostExplorer.Types.TermInYears
import qualified Network.AWS.Lens as Lens

-- | Contains your request parameters, Savings Plan Recommendations Summary,
-- and Details.
--
-- /See:/ 'newSavingsPlansPurchaseRecommendation' smart constructor.
data SavingsPlansPurchaseRecommendation = SavingsPlansPurchaseRecommendation'
  { -- | The payment option used to generate the recommendation.
    paymentOption :: Core.Maybe PaymentOption,
    -- | Summary metrics for your Savings Plans Recommendations.
    savingsPlansPurchaseRecommendationSummary :: Core.Maybe SavingsPlansPurchaseRecommendationSummary,
    -- | The account scope that you want your recommendations for. Amazon Web
    -- Services calculates recommendations including the management account and
    -- member accounts if the value is set to @PAYER@. If the value is
    -- @LINKED@, recommendations are calculated for individual member accounts
    -- only.
    accountScope :: Core.Maybe AccountScope,
    -- | The Savings Plans recommendation term in years, used to generate the
    -- recommendation.
    termInYears :: Core.Maybe TermInYears,
    -- | Details for the Savings Plans we recommend that you purchase to cover
    -- existing Savings Plans eligible workloads.
    savingsPlansPurchaseRecommendationDetails :: Core.Maybe [SavingsPlansPurchaseRecommendationDetail],
    -- | The requested Savings Plans recommendation type.
    savingsPlansType :: Core.Maybe SupportedSavingsPlansType,
    -- | The lookback period in days, used to generate the recommendation.
    lookbackPeriodInDays :: Core.Maybe LookbackPeriodInDays
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SavingsPlansPurchaseRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paymentOption', 'savingsPlansPurchaseRecommendation_paymentOption' - The payment option used to generate the recommendation.
--
-- 'savingsPlansPurchaseRecommendationSummary', 'savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary' - Summary metrics for your Savings Plans Recommendations.
--
-- 'accountScope', 'savingsPlansPurchaseRecommendation_accountScope' - The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations including the management account and
-- member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
--
-- 'termInYears', 'savingsPlansPurchaseRecommendation_termInYears' - The Savings Plans recommendation term in years, used to generate the
-- recommendation.
--
-- 'savingsPlansPurchaseRecommendationDetails', 'savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails' - Details for the Savings Plans we recommend that you purchase to cover
-- existing Savings Plans eligible workloads.
--
-- 'savingsPlansType', 'savingsPlansPurchaseRecommendation_savingsPlansType' - The requested Savings Plans recommendation type.
--
-- 'lookbackPeriodInDays', 'savingsPlansPurchaseRecommendation_lookbackPeriodInDays' - The lookback period in days, used to generate the recommendation.
newSavingsPlansPurchaseRecommendation ::
  SavingsPlansPurchaseRecommendation
newSavingsPlansPurchaseRecommendation =
  SavingsPlansPurchaseRecommendation'
    { paymentOption =
        Core.Nothing,
      savingsPlansPurchaseRecommendationSummary =
        Core.Nothing,
      accountScope = Core.Nothing,
      termInYears = Core.Nothing,
      savingsPlansPurchaseRecommendationDetails =
        Core.Nothing,
      savingsPlansType = Core.Nothing,
      lookbackPeriodInDays = Core.Nothing
    }

-- | The payment option used to generate the recommendation.
savingsPlansPurchaseRecommendation_paymentOption :: Lens.Lens' SavingsPlansPurchaseRecommendation (Core.Maybe PaymentOption)
savingsPlansPurchaseRecommendation_paymentOption = Lens.lens (\SavingsPlansPurchaseRecommendation' {paymentOption} -> paymentOption) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {paymentOption = a} :: SavingsPlansPurchaseRecommendation)

-- | Summary metrics for your Savings Plans Recommendations.
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary :: Lens.Lens' SavingsPlansPurchaseRecommendation (Core.Maybe SavingsPlansPurchaseRecommendationSummary)
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansPurchaseRecommendationSummary} -> savingsPlansPurchaseRecommendationSummary) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansPurchaseRecommendationSummary = a} :: SavingsPlansPurchaseRecommendation)

-- | The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations including the management account and
-- member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
savingsPlansPurchaseRecommendation_accountScope :: Lens.Lens' SavingsPlansPurchaseRecommendation (Core.Maybe AccountScope)
savingsPlansPurchaseRecommendation_accountScope = Lens.lens (\SavingsPlansPurchaseRecommendation' {accountScope} -> accountScope) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {accountScope = a} :: SavingsPlansPurchaseRecommendation)

-- | The Savings Plans recommendation term in years, used to generate the
-- recommendation.
savingsPlansPurchaseRecommendation_termInYears :: Lens.Lens' SavingsPlansPurchaseRecommendation (Core.Maybe TermInYears)
savingsPlansPurchaseRecommendation_termInYears = Lens.lens (\SavingsPlansPurchaseRecommendation' {termInYears} -> termInYears) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {termInYears = a} :: SavingsPlansPurchaseRecommendation)

-- | Details for the Savings Plans we recommend that you purchase to cover
-- existing Savings Plans eligible workloads.
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails :: Lens.Lens' SavingsPlansPurchaseRecommendation (Core.Maybe [SavingsPlansPurchaseRecommendationDetail])
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansPurchaseRecommendationDetails} -> savingsPlansPurchaseRecommendationDetails) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansPurchaseRecommendationDetails = a} :: SavingsPlansPurchaseRecommendation) Core.. Lens.mapping Lens._Coerce

-- | The requested Savings Plans recommendation type.
savingsPlansPurchaseRecommendation_savingsPlansType :: Lens.Lens' SavingsPlansPurchaseRecommendation (Core.Maybe SupportedSavingsPlansType)
savingsPlansPurchaseRecommendation_savingsPlansType = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansType} -> savingsPlansType) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansType = a} :: SavingsPlansPurchaseRecommendation)

-- | The lookback period in days, used to generate the recommendation.
savingsPlansPurchaseRecommendation_lookbackPeriodInDays :: Lens.Lens' SavingsPlansPurchaseRecommendation (Core.Maybe LookbackPeriodInDays)
savingsPlansPurchaseRecommendation_lookbackPeriodInDays = Lens.lens (\SavingsPlansPurchaseRecommendation' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {lookbackPeriodInDays = a} :: SavingsPlansPurchaseRecommendation)

instance
  Core.FromJSON
    SavingsPlansPurchaseRecommendation
  where
  parseJSON =
    Core.withObject
      "SavingsPlansPurchaseRecommendation"
      ( \x ->
          SavingsPlansPurchaseRecommendation'
            Core.<$> (x Core..:? "PaymentOption")
            Core.<*> ( x
                         Core..:? "SavingsPlansPurchaseRecommendationSummary"
                     )
            Core.<*> (x Core..:? "AccountScope")
            Core.<*> (x Core..:? "TermInYears")
            Core.<*> ( x
                         Core..:? "SavingsPlansPurchaseRecommendationDetails"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "SavingsPlansType")
            Core.<*> (x Core..:? "LookbackPeriodInDays")
      )

instance
  Core.Hashable
    SavingsPlansPurchaseRecommendation

instance
  Core.NFData
    SavingsPlansPurchaseRecommendation
