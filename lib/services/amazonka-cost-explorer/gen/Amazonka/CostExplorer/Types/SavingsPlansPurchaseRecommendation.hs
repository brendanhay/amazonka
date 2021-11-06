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
-- Module      : Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendation where

import qualified Amazonka.Core as Core
import Amazonka.CostExplorer.Types.AccountScope
import Amazonka.CostExplorer.Types.LookbackPeriodInDays
import Amazonka.CostExplorer.Types.PaymentOption
import Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
import Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
import Amazonka.CostExplorer.Types.SupportedSavingsPlansType
import Amazonka.CostExplorer.Types.TermInYears
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains your request parameters, Savings Plan Recommendations Summary,
-- and Details.
--
-- /See:/ 'newSavingsPlansPurchaseRecommendation' smart constructor.
data SavingsPlansPurchaseRecommendation = SavingsPlansPurchaseRecommendation'
  { -- | Details for the Savings Plans we recommend that you purchase to cover
    -- existing Savings Plans eligible workloads.
    savingsPlansPurchaseRecommendationDetails :: Prelude.Maybe [SavingsPlansPurchaseRecommendationDetail],
    -- | The Savings Plans recommendation term in years. It\'s used to generate
    -- the recommendation.
    termInYears :: Prelude.Maybe TermInYears,
    -- | The account scope that you want your recommendations for. Amazon Web
    -- Services calculates recommendations that include the management account
    -- and member accounts if the value is set to @PAYER@. If the value is
    -- @LINKED@, recommendations are calculated for individual member accounts
    -- only.
    accountScope :: Prelude.Maybe AccountScope,
    -- | The requested Savings Plans recommendation type.
    savingsPlansType :: Prelude.Maybe SupportedSavingsPlansType,
    -- | The lookback period in days, used to generate the recommendation.
    lookbackPeriodInDays :: Prelude.Maybe LookbackPeriodInDays,
    -- | The payment option used to generate the recommendation.
    paymentOption :: Prelude.Maybe PaymentOption,
    -- | Summary metrics for your Savings Plans Recommendations.
    savingsPlansPurchaseRecommendationSummary :: Prelude.Maybe SavingsPlansPurchaseRecommendationSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansPurchaseRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'savingsPlansPurchaseRecommendationDetails', 'savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails' - Details for the Savings Plans we recommend that you purchase to cover
-- existing Savings Plans eligible workloads.
--
-- 'termInYears', 'savingsPlansPurchaseRecommendation_termInYears' - The Savings Plans recommendation term in years. It\'s used to generate
-- the recommendation.
--
-- 'accountScope', 'savingsPlansPurchaseRecommendation_accountScope' - The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations that include the management account
-- and member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
--
-- 'savingsPlansType', 'savingsPlansPurchaseRecommendation_savingsPlansType' - The requested Savings Plans recommendation type.
--
-- 'lookbackPeriodInDays', 'savingsPlansPurchaseRecommendation_lookbackPeriodInDays' - The lookback period in days, used to generate the recommendation.
--
-- 'paymentOption', 'savingsPlansPurchaseRecommendation_paymentOption' - The payment option used to generate the recommendation.
--
-- 'savingsPlansPurchaseRecommendationSummary', 'savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary' - Summary metrics for your Savings Plans Recommendations.
newSavingsPlansPurchaseRecommendation ::
  SavingsPlansPurchaseRecommendation
newSavingsPlansPurchaseRecommendation =
  SavingsPlansPurchaseRecommendation'
    { savingsPlansPurchaseRecommendationDetails =
        Prelude.Nothing,
      termInYears = Prelude.Nothing,
      accountScope = Prelude.Nothing,
      savingsPlansType = Prelude.Nothing,
      lookbackPeriodInDays = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      savingsPlansPurchaseRecommendationSummary =
        Prelude.Nothing
    }

-- | Details for the Savings Plans we recommend that you purchase to cover
-- existing Savings Plans eligible workloads.
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe [SavingsPlansPurchaseRecommendationDetail])
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansPurchaseRecommendationDetails} -> savingsPlansPurchaseRecommendationDetails) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansPurchaseRecommendationDetails = a} :: SavingsPlansPurchaseRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The Savings Plans recommendation term in years. It\'s used to generate
-- the recommendation.
savingsPlansPurchaseRecommendation_termInYears :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe TermInYears)
savingsPlansPurchaseRecommendation_termInYears = Lens.lens (\SavingsPlansPurchaseRecommendation' {termInYears} -> termInYears) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {termInYears = a} :: SavingsPlansPurchaseRecommendation)

-- | The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations that include the management account
-- and member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
savingsPlansPurchaseRecommendation_accountScope :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe AccountScope)
savingsPlansPurchaseRecommendation_accountScope = Lens.lens (\SavingsPlansPurchaseRecommendation' {accountScope} -> accountScope) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {accountScope = a} :: SavingsPlansPurchaseRecommendation)

-- | The requested Savings Plans recommendation type.
savingsPlansPurchaseRecommendation_savingsPlansType :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe SupportedSavingsPlansType)
savingsPlansPurchaseRecommendation_savingsPlansType = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansType} -> savingsPlansType) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansType = a} :: SavingsPlansPurchaseRecommendation)

-- | The lookback period in days, used to generate the recommendation.
savingsPlansPurchaseRecommendation_lookbackPeriodInDays :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe LookbackPeriodInDays)
savingsPlansPurchaseRecommendation_lookbackPeriodInDays = Lens.lens (\SavingsPlansPurchaseRecommendation' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {lookbackPeriodInDays = a} :: SavingsPlansPurchaseRecommendation)

-- | The payment option used to generate the recommendation.
savingsPlansPurchaseRecommendation_paymentOption :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe PaymentOption)
savingsPlansPurchaseRecommendation_paymentOption = Lens.lens (\SavingsPlansPurchaseRecommendation' {paymentOption} -> paymentOption) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {paymentOption = a} :: SavingsPlansPurchaseRecommendation)

-- | Summary metrics for your Savings Plans Recommendations.
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe SavingsPlansPurchaseRecommendationSummary)
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansPurchaseRecommendationSummary} -> savingsPlansPurchaseRecommendationSummary) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansPurchaseRecommendationSummary = a} :: SavingsPlansPurchaseRecommendation)

instance
  Core.FromJSON
    SavingsPlansPurchaseRecommendation
  where
  parseJSON =
    Core.withObject
      "SavingsPlansPurchaseRecommendation"
      ( \x ->
          SavingsPlansPurchaseRecommendation'
            Prelude.<$> ( x
                            Core..:? "SavingsPlansPurchaseRecommendationDetails"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "TermInYears")
            Prelude.<*> (x Core..:? "AccountScope")
            Prelude.<*> (x Core..:? "SavingsPlansType")
            Prelude.<*> (x Core..:? "LookbackPeriodInDays")
            Prelude.<*> (x Core..:? "PaymentOption")
            Prelude.<*> ( x
                            Core..:? "SavingsPlansPurchaseRecommendationSummary"
                        )
      )

instance
  Prelude.Hashable
    SavingsPlansPurchaseRecommendation

instance
  Prelude.NFData
    SavingsPlansPurchaseRecommendation
