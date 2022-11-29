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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.AccountScope
import Amazonka.CostExplorer.Types.LookbackPeriodInDays
import Amazonka.CostExplorer.Types.PaymentOption
import Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
import Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
import Amazonka.CostExplorer.Types.SupportedSavingsPlansType
import Amazonka.CostExplorer.Types.TermInYears
import qualified Amazonka.Prelude as Prelude

-- | Contains your request parameters, Savings Plan Recommendations Summary,
-- and Details.
--
-- /See:/ 'newSavingsPlansPurchaseRecommendation' smart constructor.
data SavingsPlansPurchaseRecommendation = SavingsPlansPurchaseRecommendation'
  { -- | The requested Savings Plans recommendation type.
    savingsPlansType :: Prelude.Maybe SupportedSavingsPlansType,
    -- | Details for the Savings Plans that we recommend that you purchase to
    -- cover existing Savings Plans eligible workloads.
    savingsPlansPurchaseRecommendationDetails :: Prelude.Maybe [SavingsPlansPurchaseRecommendationDetail],
    -- | The lookback period in days that\'s used to generate the recommendation.
    lookbackPeriodInDays :: Prelude.Maybe LookbackPeriodInDays,
    -- | Summary metrics for your Savings Plans Recommendations.
    savingsPlansPurchaseRecommendationSummary :: Prelude.Maybe SavingsPlansPurchaseRecommendationSummary,
    -- | The Savings Plans recommendation term in years. It\'s used to generate
    -- the recommendation.
    termInYears :: Prelude.Maybe TermInYears,
    -- | The payment option that\'s used to generate the recommendation.
    paymentOption :: Prelude.Maybe PaymentOption,
    -- | The account scope that you want your recommendations for. Amazon Web
    -- Services calculates recommendations that include the management account
    -- and member accounts if the value is set to @PAYER@. If the value is
    -- @LINKED@, recommendations are calculated for individual member accounts
    -- only.
    accountScope :: Prelude.Maybe AccountScope
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
-- 'savingsPlansType', 'savingsPlansPurchaseRecommendation_savingsPlansType' - The requested Savings Plans recommendation type.
--
-- 'savingsPlansPurchaseRecommendationDetails', 'savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails' - Details for the Savings Plans that we recommend that you purchase to
-- cover existing Savings Plans eligible workloads.
--
-- 'lookbackPeriodInDays', 'savingsPlansPurchaseRecommendation_lookbackPeriodInDays' - The lookback period in days that\'s used to generate the recommendation.
--
-- 'savingsPlansPurchaseRecommendationSummary', 'savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary' - Summary metrics for your Savings Plans Recommendations.
--
-- 'termInYears', 'savingsPlansPurchaseRecommendation_termInYears' - The Savings Plans recommendation term in years. It\'s used to generate
-- the recommendation.
--
-- 'paymentOption', 'savingsPlansPurchaseRecommendation_paymentOption' - The payment option that\'s used to generate the recommendation.
--
-- 'accountScope', 'savingsPlansPurchaseRecommendation_accountScope' - The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations that include the management account
-- and member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
newSavingsPlansPurchaseRecommendation ::
  SavingsPlansPurchaseRecommendation
newSavingsPlansPurchaseRecommendation =
  SavingsPlansPurchaseRecommendation'
    { savingsPlansType =
        Prelude.Nothing,
      savingsPlansPurchaseRecommendationDetails =
        Prelude.Nothing,
      lookbackPeriodInDays = Prelude.Nothing,
      savingsPlansPurchaseRecommendationSummary =
        Prelude.Nothing,
      termInYears = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      accountScope = Prelude.Nothing
    }

-- | The requested Savings Plans recommendation type.
savingsPlansPurchaseRecommendation_savingsPlansType :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe SupportedSavingsPlansType)
savingsPlansPurchaseRecommendation_savingsPlansType = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansType} -> savingsPlansType) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansType = a} :: SavingsPlansPurchaseRecommendation)

-- | Details for the Savings Plans that we recommend that you purchase to
-- cover existing Savings Plans eligible workloads.
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe [SavingsPlansPurchaseRecommendationDetail])
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansPurchaseRecommendationDetails} -> savingsPlansPurchaseRecommendationDetails) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansPurchaseRecommendationDetails = a} :: SavingsPlansPurchaseRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The lookback period in days that\'s used to generate the recommendation.
savingsPlansPurchaseRecommendation_lookbackPeriodInDays :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe LookbackPeriodInDays)
savingsPlansPurchaseRecommendation_lookbackPeriodInDays = Lens.lens (\SavingsPlansPurchaseRecommendation' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {lookbackPeriodInDays = a} :: SavingsPlansPurchaseRecommendation)

-- | Summary metrics for your Savings Plans Recommendations.
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe SavingsPlansPurchaseRecommendationSummary)
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansPurchaseRecommendationSummary} -> savingsPlansPurchaseRecommendationSummary) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansPurchaseRecommendationSummary = a} :: SavingsPlansPurchaseRecommendation)

-- | The Savings Plans recommendation term in years. It\'s used to generate
-- the recommendation.
savingsPlansPurchaseRecommendation_termInYears :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe TermInYears)
savingsPlansPurchaseRecommendation_termInYears = Lens.lens (\SavingsPlansPurchaseRecommendation' {termInYears} -> termInYears) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {termInYears = a} :: SavingsPlansPurchaseRecommendation)

-- | The payment option that\'s used to generate the recommendation.
savingsPlansPurchaseRecommendation_paymentOption :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe PaymentOption)
savingsPlansPurchaseRecommendation_paymentOption = Lens.lens (\SavingsPlansPurchaseRecommendation' {paymentOption} -> paymentOption) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {paymentOption = a} :: SavingsPlansPurchaseRecommendation)

-- | The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations that include the management account
-- and member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
savingsPlansPurchaseRecommendation_accountScope :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe AccountScope)
savingsPlansPurchaseRecommendation_accountScope = Lens.lens (\SavingsPlansPurchaseRecommendation' {accountScope} -> accountScope) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {accountScope = a} :: SavingsPlansPurchaseRecommendation)

instance
  Core.FromJSON
    SavingsPlansPurchaseRecommendation
  where
  parseJSON =
    Core.withObject
      "SavingsPlansPurchaseRecommendation"
      ( \x ->
          SavingsPlansPurchaseRecommendation'
            Prelude.<$> (x Core..:? "SavingsPlansType")
            Prelude.<*> ( x
                            Core..:? "SavingsPlansPurchaseRecommendationDetails"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LookbackPeriodInDays")
            Prelude.<*> ( x
                            Core..:? "SavingsPlansPurchaseRecommendationSummary"
                        )
            Prelude.<*> (x Core..:? "TermInYears")
            Prelude.<*> (x Core..:? "PaymentOption")
            Prelude.<*> (x Core..:? "AccountScope")
      )

instance
  Prelude.Hashable
    SavingsPlansPurchaseRecommendation
  where
  hashWithSalt
    _salt
    SavingsPlansPurchaseRecommendation' {..} =
      _salt `Prelude.hashWithSalt` savingsPlansType
        `Prelude.hashWithSalt` savingsPlansPurchaseRecommendationDetails
        `Prelude.hashWithSalt` lookbackPeriodInDays
        `Prelude.hashWithSalt` savingsPlansPurchaseRecommendationSummary
        `Prelude.hashWithSalt` termInYears
        `Prelude.hashWithSalt` paymentOption
        `Prelude.hashWithSalt` accountScope

instance
  Prelude.NFData
    SavingsPlansPurchaseRecommendation
  where
  rnf SavingsPlansPurchaseRecommendation' {..} =
    Prelude.rnf savingsPlansType
      `Prelude.seq` Prelude.rnf savingsPlansPurchaseRecommendationDetails
      `Prelude.seq` Prelude.rnf lookbackPeriodInDays
      `Prelude.seq` Prelude.rnf savingsPlansPurchaseRecommendationSummary
      `Prelude.seq` Prelude.rnf termInYears
      `Prelude.seq` Prelude.rnf paymentOption
      `Prelude.seq` Prelude.rnf accountScope
