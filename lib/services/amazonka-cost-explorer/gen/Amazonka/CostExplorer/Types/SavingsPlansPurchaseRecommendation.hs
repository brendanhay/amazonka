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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains your request parameters, Savings Plan Recommendations Summary,
-- and Details.
--
-- /See:/ 'newSavingsPlansPurchaseRecommendation' smart constructor.
data SavingsPlansPurchaseRecommendation = SavingsPlansPurchaseRecommendation'
  { -- | The account scope that you want your recommendations for. Amazon Web
    -- Services calculates recommendations that include the management account
    -- and member accounts if the value is set to @PAYER@. If the value is
    -- @LINKED@, recommendations are calculated for individual member accounts
    -- only.
    accountScope :: Prelude.Maybe AccountScope,
    -- | The lookback period in days that\'s used to generate the recommendation.
    lookbackPeriodInDays :: Prelude.Maybe LookbackPeriodInDays,
    -- | The payment option that\'s used to generate the recommendation.
    paymentOption :: Prelude.Maybe PaymentOption,
    -- | Details for the Savings Plans that we recommend that you purchase to
    -- cover existing Savings Plans eligible workloads.
    savingsPlansPurchaseRecommendationDetails :: Prelude.Maybe [SavingsPlansPurchaseRecommendationDetail],
    -- | Summary metrics for your Savings Plans Recommendations.
    savingsPlansPurchaseRecommendationSummary :: Prelude.Maybe SavingsPlansPurchaseRecommendationSummary,
    -- | The requested Savings Plans recommendation type.
    savingsPlansType :: Prelude.Maybe SupportedSavingsPlansType,
    -- | The Savings Plans recommendation term in years. It\'s used to generate
    -- the recommendation.
    termInYears :: Prelude.Maybe TermInYears
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
-- 'accountScope', 'savingsPlansPurchaseRecommendation_accountScope' - The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations that include the management account
-- and member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
--
-- 'lookbackPeriodInDays', 'savingsPlansPurchaseRecommendation_lookbackPeriodInDays' - The lookback period in days that\'s used to generate the recommendation.
--
-- 'paymentOption', 'savingsPlansPurchaseRecommendation_paymentOption' - The payment option that\'s used to generate the recommendation.
--
-- 'savingsPlansPurchaseRecommendationDetails', 'savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails' - Details for the Savings Plans that we recommend that you purchase to
-- cover existing Savings Plans eligible workloads.
--
-- 'savingsPlansPurchaseRecommendationSummary', 'savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary' - Summary metrics for your Savings Plans Recommendations.
--
-- 'savingsPlansType', 'savingsPlansPurchaseRecommendation_savingsPlansType' - The requested Savings Plans recommendation type.
--
-- 'termInYears', 'savingsPlansPurchaseRecommendation_termInYears' - The Savings Plans recommendation term in years. It\'s used to generate
-- the recommendation.
newSavingsPlansPurchaseRecommendation ::
  SavingsPlansPurchaseRecommendation
newSavingsPlansPurchaseRecommendation =
  SavingsPlansPurchaseRecommendation'
    { accountScope =
        Prelude.Nothing,
      lookbackPeriodInDays = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      savingsPlansPurchaseRecommendationDetails =
        Prelude.Nothing,
      savingsPlansPurchaseRecommendationSummary =
        Prelude.Nothing,
      savingsPlansType = Prelude.Nothing,
      termInYears = Prelude.Nothing
    }

-- | The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations that include the management account
-- and member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
savingsPlansPurchaseRecommendation_accountScope :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe AccountScope)
savingsPlansPurchaseRecommendation_accountScope = Lens.lens (\SavingsPlansPurchaseRecommendation' {accountScope} -> accountScope) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {accountScope = a} :: SavingsPlansPurchaseRecommendation)

-- | The lookback period in days that\'s used to generate the recommendation.
savingsPlansPurchaseRecommendation_lookbackPeriodInDays :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe LookbackPeriodInDays)
savingsPlansPurchaseRecommendation_lookbackPeriodInDays = Lens.lens (\SavingsPlansPurchaseRecommendation' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {lookbackPeriodInDays = a} :: SavingsPlansPurchaseRecommendation)

-- | The payment option that\'s used to generate the recommendation.
savingsPlansPurchaseRecommendation_paymentOption :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe PaymentOption)
savingsPlansPurchaseRecommendation_paymentOption = Lens.lens (\SavingsPlansPurchaseRecommendation' {paymentOption} -> paymentOption) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {paymentOption = a} :: SavingsPlansPurchaseRecommendation)

-- | Details for the Savings Plans that we recommend that you purchase to
-- cover existing Savings Plans eligible workloads.
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe [SavingsPlansPurchaseRecommendationDetail])
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansPurchaseRecommendationDetails} -> savingsPlansPurchaseRecommendationDetails) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansPurchaseRecommendationDetails = a} :: SavingsPlansPurchaseRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | Summary metrics for your Savings Plans Recommendations.
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe SavingsPlansPurchaseRecommendationSummary)
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansPurchaseRecommendationSummary} -> savingsPlansPurchaseRecommendationSummary) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansPurchaseRecommendationSummary = a} :: SavingsPlansPurchaseRecommendation)

-- | The requested Savings Plans recommendation type.
savingsPlansPurchaseRecommendation_savingsPlansType :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe SupportedSavingsPlansType)
savingsPlansPurchaseRecommendation_savingsPlansType = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansType} -> savingsPlansType) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansType = a} :: SavingsPlansPurchaseRecommendation)

-- | The Savings Plans recommendation term in years. It\'s used to generate
-- the recommendation.
savingsPlansPurchaseRecommendation_termInYears :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe TermInYears)
savingsPlansPurchaseRecommendation_termInYears = Lens.lens (\SavingsPlansPurchaseRecommendation' {termInYears} -> termInYears) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {termInYears = a} :: SavingsPlansPurchaseRecommendation)

instance
  Data.FromJSON
    SavingsPlansPurchaseRecommendation
  where
  parseJSON =
    Data.withObject
      "SavingsPlansPurchaseRecommendation"
      ( \x ->
          SavingsPlansPurchaseRecommendation'
            Prelude.<$> (x Data..:? "AccountScope")
            Prelude.<*> (x Data..:? "LookbackPeriodInDays")
            Prelude.<*> (x Data..:? "PaymentOption")
            Prelude.<*> ( x
                            Data..:? "SavingsPlansPurchaseRecommendationDetails"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "SavingsPlansPurchaseRecommendationSummary"
                        )
            Prelude.<*> (x Data..:? "SavingsPlansType")
            Prelude.<*> (x Data..:? "TermInYears")
      )

instance
  Prelude.Hashable
    SavingsPlansPurchaseRecommendation
  where
  hashWithSalt
    _salt
    SavingsPlansPurchaseRecommendation' {..} =
      _salt `Prelude.hashWithSalt` accountScope
        `Prelude.hashWithSalt` lookbackPeriodInDays
        `Prelude.hashWithSalt` paymentOption
        `Prelude.hashWithSalt` savingsPlansPurchaseRecommendationDetails
        `Prelude.hashWithSalt` savingsPlansPurchaseRecommendationSummary
        `Prelude.hashWithSalt` savingsPlansType
        `Prelude.hashWithSalt` termInYears

instance
  Prelude.NFData
    SavingsPlansPurchaseRecommendation
  where
  rnf SavingsPlansPurchaseRecommendation' {..} =
    Prelude.rnf accountScope
      `Prelude.seq` Prelude.rnf lookbackPeriodInDays
      `Prelude.seq` Prelude.rnf paymentOption
      `Prelude.seq` Prelude.rnf savingsPlansPurchaseRecommendationDetails
      `Prelude.seq` Prelude.rnf savingsPlansPurchaseRecommendationSummary
      `Prelude.seq` Prelude.rnf savingsPlansType
      `Prelude.seq` Prelude.rnf termInYears
