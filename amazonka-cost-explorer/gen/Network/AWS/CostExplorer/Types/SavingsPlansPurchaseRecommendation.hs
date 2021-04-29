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
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendation where

import Network.AWS.CostExplorer.Types.AccountScope
import Network.AWS.CostExplorer.Types.LookbackPeriodInDays
import Network.AWS.CostExplorer.Types.PaymentOption
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
import Network.AWS.CostExplorer.Types.SupportedSavingsPlansType
import Network.AWS.CostExplorer.Types.TermInYears
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains your request parameters, Savings Plan Recommendations Summary,
-- and Details.
--
-- /See:/ 'newSavingsPlansPurchaseRecommendation' smart constructor.
data SavingsPlansPurchaseRecommendation = SavingsPlansPurchaseRecommendation'
  { -- | The payment option used to generate the recommendation.
    paymentOption :: Prelude.Maybe PaymentOption,
    -- | Summary metrics for your Savings Plans Recommendations.
    savingsPlansPurchaseRecommendationSummary :: Prelude.Maybe SavingsPlansPurchaseRecommendationSummary,
    -- | The account scope that you want your recommendations for. Amazon Web
    -- Services calculates recommendations including the management account and
    -- member accounts if the value is set to @PAYER@. If the value is
    -- @LINKED@, recommendations are calculated for individual member accounts
    -- only.
    accountScope :: Prelude.Maybe AccountScope,
    -- | The Savings Plans recommendation term in years, used to generate the
    -- recommendation.
    termInYears :: Prelude.Maybe TermInYears,
    -- | Details for the Savings Plans we recommend that you purchase to cover
    -- existing Savings Plans eligible workloads.
    savingsPlansPurchaseRecommendationDetails :: Prelude.Maybe [SavingsPlansPurchaseRecommendationDetail],
    -- | The requested Savings Plans recommendation type.
    savingsPlansType :: Prelude.Maybe SupportedSavingsPlansType,
    -- | The lookback period in days, used to generate the recommendation.
    lookbackPeriodInDays :: Prelude.Maybe LookbackPeriodInDays
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      savingsPlansPurchaseRecommendationSummary =
        Prelude.Nothing,
      accountScope = Prelude.Nothing,
      termInYears = Prelude.Nothing,
      savingsPlansPurchaseRecommendationDetails =
        Prelude.Nothing,
      savingsPlansType = Prelude.Nothing,
      lookbackPeriodInDays = Prelude.Nothing
    }

-- | The payment option used to generate the recommendation.
savingsPlansPurchaseRecommendation_paymentOption :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe PaymentOption)
savingsPlansPurchaseRecommendation_paymentOption = Lens.lens (\SavingsPlansPurchaseRecommendation' {paymentOption} -> paymentOption) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {paymentOption = a} :: SavingsPlansPurchaseRecommendation)

-- | Summary metrics for your Savings Plans Recommendations.
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe SavingsPlansPurchaseRecommendationSummary)
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansPurchaseRecommendationSummary} -> savingsPlansPurchaseRecommendationSummary) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansPurchaseRecommendationSummary = a} :: SavingsPlansPurchaseRecommendation)

-- | The account scope that you want your recommendations for. Amazon Web
-- Services calculates recommendations including the management account and
-- member accounts if the value is set to @PAYER@. If the value is
-- @LINKED@, recommendations are calculated for individual member accounts
-- only.
savingsPlansPurchaseRecommendation_accountScope :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe AccountScope)
savingsPlansPurchaseRecommendation_accountScope = Lens.lens (\SavingsPlansPurchaseRecommendation' {accountScope} -> accountScope) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {accountScope = a} :: SavingsPlansPurchaseRecommendation)

-- | The Savings Plans recommendation term in years, used to generate the
-- recommendation.
savingsPlansPurchaseRecommendation_termInYears :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe TermInYears)
savingsPlansPurchaseRecommendation_termInYears = Lens.lens (\SavingsPlansPurchaseRecommendation' {termInYears} -> termInYears) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {termInYears = a} :: SavingsPlansPurchaseRecommendation)

-- | Details for the Savings Plans we recommend that you purchase to cover
-- existing Savings Plans eligible workloads.
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe [SavingsPlansPurchaseRecommendationDetail])
savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansPurchaseRecommendationDetails} -> savingsPlansPurchaseRecommendationDetails) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansPurchaseRecommendationDetails = a} :: SavingsPlansPurchaseRecommendation) Prelude.. Lens.mapping Prelude._Coerce

-- | The requested Savings Plans recommendation type.
savingsPlansPurchaseRecommendation_savingsPlansType :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe SupportedSavingsPlansType)
savingsPlansPurchaseRecommendation_savingsPlansType = Lens.lens (\SavingsPlansPurchaseRecommendation' {savingsPlansType} -> savingsPlansType) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {savingsPlansType = a} :: SavingsPlansPurchaseRecommendation)

-- | The lookback period in days, used to generate the recommendation.
savingsPlansPurchaseRecommendation_lookbackPeriodInDays :: Lens.Lens' SavingsPlansPurchaseRecommendation (Prelude.Maybe LookbackPeriodInDays)
savingsPlansPurchaseRecommendation_lookbackPeriodInDays = Lens.lens (\SavingsPlansPurchaseRecommendation' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@SavingsPlansPurchaseRecommendation' {} a -> s {lookbackPeriodInDays = a} :: SavingsPlansPurchaseRecommendation)

instance
  Prelude.FromJSON
    SavingsPlansPurchaseRecommendation
  where
  parseJSON =
    Prelude.withObject
      "SavingsPlansPurchaseRecommendation"
      ( \x ->
          SavingsPlansPurchaseRecommendation'
            Prelude.<$> (x Prelude..:? "PaymentOption")
            Prelude.<*> ( x
                            Prelude..:? "SavingsPlansPurchaseRecommendationSummary"
                        )
            Prelude.<*> (x Prelude..:? "AccountScope")
            Prelude.<*> (x Prelude..:? "TermInYears")
            Prelude.<*> ( x
                            Prelude..:? "SavingsPlansPurchaseRecommendationDetails"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "SavingsPlansType")
            Prelude.<*> (x Prelude..:? "LookbackPeriodInDays")
      )

instance
  Prelude.Hashable
    SavingsPlansPurchaseRecommendation

instance
  Prelude.NFData
    SavingsPlansPurchaseRecommendation
