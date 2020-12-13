{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendation
  ( SavingsPlansPurchaseRecommendation (..),

    -- * Smart constructor
    mkSavingsPlansPurchaseRecommendation,

    -- * Lenses
    spprSavingsPlansPurchaseRecommendationDetails,
    spprTermInYears,
    spprAccountScope,
    spprSavingsPlansType,
    spprLookbackPeriodInDays,
    spprPaymentOption,
    spprSavingsPlansPurchaseRecommendationSummary,
  )
where

import Network.AWS.CostExplorer.Types.AccountScope
import Network.AWS.CostExplorer.Types.LookbackPeriodInDays
import Network.AWS.CostExplorer.Types.PaymentOption
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
import Network.AWS.CostExplorer.Types.SupportedSavingsPlansType
import Network.AWS.CostExplorer.Types.TermInYears
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains your request parameters, Savings Plan Recommendations Summary, and Details.
--
-- /See:/ 'mkSavingsPlansPurchaseRecommendation' smart constructor.
data SavingsPlansPurchaseRecommendation = SavingsPlansPurchaseRecommendation'
  { -- | Details for the Savings Plans we recommend that you purchase to cover existing Savings Plans eligible workloads.
    savingsPlansPurchaseRecommendationDetails :: Lude.Maybe [SavingsPlansPurchaseRecommendationDetail],
    -- | The Savings Plans recommendation term in years, used to generate the recommendation.
    termInYears :: Lude.Maybe TermInYears,
    -- | The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
    accountScope :: Lude.Maybe AccountScope,
    -- | The requested Savings Plans recommendation type.
    savingsPlansType :: Lude.Maybe SupportedSavingsPlansType,
    -- | The lookback period in days, used to generate the recommendation.
    lookbackPeriodInDays :: Lude.Maybe LookbackPeriodInDays,
    -- | The payment option used to generate the recommendation.
    paymentOption :: Lude.Maybe PaymentOption,
    -- | Summary metrics for your Savings Plans Recommendations.
    savingsPlansPurchaseRecommendationSummary :: Lude.Maybe SavingsPlansPurchaseRecommendationSummary
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SavingsPlansPurchaseRecommendation' with the minimum fields required to make a request.
--
-- * 'savingsPlansPurchaseRecommendationDetails' - Details for the Savings Plans we recommend that you purchase to cover existing Savings Plans eligible workloads.
-- * 'termInYears' - The Savings Plans recommendation term in years, used to generate the recommendation.
-- * 'accountScope' - The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
-- * 'savingsPlansType' - The requested Savings Plans recommendation type.
-- * 'lookbackPeriodInDays' - The lookback period in days, used to generate the recommendation.
-- * 'paymentOption' - The payment option used to generate the recommendation.
-- * 'savingsPlansPurchaseRecommendationSummary' - Summary metrics for your Savings Plans Recommendations.
mkSavingsPlansPurchaseRecommendation ::
  SavingsPlansPurchaseRecommendation
mkSavingsPlansPurchaseRecommendation =
  SavingsPlansPurchaseRecommendation'
    { savingsPlansPurchaseRecommendationDetails =
        Lude.Nothing,
      termInYears = Lude.Nothing,
      accountScope = Lude.Nothing,
      savingsPlansType = Lude.Nothing,
      lookbackPeriodInDays = Lude.Nothing,
      paymentOption = Lude.Nothing,
      savingsPlansPurchaseRecommendationSummary = Lude.Nothing
    }

-- | Details for the Savings Plans we recommend that you purchase to cover existing Savings Plans eligible workloads.
--
-- /Note:/ Consider using 'savingsPlansPurchaseRecommendationDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprSavingsPlansPurchaseRecommendationDetails :: Lens.Lens' SavingsPlansPurchaseRecommendation (Lude.Maybe [SavingsPlansPurchaseRecommendationDetail])
spprSavingsPlansPurchaseRecommendationDetails = Lens.lens (savingsPlansPurchaseRecommendationDetails :: SavingsPlansPurchaseRecommendation -> Lude.Maybe [SavingsPlansPurchaseRecommendationDetail]) (\s a -> s {savingsPlansPurchaseRecommendationDetails = a} :: SavingsPlansPurchaseRecommendation)
{-# DEPRECATED spprSavingsPlansPurchaseRecommendationDetails "Use generic-lens or generic-optics with 'savingsPlansPurchaseRecommendationDetails' instead." #-}

-- | The Savings Plans recommendation term in years, used to generate the recommendation.
--
-- /Note:/ Consider using 'termInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprTermInYears :: Lens.Lens' SavingsPlansPurchaseRecommendation (Lude.Maybe TermInYears)
spprTermInYears = Lens.lens (termInYears :: SavingsPlansPurchaseRecommendation -> Lude.Maybe TermInYears) (\s a -> s {termInYears = a} :: SavingsPlansPurchaseRecommendation)
{-# DEPRECATED spprTermInYears "Use generic-lens or generic-optics with 'termInYears' instead." #-}

-- | The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
--
-- /Note:/ Consider using 'accountScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprAccountScope :: Lens.Lens' SavingsPlansPurchaseRecommendation (Lude.Maybe AccountScope)
spprAccountScope = Lens.lens (accountScope :: SavingsPlansPurchaseRecommendation -> Lude.Maybe AccountScope) (\s a -> s {accountScope = a} :: SavingsPlansPurchaseRecommendation)
{-# DEPRECATED spprAccountScope "Use generic-lens or generic-optics with 'accountScope' instead." #-}

-- | The requested Savings Plans recommendation type.
--
-- /Note:/ Consider using 'savingsPlansType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprSavingsPlansType :: Lens.Lens' SavingsPlansPurchaseRecommendation (Lude.Maybe SupportedSavingsPlansType)
spprSavingsPlansType = Lens.lens (savingsPlansType :: SavingsPlansPurchaseRecommendation -> Lude.Maybe SupportedSavingsPlansType) (\s a -> s {savingsPlansType = a} :: SavingsPlansPurchaseRecommendation)
{-# DEPRECATED spprSavingsPlansType "Use generic-lens or generic-optics with 'savingsPlansType' instead." #-}

-- | The lookback period in days, used to generate the recommendation.
--
-- /Note:/ Consider using 'lookbackPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprLookbackPeriodInDays :: Lens.Lens' SavingsPlansPurchaseRecommendation (Lude.Maybe LookbackPeriodInDays)
spprLookbackPeriodInDays = Lens.lens (lookbackPeriodInDays :: SavingsPlansPurchaseRecommendation -> Lude.Maybe LookbackPeriodInDays) (\s a -> s {lookbackPeriodInDays = a} :: SavingsPlansPurchaseRecommendation)
{-# DEPRECATED spprLookbackPeriodInDays "Use generic-lens or generic-optics with 'lookbackPeriodInDays' instead." #-}

-- | The payment option used to generate the recommendation.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprPaymentOption :: Lens.Lens' SavingsPlansPurchaseRecommendation (Lude.Maybe PaymentOption)
spprPaymentOption = Lens.lens (paymentOption :: SavingsPlansPurchaseRecommendation -> Lude.Maybe PaymentOption) (\s a -> s {paymentOption = a} :: SavingsPlansPurchaseRecommendation)
{-# DEPRECATED spprPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

-- | Summary metrics for your Savings Plans Recommendations.
--
-- /Note:/ Consider using 'savingsPlansPurchaseRecommendationSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprSavingsPlansPurchaseRecommendationSummary :: Lens.Lens' SavingsPlansPurchaseRecommendation (Lude.Maybe SavingsPlansPurchaseRecommendationSummary)
spprSavingsPlansPurchaseRecommendationSummary = Lens.lens (savingsPlansPurchaseRecommendationSummary :: SavingsPlansPurchaseRecommendation -> Lude.Maybe SavingsPlansPurchaseRecommendationSummary) (\s a -> s {savingsPlansPurchaseRecommendationSummary = a} :: SavingsPlansPurchaseRecommendation)
{-# DEPRECATED spprSavingsPlansPurchaseRecommendationSummary "Use generic-lens or generic-optics with 'savingsPlansPurchaseRecommendationSummary' instead." #-}

instance Lude.FromJSON SavingsPlansPurchaseRecommendation where
  parseJSON =
    Lude.withObject
      "SavingsPlansPurchaseRecommendation"
      ( \x ->
          SavingsPlansPurchaseRecommendation'
            Lude.<$> ( x Lude..:? "SavingsPlansPurchaseRecommendationDetails"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..:? "TermInYears")
            Lude.<*> (x Lude..:? "AccountScope")
            Lude.<*> (x Lude..:? "SavingsPlansType")
            Lude.<*> (x Lude..:? "LookbackPeriodInDays")
            Lude.<*> (x Lude..:? "PaymentOption")
            Lude.<*> (x Lude..:? "SavingsPlansPurchaseRecommendationSummary")
      )
