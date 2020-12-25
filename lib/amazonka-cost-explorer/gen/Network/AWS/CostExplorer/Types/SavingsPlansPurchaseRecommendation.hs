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
    spprAccountScope,
    spprLookbackPeriodInDays,
    spprPaymentOption,
    spprSavingsPlansPurchaseRecommendationDetails,
    spprSavingsPlansPurchaseRecommendationSummary,
    spprSavingsPlansType,
    spprTermInYears,
  )
where

import qualified Network.AWS.CostExplorer.Types.AccountScope as Types
import qualified Network.AWS.CostExplorer.Types.LookbackPeriodInDays as Types
import qualified Network.AWS.CostExplorer.Types.PaymentOption as Types
import qualified Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail as Types
import qualified Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary as Types
import qualified Network.AWS.CostExplorer.Types.SupportedSavingsPlansType as Types
import qualified Network.AWS.CostExplorer.Types.TermInYears as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains your request parameters, Savings Plan Recommendations Summary, and Details.
--
-- /See:/ 'mkSavingsPlansPurchaseRecommendation' smart constructor.
data SavingsPlansPurchaseRecommendation = SavingsPlansPurchaseRecommendation'
  { -- | The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
    accountScope :: Core.Maybe Types.AccountScope,
    -- | The lookback period in days, used to generate the recommendation.
    lookbackPeriodInDays :: Core.Maybe Types.LookbackPeriodInDays,
    -- | The payment option used to generate the recommendation.
    paymentOption :: Core.Maybe Types.PaymentOption,
    -- | Details for the Savings Plans we recommend that you purchase to cover existing Savings Plans eligible workloads.
    savingsPlansPurchaseRecommendationDetails :: Core.Maybe [Types.SavingsPlansPurchaseRecommendationDetail],
    -- | Summary metrics for your Savings Plans Recommendations.
    savingsPlansPurchaseRecommendationSummary :: Core.Maybe Types.SavingsPlansPurchaseRecommendationSummary,
    -- | The requested Savings Plans recommendation type.
    savingsPlansType :: Core.Maybe Types.SupportedSavingsPlansType,
    -- | The Savings Plans recommendation term in years, used to generate the recommendation.
    termInYears :: Core.Maybe Types.TermInYears
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SavingsPlansPurchaseRecommendation' value with any optional fields omitted.
mkSavingsPlansPurchaseRecommendation ::
  SavingsPlansPurchaseRecommendation
mkSavingsPlansPurchaseRecommendation =
  SavingsPlansPurchaseRecommendation'
    { accountScope = Core.Nothing,
      lookbackPeriodInDays = Core.Nothing,
      paymentOption = Core.Nothing,
      savingsPlansPurchaseRecommendationDetails = Core.Nothing,
      savingsPlansPurchaseRecommendationSummary = Core.Nothing,
      savingsPlansType = Core.Nothing,
      termInYears = Core.Nothing
    }

-- | The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
--
-- /Note:/ Consider using 'accountScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprAccountScope :: Lens.Lens' SavingsPlansPurchaseRecommendation (Core.Maybe Types.AccountScope)
spprAccountScope = Lens.field @"accountScope"
{-# DEPRECATED spprAccountScope "Use generic-lens or generic-optics with 'accountScope' instead." #-}

-- | The lookback period in days, used to generate the recommendation.
--
-- /Note:/ Consider using 'lookbackPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprLookbackPeriodInDays :: Lens.Lens' SavingsPlansPurchaseRecommendation (Core.Maybe Types.LookbackPeriodInDays)
spprLookbackPeriodInDays = Lens.field @"lookbackPeriodInDays"
{-# DEPRECATED spprLookbackPeriodInDays "Use generic-lens or generic-optics with 'lookbackPeriodInDays' instead." #-}

-- | The payment option used to generate the recommendation.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprPaymentOption :: Lens.Lens' SavingsPlansPurchaseRecommendation (Core.Maybe Types.PaymentOption)
spprPaymentOption = Lens.field @"paymentOption"
{-# DEPRECATED spprPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

-- | Details for the Savings Plans we recommend that you purchase to cover existing Savings Plans eligible workloads.
--
-- /Note:/ Consider using 'savingsPlansPurchaseRecommendationDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprSavingsPlansPurchaseRecommendationDetails :: Lens.Lens' SavingsPlansPurchaseRecommendation (Core.Maybe [Types.SavingsPlansPurchaseRecommendationDetail])
spprSavingsPlansPurchaseRecommendationDetails = Lens.field @"savingsPlansPurchaseRecommendationDetails"
{-# DEPRECATED spprSavingsPlansPurchaseRecommendationDetails "Use generic-lens or generic-optics with 'savingsPlansPurchaseRecommendationDetails' instead." #-}

-- | Summary metrics for your Savings Plans Recommendations.
--
-- /Note:/ Consider using 'savingsPlansPurchaseRecommendationSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprSavingsPlansPurchaseRecommendationSummary :: Lens.Lens' SavingsPlansPurchaseRecommendation (Core.Maybe Types.SavingsPlansPurchaseRecommendationSummary)
spprSavingsPlansPurchaseRecommendationSummary = Lens.field @"savingsPlansPurchaseRecommendationSummary"
{-# DEPRECATED spprSavingsPlansPurchaseRecommendationSummary "Use generic-lens or generic-optics with 'savingsPlansPurchaseRecommendationSummary' instead." #-}

-- | The requested Savings Plans recommendation type.
--
-- /Note:/ Consider using 'savingsPlansType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprSavingsPlansType :: Lens.Lens' SavingsPlansPurchaseRecommendation (Core.Maybe Types.SupportedSavingsPlansType)
spprSavingsPlansType = Lens.field @"savingsPlansType"
{-# DEPRECATED spprSavingsPlansType "Use generic-lens or generic-optics with 'savingsPlansType' instead." #-}

-- | The Savings Plans recommendation term in years, used to generate the recommendation.
--
-- /Note:/ Consider using 'termInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprTermInYears :: Lens.Lens' SavingsPlansPurchaseRecommendation (Core.Maybe Types.TermInYears)
spprTermInYears = Lens.field @"termInYears"
{-# DEPRECATED spprTermInYears "Use generic-lens or generic-optics with 'termInYears' instead." #-}

instance Core.FromJSON SavingsPlansPurchaseRecommendation where
  parseJSON =
    Core.withObject "SavingsPlansPurchaseRecommendation" Core.$
      \x ->
        SavingsPlansPurchaseRecommendation'
          Core.<$> (x Core..:? "AccountScope")
          Core.<*> (x Core..:? "LookbackPeriodInDays")
          Core.<*> (x Core..:? "PaymentOption")
          Core.<*> (x Core..:? "SavingsPlansPurchaseRecommendationDetails")
          Core.<*> (x Core..:? "SavingsPlansPurchaseRecommendationSummary")
          Core.<*> (x Core..:? "SavingsPlansType")
          Core.<*> (x Core..:? "TermInYears")
