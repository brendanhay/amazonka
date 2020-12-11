-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary
  ( RightsizingRecommendationSummary (..),

    -- * Smart constructor
    mkRightsizingRecommendationSummary,

    -- * Lenses
    rrsSavingsPercentage,
    rrsSavingsCurrencyCode,
    rrsTotalRecommendationCount,
    rrsEstimatedTotalMonthlySavingsAmount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary of rightsizing recommendations
--
-- /See:/ 'mkRightsizingRecommendationSummary' smart constructor.
data RightsizingRecommendationSummary = RightsizingRecommendationSummary'
  { savingsPercentage ::
      Lude.Maybe Lude.Text,
    savingsCurrencyCode ::
      Lude.Maybe Lude.Text,
    totalRecommendationCount ::
      Lude.Maybe Lude.Text,
    estimatedTotalMonthlySavingsAmount ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RightsizingRecommendationSummary' with the minimum fields required to make a request.
--
-- * 'estimatedTotalMonthlySavingsAmount' - Estimated total savings resulting from modifications, on a monthly basis.
-- * 'savingsCurrencyCode' - The currency code that AWS used to calculate the savings.
-- * 'savingsPercentage' - Savings percentage based on the recommended modifications, relative to the total On-Demand costs associated with these instances.
-- * 'totalRecommendationCount' - Total number of instance recommendations.
mkRightsizingRecommendationSummary ::
  RightsizingRecommendationSummary
mkRightsizingRecommendationSummary =
  RightsizingRecommendationSummary'
    { savingsPercentage =
        Lude.Nothing,
      savingsCurrencyCode = Lude.Nothing,
      totalRecommendationCount = Lude.Nothing,
      estimatedTotalMonthlySavingsAmount = Lude.Nothing
    }

-- | Savings percentage based on the recommended modifications, relative to the total On-Demand costs associated with these instances.
--
-- /Note:/ Consider using 'savingsPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsSavingsPercentage :: Lens.Lens' RightsizingRecommendationSummary (Lude.Maybe Lude.Text)
rrsSavingsPercentage = Lens.lens (savingsPercentage :: RightsizingRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {savingsPercentage = a} :: RightsizingRecommendationSummary)
{-# DEPRECATED rrsSavingsPercentage "Use generic-lens or generic-optics with 'savingsPercentage' instead." #-}

-- | The currency code that AWS used to calculate the savings.
--
-- /Note:/ Consider using 'savingsCurrencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsSavingsCurrencyCode :: Lens.Lens' RightsizingRecommendationSummary (Lude.Maybe Lude.Text)
rrsSavingsCurrencyCode = Lens.lens (savingsCurrencyCode :: RightsizingRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {savingsCurrencyCode = a} :: RightsizingRecommendationSummary)
{-# DEPRECATED rrsSavingsCurrencyCode "Use generic-lens or generic-optics with 'savingsCurrencyCode' instead." #-}

-- | Total number of instance recommendations.
--
-- /Note:/ Consider using 'totalRecommendationCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsTotalRecommendationCount :: Lens.Lens' RightsizingRecommendationSummary (Lude.Maybe Lude.Text)
rrsTotalRecommendationCount = Lens.lens (totalRecommendationCount :: RightsizingRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {totalRecommendationCount = a} :: RightsizingRecommendationSummary)
{-# DEPRECATED rrsTotalRecommendationCount "Use generic-lens or generic-optics with 'totalRecommendationCount' instead." #-}

-- | Estimated total savings resulting from modifications, on a monthly basis.
--
-- /Note:/ Consider using 'estimatedTotalMonthlySavingsAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsEstimatedTotalMonthlySavingsAmount :: Lens.Lens' RightsizingRecommendationSummary (Lude.Maybe Lude.Text)
rrsEstimatedTotalMonthlySavingsAmount = Lens.lens (estimatedTotalMonthlySavingsAmount :: RightsizingRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {estimatedTotalMonthlySavingsAmount = a} :: RightsizingRecommendationSummary)
{-# DEPRECATED rrsEstimatedTotalMonthlySavingsAmount "Use generic-lens or generic-optics with 'estimatedTotalMonthlySavingsAmount' instead." #-}

instance Lude.FromJSON RightsizingRecommendationSummary where
  parseJSON =
    Lude.withObject
      "RightsizingRecommendationSummary"
      ( \x ->
          RightsizingRecommendationSummary'
            Lude.<$> (x Lude..:? "SavingsPercentage")
            Lude.<*> (x Lude..:? "SavingsCurrencyCode")
            Lude.<*> (x Lude..:? "TotalRecommendationCount")
            Lude.<*> (x Lude..:? "EstimatedTotalMonthlySavingsAmount")
      )
