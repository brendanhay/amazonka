{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationSummary
  ( ReservationPurchaseRecommendationSummary (..),

    -- * Smart constructor
    mkReservationPurchaseRecommendationSummary,

    -- * Lenses
    rprsCurrencyCode,
    rprsTotalEstimatedMonthlySavingsPercentage,
    rprsTotalEstimatedMonthlySavingsAmount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary about this recommendation, such as the currency code, the amount that AWS estimates that you could save, and the total amount of reservation to purchase.
--
-- /See:/ 'mkReservationPurchaseRecommendationSummary' smart constructor.
data ReservationPurchaseRecommendationSummary = ReservationPurchaseRecommendationSummary'
  { currencyCode ::
      Lude.Maybe
        Lude.Text,
    totalEstimatedMonthlySavingsPercentage ::
      Lude.Maybe
        Lude.Text,
    totalEstimatedMonthlySavingsAmount ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservationPurchaseRecommendationSummary' with the minimum fields required to make a request.
--
-- * 'currencyCode' - The currency code used for this recommendation.
-- * 'totalEstimatedMonthlySavingsAmount' - The total amount that AWS estimates that this recommendation could save you in a month.
-- * 'totalEstimatedMonthlySavingsPercentage' - The total amount that AWS estimates that this recommendation could save you in a month, as a percentage of your costs.
mkReservationPurchaseRecommendationSummary ::
  ReservationPurchaseRecommendationSummary
mkReservationPurchaseRecommendationSummary =
  ReservationPurchaseRecommendationSummary'
    { currencyCode =
        Lude.Nothing,
      totalEstimatedMonthlySavingsPercentage = Lude.Nothing,
      totalEstimatedMonthlySavingsAmount = Lude.Nothing
    }

-- | The currency code used for this recommendation.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprsCurrencyCode :: Lens.Lens' ReservationPurchaseRecommendationSummary (Lude.Maybe Lude.Text)
rprsCurrencyCode = Lens.lens (currencyCode :: ReservationPurchaseRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: ReservationPurchaseRecommendationSummary)
{-# DEPRECATED rprsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The total amount that AWS estimates that this recommendation could save you in a month, as a percentage of your costs.
--
-- /Note:/ Consider using 'totalEstimatedMonthlySavingsPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprsTotalEstimatedMonthlySavingsPercentage :: Lens.Lens' ReservationPurchaseRecommendationSummary (Lude.Maybe Lude.Text)
rprsTotalEstimatedMonthlySavingsPercentage = Lens.lens (totalEstimatedMonthlySavingsPercentage :: ReservationPurchaseRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {totalEstimatedMonthlySavingsPercentage = a} :: ReservationPurchaseRecommendationSummary)
{-# DEPRECATED rprsTotalEstimatedMonthlySavingsPercentage "Use generic-lens or generic-optics with 'totalEstimatedMonthlySavingsPercentage' instead." #-}

-- | The total amount that AWS estimates that this recommendation could save you in a month.
--
-- /Note:/ Consider using 'totalEstimatedMonthlySavingsAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprsTotalEstimatedMonthlySavingsAmount :: Lens.Lens' ReservationPurchaseRecommendationSummary (Lude.Maybe Lude.Text)
rprsTotalEstimatedMonthlySavingsAmount = Lens.lens (totalEstimatedMonthlySavingsAmount :: ReservationPurchaseRecommendationSummary -> Lude.Maybe Lude.Text) (\s a -> s {totalEstimatedMonthlySavingsAmount = a} :: ReservationPurchaseRecommendationSummary)
{-# DEPRECATED rprsTotalEstimatedMonthlySavingsAmount "Use generic-lens or generic-optics with 'totalEstimatedMonthlySavingsAmount' instead." #-}

instance Lude.FromJSON ReservationPurchaseRecommendationSummary where
  parseJSON =
    Lude.withObject
      "ReservationPurchaseRecommendationSummary"
      ( \x ->
          ReservationPurchaseRecommendationSummary'
            Lude.<$> (x Lude..:? "CurrencyCode")
            Lude.<*> (x Lude..:? "TotalEstimatedMonthlySavingsPercentage")
            Lude.<*> (x Lude..:? "TotalEstimatedMonthlySavingsAmount")
      )
