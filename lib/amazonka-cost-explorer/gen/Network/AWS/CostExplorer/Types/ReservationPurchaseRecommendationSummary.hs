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
    rprsTotalEstimatedMonthlySavingsAmount,
    rprsTotalEstimatedMonthlySavingsPercentage,
  )
where

import qualified Network.AWS.CostExplorer.Types.GenericString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A summary about this recommendation, such as the currency code, the amount that AWS estimates that you could save, and the total amount of reservation to purchase.
--
-- /See:/ 'mkReservationPurchaseRecommendationSummary' smart constructor.
data ReservationPurchaseRecommendationSummary = ReservationPurchaseRecommendationSummary'
  { -- | The currency code used for this recommendation.
    currencyCode :: Core.Maybe Types.GenericString,
    -- | The total amount that AWS estimates that this recommendation could save you in a month.
    totalEstimatedMonthlySavingsAmount :: Core.Maybe Types.GenericString,
    -- | The total amount that AWS estimates that this recommendation could save you in a month, as a percentage of your costs.
    totalEstimatedMonthlySavingsPercentage :: Core.Maybe Types.GenericString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservationPurchaseRecommendationSummary' value with any optional fields omitted.
mkReservationPurchaseRecommendationSummary ::
  ReservationPurchaseRecommendationSummary
mkReservationPurchaseRecommendationSummary =
  ReservationPurchaseRecommendationSummary'
    { currencyCode =
        Core.Nothing,
      totalEstimatedMonthlySavingsAmount = Core.Nothing,
      totalEstimatedMonthlySavingsPercentage = Core.Nothing
    }

-- | The currency code used for this recommendation.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprsCurrencyCode :: Lens.Lens' ReservationPurchaseRecommendationSummary (Core.Maybe Types.GenericString)
rprsCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED rprsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The total amount that AWS estimates that this recommendation could save you in a month.
--
-- /Note:/ Consider using 'totalEstimatedMonthlySavingsAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprsTotalEstimatedMonthlySavingsAmount :: Lens.Lens' ReservationPurchaseRecommendationSummary (Core.Maybe Types.GenericString)
rprsTotalEstimatedMonthlySavingsAmount = Lens.field @"totalEstimatedMonthlySavingsAmount"
{-# DEPRECATED rprsTotalEstimatedMonthlySavingsAmount "Use generic-lens or generic-optics with 'totalEstimatedMonthlySavingsAmount' instead." #-}

-- | The total amount that AWS estimates that this recommendation could save you in a month, as a percentage of your costs.
--
-- /Note:/ Consider using 'totalEstimatedMonthlySavingsPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprsTotalEstimatedMonthlySavingsPercentage :: Lens.Lens' ReservationPurchaseRecommendationSummary (Core.Maybe Types.GenericString)
rprsTotalEstimatedMonthlySavingsPercentage = Lens.field @"totalEstimatedMonthlySavingsPercentage"
{-# DEPRECATED rprsTotalEstimatedMonthlySavingsPercentage "Use generic-lens or generic-optics with 'totalEstimatedMonthlySavingsPercentage' instead." #-}

instance Core.FromJSON ReservationPurchaseRecommendationSummary where
  parseJSON =
    Core.withObject "ReservationPurchaseRecommendationSummary" Core.$
      \x ->
        ReservationPurchaseRecommendationSummary'
          Core.<$> (x Core..:? "CurrencyCode")
          Core.<*> (x Core..:? "TotalEstimatedMonthlySavingsAmount")
          Core.<*> (x Core..:? "TotalEstimatedMonthlySavingsPercentage")
