{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendation
  ( ReservationPurchaseRecommendation (..),

    -- * Smart constructor
    mkReservationPurchaseRecommendation,

    -- * Lenses
    rprAccountScope,
    rprLookbackPeriodInDays,
    rprPaymentOption,
    rprRecommendationDetails,
    rprRecommendationSummary,
    rprServiceSpecification,
    rprTermInYears,
  )
where

import qualified Network.AWS.CostExplorer.Types.AccountScope as Types
import qualified Network.AWS.CostExplorer.Types.LookbackPeriodInDays as Types
import qualified Network.AWS.CostExplorer.Types.PaymentOption as Types
import qualified Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail as Types
import qualified Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationSummary as Types
import qualified Network.AWS.CostExplorer.Types.ServiceSpecification as Types
import qualified Network.AWS.CostExplorer.Types.TermInYears as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A specific reservation that AWS recommends for purchase.
--
-- /See:/ 'mkReservationPurchaseRecommendation' smart constructor.
data ReservationPurchaseRecommendation = ReservationPurchaseRecommendation'
  { -- | The account scope that AWS recommends that you purchase this instance for. For example, you can purchase this reservation for an entire organization in AWS Organizations.
    accountScope :: Core.Maybe Types.AccountScope,
    -- | How many days of previous usage that AWS considers when making this recommendation.
    lookbackPeriodInDays :: Core.Maybe Types.LookbackPeriodInDays,
    -- | The payment option for the reservation. For example, @AllUpfront@ or @NoUpfront@ .
    paymentOption :: Core.Maybe Types.PaymentOption,
    -- | Details about the recommended purchases.
    recommendationDetails :: Core.Maybe [Types.ReservationPurchaseRecommendationDetail],
    -- | A summary about the recommended purchase.
    recommendationSummary :: Core.Maybe Types.ReservationPurchaseRecommendationSummary,
    -- | Hardware specifications for the service that you want recommendations for.
    serviceSpecification :: Core.Maybe Types.ServiceSpecification,
    -- | The term of the reservation that you want recommendations for, in years.
    termInYears :: Core.Maybe Types.TermInYears
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservationPurchaseRecommendation' value with any optional fields omitted.
mkReservationPurchaseRecommendation ::
  ReservationPurchaseRecommendation
mkReservationPurchaseRecommendation =
  ReservationPurchaseRecommendation'
    { accountScope = Core.Nothing,
      lookbackPeriodInDays = Core.Nothing,
      paymentOption = Core.Nothing,
      recommendationDetails = Core.Nothing,
      recommendationSummary = Core.Nothing,
      serviceSpecification = Core.Nothing,
      termInYears = Core.Nothing
    }

-- | The account scope that AWS recommends that you purchase this instance for. For example, you can purchase this reservation for an entire organization in AWS Organizations.
--
-- /Note:/ Consider using 'accountScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprAccountScope :: Lens.Lens' ReservationPurchaseRecommendation (Core.Maybe Types.AccountScope)
rprAccountScope = Lens.field @"accountScope"
{-# DEPRECATED rprAccountScope "Use generic-lens or generic-optics with 'accountScope' instead." #-}

-- | How many days of previous usage that AWS considers when making this recommendation.
--
-- /Note:/ Consider using 'lookbackPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprLookbackPeriodInDays :: Lens.Lens' ReservationPurchaseRecommendation (Core.Maybe Types.LookbackPeriodInDays)
rprLookbackPeriodInDays = Lens.field @"lookbackPeriodInDays"
{-# DEPRECATED rprLookbackPeriodInDays "Use generic-lens or generic-optics with 'lookbackPeriodInDays' instead." #-}

-- | The payment option for the reservation. For example, @AllUpfront@ or @NoUpfront@ .
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprPaymentOption :: Lens.Lens' ReservationPurchaseRecommendation (Core.Maybe Types.PaymentOption)
rprPaymentOption = Lens.field @"paymentOption"
{-# DEPRECATED rprPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

-- | Details about the recommended purchases.
--
-- /Note:/ Consider using 'recommendationDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprRecommendationDetails :: Lens.Lens' ReservationPurchaseRecommendation (Core.Maybe [Types.ReservationPurchaseRecommendationDetail])
rprRecommendationDetails = Lens.field @"recommendationDetails"
{-# DEPRECATED rprRecommendationDetails "Use generic-lens or generic-optics with 'recommendationDetails' instead." #-}

-- | A summary about the recommended purchase.
--
-- /Note:/ Consider using 'recommendationSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprRecommendationSummary :: Lens.Lens' ReservationPurchaseRecommendation (Core.Maybe Types.ReservationPurchaseRecommendationSummary)
rprRecommendationSummary = Lens.field @"recommendationSummary"
{-# DEPRECATED rprRecommendationSummary "Use generic-lens or generic-optics with 'recommendationSummary' instead." #-}

-- | Hardware specifications for the service that you want recommendations for.
--
-- /Note:/ Consider using 'serviceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprServiceSpecification :: Lens.Lens' ReservationPurchaseRecommendation (Core.Maybe Types.ServiceSpecification)
rprServiceSpecification = Lens.field @"serviceSpecification"
{-# DEPRECATED rprServiceSpecification "Use generic-lens or generic-optics with 'serviceSpecification' instead." #-}

-- | The term of the reservation that you want recommendations for, in years.
--
-- /Note:/ Consider using 'termInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprTermInYears :: Lens.Lens' ReservationPurchaseRecommendation (Core.Maybe Types.TermInYears)
rprTermInYears = Lens.field @"termInYears"
{-# DEPRECATED rprTermInYears "Use generic-lens or generic-optics with 'termInYears' instead." #-}

instance Core.FromJSON ReservationPurchaseRecommendation where
  parseJSON =
    Core.withObject "ReservationPurchaseRecommendation" Core.$
      \x ->
        ReservationPurchaseRecommendation'
          Core.<$> (x Core..:? "AccountScope")
          Core.<*> (x Core..:? "LookbackPeriodInDays")
          Core.<*> (x Core..:? "PaymentOption")
          Core.<*> (x Core..:? "RecommendationDetails")
          Core.<*> (x Core..:? "RecommendationSummary")
          Core.<*> (x Core..:? "ServiceSpecification")
          Core.<*> (x Core..:? "TermInYears")
