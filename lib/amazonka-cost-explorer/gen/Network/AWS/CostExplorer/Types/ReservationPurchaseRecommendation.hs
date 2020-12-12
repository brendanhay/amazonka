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
    rprTermInYears,
    rprRecommendationSummary,
    rprServiceSpecification,
    rprAccountScope,
    rprRecommendationDetails,
    rprLookbackPeriodInDays,
    rprPaymentOption,
  )
where

import Network.AWS.CostExplorer.Types.AccountScope
import Network.AWS.CostExplorer.Types.LookbackPeriodInDays
import Network.AWS.CostExplorer.Types.PaymentOption
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationSummary
import Network.AWS.CostExplorer.Types.ServiceSpecification
import Network.AWS.CostExplorer.Types.TermInYears
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A specific reservation that AWS recommends for purchase.
--
-- /See:/ 'mkReservationPurchaseRecommendation' smart constructor.
data ReservationPurchaseRecommendation = ReservationPurchaseRecommendation'
  { termInYears ::
      Lude.Maybe TermInYears,
    recommendationSummary ::
      Lude.Maybe
        ReservationPurchaseRecommendationSummary,
    serviceSpecification ::
      Lude.Maybe
        ServiceSpecification,
    accountScope ::
      Lude.Maybe AccountScope,
    recommendationDetails ::
      Lude.Maybe
        [ReservationPurchaseRecommendationDetail],
    lookbackPeriodInDays ::
      Lude.Maybe
        LookbackPeriodInDays,
    paymentOption ::
      Lude.Maybe
        PaymentOption
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservationPurchaseRecommendation' with the minimum fields required to make a request.
--
-- * 'accountScope' - The account scope that AWS recommends that you purchase this instance for. For example, you can purchase this reservation for an entire organization in AWS Organizations.
-- * 'lookbackPeriodInDays' - How many days of previous usage that AWS considers when making this recommendation.
-- * 'paymentOption' - The payment option for the reservation. For example, @AllUpfront@ or @NoUpfront@ .
-- * 'recommendationDetails' - Details about the recommended purchases.
-- * 'recommendationSummary' - A summary about the recommended purchase.
-- * 'serviceSpecification' - Hardware specifications for the service that you want recommendations for.
-- * 'termInYears' - The term of the reservation that you want recommendations for, in years.
mkReservationPurchaseRecommendation ::
  ReservationPurchaseRecommendation
mkReservationPurchaseRecommendation =
  ReservationPurchaseRecommendation'
    { termInYears = Lude.Nothing,
      recommendationSummary = Lude.Nothing,
      serviceSpecification = Lude.Nothing,
      accountScope = Lude.Nothing,
      recommendationDetails = Lude.Nothing,
      lookbackPeriodInDays = Lude.Nothing,
      paymentOption = Lude.Nothing
    }

-- | The term of the reservation that you want recommendations for, in years.
--
-- /Note:/ Consider using 'termInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprTermInYears :: Lens.Lens' ReservationPurchaseRecommendation (Lude.Maybe TermInYears)
rprTermInYears = Lens.lens (termInYears :: ReservationPurchaseRecommendation -> Lude.Maybe TermInYears) (\s a -> s {termInYears = a} :: ReservationPurchaseRecommendation)
{-# DEPRECATED rprTermInYears "Use generic-lens or generic-optics with 'termInYears' instead." #-}

-- | A summary about the recommended purchase.
--
-- /Note:/ Consider using 'recommendationSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprRecommendationSummary :: Lens.Lens' ReservationPurchaseRecommendation (Lude.Maybe ReservationPurchaseRecommendationSummary)
rprRecommendationSummary = Lens.lens (recommendationSummary :: ReservationPurchaseRecommendation -> Lude.Maybe ReservationPurchaseRecommendationSummary) (\s a -> s {recommendationSummary = a} :: ReservationPurchaseRecommendation)
{-# DEPRECATED rprRecommendationSummary "Use generic-lens or generic-optics with 'recommendationSummary' instead." #-}

-- | Hardware specifications for the service that you want recommendations for.
--
-- /Note:/ Consider using 'serviceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprServiceSpecification :: Lens.Lens' ReservationPurchaseRecommendation (Lude.Maybe ServiceSpecification)
rprServiceSpecification = Lens.lens (serviceSpecification :: ReservationPurchaseRecommendation -> Lude.Maybe ServiceSpecification) (\s a -> s {serviceSpecification = a} :: ReservationPurchaseRecommendation)
{-# DEPRECATED rprServiceSpecification "Use generic-lens or generic-optics with 'serviceSpecification' instead." #-}

-- | The account scope that AWS recommends that you purchase this instance for. For example, you can purchase this reservation for an entire organization in AWS Organizations.
--
-- /Note:/ Consider using 'accountScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprAccountScope :: Lens.Lens' ReservationPurchaseRecommendation (Lude.Maybe AccountScope)
rprAccountScope = Lens.lens (accountScope :: ReservationPurchaseRecommendation -> Lude.Maybe AccountScope) (\s a -> s {accountScope = a} :: ReservationPurchaseRecommendation)
{-# DEPRECATED rprAccountScope "Use generic-lens or generic-optics with 'accountScope' instead." #-}

-- | Details about the recommended purchases.
--
-- /Note:/ Consider using 'recommendationDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprRecommendationDetails :: Lens.Lens' ReservationPurchaseRecommendation (Lude.Maybe [ReservationPurchaseRecommendationDetail])
rprRecommendationDetails = Lens.lens (recommendationDetails :: ReservationPurchaseRecommendation -> Lude.Maybe [ReservationPurchaseRecommendationDetail]) (\s a -> s {recommendationDetails = a} :: ReservationPurchaseRecommendation)
{-# DEPRECATED rprRecommendationDetails "Use generic-lens or generic-optics with 'recommendationDetails' instead." #-}

-- | How many days of previous usage that AWS considers when making this recommendation.
--
-- /Note:/ Consider using 'lookbackPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprLookbackPeriodInDays :: Lens.Lens' ReservationPurchaseRecommendation (Lude.Maybe LookbackPeriodInDays)
rprLookbackPeriodInDays = Lens.lens (lookbackPeriodInDays :: ReservationPurchaseRecommendation -> Lude.Maybe LookbackPeriodInDays) (\s a -> s {lookbackPeriodInDays = a} :: ReservationPurchaseRecommendation)
{-# DEPRECATED rprLookbackPeriodInDays "Use generic-lens or generic-optics with 'lookbackPeriodInDays' instead." #-}

-- | The payment option for the reservation. For example, @AllUpfront@ or @NoUpfront@ .
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprPaymentOption :: Lens.Lens' ReservationPurchaseRecommendation (Lude.Maybe PaymentOption)
rprPaymentOption = Lens.lens (paymentOption :: ReservationPurchaseRecommendation -> Lude.Maybe PaymentOption) (\s a -> s {paymentOption = a} :: ReservationPurchaseRecommendation)
{-# DEPRECATED rprPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

instance Lude.FromJSON ReservationPurchaseRecommendation where
  parseJSON =
    Lude.withObject
      "ReservationPurchaseRecommendation"
      ( \x ->
          ReservationPurchaseRecommendation'
            Lude.<$> (x Lude..:? "TermInYears")
            Lude.<*> (x Lude..:? "RecommendationSummary")
            Lude.<*> (x Lude..:? "ServiceSpecification")
            Lude.<*> (x Lude..:? "AccountScope")
            Lude.<*> (x Lude..:? "RecommendationDetails" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LookbackPeriodInDays")
            Lude.<*> (x Lude..:? "PaymentOption")
      )
