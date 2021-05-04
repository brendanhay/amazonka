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
-- Module      : Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendation where

import Network.AWS.CostExplorer.Types.AccountScope
import Network.AWS.CostExplorer.Types.LookbackPeriodInDays
import Network.AWS.CostExplorer.Types.PaymentOption
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationSummary
import Network.AWS.CostExplorer.Types.ServiceSpecification
import Network.AWS.CostExplorer.Types.TermInYears
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A specific reservation that AWS recommends for purchase.
--
-- /See:/ 'newReservationPurchaseRecommendation' smart constructor.
data ReservationPurchaseRecommendation = ReservationPurchaseRecommendation'
  { -- | The payment option for the reservation. For example, @AllUpfront@ or
    -- @NoUpfront@.
    paymentOption :: Prelude.Maybe PaymentOption,
    -- | The account scope that AWS recommends that you purchase this instance
    -- for. For example, you can purchase this reservation for an entire
    -- organization in AWS Organizations.
    accountScope :: Prelude.Maybe AccountScope,
    -- | Details about the recommended purchases.
    recommendationDetails :: Prelude.Maybe [ReservationPurchaseRecommendationDetail],
    -- | Hardware specifications for the service that you want recommendations
    -- for.
    serviceSpecification :: Prelude.Maybe ServiceSpecification,
    -- | The term of the reservation that you want recommendations for, in years.
    termInYears :: Prelude.Maybe TermInYears,
    -- | A summary about the recommended purchase.
    recommendationSummary :: Prelude.Maybe ReservationPurchaseRecommendationSummary,
    -- | How many days of previous usage that AWS considers when making this
    -- recommendation.
    lookbackPeriodInDays :: Prelude.Maybe LookbackPeriodInDays
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReservationPurchaseRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paymentOption', 'reservationPurchaseRecommendation_paymentOption' - The payment option for the reservation. For example, @AllUpfront@ or
-- @NoUpfront@.
--
-- 'accountScope', 'reservationPurchaseRecommendation_accountScope' - The account scope that AWS recommends that you purchase this instance
-- for. For example, you can purchase this reservation for an entire
-- organization in AWS Organizations.
--
-- 'recommendationDetails', 'reservationPurchaseRecommendation_recommendationDetails' - Details about the recommended purchases.
--
-- 'serviceSpecification', 'reservationPurchaseRecommendation_serviceSpecification' - Hardware specifications for the service that you want recommendations
-- for.
--
-- 'termInYears', 'reservationPurchaseRecommendation_termInYears' - The term of the reservation that you want recommendations for, in years.
--
-- 'recommendationSummary', 'reservationPurchaseRecommendation_recommendationSummary' - A summary about the recommended purchase.
--
-- 'lookbackPeriodInDays', 'reservationPurchaseRecommendation_lookbackPeriodInDays' - How many days of previous usage that AWS considers when making this
-- recommendation.
newReservationPurchaseRecommendation ::
  ReservationPurchaseRecommendation
newReservationPurchaseRecommendation =
  ReservationPurchaseRecommendation'
    { paymentOption =
        Prelude.Nothing,
      accountScope = Prelude.Nothing,
      recommendationDetails = Prelude.Nothing,
      serviceSpecification = Prelude.Nothing,
      termInYears = Prelude.Nothing,
      recommendationSummary = Prelude.Nothing,
      lookbackPeriodInDays = Prelude.Nothing
    }

-- | The payment option for the reservation. For example, @AllUpfront@ or
-- @NoUpfront@.
reservationPurchaseRecommendation_paymentOption :: Lens.Lens' ReservationPurchaseRecommendation (Prelude.Maybe PaymentOption)
reservationPurchaseRecommendation_paymentOption = Lens.lens (\ReservationPurchaseRecommendation' {paymentOption} -> paymentOption) (\s@ReservationPurchaseRecommendation' {} a -> s {paymentOption = a} :: ReservationPurchaseRecommendation)

-- | The account scope that AWS recommends that you purchase this instance
-- for. For example, you can purchase this reservation for an entire
-- organization in AWS Organizations.
reservationPurchaseRecommendation_accountScope :: Lens.Lens' ReservationPurchaseRecommendation (Prelude.Maybe AccountScope)
reservationPurchaseRecommendation_accountScope = Lens.lens (\ReservationPurchaseRecommendation' {accountScope} -> accountScope) (\s@ReservationPurchaseRecommendation' {} a -> s {accountScope = a} :: ReservationPurchaseRecommendation)

-- | Details about the recommended purchases.
reservationPurchaseRecommendation_recommendationDetails :: Lens.Lens' ReservationPurchaseRecommendation (Prelude.Maybe [ReservationPurchaseRecommendationDetail])
reservationPurchaseRecommendation_recommendationDetails = Lens.lens (\ReservationPurchaseRecommendation' {recommendationDetails} -> recommendationDetails) (\s@ReservationPurchaseRecommendation' {} a -> s {recommendationDetails = a} :: ReservationPurchaseRecommendation) Prelude.. Lens.mapping Prelude._Coerce

-- | Hardware specifications for the service that you want recommendations
-- for.
reservationPurchaseRecommendation_serviceSpecification :: Lens.Lens' ReservationPurchaseRecommendation (Prelude.Maybe ServiceSpecification)
reservationPurchaseRecommendation_serviceSpecification = Lens.lens (\ReservationPurchaseRecommendation' {serviceSpecification} -> serviceSpecification) (\s@ReservationPurchaseRecommendation' {} a -> s {serviceSpecification = a} :: ReservationPurchaseRecommendation)

-- | The term of the reservation that you want recommendations for, in years.
reservationPurchaseRecommendation_termInYears :: Lens.Lens' ReservationPurchaseRecommendation (Prelude.Maybe TermInYears)
reservationPurchaseRecommendation_termInYears = Lens.lens (\ReservationPurchaseRecommendation' {termInYears} -> termInYears) (\s@ReservationPurchaseRecommendation' {} a -> s {termInYears = a} :: ReservationPurchaseRecommendation)

-- | A summary about the recommended purchase.
reservationPurchaseRecommendation_recommendationSummary :: Lens.Lens' ReservationPurchaseRecommendation (Prelude.Maybe ReservationPurchaseRecommendationSummary)
reservationPurchaseRecommendation_recommendationSummary = Lens.lens (\ReservationPurchaseRecommendation' {recommendationSummary} -> recommendationSummary) (\s@ReservationPurchaseRecommendation' {} a -> s {recommendationSummary = a} :: ReservationPurchaseRecommendation)

-- | How many days of previous usage that AWS considers when making this
-- recommendation.
reservationPurchaseRecommendation_lookbackPeriodInDays :: Lens.Lens' ReservationPurchaseRecommendation (Prelude.Maybe LookbackPeriodInDays)
reservationPurchaseRecommendation_lookbackPeriodInDays = Lens.lens (\ReservationPurchaseRecommendation' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@ReservationPurchaseRecommendation' {} a -> s {lookbackPeriodInDays = a} :: ReservationPurchaseRecommendation)

instance
  Prelude.FromJSON
    ReservationPurchaseRecommendation
  where
  parseJSON =
    Prelude.withObject
      "ReservationPurchaseRecommendation"
      ( \x ->
          ReservationPurchaseRecommendation'
            Prelude.<$> (x Prelude..:? "PaymentOption")
            Prelude.<*> (x Prelude..:? "AccountScope")
            Prelude.<*> ( x Prelude..:? "RecommendationDetails"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ServiceSpecification")
            Prelude.<*> (x Prelude..:? "TermInYears")
            Prelude.<*> (x Prelude..:? "RecommendationSummary")
            Prelude.<*> (x Prelude..:? "LookbackPeriodInDays")
      )

instance
  Prelude.Hashable
    ReservationPurchaseRecommendation

instance
  Prelude.NFData
    ReservationPurchaseRecommendation
