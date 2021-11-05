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
-- Module      : Amazonka.CostExplorer.Types.ReservationPurchaseRecommendation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ReservationPurchaseRecommendation where

import qualified Amazonka.Core as Core
import Amazonka.CostExplorer.Types.AccountScope
import Amazonka.CostExplorer.Types.LookbackPeriodInDays
import Amazonka.CostExplorer.Types.PaymentOption
import Amazonka.CostExplorer.Types.ReservationPurchaseRecommendationDetail
import Amazonka.CostExplorer.Types.ReservationPurchaseRecommendationSummary
import Amazonka.CostExplorer.Types.ServiceSpecification
import Amazonka.CostExplorer.Types.TermInYears
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A specific reservation that Amazon Web Services recommends for purchase.
--
-- /See:/ 'newReservationPurchaseRecommendation' smart constructor.
data ReservationPurchaseRecommendation = ReservationPurchaseRecommendation'
  { -- | The term of the reservation that you want recommendations for, in years.
    termInYears :: Prelude.Maybe TermInYears,
    -- | A summary about the recommended purchase.
    recommendationSummary :: Prelude.Maybe ReservationPurchaseRecommendationSummary,
    -- | Hardware specifications for the service that you want recommendations
    -- for.
    serviceSpecification :: Prelude.Maybe ServiceSpecification,
    -- | The account scope that Amazon Web Services recommends that you purchase
    -- this instance for. For example, you can purchase this reservation for an
    -- entire organization in Amazon Web Services Organizations.
    accountScope :: Prelude.Maybe AccountScope,
    -- | Details about the recommended purchases.
    recommendationDetails :: Prelude.Maybe [ReservationPurchaseRecommendationDetail],
    -- | How many days of previous usage that Amazon Web Services considers when
    -- making this recommendation.
    lookbackPeriodInDays :: Prelude.Maybe LookbackPeriodInDays,
    -- | The payment option for the reservation (for example, @AllUpfront@ or
    -- @NoUpfront@).
    paymentOption :: Prelude.Maybe PaymentOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservationPurchaseRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'termInYears', 'reservationPurchaseRecommendation_termInYears' - The term of the reservation that you want recommendations for, in years.
--
-- 'recommendationSummary', 'reservationPurchaseRecommendation_recommendationSummary' - A summary about the recommended purchase.
--
-- 'serviceSpecification', 'reservationPurchaseRecommendation_serviceSpecification' - Hardware specifications for the service that you want recommendations
-- for.
--
-- 'accountScope', 'reservationPurchaseRecommendation_accountScope' - The account scope that Amazon Web Services recommends that you purchase
-- this instance for. For example, you can purchase this reservation for an
-- entire organization in Amazon Web Services Organizations.
--
-- 'recommendationDetails', 'reservationPurchaseRecommendation_recommendationDetails' - Details about the recommended purchases.
--
-- 'lookbackPeriodInDays', 'reservationPurchaseRecommendation_lookbackPeriodInDays' - How many days of previous usage that Amazon Web Services considers when
-- making this recommendation.
--
-- 'paymentOption', 'reservationPurchaseRecommendation_paymentOption' - The payment option for the reservation (for example, @AllUpfront@ or
-- @NoUpfront@).
newReservationPurchaseRecommendation ::
  ReservationPurchaseRecommendation
newReservationPurchaseRecommendation =
  ReservationPurchaseRecommendation'
    { termInYears =
        Prelude.Nothing,
      recommendationSummary = Prelude.Nothing,
      serviceSpecification = Prelude.Nothing,
      accountScope = Prelude.Nothing,
      recommendationDetails = Prelude.Nothing,
      lookbackPeriodInDays = Prelude.Nothing,
      paymentOption = Prelude.Nothing
    }

-- | The term of the reservation that you want recommendations for, in years.
reservationPurchaseRecommendation_termInYears :: Lens.Lens' ReservationPurchaseRecommendation (Prelude.Maybe TermInYears)
reservationPurchaseRecommendation_termInYears = Lens.lens (\ReservationPurchaseRecommendation' {termInYears} -> termInYears) (\s@ReservationPurchaseRecommendation' {} a -> s {termInYears = a} :: ReservationPurchaseRecommendation)

-- | A summary about the recommended purchase.
reservationPurchaseRecommendation_recommendationSummary :: Lens.Lens' ReservationPurchaseRecommendation (Prelude.Maybe ReservationPurchaseRecommendationSummary)
reservationPurchaseRecommendation_recommendationSummary = Lens.lens (\ReservationPurchaseRecommendation' {recommendationSummary} -> recommendationSummary) (\s@ReservationPurchaseRecommendation' {} a -> s {recommendationSummary = a} :: ReservationPurchaseRecommendation)

-- | Hardware specifications for the service that you want recommendations
-- for.
reservationPurchaseRecommendation_serviceSpecification :: Lens.Lens' ReservationPurchaseRecommendation (Prelude.Maybe ServiceSpecification)
reservationPurchaseRecommendation_serviceSpecification = Lens.lens (\ReservationPurchaseRecommendation' {serviceSpecification} -> serviceSpecification) (\s@ReservationPurchaseRecommendation' {} a -> s {serviceSpecification = a} :: ReservationPurchaseRecommendation)

-- | The account scope that Amazon Web Services recommends that you purchase
-- this instance for. For example, you can purchase this reservation for an
-- entire organization in Amazon Web Services Organizations.
reservationPurchaseRecommendation_accountScope :: Lens.Lens' ReservationPurchaseRecommendation (Prelude.Maybe AccountScope)
reservationPurchaseRecommendation_accountScope = Lens.lens (\ReservationPurchaseRecommendation' {accountScope} -> accountScope) (\s@ReservationPurchaseRecommendation' {} a -> s {accountScope = a} :: ReservationPurchaseRecommendation)

-- | Details about the recommended purchases.
reservationPurchaseRecommendation_recommendationDetails :: Lens.Lens' ReservationPurchaseRecommendation (Prelude.Maybe [ReservationPurchaseRecommendationDetail])
reservationPurchaseRecommendation_recommendationDetails = Lens.lens (\ReservationPurchaseRecommendation' {recommendationDetails} -> recommendationDetails) (\s@ReservationPurchaseRecommendation' {} a -> s {recommendationDetails = a} :: ReservationPurchaseRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | How many days of previous usage that Amazon Web Services considers when
-- making this recommendation.
reservationPurchaseRecommendation_lookbackPeriodInDays :: Lens.Lens' ReservationPurchaseRecommendation (Prelude.Maybe LookbackPeriodInDays)
reservationPurchaseRecommendation_lookbackPeriodInDays = Lens.lens (\ReservationPurchaseRecommendation' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@ReservationPurchaseRecommendation' {} a -> s {lookbackPeriodInDays = a} :: ReservationPurchaseRecommendation)

-- | The payment option for the reservation (for example, @AllUpfront@ or
-- @NoUpfront@).
reservationPurchaseRecommendation_paymentOption :: Lens.Lens' ReservationPurchaseRecommendation (Prelude.Maybe PaymentOption)
reservationPurchaseRecommendation_paymentOption = Lens.lens (\ReservationPurchaseRecommendation' {paymentOption} -> paymentOption) (\s@ReservationPurchaseRecommendation' {} a -> s {paymentOption = a} :: ReservationPurchaseRecommendation)

instance
  Core.FromJSON
    ReservationPurchaseRecommendation
  where
  parseJSON =
    Core.withObject
      "ReservationPurchaseRecommendation"
      ( \x ->
          ReservationPurchaseRecommendation'
            Prelude.<$> (x Core..:? "TermInYears")
            Prelude.<*> (x Core..:? "RecommendationSummary")
            Prelude.<*> (x Core..:? "ServiceSpecification")
            Prelude.<*> (x Core..:? "AccountScope")
            Prelude.<*> ( x Core..:? "RecommendationDetails"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LookbackPeriodInDays")
            Prelude.<*> (x Core..:? "PaymentOption")
      )

instance
  Prelude.Hashable
    ReservationPurchaseRecommendation

instance
  Prelude.NFData
    ReservationPurchaseRecommendation
