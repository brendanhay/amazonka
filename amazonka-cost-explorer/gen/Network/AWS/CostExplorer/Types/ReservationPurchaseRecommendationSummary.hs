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
-- Module      : Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A summary about this recommendation, such as the currency code, the
-- amount that AWS estimates that you could save, and the total amount of
-- reservation to purchase.
--
-- /See:/ 'newReservationPurchaseRecommendationSummary' smart constructor.
data ReservationPurchaseRecommendationSummary = ReservationPurchaseRecommendationSummary'
  { -- | The total amount that AWS estimates that this recommendation could save
    -- you in a month.
    totalEstimatedMonthlySavingsAmount :: Prelude.Maybe Prelude.Text,
    -- | The currency code used for this recommendation.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The total amount that AWS estimates that this recommendation could save
    -- you in a month, as a percentage of your costs.
    totalEstimatedMonthlySavingsPercentage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReservationPurchaseRecommendationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalEstimatedMonthlySavingsAmount', 'reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsAmount' - The total amount that AWS estimates that this recommendation could save
-- you in a month.
--
-- 'currencyCode', 'reservationPurchaseRecommendationSummary_currencyCode' - The currency code used for this recommendation.
--
-- 'totalEstimatedMonthlySavingsPercentage', 'reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsPercentage' - The total amount that AWS estimates that this recommendation could save
-- you in a month, as a percentage of your costs.
newReservationPurchaseRecommendationSummary ::
  ReservationPurchaseRecommendationSummary
newReservationPurchaseRecommendationSummary =
  ReservationPurchaseRecommendationSummary'
    { totalEstimatedMonthlySavingsAmount =
        Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      totalEstimatedMonthlySavingsPercentage =
        Prelude.Nothing
    }

-- | The total amount that AWS estimates that this recommendation could save
-- you in a month.
reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsAmount :: Lens.Lens' ReservationPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsAmount = Lens.lens (\ReservationPurchaseRecommendationSummary' {totalEstimatedMonthlySavingsAmount} -> totalEstimatedMonthlySavingsAmount) (\s@ReservationPurchaseRecommendationSummary' {} a -> s {totalEstimatedMonthlySavingsAmount = a} :: ReservationPurchaseRecommendationSummary)

-- | The currency code used for this recommendation.
reservationPurchaseRecommendationSummary_currencyCode :: Lens.Lens' ReservationPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationSummary_currencyCode = Lens.lens (\ReservationPurchaseRecommendationSummary' {currencyCode} -> currencyCode) (\s@ReservationPurchaseRecommendationSummary' {} a -> s {currencyCode = a} :: ReservationPurchaseRecommendationSummary)

-- | The total amount that AWS estimates that this recommendation could save
-- you in a month, as a percentage of your costs.
reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsPercentage :: Lens.Lens' ReservationPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsPercentage = Lens.lens (\ReservationPurchaseRecommendationSummary' {totalEstimatedMonthlySavingsPercentage} -> totalEstimatedMonthlySavingsPercentage) (\s@ReservationPurchaseRecommendationSummary' {} a -> s {totalEstimatedMonthlySavingsPercentage = a} :: ReservationPurchaseRecommendationSummary)

instance
  Prelude.FromJSON
    ReservationPurchaseRecommendationSummary
  where
  parseJSON =
    Prelude.withObject
      "ReservationPurchaseRecommendationSummary"
      ( \x ->
          ReservationPurchaseRecommendationSummary'
            Prelude.<$> (x Prelude..:? "TotalEstimatedMonthlySavingsAmount")
            Prelude.<*> (x Prelude..:? "CurrencyCode")
            Prelude.<*> ( x
                            Prelude..:? "TotalEstimatedMonthlySavingsPercentage"
                        )
      )

instance
  Prelude.Hashable
    ReservationPurchaseRecommendationSummary

instance
  Prelude.NFData
    ReservationPurchaseRecommendationSummary
