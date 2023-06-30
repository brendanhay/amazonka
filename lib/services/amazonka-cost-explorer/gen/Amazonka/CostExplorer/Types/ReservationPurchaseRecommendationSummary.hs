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
-- Module      : Amazonka.CostExplorer.Types.ReservationPurchaseRecommendationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ReservationPurchaseRecommendationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary about this recommendation, such as the currency code, the
-- amount that Amazon Web Services estimates that you could save, and the
-- total amount of reservation to purchase.
--
-- /See:/ 'newReservationPurchaseRecommendationSummary' smart constructor.
data ReservationPurchaseRecommendationSummary = ReservationPurchaseRecommendationSummary'
  { -- | The currency code used for this recommendation.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The total amount that Amazon Web Services estimates that this
    -- recommendation could save you in a month.
    totalEstimatedMonthlySavingsAmount :: Prelude.Maybe Prelude.Text,
    -- | The total amount that Amazon Web Services estimates that this
    -- recommendation could save you in a month, as a percentage of your costs.
    totalEstimatedMonthlySavingsPercentage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservationPurchaseRecommendationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currencyCode', 'reservationPurchaseRecommendationSummary_currencyCode' - The currency code used for this recommendation.
--
-- 'totalEstimatedMonthlySavingsAmount', 'reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsAmount' - The total amount that Amazon Web Services estimates that this
-- recommendation could save you in a month.
--
-- 'totalEstimatedMonthlySavingsPercentage', 'reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsPercentage' - The total amount that Amazon Web Services estimates that this
-- recommendation could save you in a month, as a percentage of your costs.
newReservationPurchaseRecommendationSummary ::
  ReservationPurchaseRecommendationSummary
newReservationPurchaseRecommendationSummary =
  ReservationPurchaseRecommendationSummary'
    { currencyCode =
        Prelude.Nothing,
      totalEstimatedMonthlySavingsAmount =
        Prelude.Nothing,
      totalEstimatedMonthlySavingsPercentage =
        Prelude.Nothing
    }

-- | The currency code used for this recommendation.
reservationPurchaseRecommendationSummary_currencyCode :: Lens.Lens' ReservationPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationSummary_currencyCode = Lens.lens (\ReservationPurchaseRecommendationSummary' {currencyCode} -> currencyCode) (\s@ReservationPurchaseRecommendationSummary' {} a -> s {currencyCode = a} :: ReservationPurchaseRecommendationSummary)

-- | The total amount that Amazon Web Services estimates that this
-- recommendation could save you in a month.
reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsAmount :: Lens.Lens' ReservationPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsAmount = Lens.lens (\ReservationPurchaseRecommendationSummary' {totalEstimatedMonthlySavingsAmount} -> totalEstimatedMonthlySavingsAmount) (\s@ReservationPurchaseRecommendationSummary' {} a -> s {totalEstimatedMonthlySavingsAmount = a} :: ReservationPurchaseRecommendationSummary)

-- | The total amount that Amazon Web Services estimates that this
-- recommendation could save you in a month, as a percentage of your costs.
reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsPercentage :: Lens.Lens' ReservationPurchaseRecommendationSummary (Prelude.Maybe Prelude.Text)
reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsPercentage = Lens.lens (\ReservationPurchaseRecommendationSummary' {totalEstimatedMonthlySavingsPercentage} -> totalEstimatedMonthlySavingsPercentage) (\s@ReservationPurchaseRecommendationSummary' {} a -> s {totalEstimatedMonthlySavingsPercentage = a} :: ReservationPurchaseRecommendationSummary)

instance
  Data.FromJSON
    ReservationPurchaseRecommendationSummary
  where
  parseJSON =
    Data.withObject
      "ReservationPurchaseRecommendationSummary"
      ( \x ->
          ReservationPurchaseRecommendationSummary'
            Prelude.<$> (x Data..:? "CurrencyCode")
            Prelude.<*> (x Data..:? "TotalEstimatedMonthlySavingsAmount")
            Prelude.<*> ( x
                            Data..:? "TotalEstimatedMonthlySavingsPercentage"
                        )
      )

instance
  Prelude.Hashable
    ReservationPurchaseRecommendationSummary
  where
  hashWithSalt
    _salt
    ReservationPurchaseRecommendationSummary' {..} =
      _salt
        `Prelude.hashWithSalt` currencyCode
        `Prelude.hashWithSalt` totalEstimatedMonthlySavingsAmount
        `Prelude.hashWithSalt` totalEstimatedMonthlySavingsPercentage

instance
  Prelude.NFData
    ReservationPurchaseRecommendationSummary
  where
  rnf ReservationPurchaseRecommendationSummary' {..} =
    Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf totalEstimatedMonthlySavingsAmount
      `Prelude.seq` Prelude.rnf totalEstimatedMonthlySavingsPercentage
