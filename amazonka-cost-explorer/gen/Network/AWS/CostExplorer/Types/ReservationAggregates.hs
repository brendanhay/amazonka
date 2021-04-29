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
-- Module      : Network.AWS.CostExplorer.Types.ReservationAggregates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationAggregates where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The aggregated numbers for your reservation usage.
--
-- /See:/ 'newReservationAggregates' smart constructor.
data ReservationAggregates = ReservationAggregates'
  { -- | The number of reservation hours that you didn\'t use.
    unusedHours :: Prelude.Maybe Prelude.Text,
    -- | The realized savings due to purchasing and using a reservation.
    realizedSavings :: Prelude.Maybe Prelude.Text,
    -- | The total number of reservation hours that you used.
    totalActualHours :: Prelude.Maybe Prelude.Text,
    -- | How many reservation hours that you purchased.
    purchasedHours :: Prelude.Maybe Prelude.Text,
    -- | The unrealized savings due to purchasing and using a reservation.
    unrealizedSavings :: Prelude.Maybe Prelude.Text,
    -- | How much your reservation would cost if charged On-Demand rates.
    onDemandCostOfRIHoursUsed :: Prelude.Maybe Prelude.Text,
    -- | The monthly cost of your reservation, amortized over the reservation
    -- period.
    amortizedRecurringFee :: Prelude.Maybe Prelude.Text,
    -- | The cost of unused hours for your reservation.
    rICostForUnusedHours :: Prelude.Maybe Prelude.Text,
    -- | The number of Amazon EC2 reservation hours that you didn\'t use,
    -- converted to normalized units. Normalized units are available only for
    -- Amazon EC2 usage after November 11, 2017.
    unusedUnits :: Prelude.Maybe Prelude.Text,
    -- | The total number of Amazon EC2 reservation hours that you used,
    -- converted to normalized units. Normalized units are available only for
    -- Amazon EC2 usage after November 11, 2017.
    totalActualUnits :: Prelude.Maybe Prelude.Text,
    -- | How much you could save if you use your entire reservation.
    totalPotentialRISavings :: Prelude.Maybe Prelude.Text,
    -- | How much you saved due to purchasing and utilizing reservation. AWS
    -- calculates this by subtracting @TotalAmortizedFee@ from
    -- @OnDemandCostOfRIHoursUsed@.
    netRISavings :: Prelude.Maybe Prelude.Text,
    -- | The total cost of your reservation, amortized over the reservation
    -- period.
    totalAmortizedFee :: Prelude.Maybe Prelude.Text,
    -- | The percentage of Amazon EC2 reservation time that you used, converted
    -- to normalized units. Normalized units are available only for Amazon EC2
    -- usage after November 11, 2017.
    utilizationPercentageInUnits :: Prelude.Maybe Prelude.Text,
    -- | The upfront cost of your reservation, amortized over the reservation
    -- period.
    amortizedUpfrontFee :: Prelude.Maybe Prelude.Text,
    -- | The percentage of reservation time that you used.
    utilizationPercentage :: Prelude.Maybe Prelude.Text,
    -- | How many Amazon EC2 reservation hours that you purchased, converted to
    -- normalized units. Normalized units are available only for Amazon EC2
    -- usage after November 11, 2017.
    purchasedUnits :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReservationAggregates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unusedHours', 'reservationAggregates_unusedHours' - The number of reservation hours that you didn\'t use.
--
-- 'realizedSavings', 'reservationAggregates_realizedSavings' - The realized savings due to purchasing and using a reservation.
--
-- 'totalActualHours', 'reservationAggregates_totalActualHours' - The total number of reservation hours that you used.
--
-- 'purchasedHours', 'reservationAggregates_purchasedHours' - How many reservation hours that you purchased.
--
-- 'unrealizedSavings', 'reservationAggregates_unrealizedSavings' - The unrealized savings due to purchasing and using a reservation.
--
-- 'onDemandCostOfRIHoursUsed', 'reservationAggregates_onDemandCostOfRIHoursUsed' - How much your reservation would cost if charged On-Demand rates.
--
-- 'amortizedRecurringFee', 'reservationAggregates_amortizedRecurringFee' - The monthly cost of your reservation, amortized over the reservation
-- period.
--
-- 'rICostForUnusedHours', 'reservationAggregates_rICostForUnusedHours' - The cost of unused hours for your reservation.
--
-- 'unusedUnits', 'reservationAggregates_unusedUnits' - The number of Amazon EC2 reservation hours that you didn\'t use,
-- converted to normalized units. Normalized units are available only for
-- Amazon EC2 usage after November 11, 2017.
--
-- 'totalActualUnits', 'reservationAggregates_totalActualUnits' - The total number of Amazon EC2 reservation hours that you used,
-- converted to normalized units. Normalized units are available only for
-- Amazon EC2 usage after November 11, 2017.
--
-- 'totalPotentialRISavings', 'reservationAggregates_totalPotentialRISavings' - How much you could save if you use your entire reservation.
--
-- 'netRISavings', 'reservationAggregates_netRISavings' - How much you saved due to purchasing and utilizing reservation. AWS
-- calculates this by subtracting @TotalAmortizedFee@ from
-- @OnDemandCostOfRIHoursUsed@.
--
-- 'totalAmortizedFee', 'reservationAggregates_totalAmortizedFee' - The total cost of your reservation, amortized over the reservation
-- period.
--
-- 'utilizationPercentageInUnits', 'reservationAggregates_utilizationPercentageInUnits' - The percentage of Amazon EC2 reservation time that you used, converted
-- to normalized units. Normalized units are available only for Amazon EC2
-- usage after November 11, 2017.
--
-- 'amortizedUpfrontFee', 'reservationAggregates_amortizedUpfrontFee' - The upfront cost of your reservation, amortized over the reservation
-- period.
--
-- 'utilizationPercentage', 'reservationAggregates_utilizationPercentage' - The percentage of reservation time that you used.
--
-- 'purchasedUnits', 'reservationAggregates_purchasedUnits' - How many Amazon EC2 reservation hours that you purchased, converted to
-- normalized units. Normalized units are available only for Amazon EC2
-- usage after November 11, 2017.
newReservationAggregates ::
  ReservationAggregates
newReservationAggregates =
  ReservationAggregates'
    { unusedHours =
        Prelude.Nothing,
      realizedSavings = Prelude.Nothing,
      totalActualHours = Prelude.Nothing,
      purchasedHours = Prelude.Nothing,
      unrealizedSavings = Prelude.Nothing,
      onDemandCostOfRIHoursUsed = Prelude.Nothing,
      amortizedRecurringFee = Prelude.Nothing,
      rICostForUnusedHours = Prelude.Nothing,
      unusedUnits = Prelude.Nothing,
      totalActualUnits = Prelude.Nothing,
      totalPotentialRISavings = Prelude.Nothing,
      netRISavings = Prelude.Nothing,
      totalAmortizedFee = Prelude.Nothing,
      utilizationPercentageInUnits = Prelude.Nothing,
      amortizedUpfrontFee = Prelude.Nothing,
      utilizationPercentage = Prelude.Nothing,
      purchasedUnits = Prelude.Nothing
    }

-- | The number of reservation hours that you didn\'t use.
reservationAggregates_unusedHours :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_unusedHours = Lens.lens (\ReservationAggregates' {unusedHours} -> unusedHours) (\s@ReservationAggregates' {} a -> s {unusedHours = a} :: ReservationAggregates)

-- | The realized savings due to purchasing and using a reservation.
reservationAggregates_realizedSavings :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_realizedSavings = Lens.lens (\ReservationAggregates' {realizedSavings} -> realizedSavings) (\s@ReservationAggregates' {} a -> s {realizedSavings = a} :: ReservationAggregates)

-- | The total number of reservation hours that you used.
reservationAggregates_totalActualHours :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_totalActualHours = Lens.lens (\ReservationAggregates' {totalActualHours} -> totalActualHours) (\s@ReservationAggregates' {} a -> s {totalActualHours = a} :: ReservationAggregates)

-- | How many reservation hours that you purchased.
reservationAggregates_purchasedHours :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_purchasedHours = Lens.lens (\ReservationAggregates' {purchasedHours} -> purchasedHours) (\s@ReservationAggregates' {} a -> s {purchasedHours = a} :: ReservationAggregates)

-- | The unrealized savings due to purchasing and using a reservation.
reservationAggregates_unrealizedSavings :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_unrealizedSavings = Lens.lens (\ReservationAggregates' {unrealizedSavings} -> unrealizedSavings) (\s@ReservationAggregates' {} a -> s {unrealizedSavings = a} :: ReservationAggregates)

-- | How much your reservation would cost if charged On-Demand rates.
reservationAggregates_onDemandCostOfRIHoursUsed :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_onDemandCostOfRIHoursUsed = Lens.lens (\ReservationAggregates' {onDemandCostOfRIHoursUsed} -> onDemandCostOfRIHoursUsed) (\s@ReservationAggregates' {} a -> s {onDemandCostOfRIHoursUsed = a} :: ReservationAggregates)

-- | The monthly cost of your reservation, amortized over the reservation
-- period.
reservationAggregates_amortizedRecurringFee :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_amortizedRecurringFee = Lens.lens (\ReservationAggregates' {amortizedRecurringFee} -> amortizedRecurringFee) (\s@ReservationAggregates' {} a -> s {amortizedRecurringFee = a} :: ReservationAggregates)

-- | The cost of unused hours for your reservation.
reservationAggregates_rICostForUnusedHours :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_rICostForUnusedHours = Lens.lens (\ReservationAggregates' {rICostForUnusedHours} -> rICostForUnusedHours) (\s@ReservationAggregates' {} a -> s {rICostForUnusedHours = a} :: ReservationAggregates)

-- | The number of Amazon EC2 reservation hours that you didn\'t use,
-- converted to normalized units. Normalized units are available only for
-- Amazon EC2 usage after November 11, 2017.
reservationAggregates_unusedUnits :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_unusedUnits = Lens.lens (\ReservationAggregates' {unusedUnits} -> unusedUnits) (\s@ReservationAggregates' {} a -> s {unusedUnits = a} :: ReservationAggregates)

-- | The total number of Amazon EC2 reservation hours that you used,
-- converted to normalized units. Normalized units are available only for
-- Amazon EC2 usage after November 11, 2017.
reservationAggregates_totalActualUnits :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_totalActualUnits = Lens.lens (\ReservationAggregates' {totalActualUnits} -> totalActualUnits) (\s@ReservationAggregates' {} a -> s {totalActualUnits = a} :: ReservationAggregates)

-- | How much you could save if you use your entire reservation.
reservationAggregates_totalPotentialRISavings :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_totalPotentialRISavings = Lens.lens (\ReservationAggregates' {totalPotentialRISavings} -> totalPotentialRISavings) (\s@ReservationAggregates' {} a -> s {totalPotentialRISavings = a} :: ReservationAggregates)

-- | How much you saved due to purchasing and utilizing reservation. AWS
-- calculates this by subtracting @TotalAmortizedFee@ from
-- @OnDemandCostOfRIHoursUsed@.
reservationAggregates_netRISavings :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_netRISavings = Lens.lens (\ReservationAggregates' {netRISavings} -> netRISavings) (\s@ReservationAggregates' {} a -> s {netRISavings = a} :: ReservationAggregates)

-- | The total cost of your reservation, amortized over the reservation
-- period.
reservationAggregates_totalAmortizedFee :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_totalAmortizedFee = Lens.lens (\ReservationAggregates' {totalAmortizedFee} -> totalAmortizedFee) (\s@ReservationAggregates' {} a -> s {totalAmortizedFee = a} :: ReservationAggregates)

-- | The percentage of Amazon EC2 reservation time that you used, converted
-- to normalized units. Normalized units are available only for Amazon EC2
-- usage after November 11, 2017.
reservationAggregates_utilizationPercentageInUnits :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_utilizationPercentageInUnits = Lens.lens (\ReservationAggregates' {utilizationPercentageInUnits} -> utilizationPercentageInUnits) (\s@ReservationAggregates' {} a -> s {utilizationPercentageInUnits = a} :: ReservationAggregates)

-- | The upfront cost of your reservation, amortized over the reservation
-- period.
reservationAggregates_amortizedUpfrontFee :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_amortizedUpfrontFee = Lens.lens (\ReservationAggregates' {amortizedUpfrontFee} -> amortizedUpfrontFee) (\s@ReservationAggregates' {} a -> s {amortizedUpfrontFee = a} :: ReservationAggregates)

-- | The percentage of reservation time that you used.
reservationAggregates_utilizationPercentage :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_utilizationPercentage = Lens.lens (\ReservationAggregates' {utilizationPercentage} -> utilizationPercentage) (\s@ReservationAggregates' {} a -> s {utilizationPercentage = a} :: ReservationAggregates)

-- | How many Amazon EC2 reservation hours that you purchased, converted to
-- normalized units. Normalized units are available only for Amazon EC2
-- usage after November 11, 2017.
reservationAggregates_purchasedUnits :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_purchasedUnits = Lens.lens (\ReservationAggregates' {purchasedUnits} -> purchasedUnits) (\s@ReservationAggregates' {} a -> s {purchasedUnits = a} :: ReservationAggregates)

instance Prelude.FromJSON ReservationAggregates where
  parseJSON =
    Prelude.withObject
      "ReservationAggregates"
      ( \x ->
          ReservationAggregates'
            Prelude.<$> (x Prelude..:? "UnusedHours")
            Prelude.<*> (x Prelude..:? "RealizedSavings")
            Prelude.<*> (x Prelude..:? "TotalActualHours")
            Prelude.<*> (x Prelude..:? "PurchasedHours")
            Prelude.<*> (x Prelude..:? "UnrealizedSavings")
            Prelude.<*> (x Prelude..:? "OnDemandCostOfRIHoursUsed")
            Prelude.<*> (x Prelude..:? "AmortizedRecurringFee")
            Prelude.<*> (x Prelude..:? "RICostForUnusedHours")
            Prelude.<*> (x Prelude..:? "UnusedUnits")
            Prelude.<*> (x Prelude..:? "TotalActualUnits")
            Prelude.<*> (x Prelude..:? "TotalPotentialRISavings")
            Prelude.<*> (x Prelude..:? "NetRISavings")
            Prelude.<*> (x Prelude..:? "TotalAmortizedFee")
            Prelude.<*> (x Prelude..:? "UtilizationPercentageInUnits")
            Prelude.<*> (x Prelude..:? "AmortizedUpfrontFee")
            Prelude.<*> (x Prelude..:? "UtilizationPercentage")
            Prelude.<*> (x Prelude..:? "PurchasedUnits")
      )

instance Prelude.Hashable ReservationAggregates

instance Prelude.NFData ReservationAggregates
