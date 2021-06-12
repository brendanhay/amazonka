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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The aggregated numbers for your reservation usage.
--
-- /See:/ 'newReservationAggregates' smart constructor.
data ReservationAggregates = ReservationAggregates'
  { -- | The number of reservation hours that you didn\'t use.
    unusedHours :: Core.Maybe Core.Text,
    -- | The realized savings due to purchasing and using a reservation.
    realizedSavings :: Core.Maybe Core.Text,
    -- | The total number of reservation hours that you used.
    totalActualHours :: Core.Maybe Core.Text,
    -- | How many reservation hours that you purchased.
    purchasedHours :: Core.Maybe Core.Text,
    -- | The unrealized savings due to purchasing and using a reservation.
    unrealizedSavings :: Core.Maybe Core.Text,
    -- | How much your reservation would cost if charged On-Demand rates.
    onDemandCostOfRIHoursUsed :: Core.Maybe Core.Text,
    -- | The monthly cost of your reservation, amortized over the reservation
    -- period.
    amortizedRecurringFee :: Core.Maybe Core.Text,
    -- | The cost of unused hours for your reservation.
    rICostForUnusedHours :: Core.Maybe Core.Text,
    -- | The number of Amazon EC2 reservation hours that you didn\'t use,
    -- converted to normalized units. Normalized units are available only for
    -- Amazon EC2 usage after November 11, 2017.
    unusedUnits :: Core.Maybe Core.Text,
    -- | The total number of Amazon EC2 reservation hours that you used,
    -- converted to normalized units. Normalized units are available only for
    -- Amazon EC2 usage after November 11, 2017.
    totalActualUnits :: Core.Maybe Core.Text,
    -- | How much you could save if you use your entire reservation.
    totalPotentialRISavings :: Core.Maybe Core.Text,
    -- | How much you saved due to purchasing and utilizing reservation. AWS
    -- calculates this by subtracting @TotalAmortizedFee@ from
    -- @OnDemandCostOfRIHoursUsed@.
    netRISavings :: Core.Maybe Core.Text,
    -- | The total cost of your reservation, amortized over the reservation
    -- period.
    totalAmortizedFee :: Core.Maybe Core.Text,
    -- | The percentage of Amazon EC2 reservation time that you used, converted
    -- to normalized units. Normalized units are available only for Amazon EC2
    -- usage after November 11, 2017.
    utilizationPercentageInUnits :: Core.Maybe Core.Text,
    -- | The upfront cost of your reservation, amortized over the reservation
    -- period.
    amortizedUpfrontFee :: Core.Maybe Core.Text,
    -- | The percentage of reservation time that you used.
    utilizationPercentage :: Core.Maybe Core.Text,
    -- | How many Amazon EC2 reservation hours that you purchased, converted to
    -- normalized units. Normalized units are available only for Amazon EC2
    -- usage after November 11, 2017.
    purchasedUnits :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { unusedHours = Core.Nothing,
      realizedSavings = Core.Nothing,
      totalActualHours = Core.Nothing,
      purchasedHours = Core.Nothing,
      unrealizedSavings = Core.Nothing,
      onDemandCostOfRIHoursUsed = Core.Nothing,
      amortizedRecurringFee = Core.Nothing,
      rICostForUnusedHours = Core.Nothing,
      unusedUnits = Core.Nothing,
      totalActualUnits = Core.Nothing,
      totalPotentialRISavings = Core.Nothing,
      netRISavings = Core.Nothing,
      totalAmortizedFee = Core.Nothing,
      utilizationPercentageInUnits = Core.Nothing,
      amortizedUpfrontFee = Core.Nothing,
      utilizationPercentage = Core.Nothing,
      purchasedUnits = Core.Nothing
    }

-- | The number of reservation hours that you didn\'t use.
reservationAggregates_unusedHours :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_unusedHours = Lens.lens (\ReservationAggregates' {unusedHours} -> unusedHours) (\s@ReservationAggregates' {} a -> s {unusedHours = a} :: ReservationAggregates)

-- | The realized savings due to purchasing and using a reservation.
reservationAggregates_realizedSavings :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_realizedSavings = Lens.lens (\ReservationAggregates' {realizedSavings} -> realizedSavings) (\s@ReservationAggregates' {} a -> s {realizedSavings = a} :: ReservationAggregates)

-- | The total number of reservation hours that you used.
reservationAggregates_totalActualHours :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_totalActualHours = Lens.lens (\ReservationAggregates' {totalActualHours} -> totalActualHours) (\s@ReservationAggregates' {} a -> s {totalActualHours = a} :: ReservationAggregates)

-- | How many reservation hours that you purchased.
reservationAggregates_purchasedHours :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_purchasedHours = Lens.lens (\ReservationAggregates' {purchasedHours} -> purchasedHours) (\s@ReservationAggregates' {} a -> s {purchasedHours = a} :: ReservationAggregates)

-- | The unrealized savings due to purchasing and using a reservation.
reservationAggregates_unrealizedSavings :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_unrealizedSavings = Lens.lens (\ReservationAggregates' {unrealizedSavings} -> unrealizedSavings) (\s@ReservationAggregates' {} a -> s {unrealizedSavings = a} :: ReservationAggregates)

-- | How much your reservation would cost if charged On-Demand rates.
reservationAggregates_onDemandCostOfRIHoursUsed :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_onDemandCostOfRIHoursUsed = Lens.lens (\ReservationAggregates' {onDemandCostOfRIHoursUsed} -> onDemandCostOfRIHoursUsed) (\s@ReservationAggregates' {} a -> s {onDemandCostOfRIHoursUsed = a} :: ReservationAggregates)

-- | The monthly cost of your reservation, amortized over the reservation
-- period.
reservationAggregates_amortizedRecurringFee :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_amortizedRecurringFee = Lens.lens (\ReservationAggregates' {amortizedRecurringFee} -> amortizedRecurringFee) (\s@ReservationAggregates' {} a -> s {amortizedRecurringFee = a} :: ReservationAggregates)

-- | The cost of unused hours for your reservation.
reservationAggregates_rICostForUnusedHours :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_rICostForUnusedHours = Lens.lens (\ReservationAggregates' {rICostForUnusedHours} -> rICostForUnusedHours) (\s@ReservationAggregates' {} a -> s {rICostForUnusedHours = a} :: ReservationAggregates)

-- | The number of Amazon EC2 reservation hours that you didn\'t use,
-- converted to normalized units. Normalized units are available only for
-- Amazon EC2 usage after November 11, 2017.
reservationAggregates_unusedUnits :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_unusedUnits = Lens.lens (\ReservationAggregates' {unusedUnits} -> unusedUnits) (\s@ReservationAggregates' {} a -> s {unusedUnits = a} :: ReservationAggregates)

-- | The total number of Amazon EC2 reservation hours that you used,
-- converted to normalized units. Normalized units are available only for
-- Amazon EC2 usage after November 11, 2017.
reservationAggregates_totalActualUnits :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_totalActualUnits = Lens.lens (\ReservationAggregates' {totalActualUnits} -> totalActualUnits) (\s@ReservationAggregates' {} a -> s {totalActualUnits = a} :: ReservationAggregates)

-- | How much you could save if you use your entire reservation.
reservationAggregates_totalPotentialRISavings :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_totalPotentialRISavings = Lens.lens (\ReservationAggregates' {totalPotentialRISavings} -> totalPotentialRISavings) (\s@ReservationAggregates' {} a -> s {totalPotentialRISavings = a} :: ReservationAggregates)

-- | How much you saved due to purchasing and utilizing reservation. AWS
-- calculates this by subtracting @TotalAmortizedFee@ from
-- @OnDemandCostOfRIHoursUsed@.
reservationAggregates_netRISavings :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_netRISavings = Lens.lens (\ReservationAggregates' {netRISavings} -> netRISavings) (\s@ReservationAggregates' {} a -> s {netRISavings = a} :: ReservationAggregates)

-- | The total cost of your reservation, amortized over the reservation
-- period.
reservationAggregates_totalAmortizedFee :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_totalAmortizedFee = Lens.lens (\ReservationAggregates' {totalAmortizedFee} -> totalAmortizedFee) (\s@ReservationAggregates' {} a -> s {totalAmortizedFee = a} :: ReservationAggregates)

-- | The percentage of Amazon EC2 reservation time that you used, converted
-- to normalized units. Normalized units are available only for Amazon EC2
-- usage after November 11, 2017.
reservationAggregates_utilizationPercentageInUnits :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_utilizationPercentageInUnits = Lens.lens (\ReservationAggregates' {utilizationPercentageInUnits} -> utilizationPercentageInUnits) (\s@ReservationAggregates' {} a -> s {utilizationPercentageInUnits = a} :: ReservationAggregates)

-- | The upfront cost of your reservation, amortized over the reservation
-- period.
reservationAggregates_amortizedUpfrontFee :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_amortizedUpfrontFee = Lens.lens (\ReservationAggregates' {amortizedUpfrontFee} -> amortizedUpfrontFee) (\s@ReservationAggregates' {} a -> s {amortizedUpfrontFee = a} :: ReservationAggregates)

-- | The percentage of reservation time that you used.
reservationAggregates_utilizationPercentage :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_utilizationPercentage = Lens.lens (\ReservationAggregates' {utilizationPercentage} -> utilizationPercentage) (\s@ReservationAggregates' {} a -> s {utilizationPercentage = a} :: ReservationAggregates)

-- | How many Amazon EC2 reservation hours that you purchased, converted to
-- normalized units. Normalized units are available only for Amazon EC2
-- usage after November 11, 2017.
reservationAggregates_purchasedUnits :: Lens.Lens' ReservationAggregates (Core.Maybe Core.Text)
reservationAggregates_purchasedUnits = Lens.lens (\ReservationAggregates' {purchasedUnits} -> purchasedUnits) (\s@ReservationAggregates' {} a -> s {purchasedUnits = a} :: ReservationAggregates)

instance Core.FromJSON ReservationAggregates where
  parseJSON =
    Core.withObject
      "ReservationAggregates"
      ( \x ->
          ReservationAggregates'
            Core.<$> (x Core..:? "UnusedHours")
            Core.<*> (x Core..:? "RealizedSavings")
            Core.<*> (x Core..:? "TotalActualHours")
            Core.<*> (x Core..:? "PurchasedHours")
            Core.<*> (x Core..:? "UnrealizedSavings")
            Core.<*> (x Core..:? "OnDemandCostOfRIHoursUsed")
            Core.<*> (x Core..:? "AmortizedRecurringFee")
            Core.<*> (x Core..:? "RICostForUnusedHours")
            Core.<*> (x Core..:? "UnusedUnits")
            Core.<*> (x Core..:? "TotalActualUnits")
            Core.<*> (x Core..:? "TotalPotentialRISavings")
            Core.<*> (x Core..:? "NetRISavings")
            Core.<*> (x Core..:? "TotalAmortizedFee")
            Core.<*> (x Core..:? "UtilizationPercentageInUnits")
            Core.<*> (x Core..:? "AmortizedUpfrontFee")
            Core.<*> (x Core..:? "UtilizationPercentage")
            Core.<*> (x Core..:? "PurchasedUnits")
      )

instance Core.Hashable ReservationAggregates

instance Core.NFData ReservationAggregates
