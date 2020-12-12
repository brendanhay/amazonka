{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationAggregates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationAggregates
  ( ReservationAggregates (..),

    -- * Smart constructor
    mkReservationAggregates,

    -- * Lenses
    raPurchasedHours,
    raTotalActualHours,
    raUtilizationPercentage,
    raTotalAmortizedFee,
    raUnusedUnits,
    raUnusedHours,
    raPurchasedUnits,
    raAmortizedUpfrontFee,
    raAmortizedRecurringFee,
    raUtilizationPercentageInUnits,
    raNetRISavings,
    raOnDemandCostOfRIHoursUsed,
    raTotalPotentialRISavings,
    raTotalActualUnits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The aggregated numbers for your reservation usage.
--
-- /See:/ 'mkReservationAggregates' smart constructor.
data ReservationAggregates = ReservationAggregates'
  { purchasedHours ::
      Lude.Maybe Lude.Text,
    totalActualHours :: Lude.Maybe Lude.Text,
    utilizationPercentage :: Lude.Maybe Lude.Text,
    totalAmortizedFee :: Lude.Maybe Lude.Text,
    unusedUnits :: Lude.Maybe Lude.Text,
    unusedHours :: Lude.Maybe Lude.Text,
    purchasedUnits :: Lude.Maybe Lude.Text,
    amortizedUpfrontFee :: Lude.Maybe Lude.Text,
    amortizedRecurringFee :: Lude.Maybe Lude.Text,
    utilizationPercentageInUnits ::
      Lude.Maybe Lude.Text,
    netRISavings :: Lude.Maybe Lude.Text,
    onDemandCostOfRIHoursUsed ::
      Lude.Maybe Lude.Text,
    totalPotentialRISavings :: Lude.Maybe Lude.Text,
    totalActualUnits :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservationAggregates' with the minimum fields required to make a request.
--
-- * 'amortizedRecurringFee' - The monthly cost of your reservation, amortized over the reservation period.
-- * 'amortizedUpfrontFee' - The upfront cost of your reservation, amortized over the reservation period.
-- * 'netRISavings' - How much you saved due to purchasing and utilizing reservation. AWS calculates this by subtracting @TotalAmortizedFee@ from @OnDemandCostOfRIHoursUsed@ .
-- * 'onDemandCostOfRIHoursUsed' - How much your reservation would cost if charged On-Demand rates.
-- * 'purchasedHours' - How many reservation hours that you purchased.
-- * 'purchasedUnits' - How many Amazon EC2 reservation hours that you purchased, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
-- * 'totalActualHours' - The total number of reservation hours that you used.
-- * 'totalActualUnits' - The total number of Amazon EC2 reservation hours that you used, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
-- * 'totalAmortizedFee' - The total cost of your reservation, amortized over the reservation period.
-- * 'totalPotentialRISavings' - How much you could save if you use your entire reservation.
-- * 'unusedHours' - The number of reservation hours that you didn't use.
-- * 'unusedUnits' - The number of Amazon EC2 reservation hours that you didn't use, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
-- * 'utilizationPercentage' - The percentage of reservation time that you used.
-- * 'utilizationPercentageInUnits' - The percentage of Amazon EC2 reservation time that you used, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
mkReservationAggregates ::
  ReservationAggregates
mkReservationAggregates =
  ReservationAggregates'
    { purchasedHours = Lude.Nothing,
      totalActualHours = Lude.Nothing,
      utilizationPercentage = Lude.Nothing,
      totalAmortizedFee = Lude.Nothing,
      unusedUnits = Lude.Nothing,
      unusedHours = Lude.Nothing,
      purchasedUnits = Lude.Nothing,
      amortizedUpfrontFee = Lude.Nothing,
      amortizedRecurringFee = Lude.Nothing,
      utilizationPercentageInUnits = Lude.Nothing,
      netRISavings = Lude.Nothing,
      onDemandCostOfRIHoursUsed = Lude.Nothing,
      totalPotentialRISavings = Lude.Nothing,
      totalActualUnits = Lude.Nothing
    }

-- | How many reservation hours that you purchased.
--
-- /Note:/ Consider using 'purchasedHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raPurchasedHours :: Lens.Lens' ReservationAggregates (Lude.Maybe Lude.Text)
raPurchasedHours = Lens.lens (purchasedHours :: ReservationAggregates -> Lude.Maybe Lude.Text) (\s a -> s {purchasedHours = a} :: ReservationAggregates)
{-# DEPRECATED raPurchasedHours "Use generic-lens or generic-optics with 'purchasedHours' instead." #-}

-- | The total number of reservation hours that you used.
--
-- /Note:/ Consider using 'totalActualHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raTotalActualHours :: Lens.Lens' ReservationAggregates (Lude.Maybe Lude.Text)
raTotalActualHours = Lens.lens (totalActualHours :: ReservationAggregates -> Lude.Maybe Lude.Text) (\s a -> s {totalActualHours = a} :: ReservationAggregates)
{-# DEPRECATED raTotalActualHours "Use generic-lens or generic-optics with 'totalActualHours' instead." #-}

-- | The percentage of reservation time that you used.
--
-- /Note:/ Consider using 'utilizationPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raUtilizationPercentage :: Lens.Lens' ReservationAggregates (Lude.Maybe Lude.Text)
raUtilizationPercentage = Lens.lens (utilizationPercentage :: ReservationAggregates -> Lude.Maybe Lude.Text) (\s a -> s {utilizationPercentage = a} :: ReservationAggregates)
{-# DEPRECATED raUtilizationPercentage "Use generic-lens or generic-optics with 'utilizationPercentage' instead." #-}

-- | The total cost of your reservation, amortized over the reservation period.
--
-- /Note:/ Consider using 'totalAmortizedFee' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raTotalAmortizedFee :: Lens.Lens' ReservationAggregates (Lude.Maybe Lude.Text)
raTotalAmortizedFee = Lens.lens (totalAmortizedFee :: ReservationAggregates -> Lude.Maybe Lude.Text) (\s a -> s {totalAmortizedFee = a} :: ReservationAggregates)
{-# DEPRECATED raTotalAmortizedFee "Use generic-lens or generic-optics with 'totalAmortizedFee' instead." #-}

-- | The number of Amazon EC2 reservation hours that you didn't use, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
--
-- /Note:/ Consider using 'unusedUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raUnusedUnits :: Lens.Lens' ReservationAggregates (Lude.Maybe Lude.Text)
raUnusedUnits = Lens.lens (unusedUnits :: ReservationAggregates -> Lude.Maybe Lude.Text) (\s a -> s {unusedUnits = a} :: ReservationAggregates)
{-# DEPRECATED raUnusedUnits "Use generic-lens or generic-optics with 'unusedUnits' instead." #-}

-- | The number of reservation hours that you didn't use.
--
-- /Note:/ Consider using 'unusedHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raUnusedHours :: Lens.Lens' ReservationAggregates (Lude.Maybe Lude.Text)
raUnusedHours = Lens.lens (unusedHours :: ReservationAggregates -> Lude.Maybe Lude.Text) (\s a -> s {unusedHours = a} :: ReservationAggregates)
{-# DEPRECATED raUnusedHours "Use generic-lens or generic-optics with 'unusedHours' instead." #-}

-- | How many Amazon EC2 reservation hours that you purchased, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
--
-- /Note:/ Consider using 'purchasedUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raPurchasedUnits :: Lens.Lens' ReservationAggregates (Lude.Maybe Lude.Text)
raPurchasedUnits = Lens.lens (purchasedUnits :: ReservationAggregates -> Lude.Maybe Lude.Text) (\s a -> s {purchasedUnits = a} :: ReservationAggregates)
{-# DEPRECATED raPurchasedUnits "Use generic-lens or generic-optics with 'purchasedUnits' instead." #-}

-- | The upfront cost of your reservation, amortized over the reservation period.
--
-- /Note:/ Consider using 'amortizedUpfrontFee' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAmortizedUpfrontFee :: Lens.Lens' ReservationAggregates (Lude.Maybe Lude.Text)
raAmortizedUpfrontFee = Lens.lens (amortizedUpfrontFee :: ReservationAggregates -> Lude.Maybe Lude.Text) (\s a -> s {amortizedUpfrontFee = a} :: ReservationAggregates)
{-# DEPRECATED raAmortizedUpfrontFee "Use generic-lens or generic-optics with 'amortizedUpfrontFee' instead." #-}

-- | The monthly cost of your reservation, amortized over the reservation period.
--
-- /Note:/ Consider using 'amortizedRecurringFee' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAmortizedRecurringFee :: Lens.Lens' ReservationAggregates (Lude.Maybe Lude.Text)
raAmortizedRecurringFee = Lens.lens (amortizedRecurringFee :: ReservationAggregates -> Lude.Maybe Lude.Text) (\s a -> s {amortizedRecurringFee = a} :: ReservationAggregates)
{-# DEPRECATED raAmortizedRecurringFee "Use generic-lens or generic-optics with 'amortizedRecurringFee' instead." #-}

-- | The percentage of Amazon EC2 reservation time that you used, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
--
-- /Note:/ Consider using 'utilizationPercentageInUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raUtilizationPercentageInUnits :: Lens.Lens' ReservationAggregates (Lude.Maybe Lude.Text)
raUtilizationPercentageInUnits = Lens.lens (utilizationPercentageInUnits :: ReservationAggregates -> Lude.Maybe Lude.Text) (\s a -> s {utilizationPercentageInUnits = a} :: ReservationAggregates)
{-# DEPRECATED raUtilizationPercentageInUnits "Use generic-lens or generic-optics with 'utilizationPercentageInUnits' instead." #-}

-- | How much you saved due to purchasing and utilizing reservation. AWS calculates this by subtracting @TotalAmortizedFee@ from @OnDemandCostOfRIHoursUsed@ .
--
-- /Note:/ Consider using 'netRISavings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raNetRISavings :: Lens.Lens' ReservationAggregates (Lude.Maybe Lude.Text)
raNetRISavings = Lens.lens (netRISavings :: ReservationAggregates -> Lude.Maybe Lude.Text) (\s a -> s {netRISavings = a} :: ReservationAggregates)
{-# DEPRECATED raNetRISavings "Use generic-lens or generic-optics with 'netRISavings' instead." #-}

-- | How much your reservation would cost if charged On-Demand rates.
--
-- /Note:/ Consider using 'onDemandCostOfRIHoursUsed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raOnDemandCostOfRIHoursUsed :: Lens.Lens' ReservationAggregates (Lude.Maybe Lude.Text)
raOnDemandCostOfRIHoursUsed = Lens.lens (onDemandCostOfRIHoursUsed :: ReservationAggregates -> Lude.Maybe Lude.Text) (\s a -> s {onDemandCostOfRIHoursUsed = a} :: ReservationAggregates)
{-# DEPRECATED raOnDemandCostOfRIHoursUsed "Use generic-lens or generic-optics with 'onDemandCostOfRIHoursUsed' instead." #-}

-- | How much you could save if you use your entire reservation.
--
-- /Note:/ Consider using 'totalPotentialRISavings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raTotalPotentialRISavings :: Lens.Lens' ReservationAggregates (Lude.Maybe Lude.Text)
raTotalPotentialRISavings = Lens.lens (totalPotentialRISavings :: ReservationAggregates -> Lude.Maybe Lude.Text) (\s a -> s {totalPotentialRISavings = a} :: ReservationAggregates)
{-# DEPRECATED raTotalPotentialRISavings "Use generic-lens or generic-optics with 'totalPotentialRISavings' instead." #-}

-- | The total number of Amazon EC2 reservation hours that you used, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
--
-- /Note:/ Consider using 'totalActualUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raTotalActualUnits :: Lens.Lens' ReservationAggregates (Lude.Maybe Lude.Text)
raTotalActualUnits = Lens.lens (totalActualUnits :: ReservationAggregates -> Lude.Maybe Lude.Text) (\s a -> s {totalActualUnits = a} :: ReservationAggregates)
{-# DEPRECATED raTotalActualUnits "Use generic-lens or generic-optics with 'totalActualUnits' instead." #-}

instance Lude.FromJSON ReservationAggregates where
  parseJSON =
    Lude.withObject
      "ReservationAggregates"
      ( \x ->
          ReservationAggregates'
            Lude.<$> (x Lude..:? "PurchasedHours")
            Lude.<*> (x Lude..:? "TotalActualHours")
            Lude.<*> (x Lude..:? "UtilizationPercentage")
            Lude.<*> (x Lude..:? "TotalAmortizedFee")
            Lude.<*> (x Lude..:? "UnusedUnits")
            Lude.<*> (x Lude..:? "UnusedHours")
            Lude.<*> (x Lude..:? "PurchasedUnits")
            Lude.<*> (x Lude..:? "AmortizedUpfrontFee")
            Lude.<*> (x Lude..:? "AmortizedRecurringFee")
            Lude.<*> (x Lude..:? "UtilizationPercentageInUnits")
            Lude.<*> (x Lude..:? "NetRISavings")
            Lude.<*> (x Lude..:? "OnDemandCostOfRIHoursUsed")
            Lude.<*> (x Lude..:? "TotalPotentialRISavings")
            Lude.<*> (x Lude..:? "TotalActualUnits")
      )
