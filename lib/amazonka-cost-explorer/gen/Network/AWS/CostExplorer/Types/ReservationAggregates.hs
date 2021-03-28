{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationAggregates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.ReservationAggregates
  ( ReservationAggregates (..)
  -- * Smart constructor
  , mkReservationAggregates
  -- * Lenses
  , raAmortizedRecurringFee
  , raAmortizedUpfrontFee
  , raNetRISavings
  , raOnDemandCostOfRIHoursUsed
  , raPurchasedHours
  , raPurchasedUnits
  , raTotalActualHours
  , raTotalActualUnits
  , raTotalAmortizedFee
  , raTotalPotentialRISavings
  , raUnusedHours
  , raUnusedUnits
  , raUtilizationPercentage
  , raUtilizationPercentageInUnits
  ) where

import qualified Network.AWS.CostExplorer.Types.AmortizedRecurringFee as Types
import qualified Network.AWS.CostExplorer.Types.AmortizedUpfrontFee as Types
import qualified Network.AWS.CostExplorer.Types.NetRISavings as Types
import qualified Network.AWS.CostExplorer.Types.OnDemandCostOfRIHoursUsed as Types
import qualified Network.AWS.CostExplorer.Types.PurchasedHours as Types
import qualified Network.AWS.CostExplorer.Types.PurchasedUnits as Types
import qualified Network.AWS.CostExplorer.Types.TotalActualHours as Types
import qualified Network.AWS.CostExplorer.Types.TotalActualUnits as Types
import qualified Network.AWS.CostExplorer.Types.TotalAmortizedFee as Types
import qualified Network.AWS.CostExplorer.Types.TotalPotentialRISavings as Types
import qualified Network.AWS.CostExplorer.Types.UnusedHours as Types
import qualified Network.AWS.CostExplorer.Types.UnusedUnits as Types
import qualified Network.AWS.CostExplorer.Types.UtilizationPercentage as Types
import qualified Network.AWS.CostExplorer.Types.UtilizationPercentageInUnits as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The aggregated numbers for your reservation usage.
--
-- /See:/ 'mkReservationAggregates' smart constructor.
data ReservationAggregates = ReservationAggregates'
  { amortizedRecurringFee :: Core.Maybe Types.AmortizedRecurringFee
    -- ^ The monthly cost of your reservation, amortized over the reservation period.
  , amortizedUpfrontFee :: Core.Maybe Types.AmortizedUpfrontFee
    -- ^ The upfront cost of your reservation, amortized over the reservation period.
  , netRISavings :: Core.Maybe Types.NetRISavings
    -- ^ How much you saved due to purchasing and utilizing reservation. AWS calculates this by subtracting @TotalAmortizedFee@ from @OnDemandCostOfRIHoursUsed@ .
  , onDemandCostOfRIHoursUsed :: Core.Maybe Types.OnDemandCostOfRIHoursUsed
    -- ^ How much your reservation would cost if charged On-Demand rates.
  , purchasedHours :: Core.Maybe Types.PurchasedHours
    -- ^ How many reservation hours that you purchased.
  , purchasedUnits :: Core.Maybe Types.PurchasedUnits
    -- ^ How many Amazon EC2 reservation hours that you purchased, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
  , totalActualHours :: Core.Maybe Types.TotalActualHours
    -- ^ The total number of reservation hours that you used.
  , totalActualUnits :: Core.Maybe Types.TotalActualUnits
    -- ^ The total number of Amazon EC2 reservation hours that you used, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
  , totalAmortizedFee :: Core.Maybe Types.TotalAmortizedFee
    -- ^ The total cost of your reservation, amortized over the reservation period.
  , totalPotentialRISavings :: Core.Maybe Types.TotalPotentialRISavings
    -- ^ How much you could save if you use your entire reservation.
  , unusedHours :: Core.Maybe Types.UnusedHours
    -- ^ The number of reservation hours that you didn't use.
  , unusedUnits :: Core.Maybe Types.UnusedUnits
    -- ^ The number of Amazon EC2 reservation hours that you didn't use, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
  , utilizationPercentage :: Core.Maybe Types.UtilizationPercentage
    -- ^ The percentage of reservation time that you used.
  , utilizationPercentageInUnits :: Core.Maybe Types.UtilizationPercentageInUnits
    -- ^ The percentage of Amazon EC2 reservation time that you used, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservationAggregates' value with any optional fields omitted.
mkReservationAggregates
    :: ReservationAggregates
mkReservationAggregates
  = ReservationAggregates'{amortizedRecurringFee = Core.Nothing,
                           amortizedUpfrontFee = Core.Nothing, netRISavings = Core.Nothing,
                           onDemandCostOfRIHoursUsed = Core.Nothing,
                           purchasedHours = Core.Nothing, purchasedUnits = Core.Nothing,
                           totalActualHours = Core.Nothing, totalActualUnits = Core.Nothing,
                           totalAmortizedFee = Core.Nothing,
                           totalPotentialRISavings = Core.Nothing, unusedHours = Core.Nothing,
                           unusedUnits = Core.Nothing, utilizationPercentage = Core.Nothing,
                           utilizationPercentageInUnits = Core.Nothing}

-- | The monthly cost of your reservation, amortized over the reservation period.
--
-- /Note:/ Consider using 'amortizedRecurringFee' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAmortizedRecurringFee :: Lens.Lens' ReservationAggregates (Core.Maybe Types.AmortizedRecurringFee)
raAmortizedRecurringFee = Lens.field @"amortizedRecurringFee"
{-# INLINEABLE raAmortizedRecurringFee #-}
{-# DEPRECATED amortizedRecurringFee "Use generic-lens or generic-optics with 'amortizedRecurringFee' instead"  #-}

-- | The upfront cost of your reservation, amortized over the reservation period.
--
-- /Note:/ Consider using 'amortizedUpfrontFee' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAmortizedUpfrontFee :: Lens.Lens' ReservationAggregates (Core.Maybe Types.AmortizedUpfrontFee)
raAmortizedUpfrontFee = Lens.field @"amortizedUpfrontFee"
{-# INLINEABLE raAmortizedUpfrontFee #-}
{-# DEPRECATED amortizedUpfrontFee "Use generic-lens or generic-optics with 'amortizedUpfrontFee' instead"  #-}

-- | How much you saved due to purchasing and utilizing reservation. AWS calculates this by subtracting @TotalAmortizedFee@ from @OnDemandCostOfRIHoursUsed@ .
--
-- /Note:/ Consider using 'netRISavings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raNetRISavings :: Lens.Lens' ReservationAggregates (Core.Maybe Types.NetRISavings)
raNetRISavings = Lens.field @"netRISavings"
{-# INLINEABLE raNetRISavings #-}
{-# DEPRECATED netRISavings "Use generic-lens or generic-optics with 'netRISavings' instead"  #-}

-- | How much your reservation would cost if charged On-Demand rates.
--
-- /Note:/ Consider using 'onDemandCostOfRIHoursUsed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raOnDemandCostOfRIHoursUsed :: Lens.Lens' ReservationAggregates (Core.Maybe Types.OnDemandCostOfRIHoursUsed)
raOnDemandCostOfRIHoursUsed = Lens.field @"onDemandCostOfRIHoursUsed"
{-# INLINEABLE raOnDemandCostOfRIHoursUsed #-}
{-# DEPRECATED onDemandCostOfRIHoursUsed "Use generic-lens or generic-optics with 'onDemandCostOfRIHoursUsed' instead"  #-}

-- | How many reservation hours that you purchased.
--
-- /Note:/ Consider using 'purchasedHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raPurchasedHours :: Lens.Lens' ReservationAggregates (Core.Maybe Types.PurchasedHours)
raPurchasedHours = Lens.field @"purchasedHours"
{-# INLINEABLE raPurchasedHours #-}
{-# DEPRECATED purchasedHours "Use generic-lens or generic-optics with 'purchasedHours' instead"  #-}

-- | How many Amazon EC2 reservation hours that you purchased, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
--
-- /Note:/ Consider using 'purchasedUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raPurchasedUnits :: Lens.Lens' ReservationAggregates (Core.Maybe Types.PurchasedUnits)
raPurchasedUnits = Lens.field @"purchasedUnits"
{-# INLINEABLE raPurchasedUnits #-}
{-# DEPRECATED purchasedUnits "Use generic-lens or generic-optics with 'purchasedUnits' instead"  #-}

-- | The total number of reservation hours that you used.
--
-- /Note:/ Consider using 'totalActualHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raTotalActualHours :: Lens.Lens' ReservationAggregates (Core.Maybe Types.TotalActualHours)
raTotalActualHours = Lens.field @"totalActualHours"
{-# INLINEABLE raTotalActualHours #-}
{-# DEPRECATED totalActualHours "Use generic-lens or generic-optics with 'totalActualHours' instead"  #-}

-- | The total number of Amazon EC2 reservation hours that you used, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
--
-- /Note:/ Consider using 'totalActualUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raTotalActualUnits :: Lens.Lens' ReservationAggregates (Core.Maybe Types.TotalActualUnits)
raTotalActualUnits = Lens.field @"totalActualUnits"
{-# INLINEABLE raTotalActualUnits #-}
{-# DEPRECATED totalActualUnits "Use generic-lens or generic-optics with 'totalActualUnits' instead"  #-}

-- | The total cost of your reservation, amortized over the reservation period.
--
-- /Note:/ Consider using 'totalAmortizedFee' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raTotalAmortizedFee :: Lens.Lens' ReservationAggregates (Core.Maybe Types.TotalAmortizedFee)
raTotalAmortizedFee = Lens.field @"totalAmortizedFee"
{-# INLINEABLE raTotalAmortizedFee #-}
{-# DEPRECATED totalAmortizedFee "Use generic-lens or generic-optics with 'totalAmortizedFee' instead"  #-}

-- | How much you could save if you use your entire reservation.
--
-- /Note:/ Consider using 'totalPotentialRISavings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raTotalPotentialRISavings :: Lens.Lens' ReservationAggregates (Core.Maybe Types.TotalPotentialRISavings)
raTotalPotentialRISavings = Lens.field @"totalPotentialRISavings"
{-# INLINEABLE raTotalPotentialRISavings #-}
{-# DEPRECATED totalPotentialRISavings "Use generic-lens or generic-optics with 'totalPotentialRISavings' instead"  #-}

-- | The number of reservation hours that you didn't use.
--
-- /Note:/ Consider using 'unusedHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raUnusedHours :: Lens.Lens' ReservationAggregates (Core.Maybe Types.UnusedHours)
raUnusedHours = Lens.field @"unusedHours"
{-# INLINEABLE raUnusedHours #-}
{-# DEPRECATED unusedHours "Use generic-lens or generic-optics with 'unusedHours' instead"  #-}

-- | The number of Amazon EC2 reservation hours that you didn't use, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
--
-- /Note:/ Consider using 'unusedUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raUnusedUnits :: Lens.Lens' ReservationAggregates (Core.Maybe Types.UnusedUnits)
raUnusedUnits = Lens.field @"unusedUnits"
{-# INLINEABLE raUnusedUnits #-}
{-# DEPRECATED unusedUnits "Use generic-lens or generic-optics with 'unusedUnits' instead"  #-}

-- | The percentage of reservation time that you used.
--
-- /Note:/ Consider using 'utilizationPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raUtilizationPercentage :: Lens.Lens' ReservationAggregates (Core.Maybe Types.UtilizationPercentage)
raUtilizationPercentage = Lens.field @"utilizationPercentage"
{-# INLINEABLE raUtilizationPercentage #-}
{-# DEPRECATED utilizationPercentage "Use generic-lens or generic-optics with 'utilizationPercentage' instead"  #-}

-- | The percentage of Amazon EC2 reservation time that you used, converted to normalized units. Normalized units are available only for Amazon EC2 usage after November 11, 2017.
--
-- /Note:/ Consider using 'utilizationPercentageInUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raUtilizationPercentageInUnits :: Lens.Lens' ReservationAggregates (Core.Maybe Types.UtilizationPercentageInUnits)
raUtilizationPercentageInUnits = Lens.field @"utilizationPercentageInUnits"
{-# INLINEABLE raUtilizationPercentageInUnits #-}
{-# DEPRECATED utilizationPercentageInUnits "Use generic-lens or generic-optics with 'utilizationPercentageInUnits' instead"  #-}

instance Core.FromJSON ReservationAggregates where
        parseJSON
          = Core.withObject "ReservationAggregates" Core.$
              \ x ->
                ReservationAggregates' Core.<$>
                  (x Core..:? "AmortizedRecurringFee") Core.<*>
                    x Core..:? "AmortizedUpfrontFee"
                    Core.<*> x Core..:? "NetRISavings"
                    Core.<*> x Core..:? "OnDemandCostOfRIHoursUsed"
                    Core.<*> x Core..:? "PurchasedHours"
                    Core.<*> x Core..:? "PurchasedUnits"
                    Core.<*> x Core..:? "TotalActualHours"
                    Core.<*> x Core..:? "TotalActualUnits"
                    Core.<*> x Core..:? "TotalAmortizedFee"
                    Core.<*> x Core..:? "TotalPotentialRISavings"
                    Core.<*> x Core..:? "UnusedHours"
                    Core.<*> x Core..:? "UnusedUnits"
                    Core.<*> x Core..:? "UtilizationPercentage"
                    Core.<*> x Core..:? "UtilizationPercentageInUnits"
