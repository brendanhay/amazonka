{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansSavings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.SavingsPlansSavings
  ( SavingsPlansSavings (..)
  -- * Smart constructor
  , mkSavingsPlansSavings
  -- * Lenses
  , spsNetSavings
  , spsOnDemandCostEquivalent
  ) where

import qualified Network.AWS.CostExplorer.Types.NetSavings as Types
import qualified Network.AWS.CostExplorer.Types.OnDemandCostEquivalent as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The amount of savings you're accumulating, against the public On-Demand rate of the usage accrued in an account.
--
-- /See:/ 'mkSavingsPlansSavings' smart constructor.
data SavingsPlansSavings = SavingsPlansSavings'
  { netSavings :: Core.Maybe Types.NetSavings
    -- ^ The savings amount that you are accumulating for the usage that is covered by a Savings Plans, when compared to the On-Demand equivalent of the same usage.
  , onDemandCostEquivalent :: Core.Maybe Types.OnDemandCostEquivalent
    -- ^ How much the amount that the usage would have cost if it was accrued at the On-Demand rate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SavingsPlansSavings' value with any optional fields omitted.
mkSavingsPlansSavings
    :: SavingsPlansSavings
mkSavingsPlansSavings
  = SavingsPlansSavings'{netSavings = Core.Nothing,
                         onDemandCostEquivalent = Core.Nothing}

-- | The savings amount that you are accumulating for the usage that is covered by a Savings Plans, when compared to the On-Demand equivalent of the same usage.
--
-- /Note:/ Consider using 'netSavings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsNetSavings :: Lens.Lens' SavingsPlansSavings (Core.Maybe Types.NetSavings)
spsNetSavings = Lens.field @"netSavings"
{-# INLINEABLE spsNetSavings #-}
{-# DEPRECATED netSavings "Use generic-lens or generic-optics with 'netSavings' instead"  #-}

-- | How much the amount that the usage would have cost if it was accrued at the On-Demand rate.
--
-- /Note:/ Consider using 'onDemandCostEquivalent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsOnDemandCostEquivalent :: Lens.Lens' SavingsPlansSavings (Core.Maybe Types.OnDemandCostEquivalent)
spsOnDemandCostEquivalent = Lens.field @"onDemandCostEquivalent"
{-# INLINEABLE spsOnDemandCostEquivalent #-}
{-# DEPRECATED onDemandCostEquivalent "Use generic-lens or generic-optics with 'onDemandCostEquivalent' instead"  #-}

instance Core.FromJSON SavingsPlansSavings where
        parseJSON
          = Core.withObject "SavingsPlansSavings" Core.$
              \ x ->
                SavingsPlansSavings' Core.<$>
                  (x Core..:? "NetSavings") Core.<*>
                    x Core..:? "OnDemandCostEquivalent"
