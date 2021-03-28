{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.RecurringCharge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.RecurringCharge
  ( RecurringCharge (..)
  -- * Smart constructor
  , mkRecurringCharge
  -- * Lenses
  , rcCost
  , rcFrequency
  ) where

import qualified Network.AWS.DeviceFarm.Types.MonetaryAmount as Types
import qualified Network.AWS.DeviceFarm.Types.RecurringChargeFrequency as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies whether charges for devices are recurring.
--
-- /See:/ 'mkRecurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { cost :: Core.Maybe Types.MonetaryAmount
    -- ^ The cost of the recurring charge.
  , frequency :: Core.Maybe Types.RecurringChargeFrequency
    -- ^ The frequency in which charges recur.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecurringCharge' value with any optional fields omitted.
mkRecurringCharge
    :: RecurringCharge
mkRecurringCharge
  = RecurringCharge'{cost = Core.Nothing, frequency = Core.Nothing}

-- | The cost of the recurring charge.
--
-- /Note:/ Consider using 'cost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCost :: Lens.Lens' RecurringCharge (Core.Maybe Types.MonetaryAmount)
rcCost = Lens.field @"cost"
{-# INLINEABLE rcCost #-}
{-# DEPRECATED cost "Use generic-lens or generic-optics with 'cost' instead"  #-}

-- | The frequency in which charges recur.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcFrequency :: Lens.Lens' RecurringCharge (Core.Maybe Types.RecurringChargeFrequency)
rcFrequency = Lens.field @"frequency"
{-# INLINEABLE rcFrequency #-}
{-# DEPRECATED frequency "Use generic-lens or generic-optics with 'frequency' instead"  #-}

instance Core.FromJSON RecurringCharge where
        parseJSON
          = Core.withObject "RecurringCharge" Core.$
              \ x ->
                RecurringCharge' Core.<$>
                  (x Core..:? "cost") Core.<*> x Core..:? "frequency"
