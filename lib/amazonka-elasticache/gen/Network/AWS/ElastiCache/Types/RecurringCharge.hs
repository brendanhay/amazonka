{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.RecurringCharge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.RecurringCharge
  ( RecurringCharge (..)
  -- * Smart constructor
  , mkRecurringCharge
  -- * Lenses
  , rcRecurringChargeAmount
  , rcRecurringChargeFrequency
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the specific price and frequency of a recurring charges for a reserved cache node, or for a reserved cache node offering.
--
-- /See:/ 'mkRecurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { recurringChargeAmount :: Core.Maybe Core.Double
    -- ^ The monetary amount of the recurring charge.
  , recurringChargeFrequency :: Core.Maybe Core.Text
    -- ^ The frequency of the recurring charge.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecurringCharge' value with any optional fields omitted.
mkRecurringCharge
    :: RecurringCharge
mkRecurringCharge
  = RecurringCharge'{recurringChargeAmount = Core.Nothing,
                     recurringChargeFrequency = Core.Nothing}

-- | The monetary amount of the recurring charge.
--
-- /Note:/ Consider using 'recurringChargeAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecurringChargeAmount :: Lens.Lens' RecurringCharge (Core.Maybe Core.Double)
rcRecurringChargeAmount = Lens.field @"recurringChargeAmount"
{-# INLINEABLE rcRecurringChargeAmount #-}
{-# DEPRECATED recurringChargeAmount "Use generic-lens or generic-optics with 'recurringChargeAmount' instead"  #-}

-- | The frequency of the recurring charge.
--
-- /Note:/ Consider using 'recurringChargeFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecurringChargeFrequency :: Lens.Lens' RecurringCharge (Core.Maybe Core.Text)
rcRecurringChargeFrequency = Lens.field @"recurringChargeFrequency"
{-# INLINEABLE rcRecurringChargeFrequency #-}
{-# DEPRECATED recurringChargeFrequency "Use generic-lens or generic-optics with 'recurringChargeFrequency' instead"  #-}

instance Core.FromXML RecurringCharge where
        parseXML x
          = RecurringCharge' Core.<$>
              (x Core..@? "RecurringChargeAmount") Core.<*>
                x Core..@? "RecurringChargeFrequency"
