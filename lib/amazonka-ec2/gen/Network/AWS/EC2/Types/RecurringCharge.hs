{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RecurringCharge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RecurringCharge
  ( RecurringCharge (..),

    -- * Smart constructor
    mkRecurringCharge,

    -- * Lenses
    rcAmount,
    rcFrequency,
  )
where

import qualified Network.AWS.EC2.Types.RecurringChargeFrequency as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a recurring charge.
--
-- /See:/ 'mkRecurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { -- | The amount of the recurring charge.
    amount :: Core.Maybe Core.Double,
    -- | The frequency of the recurring charge.
    frequency :: Core.Maybe Types.RecurringChargeFrequency
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecurringCharge' value with any optional fields omitted.
mkRecurringCharge ::
  RecurringCharge
mkRecurringCharge =
  RecurringCharge' {amount = Core.Nothing, frequency = Core.Nothing}

-- | The amount of the recurring charge.
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcAmount :: Lens.Lens' RecurringCharge (Core.Maybe Core.Double)
rcAmount = Lens.field @"amount"
{-# DEPRECATED rcAmount "Use generic-lens or generic-optics with 'amount' instead." #-}

-- | The frequency of the recurring charge.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcFrequency :: Lens.Lens' RecurringCharge (Core.Maybe Types.RecurringChargeFrequency)
rcFrequency = Lens.field @"frequency"
{-# DEPRECATED rcFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

instance Core.FromXML RecurringCharge where
  parseXML x =
    RecurringCharge'
      Core.<$> (x Core..@? "amount") Core.<*> (x Core..@? "frequency")
