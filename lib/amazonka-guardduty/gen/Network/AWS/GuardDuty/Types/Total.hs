{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Total
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.Total
  ( Total (..)
  -- * Smart constructor
  , mkTotal
  -- * Lenses
  , tAmount
  , tUnit
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the total usage with the corresponding currency unit for that value.
--
-- /See:/ 'mkTotal' smart constructor.
data Total = Total'
  { amount :: Core.Maybe Core.Text
    -- ^ The total usage.
  , unit :: Core.Maybe Core.Text
    -- ^ The currency unit that the amount is given in.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Total' value with any optional fields omitted.
mkTotal
    :: Total
mkTotal = Total'{amount = Core.Nothing, unit = Core.Nothing}

-- | The total usage.
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tAmount :: Lens.Lens' Total (Core.Maybe Core.Text)
tAmount = Lens.field @"amount"
{-# INLINEABLE tAmount #-}
{-# DEPRECATED amount "Use generic-lens or generic-optics with 'amount' instead"  #-}

-- | The currency unit that the amount is given in.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tUnit :: Lens.Lens' Total (Core.Maybe Core.Text)
tUnit = Lens.field @"unit"
{-# INLINEABLE tUnit #-}
{-# DEPRECATED unit "Use generic-lens or generic-optics with 'unit' instead"  #-}

instance Core.FromJSON Total where
        parseJSON
          = Core.withObject "Total" Core.$
              \ x ->
                Total' Core.<$> (x Core..:? "amount") Core.<*> x Core..:? "unit"
