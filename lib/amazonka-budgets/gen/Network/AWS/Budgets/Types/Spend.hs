{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Spend
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.Spend
  ( Spend (..)
  -- * Smart constructor
  , mkSpend
  -- * Lenses
  , sAmount
  , sUnit
  ) where

import qualified Network.AWS.Budgets.Types.Amount as Types
import qualified Network.AWS.Budgets.Types.Unit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The amount of cost or usage that is measured for a budget.
--
-- For example, a @Spend@ for @3 GB@ of S3 usage would have the following parameters:
--
--     * An @Amount@ of @3@ 
--
--
--     * A @unit@ of @GB@ 
--
--
--
-- /See:/ 'mkSpend' smart constructor.
data Spend = Spend'
  { amount :: Types.Amount
    -- ^ The cost or usage amount that is associated with a budget forecast, actual spend, or budget threshold.
  , unit :: Types.Unit
    -- ^ The unit of measurement that is used for the budget forecast, actual spend, or budget threshold, such as dollars or GB.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Spend' value with any optional fields omitted.
mkSpend
    :: Types.Amount -- ^ 'amount'
    -> Types.Unit -- ^ 'unit'
    -> Spend
mkSpend amount unit = Spend'{amount, unit}

-- | The cost or usage amount that is associated with a budget forecast, actual spend, or budget threshold.
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAmount :: Lens.Lens' Spend Types.Amount
sAmount = Lens.field @"amount"
{-# INLINEABLE sAmount #-}
{-# DEPRECATED amount "Use generic-lens or generic-optics with 'amount' instead"  #-}

-- | The unit of measurement that is used for the budget forecast, actual spend, or budget threshold, such as dollars or GB.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUnit :: Lens.Lens' Spend Types.Unit
sUnit = Lens.field @"unit"
{-# INLINEABLE sUnit #-}
{-# DEPRECATED unit "Use generic-lens or generic-optics with 'unit' instead"  #-}

instance Core.FromJSON Spend where
        toJSON Spend{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Amount" Core..= amount),
                  Core.Just ("Unit" Core..= unit)])

instance Core.FromJSON Spend where
        parseJSON
          = Core.withObject "Spend" Core.$
              \ x ->
                Spend' Core.<$> (x Core..: "Amount") Core.<*> x Core..: "Unit"
