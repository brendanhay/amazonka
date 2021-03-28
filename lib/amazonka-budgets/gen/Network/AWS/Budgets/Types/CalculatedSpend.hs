{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.CalculatedSpend
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.CalculatedSpend
  ( CalculatedSpend (..)
  -- * Smart constructor
  , mkCalculatedSpend
  -- * Lenses
  , csActualSpend
  , csForecastedSpend
  ) where

import qualified Network.AWS.Budgets.Types.Spend as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The spend objects that are associated with this budget. The @actualSpend@ tracks how much you've used, cost, usage, RI units, or Savings Plans units and the @forecastedSpend@ tracks how much you are predicted to spend based on your historical usage profile.
--
-- For example, if it is the 20th of the month and you have spent @50@ dollars on Amazon EC2, your @actualSpend@ is @50 USD@ , and your @forecastedSpend@ is @75 USD@ .
--
-- /See:/ 'mkCalculatedSpend' smart constructor.
data CalculatedSpend = CalculatedSpend'
  { actualSpend :: Types.Spend
    -- ^ The amount of cost, usage, RI units, or Savings Plans units that you have used.
  , forecastedSpend :: Core.Maybe Types.Spend
    -- ^ The amount of cost, usage, RI units, or Savings Plans units that you are forecasted to use.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CalculatedSpend' value with any optional fields omitted.
mkCalculatedSpend
    :: Types.Spend -- ^ 'actualSpend'
    -> CalculatedSpend
mkCalculatedSpend actualSpend
  = CalculatedSpend'{actualSpend, forecastedSpend = Core.Nothing}

-- | The amount of cost, usage, RI units, or Savings Plans units that you have used.
--
-- /Note:/ Consider using 'actualSpend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csActualSpend :: Lens.Lens' CalculatedSpend Types.Spend
csActualSpend = Lens.field @"actualSpend"
{-# INLINEABLE csActualSpend #-}
{-# DEPRECATED actualSpend "Use generic-lens or generic-optics with 'actualSpend' instead"  #-}

-- | The amount of cost, usage, RI units, or Savings Plans units that you are forecasted to use.
--
-- /Note:/ Consider using 'forecastedSpend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csForecastedSpend :: Lens.Lens' CalculatedSpend (Core.Maybe Types.Spend)
csForecastedSpend = Lens.field @"forecastedSpend"
{-# INLINEABLE csForecastedSpend #-}
{-# DEPRECATED forecastedSpend "Use generic-lens or generic-optics with 'forecastedSpend' instead"  #-}

instance Core.FromJSON CalculatedSpend where
        toJSON CalculatedSpend{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ActualSpend" Core..= actualSpend),
                  ("ForecastedSpend" Core..=) Core.<$> forecastedSpend])

instance Core.FromJSON CalculatedSpend where
        parseJSON
          = Core.withObject "CalculatedSpend" Core.$
              \ x ->
                CalculatedSpend' Core.<$>
                  (x Core..: "ActualSpend") Core.<*> x Core..:? "ForecastedSpend"
