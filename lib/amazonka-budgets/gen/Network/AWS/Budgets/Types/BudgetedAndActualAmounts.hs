{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.BudgetedAndActualAmounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.BudgetedAndActualAmounts
  ( BudgetedAndActualAmounts (..),

    -- * Smart constructor
    mkBudgetedAndActualAmounts,

    -- * Lenses
    baaaActualAmount,
    baaaBudgetedAmount,
    baaaTimePeriod,
  )
where

import qualified Network.AWS.Budgets.Types.Spend as Types
import qualified Network.AWS.Budgets.Types.TimePeriod as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The amount of cost or usage that you created the budget for, compared to your actual costs or usage.
--
-- /See:/ 'mkBudgetedAndActualAmounts' smart constructor.
data BudgetedAndActualAmounts = BudgetedAndActualAmounts'
  { -- | Your actual costs or usage for a budget period.
    actualAmount :: Core.Maybe Types.Spend,
    -- | The amount of cost or usage that you created the budget for.
    budgetedAmount :: Core.Maybe Types.Spend,
    -- | The time period covered by this budget comparison.
    timePeriod :: Core.Maybe Types.TimePeriod
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BudgetedAndActualAmounts' value with any optional fields omitted.
mkBudgetedAndActualAmounts ::
  BudgetedAndActualAmounts
mkBudgetedAndActualAmounts =
  BudgetedAndActualAmounts'
    { actualAmount = Core.Nothing,
      budgetedAmount = Core.Nothing,
      timePeriod = Core.Nothing
    }

-- | Your actual costs or usage for a budget period.
--
-- /Note:/ Consider using 'actualAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baaaActualAmount :: Lens.Lens' BudgetedAndActualAmounts (Core.Maybe Types.Spend)
baaaActualAmount = Lens.field @"actualAmount"
{-# DEPRECATED baaaActualAmount "Use generic-lens or generic-optics with 'actualAmount' instead." #-}

-- | The amount of cost or usage that you created the budget for.
--
-- /Note:/ Consider using 'budgetedAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baaaBudgetedAmount :: Lens.Lens' BudgetedAndActualAmounts (Core.Maybe Types.Spend)
baaaBudgetedAmount = Lens.field @"budgetedAmount"
{-# DEPRECATED baaaBudgetedAmount "Use generic-lens or generic-optics with 'budgetedAmount' instead." #-}

-- | The time period covered by this budget comparison.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baaaTimePeriod :: Lens.Lens' BudgetedAndActualAmounts (Core.Maybe Types.TimePeriod)
baaaTimePeriod = Lens.field @"timePeriod"
{-# DEPRECATED baaaTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

instance Core.FromJSON BudgetedAndActualAmounts where
  parseJSON =
    Core.withObject "BudgetedAndActualAmounts" Core.$
      \x ->
        BudgetedAndActualAmounts'
          Core.<$> (x Core..:? "ActualAmount")
          Core.<*> (x Core..:? "BudgetedAmount")
          Core.<*> (x Core..:? "TimePeriod")
