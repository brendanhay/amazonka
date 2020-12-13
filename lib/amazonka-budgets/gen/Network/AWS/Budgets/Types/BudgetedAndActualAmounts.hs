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
    baaaTimePeriod,
    baaaActualAmount,
    baaaBudgetedAmount,
  )
where

import Network.AWS.Budgets.Types.Spend
import Network.AWS.Budgets.Types.TimePeriod
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The amount of cost or usage that you created the budget for, compared to your actual costs or usage.
--
-- /See:/ 'mkBudgetedAndActualAmounts' smart constructor.
data BudgetedAndActualAmounts = BudgetedAndActualAmounts'
  { -- | The time period covered by this budget comparison.
    timePeriod :: Lude.Maybe TimePeriod,
    -- | Your actual costs or usage for a budget period.
    actualAmount :: Lude.Maybe Spend,
    -- | The amount of cost or usage that you created the budget for.
    budgetedAmount :: Lude.Maybe Spend
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BudgetedAndActualAmounts' with the minimum fields required to make a request.
--
-- * 'timePeriod' - The time period covered by this budget comparison.
-- * 'actualAmount' - Your actual costs or usage for a budget period.
-- * 'budgetedAmount' - The amount of cost or usage that you created the budget for.
mkBudgetedAndActualAmounts ::
  BudgetedAndActualAmounts
mkBudgetedAndActualAmounts =
  BudgetedAndActualAmounts'
    { timePeriod = Lude.Nothing,
      actualAmount = Lude.Nothing,
      budgetedAmount = Lude.Nothing
    }

-- | The time period covered by this budget comparison.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baaaTimePeriod :: Lens.Lens' BudgetedAndActualAmounts (Lude.Maybe TimePeriod)
baaaTimePeriod = Lens.lens (timePeriod :: BudgetedAndActualAmounts -> Lude.Maybe TimePeriod) (\s a -> s {timePeriod = a} :: BudgetedAndActualAmounts)
{-# DEPRECATED baaaTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | Your actual costs or usage for a budget period.
--
-- /Note:/ Consider using 'actualAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baaaActualAmount :: Lens.Lens' BudgetedAndActualAmounts (Lude.Maybe Spend)
baaaActualAmount = Lens.lens (actualAmount :: BudgetedAndActualAmounts -> Lude.Maybe Spend) (\s a -> s {actualAmount = a} :: BudgetedAndActualAmounts)
{-# DEPRECATED baaaActualAmount "Use generic-lens or generic-optics with 'actualAmount' instead." #-}

-- | The amount of cost or usage that you created the budget for.
--
-- /Note:/ Consider using 'budgetedAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baaaBudgetedAmount :: Lens.Lens' BudgetedAndActualAmounts (Lude.Maybe Spend)
baaaBudgetedAmount = Lens.lens (budgetedAmount :: BudgetedAndActualAmounts -> Lude.Maybe Spend) (\s a -> s {budgetedAmount = a} :: BudgetedAndActualAmounts)
{-# DEPRECATED baaaBudgetedAmount "Use generic-lens or generic-optics with 'budgetedAmount' instead." #-}

instance Lude.FromJSON BudgetedAndActualAmounts where
  parseJSON =
    Lude.withObject
      "BudgetedAndActualAmounts"
      ( \x ->
          BudgetedAndActualAmounts'
            Lude.<$> (x Lude..:? "TimePeriod")
            Lude.<*> (x Lude..:? "ActualAmount")
            Lude.<*> (x Lude..:? "BudgetedAmount")
      )
