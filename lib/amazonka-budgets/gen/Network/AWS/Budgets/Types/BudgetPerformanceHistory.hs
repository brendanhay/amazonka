{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.BudgetPerformanceHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.BudgetPerformanceHistory
  ( BudgetPerformanceHistory (..),

    -- * Smart constructor
    mkBudgetPerformanceHistory,

    -- * Lenses
    bphBudgetedAndActualAmountsList,
    bphTimeUnit,
    bphBudgetName,
    bphBudgetType,
    bphCostTypes,
    bphCostFilters,
  )
where

import Network.AWS.Budgets.Types.BudgetType
import Network.AWS.Budgets.Types.BudgetedAndActualAmounts
import Network.AWS.Budgets.Types.CostTypes
import Network.AWS.Budgets.Types.TimeUnit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A history of the state of a budget at the end of the budget's specified time period.
--
-- /See:/ 'mkBudgetPerformanceHistory' smart constructor.
data BudgetPerformanceHistory = BudgetPerformanceHistory'
  { budgetedAndActualAmountsList ::
      Lude.Maybe [BudgetedAndActualAmounts],
    timeUnit :: Lude.Maybe TimeUnit,
    budgetName :: Lude.Maybe Lude.Text,
    budgetType :: Lude.Maybe BudgetType,
    costTypes :: Lude.Maybe CostTypes,
    costFilters ::
      Lude.Maybe
        (Lude.HashMap Lude.Text ([Lude.Text]))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BudgetPerformanceHistory' with the minimum fields required to make a request.
--
-- * 'budgetName' - Undocumented field.
-- * 'budgetType' - Undocumented field.
-- * 'budgetedAndActualAmountsList' - A list of amounts of cost or usage that you created budgets for, compared to your actual costs or usage.
-- * 'costFilters' - The history of the cost filters for a budget during the specified time period.
-- * 'costTypes' - The history of the cost types for a budget during the specified time period.
-- * 'timeUnit' - Undocumented field.
mkBudgetPerformanceHistory ::
  BudgetPerformanceHistory
mkBudgetPerformanceHistory =
  BudgetPerformanceHistory'
    { budgetedAndActualAmountsList =
        Lude.Nothing,
      timeUnit = Lude.Nothing,
      budgetName = Lude.Nothing,
      budgetType = Lude.Nothing,
      costTypes = Lude.Nothing,
      costFilters = Lude.Nothing
    }

-- | A list of amounts of cost or usage that you created budgets for, compared to your actual costs or usage.
--
-- /Note:/ Consider using 'budgetedAndActualAmountsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bphBudgetedAndActualAmountsList :: Lens.Lens' BudgetPerformanceHistory (Lude.Maybe [BudgetedAndActualAmounts])
bphBudgetedAndActualAmountsList = Lens.lens (budgetedAndActualAmountsList :: BudgetPerformanceHistory -> Lude.Maybe [BudgetedAndActualAmounts]) (\s a -> s {budgetedAndActualAmountsList = a} :: BudgetPerformanceHistory)
{-# DEPRECATED bphBudgetedAndActualAmountsList "Use generic-lens or generic-optics with 'budgetedAndActualAmountsList' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'timeUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bphTimeUnit :: Lens.Lens' BudgetPerformanceHistory (Lude.Maybe TimeUnit)
bphTimeUnit = Lens.lens (timeUnit :: BudgetPerformanceHistory -> Lude.Maybe TimeUnit) (\s a -> s {timeUnit = a} :: BudgetPerformanceHistory)
{-# DEPRECATED bphTimeUnit "Use generic-lens or generic-optics with 'timeUnit' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bphBudgetName :: Lens.Lens' BudgetPerformanceHistory (Lude.Maybe Lude.Text)
bphBudgetName = Lens.lens (budgetName :: BudgetPerformanceHistory -> Lude.Maybe Lude.Text) (\s a -> s {budgetName = a} :: BudgetPerformanceHistory)
{-# DEPRECATED bphBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bphBudgetType :: Lens.Lens' BudgetPerformanceHistory (Lude.Maybe BudgetType)
bphBudgetType = Lens.lens (budgetType :: BudgetPerformanceHistory -> Lude.Maybe BudgetType) (\s a -> s {budgetType = a} :: BudgetPerformanceHistory)
{-# DEPRECATED bphBudgetType "Use generic-lens or generic-optics with 'budgetType' instead." #-}

-- | The history of the cost types for a budget during the specified time period.
--
-- /Note:/ Consider using 'costTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bphCostTypes :: Lens.Lens' BudgetPerformanceHistory (Lude.Maybe CostTypes)
bphCostTypes = Lens.lens (costTypes :: BudgetPerformanceHistory -> Lude.Maybe CostTypes) (\s a -> s {costTypes = a} :: BudgetPerformanceHistory)
{-# DEPRECATED bphCostTypes "Use generic-lens or generic-optics with 'costTypes' instead." #-}

-- | The history of the cost filters for a budget during the specified time period.
--
-- /Note:/ Consider using 'costFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bphCostFilters :: Lens.Lens' BudgetPerformanceHistory (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
bphCostFilters = Lens.lens (costFilters :: BudgetPerformanceHistory -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {costFilters = a} :: BudgetPerformanceHistory)
{-# DEPRECATED bphCostFilters "Use generic-lens or generic-optics with 'costFilters' instead." #-}

instance Lude.FromJSON BudgetPerformanceHistory where
  parseJSON =
    Lude.withObject
      "BudgetPerformanceHistory"
      ( \x ->
          BudgetPerformanceHistory'
            Lude.<$> (x Lude..:? "BudgetedAndActualAmountsList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TimeUnit")
            Lude.<*> (x Lude..:? "BudgetName")
            Lude.<*> (x Lude..:? "BudgetType")
            Lude.<*> (x Lude..:? "CostTypes")
            Lude.<*> (x Lude..:? "CostFilters" Lude..!= Lude.mempty)
      )
