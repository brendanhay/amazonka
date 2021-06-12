{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.BudgetPerformanceHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.BudgetPerformanceHistory where

import Network.AWS.Budgets.Types.BudgetType
import Network.AWS.Budgets.Types.BudgetedAndActualAmounts
import Network.AWS.Budgets.Types.CostTypes
import Network.AWS.Budgets.Types.TimeUnit
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A history of the state of a budget at the end of the budget\'s specified
-- time period.
--
-- /See:/ 'newBudgetPerformanceHistory' smart constructor.
data BudgetPerformanceHistory = BudgetPerformanceHistory'
  { -- | A list of amounts of cost or usage that you created budgets for,
    -- compared to your actual costs or usage.
    budgetedAndActualAmountsList :: Core.Maybe [BudgetedAndActualAmounts],
    timeUnit :: Core.Maybe TimeUnit,
    -- | The history of the cost filters for a budget during the specified time
    -- period.
    costFilters :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The history of the cost types for a budget during the specified time
    -- period.
    costTypes :: Core.Maybe CostTypes,
    budgetType :: Core.Maybe BudgetType,
    budgetName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BudgetPerformanceHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'budgetedAndActualAmountsList', 'budgetPerformanceHistory_budgetedAndActualAmountsList' - A list of amounts of cost or usage that you created budgets for,
-- compared to your actual costs or usage.
--
-- 'timeUnit', 'budgetPerformanceHistory_timeUnit' - Undocumented member.
--
-- 'costFilters', 'budgetPerformanceHistory_costFilters' - The history of the cost filters for a budget during the specified time
-- period.
--
-- 'costTypes', 'budgetPerformanceHistory_costTypes' - The history of the cost types for a budget during the specified time
-- period.
--
-- 'budgetType', 'budgetPerformanceHistory_budgetType' - Undocumented member.
--
-- 'budgetName', 'budgetPerformanceHistory_budgetName' - Undocumented member.
newBudgetPerformanceHistory ::
  BudgetPerformanceHistory
newBudgetPerformanceHistory =
  BudgetPerformanceHistory'
    { budgetedAndActualAmountsList =
        Core.Nothing,
      timeUnit = Core.Nothing,
      costFilters = Core.Nothing,
      costTypes = Core.Nothing,
      budgetType = Core.Nothing,
      budgetName = Core.Nothing
    }

-- | A list of amounts of cost or usage that you created budgets for,
-- compared to your actual costs or usage.
budgetPerformanceHistory_budgetedAndActualAmountsList :: Lens.Lens' BudgetPerformanceHistory (Core.Maybe [BudgetedAndActualAmounts])
budgetPerformanceHistory_budgetedAndActualAmountsList = Lens.lens (\BudgetPerformanceHistory' {budgetedAndActualAmountsList} -> budgetedAndActualAmountsList) (\s@BudgetPerformanceHistory' {} a -> s {budgetedAndActualAmountsList = a} :: BudgetPerformanceHistory) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
budgetPerformanceHistory_timeUnit :: Lens.Lens' BudgetPerformanceHistory (Core.Maybe TimeUnit)
budgetPerformanceHistory_timeUnit = Lens.lens (\BudgetPerformanceHistory' {timeUnit} -> timeUnit) (\s@BudgetPerformanceHistory' {} a -> s {timeUnit = a} :: BudgetPerformanceHistory)

-- | The history of the cost filters for a budget during the specified time
-- period.
budgetPerformanceHistory_costFilters :: Lens.Lens' BudgetPerformanceHistory (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
budgetPerformanceHistory_costFilters = Lens.lens (\BudgetPerformanceHistory' {costFilters} -> costFilters) (\s@BudgetPerformanceHistory' {} a -> s {costFilters = a} :: BudgetPerformanceHistory) Core.. Lens.mapping Lens._Coerce

-- | The history of the cost types for a budget during the specified time
-- period.
budgetPerformanceHistory_costTypes :: Lens.Lens' BudgetPerformanceHistory (Core.Maybe CostTypes)
budgetPerformanceHistory_costTypes = Lens.lens (\BudgetPerformanceHistory' {costTypes} -> costTypes) (\s@BudgetPerformanceHistory' {} a -> s {costTypes = a} :: BudgetPerformanceHistory)

-- | Undocumented member.
budgetPerformanceHistory_budgetType :: Lens.Lens' BudgetPerformanceHistory (Core.Maybe BudgetType)
budgetPerformanceHistory_budgetType = Lens.lens (\BudgetPerformanceHistory' {budgetType} -> budgetType) (\s@BudgetPerformanceHistory' {} a -> s {budgetType = a} :: BudgetPerformanceHistory)

-- | Undocumented member.
budgetPerformanceHistory_budgetName :: Lens.Lens' BudgetPerformanceHistory (Core.Maybe Core.Text)
budgetPerformanceHistory_budgetName = Lens.lens (\BudgetPerformanceHistory' {budgetName} -> budgetName) (\s@BudgetPerformanceHistory' {} a -> s {budgetName = a} :: BudgetPerformanceHistory)

instance Core.FromJSON BudgetPerformanceHistory where
  parseJSON =
    Core.withObject
      "BudgetPerformanceHistory"
      ( \x ->
          BudgetPerformanceHistory'
            Core.<$> ( x Core..:? "BudgetedAndActualAmountsList"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "TimeUnit")
            Core.<*> (x Core..:? "CostFilters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "CostTypes")
            Core.<*> (x Core..:? "BudgetType")
            Core.<*> (x Core..:? "BudgetName")
      )

instance Core.Hashable BudgetPerformanceHistory

instance Core.NFData BudgetPerformanceHistory
