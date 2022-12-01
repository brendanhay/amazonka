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
-- Module      : Amazonka.Budgets.Types.BudgetPerformanceHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.BudgetPerformanceHistory where

import Amazonka.Budgets.Types.BudgetType
import Amazonka.Budgets.Types.BudgetedAndActualAmounts
import Amazonka.Budgets.Types.CostTypes
import Amazonka.Budgets.Types.TimeUnit
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A history of the state of a budget at the end of the budget\'s specified
-- time period.
--
-- /See:/ 'newBudgetPerformanceHistory' smart constructor.
data BudgetPerformanceHistory = BudgetPerformanceHistory'
  { timeUnit :: Prelude.Maybe TimeUnit,
    -- | A list of amounts of cost or usage that you created budgets for, which
    -- are compared to your actual costs or usage.
    budgetedAndActualAmountsList :: Prelude.Maybe [BudgetedAndActualAmounts],
    -- | The history of the cost filters for a budget during the specified time
    -- period.
    costFilters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    budgetType :: Prelude.Maybe BudgetType,
    -- | The history of the cost types for a budget during the specified time
    -- period.
    costTypes :: Prelude.Maybe CostTypes,
    budgetName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BudgetPerformanceHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeUnit', 'budgetPerformanceHistory_timeUnit' - Undocumented member.
--
-- 'budgetedAndActualAmountsList', 'budgetPerformanceHistory_budgetedAndActualAmountsList' - A list of amounts of cost or usage that you created budgets for, which
-- are compared to your actual costs or usage.
--
-- 'costFilters', 'budgetPerformanceHistory_costFilters' - The history of the cost filters for a budget during the specified time
-- period.
--
-- 'budgetType', 'budgetPerformanceHistory_budgetType' - Undocumented member.
--
-- 'costTypes', 'budgetPerformanceHistory_costTypes' - The history of the cost types for a budget during the specified time
-- period.
--
-- 'budgetName', 'budgetPerformanceHistory_budgetName' - Undocumented member.
newBudgetPerformanceHistory ::
  BudgetPerformanceHistory
newBudgetPerformanceHistory =
  BudgetPerformanceHistory'
    { timeUnit =
        Prelude.Nothing,
      budgetedAndActualAmountsList = Prelude.Nothing,
      costFilters = Prelude.Nothing,
      budgetType = Prelude.Nothing,
      costTypes = Prelude.Nothing,
      budgetName = Prelude.Nothing
    }

-- | Undocumented member.
budgetPerformanceHistory_timeUnit :: Lens.Lens' BudgetPerformanceHistory (Prelude.Maybe TimeUnit)
budgetPerformanceHistory_timeUnit = Lens.lens (\BudgetPerformanceHistory' {timeUnit} -> timeUnit) (\s@BudgetPerformanceHistory' {} a -> s {timeUnit = a} :: BudgetPerformanceHistory)

-- | A list of amounts of cost or usage that you created budgets for, which
-- are compared to your actual costs or usage.
budgetPerformanceHistory_budgetedAndActualAmountsList :: Lens.Lens' BudgetPerformanceHistory (Prelude.Maybe [BudgetedAndActualAmounts])
budgetPerformanceHistory_budgetedAndActualAmountsList = Lens.lens (\BudgetPerformanceHistory' {budgetedAndActualAmountsList} -> budgetedAndActualAmountsList) (\s@BudgetPerformanceHistory' {} a -> s {budgetedAndActualAmountsList = a} :: BudgetPerformanceHistory) Prelude.. Lens.mapping Lens.coerced

-- | The history of the cost filters for a budget during the specified time
-- period.
budgetPerformanceHistory_costFilters :: Lens.Lens' BudgetPerformanceHistory (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
budgetPerformanceHistory_costFilters = Lens.lens (\BudgetPerformanceHistory' {costFilters} -> costFilters) (\s@BudgetPerformanceHistory' {} a -> s {costFilters = a} :: BudgetPerformanceHistory) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
budgetPerformanceHistory_budgetType :: Lens.Lens' BudgetPerformanceHistory (Prelude.Maybe BudgetType)
budgetPerformanceHistory_budgetType = Lens.lens (\BudgetPerformanceHistory' {budgetType} -> budgetType) (\s@BudgetPerformanceHistory' {} a -> s {budgetType = a} :: BudgetPerformanceHistory)

-- | The history of the cost types for a budget during the specified time
-- period.
budgetPerformanceHistory_costTypes :: Lens.Lens' BudgetPerformanceHistory (Prelude.Maybe CostTypes)
budgetPerformanceHistory_costTypes = Lens.lens (\BudgetPerformanceHistory' {costTypes} -> costTypes) (\s@BudgetPerformanceHistory' {} a -> s {costTypes = a} :: BudgetPerformanceHistory)

-- | Undocumented member.
budgetPerformanceHistory_budgetName :: Lens.Lens' BudgetPerformanceHistory (Prelude.Maybe Prelude.Text)
budgetPerformanceHistory_budgetName = Lens.lens (\BudgetPerformanceHistory' {budgetName} -> budgetName) (\s@BudgetPerformanceHistory' {} a -> s {budgetName = a} :: BudgetPerformanceHistory)

instance Core.FromJSON BudgetPerformanceHistory where
  parseJSON =
    Core.withObject
      "BudgetPerformanceHistory"
      ( \x ->
          BudgetPerformanceHistory'
            Prelude.<$> (x Core..:? "TimeUnit")
            Prelude.<*> ( x Core..:? "BudgetedAndActualAmountsList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CostFilters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "BudgetType")
            Prelude.<*> (x Core..:? "CostTypes")
            Prelude.<*> (x Core..:? "BudgetName")
      )

instance Prelude.Hashable BudgetPerformanceHistory where
  hashWithSalt _salt BudgetPerformanceHistory' {..} =
    _salt `Prelude.hashWithSalt` timeUnit
      `Prelude.hashWithSalt` budgetedAndActualAmountsList
      `Prelude.hashWithSalt` costFilters
      `Prelude.hashWithSalt` budgetType
      `Prelude.hashWithSalt` costTypes
      `Prelude.hashWithSalt` budgetName

instance Prelude.NFData BudgetPerformanceHistory where
  rnf BudgetPerformanceHistory' {..} =
    Prelude.rnf timeUnit
      `Prelude.seq` Prelude.rnf budgetedAndActualAmountsList
      `Prelude.seq` Prelude.rnf costFilters
      `Prelude.seq` Prelude.rnf budgetType
      `Prelude.seq` Prelude.rnf costTypes
      `Prelude.seq` Prelude.rnf budgetName
