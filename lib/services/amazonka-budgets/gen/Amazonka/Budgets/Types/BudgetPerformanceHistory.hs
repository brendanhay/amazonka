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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A history of the state of a budget at the end of the budget\'s specified
-- time period.
--
-- /See:/ 'newBudgetPerformanceHistory' smart constructor.
data BudgetPerformanceHistory = BudgetPerformanceHistory'
  { budgetName :: Prelude.Maybe Prelude.Text,
    budgetType :: Prelude.Maybe BudgetType,
    -- | A list of amounts of cost or usage that you created budgets for, which
    -- are compared to your actual costs or usage.
    budgetedAndActualAmountsList :: Prelude.Maybe [BudgetedAndActualAmounts],
    -- | The history of the cost filters for a budget during the specified time
    -- period.
    costFilters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The history of the cost types for a budget during the specified time
    -- period.
    costTypes :: Prelude.Maybe CostTypes,
    timeUnit :: Prelude.Maybe TimeUnit
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
-- 'budgetName', 'budgetPerformanceHistory_budgetName' - Undocumented member.
--
-- 'budgetType', 'budgetPerformanceHistory_budgetType' - Undocumented member.
--
-- 'budgetedAndActualAmountsList', 'budgetPerformanceHistory_budgetedAndActualAmountsList' - A list of amounts of cost or usage that you created budgets for, which
-- are compared to your actual costs or usage.
--
-- 'costFilters', 'budgetPerformanceHistory_costFilters' - The history of the cost filters for a budget during the specified time
-- period.
--
-- 'costTypes', 'budgetPerformanceHistory_costTypes' - The history of the cost types for a budget during the specified time
-- period.
--
-- 'timeUnit', 'budgetPerformanceHistory_timeUnit' - Undocumented member.
newBudgetPerformanceHistory ::
  BudgetPerformanceHistory
newBudgetPerformanceHistory =
  BudgetPerformanceHistory'
    { budgetName =
        Prelude.Nothing,
      budgetType = Prelude.Nothing,
      budgetedAndActualAmountsList = Prelude.Nothing,
      costFilters = Prelude.Nothing,
      costTypes = Prelude.Nothing,
      timeUnit = Prelude.Nothing
    }

-- | Undocumented member.
budgetPerformanceHistory_budgetName :: Lens.Lens' BudgetPerformanceHistory (Prelude.Maybe Prelude.Text)
budgetPerformanceHistory_budgetName = Lens.lens (\BudgetPerformanceHistory' {budgetName} -> budgetName) (\s@BudgetPerformanceHistory' {} a -> s {budgetName = a} :: BudgetPerformanceHistory)

-- | Undocumented member.
budgetPerformanceHistory_budgetType :: Lens.Lens' BudgetPerformanceHistory (Prelude.Maybe BudgetType)
budgetPerformanceHistory_budgetType = Lens.lens (\BudgetPerformanceHistory' {budgetType} -> budgetType) (\s@BudgetPerformanceHistory' {} a -> s {budgetType = a} :: BudgetPerformanceHistory)

-- | A list of amounts of cost or usage that you created budgets for, which
-- are compared to your actual costs or usage.
budgetPerformanceHistory_budgetedAndActualAmountsList :: Lens.Lens' BudgetPerformanceHistory (Prelude.Maybe [BudgetedAndActualAmounts])
budgetPerformanceHistory_budgetedAndActualAmountsList = Lens.lens (\BudgetPerformanceHistory' {budgetedAndActualAmountsList} -> budgetedAndActualAmountsList) (\s@BudgetPerformanceHistory' {} a -> s {budgetedAndActualAmountsList = a} :: BudgetPerformanceHistory) Prelude.. Lens.mapping Lens.coerced

-- | The history of the cost filters for a budget during the specified time
-- period.
budgetPerformanceHistory_costFilters :: Lens.Lens' BudgetPerformanceHistory (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
budgetPerformanceHistory_costFilters = Lens.lens (\BudgetPerformanceHistory' {costFilters} -> costFilters) (\s@BudgetPerformanceHistory' {} a -> s {costFilters = a} :: BudgetPerformanceHistory) Prelude.. Lens.mapping Lens.coerced

-- | The history of the cost types for a budget during the specified time
-- period.
budgetPerformanceHistory_costTypes :: Lens.Lens' BudgetPerformanceHistory (Prelude.Maybe CostTypes)
budgetPerformanceHistory_costTypes = Lens.lens (\BudgetPerformanceHistory' {costTypes} -> costTypes) (\s@BudgetPerformanceHistory' {} a -> s {costTypes = a} :: BudgetPerformanceHistory)

-- | Undocumented member.
budgetPerformanceHistory_timeUnit :: Lens.Lens' BudgetPerformanceHistory (Prelude.Maybe TimeUnit)
budgetPerformanceHistory_timeUnit = Lens.lens (\BudgetPerformanceHistory' {timeUnit} -> timeUnit) (\s@BudgetPerformanceHistory' {} a -> s {timeUnit = a} :: BudgetPerformanceHistory)

instance Data.FromJSON BudgetPerformanceHistory where
  parseJSON =
    Data.withObject
      "BudgetPerformanceHistory"
      ( \x ->
          BudgetPerformanceHistory'
            Prelude.<$> (x Data..:? "BudgetName")
            Prelude.<*> (x Data..:? "BudgetType")
            Prelude.<*> ( x
                            Data..:? "BudgetedAndActualAmountsList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CostFilters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CostTypes")
            Prelude.<*> (x Data..:? "TimeUnit")
      )

instance Prelude.Hashable BudgetPerformanceHistory where
  hashWithSalt _salt BudgetPerformanceHistory' {..} =
    _salt
      `Prelude.hashWithSalt` budgetName
      `Prelude.hashWithSalt` budgetType
      `Prelude.hashWithSalt` budgetedAndActualAmountsList
      `Prelude.hashWithSalt` costFilters
      `Prelude.hashWithSalt` costTypes
      `Prelude.hashWithSalt` timeUnit

instance Prelude.NFData BudgetPerformanceHistory where
  rnf BudgetPerformanceHistory' {..} =
    Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf budgetType
      `Prelude.seq` Prelude.rnf budgetedAndActualAmountsList
      `Prelude.seq` Prelude.rnf costFilters
      `Prelude.seq` Prelude.rnf costTypes
      `Prelude.seq` Prelude.rnf timeUnit
