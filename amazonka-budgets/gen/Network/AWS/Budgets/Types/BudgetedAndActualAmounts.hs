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
-- Module      : Network.AWS.Budgets.Types.BudgetedAndActualAmounts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.BudgetedAndActualAmounts where

import Network.AWS.Budgets.Types.Spend
import Network.AWS.Budgets.Types.TimePeriod
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The amount of cost or usage that you created the budget for, compared to
-- your actual costs or usage.
--
-- /See:/ 'newBudgetedAndActualAmounts' smart constructor.
data BudgetedAndActualAmounts = BudgetedAndActualAmounts'
  { -- | The time period covered by this budget comparison.
    timePeriod :: Core.Maybe TimePeriod,
    -- | The amount of cost or usage that you created the budget for.
    budgetedAmount :: Core.Maybe Spend,
    -- | Your actual costs or usage for a budget period.
    actualAmount :: Core.Maybe Spend
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BudgetedAndActualAmounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timePeriod', 'budgetedAndActualAmounts_timePeriod' - The time period covered by this budget comparison.
--
-- 'budgetedAmount', 'budgetedAndActualAmounts_budgetedAmount' - The amount of cost or usage that you created the budget for.
--
-- 'actualAmount', 'budgetedAndActualAmounts_actualAmount' - Your actual costs or usage for a budget period.
newBudgetedAndActualAmounts ::
  BudgetedAndActualAmounts
newBudgetedAndActualAmounts =
  BudgetedAndActualAmounts'
    { timePeriod =
        Core.Nothing,
      budgetedAmount = Core.Nothing,
      actualAmount = Core.Nothing
    }

-- | The time period covered by this budget comparison.
budgetedAndActualAmounts_timePeriod :: Lens.Lens' BudgetedAndActualAmounts (Core.Maybe TimePeriod)
budgetedAndActualAmounts_timePeriod = Lens.lens (\BudgetedAndActualAmounts' {timePeriod} -> timePeriod) (\s@BudgetedAndActualAmounts' {} a -> s {timePeriod = a} :: BudgetedAndActualAmounts)

-- | The amount of cost or usage that you created the budget for.
budgetedAndActualAmounts_budgetedAmount :: Lens.Lens' BudgetedAndActualAmounts (Core.Maybe Spend)
budgetedAndActualAmounts_budgetedAmount = Lens.lens (\BudgetedAndActualAmounts' {budgetedAmount} -> budgetedAmount) (\s@BudgetedAndActualAmounts' {} a -> s {budgetedAmount = a} :: BudgetedAndActualAmounts)

-- | Your actual costs or usage for a budget period.
budgetedAndActualAmounts_actualAmount :: Lens.Lens' BudgetedAndActualAmounts (Core.Maybe Spend)
budgetedAndActualAmounts_actualAmount = Lens.lens (\BudgetedAndActualAmounts' {actualAmount} -> actualAmount) (\s@BudgetedAndActualAmounts' {} a -> s {actualAmount = a} :: BudgetedAndActualAmounts)

instance Core.FromJSON BudgetedAndActualAmounts where
  parseJSON =
    Core.withObject
      "BudgetedAndActualAmounts"
      ( \x ->
          BudgetedAndActualAmounts'
            Core.<$> (x Core..:? "TimePeriod")
            Core.<*> (x Core..:? "BudgetedAmount")
            Core.<*> (x Core..:? "ActualAmount")
      )

instance Core.Hashable BudgetedAndActualAmounts

instance Core.NFData BudgetedAndActualAmounts
