{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The amount of cost or usage that you created the budget for, compared to
-- your actual costs or usage.
--
-- /See:/ 'newBudgetedAndActualAmounts' smart constructor.
data BudgetedAndActualAmounts = BudgetedAndActualAmounts'
  { -- | The time period covered by this budget comparison.
    timePeriod :: Prelude.Maybe TimePeriod,
    -- | The amount of cost or usage that you created the budget for.
    budgetedAmount :: Prelude.Maybe Spend,
    -- | Your actual costs or usage for a budget period.
    actualAmount :: Prelude.Maybe Spend
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      budgetedAmount = Prelude.Nothing,
      actualAmount = Prelude.Nothing
    }

-- | The time period covered by this budget comparison.
budgetedAndActualAmounts_timePeriod :: Lens.Lens' BudgetedAndActualAmounts (Prelude.Maybe TimePeriod)
budgetedAndActualAmounts_timePeriod = Lens.lens (\BudgetedAndActualAmounts' {timePeriod} -> timePeriod) (\s@BudgetedAndActualAmounts' {} a -> s {timePeriod = a} :: BudgetedAndActualAmounts)

-- | The amount of cost or usage that you created the budget for.
budgetedAndActualAmounts_budgetedAmount :: Lens.Lens' BudgetedAndActualAmounts (Prelude.Maybe Spend)
budgetedAndActualAmounts_budgetedAmount = Lens.lens (\BudgetedAndActualAmounts' {budgetedAmount} -> budgetedAmount) (\s@BudgetedAndActualAmounts' {} a -> s {budgetedAmount = a} :: BudgetedAndActualAmounts)

-- | Your actual costs or usage for a budget period.
budgetedAndActualAmounts_actualAmount :: Lens.Lens' BudgetedAndActualAmounts (Prelude.Maybe Spend)
budgetedAndActualAmounts_actualAmount = Lens.lens (\BudgetedAndActualAmounts' {actualAmount} -> actualAmount) (\s@BudgetedAndActualAmounts' {} a -> s {actualAmount = a} :: BudgetedAndActualAmounts)

instance Prelude.FromJSON BudgetedAndActualAmounts where
  parseJSON =
    Prelude.withObject
      "BudgetedAndActualAmounts"
      ( \x ->
          BudgetedAndActualAmounts'
            Prelude.<$> (x Prelude..:? "TimePeriod")
            Prelude.<*> (x Prelude..:? "BudgetedAmount")
            Prelude.<*> (x Prelude..:? "ActualAmount")
      )

instance Prelude.Hashable BudgetedAndActualAmounts

instance Prelude.NFData BudgetedAndActualAmounts
