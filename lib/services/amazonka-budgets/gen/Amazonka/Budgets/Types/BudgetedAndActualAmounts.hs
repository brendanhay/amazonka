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
-- Module      : Amazonka.Budgets.Types.BudgetedAndActualAmounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.BudgetedAndActualAmounts where

import Amazonka.Budgets.Types.Spend
import Amazonka.Budgets.Types.TimePeriod
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The amount of cost or usage that you created the budget for, compared to
-- your actual costs or usage.
--
-- /See:/ 'newBudgetedAndActualAmounts' smart constructor.
data BudgetedAndActualAmounts = BudgetedAndActualAmounts'
  { -- | Your actual costs or usage for a budget period.
    actualAmount :: Prelude.Maybe Spend,
    -- | The amount of cost or usage that you created the budget for.
    budgetedAmount :: Prelude.Maybe Spend,
    -- | The time period that\'s covered by this budget comparison.
    timePeriod :: Prelude.Maybe TimePeriod
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BudgetedAndActualAmounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actualAmount', 'budgetedAndActualAmounts_actualAmount' - Your actual costs or usage for a budget period.
--
-- 'budgetedAmount', 'budgetedAndActualAmounts_budgetedAmount' - The amount of cost or usage that you created the budget for.
--
-- 'timePeriod', 'budgetedAndActualAmounts_timePeriod' - The time period that\'s covered by this budget comparison.
newBudgetedAndActualAmounts ::
  BudgetedAndActualAmounts
newBudgetedAndActualAmounts =
  BudgetedAndActualAmounts'
    { actualAmount =
        Prelude.Nothing,
      budgetedAmount = Prelude.Nothing,
      timePeriod = Prelude.Nothing
    }

-- | Your actual costs or usage for a budget period.
budgetedAndActualAmounts_actualAmount :: Lens.Lens' BudgetedAndActualAmounts (Prelude.Maybe Spend)
budgetedAndActualAmounts_actualAmount = Lens.lens (\BudgetedAndActualAmounts' {actualAmount} -> actualAmount) (\s@BudgetedAndActualAmounts' {} a -> s {actualAmount = a} :: BudgetedAndActualAmounts)

-- | The amount of cost or usage that you created the budget for.
budgetedAndActualAmounts_budgetedAmount :: Lens.Lens' BudgetedAndActualAmounts (Prelude.Maybe Spend)
budgetedAndActualAmounts_budgetedAmount = Lens.lens (\BudgetedAndActualAmounts' {budgetedAmount} -> budgetedAmount) (\s@BudgetedAndActualAmounts' {} a -> s {budgetedAmount = a} :: BudgetedAndActualAmounts)

-- | The time period that\'s covered by this budget comparison.
budgetedAndActualAmounts_timePeriod :: Lens.Lens' BudgetedAndActualAmounts (Prelude.Maybe TimePeriod)
budgetedAndActualAmounts_timePeriod = Lens.lens (\BudgetedAndActualAmounts' {timePeriod} -> timePeriod) (\s@BudgetedAndActualAmounts' {} a -> s {timePeriod = a} :: BudgetedAndActualAmounts)

instance Data.FromJSON BudgetedAndActualAmounts where
  parseJSON =
    Data.withObject
      "BudgetedAndActualAmounts"
      ( \x ->
          BudgetedAndActualAmounts'
            Prelude.<$> (x Data..:? "ActualAmount")
            Prelude.<*> (x Data..:? "BudgetedAmount")
            Prelude.<*> (x Data..:? "TimePeriod")
      )

instance Prelude.Hashable BudgetedAndActualAmounts where
  hashWithSalt _salt BudgetedAndActualAmounts' {..} =
    _salt
      `Prelude.hashWithSalt` actualAmount
      `Prelude.hashWithSalt` budgetedAmount
      `Prelude.hashWithSalt` timePeriod

instance Prelude.NFData BudgetedAndActualAmounts where
  rnf BudgetedAndActualAmounts' {..} =
    Prelude.rnf actualAmount `Prelude.seq`
      Prelude.rnf budgetedAmount `Prelude.seq`
        Prelude.rnf timePeriod
