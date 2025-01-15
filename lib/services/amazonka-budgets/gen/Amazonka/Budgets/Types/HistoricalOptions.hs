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
-- Module      : Amazonka.Budgets.Types.HistoricalOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.HistoricalOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters that define or describe the historical data that your
-- auto-adjusting budget is based on.
--
-- /See:/ 'newHistoricalOptions' smart constructor.
data HistoricalOptions = HistoricalOptions'
  { -- | The integer that describes how many budget periods in your
    -- @BudgetAdjustmentPeriod@ are included in the calculation of your current
    -- @BudgetLimit@. If the first budget period in your
    -- @BudgetAdjustmentPeriod@ has no cost data, then that budget period isn’t
    -- included in the average that determines your budget limit.
    --
    -- For example, if you set @BudgetAdjustmentPeriod@ as @4@ quarters, but
    -- your account had no cost data in the first quarter, then only the last
    -- three quarters are included in the calculation. In this scenario,
    -- @LookBackAvailablePeriods@ returns @3@.
    --
    -- You can’t set your own @LookBackAvailablePeriods@. The value is
    -- automatically calculated from the @BudgetAdjustmentPeriod@ and your
    -- historical cost data.
    lookBackAvailablePeriods :: Prelude.Maybe Prelude.Natural,
    -- | The number of budget periods included in the moving-average calculation
    -- that determines your auto-adjusted budget amount. The maximum value
    -- depends on the @TimeUnit@ granularity of the budget:
    --
    -- -   For the @DAILY@ granularity, the maximum value is @60@.
    --
    -- -   For the @MONTHLY@ granularity, the maximum value is @12@.
    --
    -- -   For the @QUARTERLY@ granularity, the maximum value is @4@.
    --
    -- -   For the @ANNUALLY@ granularity, the maximum value is @1@.
    budgetAdjustmentPeriod :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HistoricalOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lookBackAvailablePeriods', 'historicalOptions_lookBackAvailablePeriods' - The integer that describes how many budget periods in your
-- @BudgetAdjustmentPeriod@ are included in the calculation of your current
-- @BudgetLimit@. If the first budget period in your
-- @BudgetAdjustmentPeriod@ has no cost data, then that budget period isn’t
-- included in the average that determines your budget limit.
--
-- For example, if you set @BudgetAdjustmentPeriod@ as @4@ quarters, but
-- your account had no cost data in the first quarter, then only the last
-- three quarters are included in the calculation. In this scenario,
-- @LookBackAvailablePeriods@ returns @3@.
--
-- You can’t set your own @LookBackAvailablePeriods@. The value is
-- automatically calculated from the @BudgetAdjustmentPeriod@ and your
-- historical cost data.
--
-- 'budgetAdjustmentPeriod', 'historicalOptions_budgetAdjustmentPeriod' - The number of budget periods included in the moving-average calculation
-- that determines your auto-adjusted budget amount. The maximum value
-- depends on the @TimeUnit@ granularity of the budget:
--
-- -   For the @DAILY@ granularity, the maximum value is @60@.
--
-- -   For the @MONTHLY@ granularity, the maximum value is @12@.
--
-- -   For the @QUARTERLY@ granularity, the maximum value is @4@.
--
-- -   For the @ANNUALLY@ granularity, the maximum value is @1@.
newHistoricalOptions ::
  -- | 'budgetAdjustmentPeriod'
  Prelude.Natural ->
  HistoricalOptions
newHistoricalOptions pBudgetAdjustmentPeriod_ =
  HistoricalOptions'
    { lookBackAvailablePeriods =
        Prelude.Nothing,
      budgetAdjustmentPeriod = pBudgetAdjustmentPeriod_
    }

-- | The integer that describes how many budget periods in your
-- @BudgetAdjustmentPeriod@ are included in the calculation of your current
-- @BudgetLimit@. If the first budget period in your
-- @BudgetAdjustmentPeriod@ has no cost data, then that budget period isn’t
-- included in the average that determines your budget limit.
--
-- For example, if you set @BudgetAdjustmentPeriod@ as @4@ quarters, but
-- your account had no cost data in the first quarter, then only the last
-- three quarters are included in the calculation. In this scenario,
-- @LookBackAvailablePeriods@ returns @3@.
--
-- You can’t set your own @LookBackAvailablePeriods@. The value is
-- automatically calculated from the @BudgetAdjustmentPeriod@ and your
-- historical cost data.
historicalOptions_lookBackAvailablePeriods :: Lens.Lens' HistoricalOptions (Prelude.Maybe Prelude.Natural)
historicalOptions_lookBackAvailablePeriods = Lens.lens (\HistoricalOptions' {lookBackAvailablePeriods} -> lookBackAvailablePeriods) (\s@HistoricalOptions' {} a -> s {lookBackAvailablePeriods = a} :: HistoricalOptions)

-- | The number of budget periods included in the moving-average calculation
-- that determines your auto-adjusted budget amount. The maximum value
-- depends on the @TimeUnit@ granularity of the budget:
--
-- -   For the @DAILY@ granularity, the maximum value is @60@.
--
-- -   For the @MONTHLY@ granularity, the maximum value is @12@.
--
-- -   For the @QUARTERLY@ granularity, the maximum value is @4@.
--
-- -   For the @ANNUALLY@ granularity, the maximum value is @1@.
historicalOptions_budgetAdjustmentPeriod :: Lens.Lens' HistoricalOptions Prelude.Natural
historicalOptions_budgetAdjustmentPeriod = Lens.lens (\HistoricalOptions' {budgetAdjustmentPeriod} -> budgetAdjustmentPeriod) (\s@HistoricalOptions' {} a -> s {budgetAdjustmentPeriod = a} :: HistoricalOptions)

instance Data.FromJSON HistoricalOptions where
  parseJSON =
    Data.withObject
      "HistoricalOptions"
      ( \x ->
          HistoricalOptions'
            Prelude.<$> (x Data..:? "LookBackAvailablePeriods")
            Prelude.<*> (x Data..: "BudgetAdjustmentPeriod")
      )

instance Prelude.Hashable HistoricalOptions where
  hashWithSalt _salt HistoricalOptions' {..} =
    _salt
      `Prelude.hashWithSalt` lookBackAvailablePeriods
      `Prelude.hashWithSalt` budgetAdjustmentPeriod

instance Prelude.NFData HistoricalOptions where
  rnf HistoricalOptions' {..} =
    Prelude.rnf lookBackAvailablePeriods `Prelude.seq`
      Prelude.rnf budgetAdjustmentPeriod

instance Data.ToJSON HistoricalOptions where
  toJSON HistoricalOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LookBackAvailablePeriods" Data..=)
              Prelude.<$> lookBackAvailablePeriods,
            Prelude.Just
              ( "BudgetAdjustmentPeriod"
                  Data..= budgetAdjustmentPeriod
              )
          ]
      )
