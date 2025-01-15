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
-- Module      : Amazonka.CostExplorer.Types.ResultByTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ResultByTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.DateInterval
import Amazonka.CostExplorer.Types.Group
import Amazonka.CostExplorer.Types.MetricValue
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The result that\'s associated with a time period.
--
-- /See:/ 'newResultByTime' smart constructor.
data ResultByTime = ResultByTime'
  { -- | Determines whether the result is estimated.
    estimated :: Prelude.Maybe Prelude.Bool,
    -- | The groups that this time period includes.
    groups :: Prelude.Maybe [Group],
    -- | The time period that the result covers.
    timePeriod :: Prelude.Maybe DateInterval,
    -- | The total amount of cost or usage accrued during the time period.
    total :: Prelude.Maybe (Prelude.HashMap Prelude.Text MetricValue)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResultByTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimated', 'resultByTime_estimated' - Determines whether the result is estimated.
--
-- 'groups', 'resultByTime_groups' - The groups that this time period includes.
--
-- 'timePeriod', 'resultByTime_timePeriod' - The time period that the result covers.
--
-- 'total', 'resultByTime_total' - The total amount of cost or usage accrued during the time period.
newResultByTime ::
  ResultByTime
newResultByTime =
  ResultByTime'
    { estimated = Prelude.Nothing,
      groups = Prelude.Nothing,
      timePeriod = Prelude.Nothing,
      total = Prelude.Nothing
    }

-- | Determines whether the result is estimated.
resultByTime_estimated :: Lens.Lens' ResultByTime (Prelude.Maybe Prelude.Bool)
resultByTime_estimated = Lens.lens (\ResultByTime' {estimated} -> estimated) (\s@ResultByTime' {} a -> s {estimated = a} :: ResultByTime)

-- | The groups that this time period includes.
resultByTime_groups :: Lens.Lens' ResultByTime (Prelude.Maybe [Group])
resultByTime_groups = Lens.lens (\ResultByTime' {groups} -> groups) (\s@ResultByTime' {} a -> s {groups = a} :: ResultByTime) Prelude.. Lens.mapping Lens.coerced

-- | The time period that the result covers.
resultByTime_timePeriod :: Lens.Lens' ResultByTime (Prelude.Maybe DateInterval)
resultByTime_timePeriod = Lens.lens (\ResultByTime' {timePeriod} -> timePeriod) (\s@ResultByTime' {} a -> s {timePeriod = a} :: ResultByTime)

-- | The total amount of cost or usage accrued during the time period.
resultByTime_total :: Lens.Lens' ResultByTime (Prelude.Maybe (Prelude.HashMap Prelude.Text MetricValue))
resultByTime_total = Lens.lens (\ResultByTime' {total} -> total) (\s@ResultByTime' {} a -> s {total = a} :: ResultByTime) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ResultByTime where
  parseJSON =
    Data.withObject
      "ResultByTime"
      ( \x ->
          ResultByTime'
            Prelude.<$> (x Data..:? "Estimated")
            Prelude.<*> (x Data..:? "Groups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TimePeriod")
            Prelude.<*> (x Data..:? "Total" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ResultByTime where
  hashWithSalt _salt ResultByTime' {..} =
    _salt
      `Prelude.hashWithSalt` estimated
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` timePeriod
      `Prelude.hashWithSalt` total

instance Prelude.NFData ResultByTime where
  rnf ResultByTime' {..} =
    Prelude.rnf estimated `Prelude.seq`
      Prelude.rnf groups `Prelude.seq`
        Prelude.rnf timePeriod `Prelude.seq`
          Prelude.rnf total
