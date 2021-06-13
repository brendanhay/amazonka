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
-- Module      : Network.AWS.CostExplorer.Types.ResultByTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ResultByTime where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.Group
import Network.AWS.CostExplorer.Types.MetricValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The result that is associated with a time period.
--
-- /See:/ 'newResultByTime' smart constructor.
data ResultByTime = ResultByTime'
  { -- | The groups that this time period includes.
    groups :: Prelude.Maybe [Group],
    -- | The time period that the result covers.
    timePeriod :: Prelude.Maybe DateInterval,
    -- | Whether the result is estimated.
    estimated :: Prelude.Maybe Prelude.Bool,
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
-- 'groups', 'resultByTime_groups' - The groups that this time period includes.
--
-- 'timePeriod', 'resultByTime_timePeriod' - The time period that the result covers.
--
-- 'estimated', 'resultByTime_estimated' - Whether the result is estimated.
--
-- 'total', 'resultByTime_total' - The total amount of cost or usage accrued during the time period.
newResultByTime ::
  ResultByTime
newResultByTime =
  ResultByTime'
    { groups = Prelude.Nothing,
      timePeriod = Prelude.Nothing,
      estimated = Prelude.Nothing,
      total = Prelude.Nothing
    }

-- | The groups that this time period includes.
resultByTime_groups :: Lens.Lens' ResultByTime (Prelude.Maybe [Group])
resultByTime_groups = Lens.lens (\ResultByTime' {groups} -> groups) (\s@ResultByTime' {} a -> s {groups = a} :: ResultByTime) Prelude.. Lens.mapping Lens._Coerce

-- | The time period that the result covers.
resultByTime_timePeriod :: Lens.Lens' ResultByTime (Prelude.Maybe DateInterval)
resultByTime_timePeriod = Lens.lens (\ResultByTime' {timePeriod} -> timePeriod) (\s@ResultByTime' {} a -> s {timePeriod = a} :: ResultByTime)

-- | Whether the result is estimated.
resultByTime_estimated :: Lens.Lens' ResultByTime (Prelude.Maybe Prelude.Bool)
resultByTime_estimated = Lens.lens (\ResultByTime' {estimated} -> estimated) (\s@ResultByTime' {} a -> s {estimated = a} :: ResultByTime)

-- | The total amount of cost or usage accrued during the time period.
resultByTime_total :: Lens.Lens' ResultByTime (Prelude.Maybe (Prelude.HashMap Prelude.Text MetricValue))
resultByTime_total = Lens.lens (\ResultByTime' {total} -> total) (\s@ResultByTime' {} a -> s {total = a} :: ResultByTime) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON ResultByTime where
  parseJSON =
    Core.withObject
      "ResultByTime"
      ( \x ->
          ResultByTime'
            Prelude.<$> (x Core..:? "Groups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TimePeriod")
            Prelude.<*> (x Core..:? "Estimated")
            Prelude.<*> (x Core..:? "Total" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ResultByTime

instance Prelude.NFData ResultByTime
