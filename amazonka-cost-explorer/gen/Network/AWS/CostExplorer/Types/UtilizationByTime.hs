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
-- Module      : Network.AWS.CostExplorer.Types.UtilizationByTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.UtilizationByTime where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.ReservationAggregates
import Network.AWS.CostExplorer.Types.ReservationUtilizationGroup
import qualified Network.AWS.Lens as Lens

-- | The amount of utilization, in hours.
--
-- /See:/ 'newUtilizationByTime' smart constructor.
data UtilizationByTime = UtilizationByTime'
  { -- | The groups that this utilization result uses.
    groups :: Core.Maybe [ReservationUtilizationGroup],
    -- | The period of time that this utilization was used for.
    timePeriod :: Core.Maybe DateInterval,
    -- | The total number of reservation hours that were used.
    total :: Core.Maybe ReservationAggregates
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UtilizationByTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'utilizationByTime_groups' - The groups that this utilization result uses.
--
-- 'timePeriod', 'utilizationByTime_timePeriod' - The period of time that this utilization was used for.
--
-- 'total', 'utilizationByTime_total' - The total number of reservation hours that were used.
newUtilizationByTime ::
  UtilizationByTime
newUtilizationByTime =
  UtilizationByTime'
    { groups = Core.Nothing,
      timePeriod = Core.Nothing,
      total = Core.Nothing
    }

-- | The groups that this utilization result uses.
utilizationByTime_groups :: Lens.Lens' UtilizationByTime (Core.Maybe [ReservationUtilizationGroup])
utilizationByTime_groups = Lens.lens (\UtilizationByTime' {groups} -> groups) (\s@UtilizationByTime' {} a -> s {groups = a} :: UtilizationByTime) Core.. Lens.mapping Lens._Coerce

-- | The period of time that this utilization was used for.
utilizationByTime_timePeriod :: Lens.Lens' UtilizationByTime (Core.Maybe DateInterval)
utilizationByTime_timePeriod = Lens.lens (\UtilizationByTime' {timePeriod} -> timePeriod) (\s@UtilizationByTime' {} a -> s {timePeriod = a} :: UtilizationByTime)

-- | The total number of reservation hours that were used.
utilizationByTime_total :: Lens.Lens' UtilizationByTime (Core.Maybe ReservationAggregates)
utilizationByTime_total = Lens.lens (\UtilizationByTime' {total} -> total) (\s@UtilizationByTime' {} a -> s {total = a} :: UtilizationByTime)

instance Core.FromJSON UtilizationByTime where
  parseJSON =
    Core.withObject
      "UtilizationByTime"
      ( \x ->
          UtilizationByTime'
            Core.<$> (x Core..:? "Groups" Core..!= Core.mempty)
            Core.<*> (x Core..:? "TimePeriod")
            Core.<*> (x Core..:? "Total")
      )

instance Core.Hashable UtilizationByTime

instance Core.NFData UtilizationByTime
