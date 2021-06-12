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
-- Module      : Network.AWS.CostExplorer.Types.CoverageByTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CoverageByTime where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.Coverage
import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.ReservationCoverageGroup
import qualified Network.AWS.Lens as Lens

-- | Reservation coverage for a specified period, in hours.
--
-- /See:/ 'newCoverageByTime' smart constructor.
data CoverageByTime = CoverageByTime'
  { -- | The groups of instances that the reservation covered.
    groups :: Core.Maybe [ReservationCoverageGroup],
    -- | The period that this coverage was used over.
    timePeriod :: Core.Maybe DateInterval,
    -- | The total reservation coverage, in hours.
    total :: Core.Maybe Coverage
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CoverageByTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'coverageByTime_groups' - The groups of instances that the reservation covered.
--
-- 'timePeriod', 'coverageByTime_timePeriod' - The period that this coverage was used over.
--
-- 'total', 'coverageByTime_total' - The total reservation coverage, in hours.
newCoverageByTime ::
  CoverageByTime
newCoverageByTime =
  CoverageByTime'
    { groups = Core.Nothing,
      timePeriod = Core.Nothing,
      total = Core.Nothing
    }

-- | The groups of instances that the reservation covered.
coverageByTime_groups :: Lens.Lens' CoverageByTime (Core.Maybe [ReservationCoverageGroup])
coverageByTime_groups = Lens.lens (\CoverageByTime' {groups} -> groups) (\s@CoverageByTime' {} a -> s {groups = a} :: CoverageByTime) Core.. Lens.mapping Lens._Coerce

-- | The period that this coverage was used over.
coverageByTime_timePeriod :: Lens.Lens' CoverageByTime (Core.Maybe DateInterval)
coverageByTime_timePeriod = Lens.lens (\CoverageByTime' {timePeriod} -> timePeriod) (\s@CoverageByTime' {} a -> s {timePeriod = a} :: CoverageByTime)

-- | The total reservation coverage, in hours.
coverageByTime_total :: Lens.Lens' CoverageByTime (Core.Maybe Coverage)
coverageByTime_total = Lens.lens (\CoverageByTime' {total} -> total) (\s@CoverageByTime' {} a -> s {total = a} :: CoverageByTime)

instance Core.FromJSON CoverageByTime where
  parseJSON =
    Core.withObject
      "CoverageByTime"
      ( \x ->
          CoverageByTime'
            Core.<$> (x Core..:? "Groups" Core..!= Core.mempty)
            Core.<*> (x Core..:? "TimePeriod")
            Core.<*> (x Core..:? "Total")
      )

instance Core.Hashable CoverageByTime

instance Core.NFData CoverageByTime
