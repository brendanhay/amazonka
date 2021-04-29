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
-- Module      : Network.AWS.CostExplorer.Types.CoverageByTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CoverageByTime where

import Network.AWS.CostExplorer.Types.Coverage
import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.ReservationCoverageGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Reservation coverage for a specified period, in hours.
--
-- /See:/ 'newCoverageByTime' smart constructor.
data CoverageByTime = CoverageByTime'
  { -- | The groups of instances that the reservation covered.
    groups :: Prelude.Maybe [ReservationCoverageGroup],
    -- | The period that this coverage was used over.
    timePeriod :: Prelude.Maybe DateInterval,
    -- | The total reservation coverage, in hours.
    total :: Prelude.Maybe Coverage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { groups = Prelude.Nothing,
      timePeriod = Prelude.Nothing,
      total = Prelude.Nothing
    }

-- | The groups of instances that the reservation covered.
coverageByTime_groups :: Lens.Lens' CoverageByTime (Prelude.Maybe [ReservationCoverageGroup])
coverageByTime_groups = Lens.lens (\CoverageByTime' {groups} -> groups) (\s@CoverageByTime' {} a -> s {groups = a} :: CoverageByTime) Prelude.. Lens.mapping Prelude._Coerce

-- | The period that this coverage was used over.
coverageByTime_timePeriod :: Lens.Lens' CoverageByTime (Prelude.Maybe DateInterval)
coverageByTime_timePeriod = Lens.lens (\CoverageByTime' {timePeriod} -> timePeriod) (\s@CoverageByTime' {} a -> s {timePeriod = a} :: CoverageByTime)

-- | The total reservation coverage, in hours.
coverageByTime_total :: Lens.Lens' CoverageByTime (Prelude.Maybe Coverage)
coverageByTime_total = Lens.lens (\CoverageByTime' {total} -> total) (\s@CoverageByTime' {} a -> s {total = a} :: CoverageByTime)

instance Prelude.FromJSON CoverageByTime where
  parseJSON =
    Prelude.withObject
      "CoverageByTime"
      ( \x ->
          CoverageByTime'
            Prelude.<$> (x Prelude..:? "Groups" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "TimePeriod")
            Prelude.<*> (x Prelude..:? "Total")
      )

instance Prelude.Hashable CoverageByTime

instance Prelude.NFData CoverageByTime
