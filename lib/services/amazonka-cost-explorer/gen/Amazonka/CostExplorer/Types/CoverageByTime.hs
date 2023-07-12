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
-- Module      : Amazonka.CostExplorer.Types.CoverageByTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CoverageByTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.Coverage
import Amazonka.CostExplorer.Types.DateInterval
import Amazonka.CostExplorer.Types.ReservationCoverageGroup
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
coverageByTime_groups = Lens.lens (\CoverageByTime' {groups} -> groups) (\s@CoverageByTime' {} a -> s {groups = a} :: CoverageByTime) Prelude.. Lens.mapping Lens.coerced

-- | The period that this coverage was used over.
coverageByTime_timePeriod :: Lens.Lens' CoverageByTime (Prelude.Maybe DateInterval)
coverageByTime_timePeriod = Lens.lens (\CoverageByTime' {timePeriod} -> timePeriod) (\s@CoverageByTime' {} a -> s {timePeriod = a} :: CoverageByTime)

-- | The total reservation coverage, in hours.
coverageByTime_total :: Lens.Lens' CoverageByTime (Prelude.Maybe Coverage)
coverageByTime_total = Lens.lens (\CoverageByTime' {total} -> total) (\s@CoverageByTime' {} a -> s {total = a} :: CoverageByTime)

instance Data.FromJSON CoverageByTime where
  parseJSON =
    Data.withObject
      "CoverageByTime"
      ( \x ->
          CoverageByTime'
            Prelude.<$> (x Data..:? "Groups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TimePeriod")
            Prelude.<*> (x Data..:? "Total")
      )

instance Prelude.Hashable CoverageByTime where
  hashWithSalt _salt CoverageByTime' {..} =
    _salt
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` timePeriod
      `Prelude.hashWithSalt` total

instance Prelude.NFData CoverageByTime where
  rnf CoverageByTime' {..} =
    Prelude.rnf groups
      `Prelude.seq` Prelude.rnf timePeriod
      `Prelude.seq` Prelude.rnf total
