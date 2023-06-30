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
-- Module      : Amazonka.CostExplorer.Types.UtilizationByTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.UtilizationByTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.DateInterval
import Amazonka.CostExplorer.Types.ReservationAggregates
import Amazonka.CostExplorer.Types.ReservationUtilizationGroup
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The amount of utilization, in hours.
--
-- /See:/ 'newUtilizationByTime' smart constructor.
data UtilizationByTime = UtilizationByTime'
  { -- | The groups that this utilization result uses.
    groups :: Prelude.Maybe [ReservationUtilizationGroup],
    -- | The period of time that this utilization was used for.
    timePeriod :: Prelude.Maybe DateInterval,
    -- | The total number of reservation hours that were used.
    total :: Prelude.Maybe ReservationAggregates
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { groups = Prelude.Nothing,
      timePeriod = Prelude.Nothing,
      total = Prelude.Nothing
    }

-- | The groups that this utilization result uses.
utilizationByTime_groups :: Lens.Lens' UtilizationByTime (Prelude.Maybe [ReservationUtilizationGroup])
utilizationByTime_groups = Lens.lens (\UtilizationByTime' {groups} -> groups) (\s@UtilizationByTime' {} a -> s {groups = a} :: UtilizationByTime) Prelude.. Lens.mapping Lens.coerced

-- | The period of time that this utilization was used for.
utilizationByTime_timePeriod :: Lens.Lens' UtilizationByTime (Prelude.Maybe DateInterval)
utilizationByTime_timePeriod = Lens.lens (\UtilizationByTime' {timePeriod} -> timePeriod) (\s@UtilizationByTime' {} a -> s {timePeriod = a} :: UtilizationByTime)

-- | The total number of reservation hours that were used.
utilizationByTime_total :: Lens.Lens' UtilizationByTime (Prelude.Maybe ReservationAggregates)
utilizationByTime_total = Lens.lens (\UtilizationByTime' {total} -> total) (\s@UtilizationByTime' {} a -> s {total = a} :: UtilizationByTime)

instance Data.FromJSON UtilizationByTime where
  parseJSON =
    Data.withObject
      "UtilizationByTime"
      ( \x ->
          UtilizationByTime'
            Prelude.<$> (x Data..:? "Groups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TimePeriod")
            Prelude.<*> (x Data..:? "Total")
      )

instance Prelude.Hashable UtilizationByTime where
  hashWithSalt _salt UtilizationByTime' {..} =
    _salt
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` timePeriod
      `Prelude.hashWithSalt` total

instance Prelude.NFData UtilizationByTime where
  rnf UtilizationByTime' {..} =
    Prelude.rnf groups
      `Prelude.seq` Prelude.rnf timePeriod
      `Prelude.seq` Prelude.rnf total
