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
-- Module      : Amazonka.MediaTailor.Types.PrefetchConsumption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.PrefetchConsumption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types.AvailMatchingCriteria
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains settings that determine how and when that
-- MediaTailor places prefetched ads into upcoming ad breaks.
--
-- /See:/ 'newPrefetchConsumption' smart constructor.
data PrefetchConsumption = PrefetchConsumption'
  { -- | The time when prefetched ads are considered for use in an ad break. If
    -- you don\'t specify StartTime, the prefetched ads are available after
    -- MediaTailor retrives them from the ad decision server.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | If you only want MediaTailor to insert prefetched ads into avails (ad
    -- breaks) that match specific dynamic variables, such as scte.event_id,
    -- set the avail matching criteria.
    availMatchingCriteria :: Prelude.Maybe [AvailMatchingCriteria],
    -- | The time when MediaTailor no longer considers the prefetched ads for use
    -- in an ad break. MediaTailor automatically deletes prefetch schedules no
    -- less than seven days after the end time. If you\'d like to manually
    -- delete the prefetch schedule, you can call DeletePrefetchSchedule.
    endTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrefetchConsumption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'prefetchConsumption_startTime' - The time when prefetched ads are considered for use in an ad break. If
-- you don\'t specify StartTime, the prefetched ads are available after
-- MediaTailor retrives them from the ad decision server.
--
-- 'availMatchingCriteria', 'prefetchConsumption_availMatchingCriteria' - If you only want MediaTailor to insert prefetched ads into avails (ad
-- breaks) that match specific dynamic variables, such as scte.event_id,
-- set the avail matching criteria.
--
-- 'endTime', 'prefetchConsumption_endTime' - The time when MediaTailor no longer considers the prefetched ads for use
-- in an ad break. MediaTailor automatically deletes prefetch schedules no
-- less than seven days after the end time. If you\'d like to manually
-- delete the prefetch schedule, you can call DeletePrefetchSchedule.
newPrefetchConsumption ::
  -- | 'endTime'
  Prelude.UTCTime ->
  PrefetchConsumption
newPrefetchConsumption pEndTime_ =
  PrefetchConsumption'
    { startTime = Prelude.Nothing,
      availMatchingCriteria = Prelude.Nothing,
      endTime = Core._Time Lens.# pEndTime_
    }

-- | The time when prefetched ads are considered for use in an ad break. If
-- you don\'t specify StartTime, the prefetched ads are available after
-- MediaTailor retrives them from the ad decision server.
prefetchConsumption_startTime :: Lens.Lens' PrefetchConsumption (Prelude.Maybe Prelude.UTCTime)
prefetchConsumption_startTime = Lens.lens (\PrefetchConsumption' {startTime} -> startTime) (\s@PrefetchConsumption' {} a -> s {startTime = a} :: PrefetchConsumption) Prelude.. Lens.mapping Core._Time

-- | If you only want MediaTailor to insert prefetched ads into avails (ad
-- breaks) that match specific dynamic variables, such as scte.event_id,
-- set the avail matching criteria.
prefetchConsumption_availMatchingCriteria :: Lens.Lens' PrefetchConsumption (Prelude.Maybe [AvailMatchingCriteria])
prefetchConsumption_availMatchingCriteria = Lens.lens (\PrefetchConsumption' {availMatchingCriteria} -> availMatchingCriteria) (\s@PrefetchConsumption' {} a -> s {availMatchingCriteria = a} :: PrefetchConsumption) Prelude.. Lens.mapping Lens.coerced

-- | The time when MediaTailor no longer considers the prefetched ads for use
-- in an ad break. MediaTailor automatically deletes prefetch schedules no
-- less than seven days after the end time. If you\'d like to manually
-- delete the prefetch schedule, you can call DeletePrefetchSchedule.
prefetchConsumption_endTime :: Lens.Lens' PrefetchConsumption Prelude.UTCTime
prefetchConsumption_endTime = Lens.lens (\PrefetchConsumption' {endTime} -> endTime) (\s@PrefetchConsumption' {} a -> s {endTime = a} :: PrefetchConsumption) Prelude.. Core._Time

instance Core.FromJSON PrefetchConsumption where
  parseJSON =
    Core.withObject
      "PrefetchConsumption"
      ( \x ->
          PrefetchConsumption'
            Prelude.<$> (x Core..:? "StartTime")
            Prelude.<*> ( x Core..:? "AvailMatchingCriteria"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "EndTime")
      )

instance Prelude.Hashable PrefetchConsumption where
  hashWithSalt salt' PrefetchConsumption' {..} =
    salt' `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` availMatchingCriteria
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData PrefetchConsumption where
  rnf PrefetchConsumption' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf availMatchingCriteria

instance Core.ToJSON PrefetchConsumption where
  toJSON PrefetchConsumption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StartTime" Core..=) Prelude.<$> startTime,
            ("AvailMatchingCriteria" Core..=)
              Prelude.<$> availMatchingCriteria,
            Prelude.Just ("EndTime" Core..= endTime)
          ]
      )
