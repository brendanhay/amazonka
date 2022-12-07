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
-- Module      : Amazonka.GroundStation.Types.TimeRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.TimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A time range with a start and end time.
--
-- /See:/ 'newTimeRange' smart constructor.
data TimeRange = TimeRange'
  { -- | Time in UTC at which the time range ends.
    endTime :: Data.POSIX,
    -- | Time in UTC at which the time range starts.
    startTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'timeRange_endTime' - Time in UTC at which the time range ends.
--
-- 'startTime', 'timeRange_startTime' - Time in UTC at which the time range starts.
newTimeRange ::
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'startTime'
  Prelude.UTCTime ->
  TimeRange
newTimeRange pEndTime_ pStartTime_ =
  TimeRange'
    { endTime = Data._Time Lens.# pEndTime_,
      startTime = Data._Time Lens.# pStartTime_
    }

-- | Time in UTC at which the time range ends.
timeRange_endTime :: Lens.Lens' TimeRange Prelude.UTCTime
timeRange_endTime = Lens.lens (\TimeRange' {endTime} -> endTime) (\s@TimeRange' {} a -> s {endTime = a} :: TimeRange) Prelude.. Data._Time

-- | Time in UTC at which the time range starts.
timeRange_startTime :: Lens.Lens' TimeRange Prelude.UTCTime
timeRange_startTime = Lens.lens (\TimeRange' {startTime} -> startTime) (\s@TimeRange' {} a -> s {startTime = a} :: TimeRange) Prelude.. Data._Time

instance Prelude.Hashable TimeRange where
  hashWithSalt _salt TimeRange' {..} =
    _salt `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData TimeRange where
  rnf TimeRange' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToJSON TimeRange where
  toJSON TimeRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("endTime" Data..= endTime),
            Prelude.Just ("startTime" Data..= startTime)
          ]
      )
