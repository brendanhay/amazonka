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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.TimeRangeFilterOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.TimeRangeFilterOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The output structure of the time range filter.
--
-- /See:/ 'newTimeRangeFilterOutput' smart constructor.
data TimeRangeFilterOutput = TimeRangeFilterOutput'
  { -- | The ending time for the time range filter.
    endTime :: Data.ISO8601,
    -- | The starting time for the time range filter.
    startTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeRangeFilterOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'timeRangeFilterOutput_endTime' - The ending time for the time range filter.
--
-- 'startTime', 'timeRangeFilterOutput_startTime' - The starting time for the time range filter.
newTimeRangeFilterOutput ::
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'startTime'
  Prelude.UTCTime ->
  TimeRangeFilterOutput
newTimeRangeFilterOutput pEndTime_ pStartTime_ =
  TimeRangeFilterOutput'
    { endTime =
        Data._Time Lens.# pEndTime_,
      startTime = Data._Time Lens.# pStartTime_
    }

-- | The ending time for the time range filter.
timeRangeFilterOutput_endTime :: Lens.Lens' TimeRangeFilterOutput Prelude.UTCTime
timeRangeFilterOutput_endTime = Lens.lens (\TimeRangeFilterOutput' {endTime} -> endTime) (\s@TimeRangeFilterOutput' {} a -> s {endTime = a} :: TimeRangeFilterOutput) Prelude.. Data._Time

-- | The starting time for the time range filter.
timeRangeFilterOutput_startTime :: Lens.Lens' TimeRangeFilterOutput Prelude.UTCTime
timeRangeFilterOutput_startTime = Lens.lens (\TimeRangeFilterOutput' {startTime} -> startTime) (\s@TimeRangeFilterOutput' {} a -> s {startTime = a} :: TimeRangeFilterOutput) Prelude.. Data._Time

instance Data.FromJSON TimeRangeFilterOutput where
  parseJSON =
    Data.withObject
      "TimeRangeFilterOutput"
      ( \x ->
          TimeRangeFilterOutput'
            Prelude.<$> (x Data..: "EndTime")
            Prelude.<*> (x Data..: "StartTime")
      )

instance Prelude.Hashable TimeRangeFilterOutput where
  hashWithSalt _salt TimeRangeFilterOutput' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData TimeRangeFilterOutput where
  rnf TimeRangeFilterOutput' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
