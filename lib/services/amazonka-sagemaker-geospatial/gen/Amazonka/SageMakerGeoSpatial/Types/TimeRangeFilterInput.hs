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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.TimeRangeFilterInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.TimeRangeFilterInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The input for the time-range filter.
--
-- /See:/ 'newTimeRangeFilterInput' smart constructor.
data TimeRangeFilterInput = TimeRangeFilterInput'
  { -- | The end time for the time-range filter.
    endTime :: Data.POSIX,
    -- | The start time for the time-range filter.
    startTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeRangeFilterInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'timeRangeFilterInput_endTime' - The end time for the time-range filter.
--
-- 'startTime', 'timeRangeFilterInput_startTime' - The start time for the time-range filter.
newTimeRangeFilterInput ::
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'startTime'
  Prelude.UTCTime ->
  TimeRangeFilterInput
newTimeRangeFilterInput pEndTime_ pStartTime_ =
  TimeRangeFilterInput'
    { endTime =
        Data._Time Lens.# pEndTime_,
      startTime = Data._Time Lens.# pStartTime_
    }

-- | The end time for the time-range filter.
timeRangeFilterInput_endTime :: Lens.Lens' TimeRangeFilterInput Prelude.UTCTime
timeRangeFilterInput_endTime = Lens.lens (\TimeRangeFilterInput' {endTime} -> endTime) (\s@TimeRangeFilterInput' {} a -> s {endTime = a} :: TimeRangeFilterInput) Prelude.. Data._Time

-- | The start time for the time-range filter.
timeRangeFilterInput_startTime :: Lens.Lens' TimeRangeFilterInput Prelude.UTCTime
timeRangeFilterInput_startTime = Lens.lens (\TimeRangeFilterInput' {startTime} -> startTime) (\s@TimeRangeFilterInput' {} a -> s {startTime = a} :: TimeRangeFilterInput) Prelude.. Data._Time

instance Prelude.Hashable TimeRangeFilterInput where
  hashWithSalt _salt TimeRangeFilterInput' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData TimeRangeFilterInput where
  rnf TimeRangeFilterInput' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToJSON TimeRangeFilterInput where
  toJSON TimeRangeFilterInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("EndTime" Data..= endTime),
            Prelude.Just ("StartTime" Data..= startTime)
          ]
      )
