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
-- Module      : Network.AWS.ElastiCache.Types.TimeRangeFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.TimeRangeFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Filters update actions from the service updates that are in available
-- status during the time range.
--
-- /See:/ 'newTimeRangeFilter' smart constructor.
data TimeRangeFilter = TimeRangeFilter'
  { -- | The start time of the time range filter
    startTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The end time of the time range filter
    endTime :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TimeRangeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'timeRangeFilter_startTime' - The start time of the time range filter
--
-- 'endTime', 'timeRangeFilter_endTime' - The end time of the time range filter
newTimeRangeFilter ::
  TimeRangeFilter
newTimeRangeFilter =
  TimeRangeFilter'
    { startTime = Prelude.Nothing,
      endTime = Prelude.Nothing
    }

-- | The start time of the time range filter
timeRangeFilter_startTime :: Lens.Lens' TimeRangeFilter (Prelude.Maybe Prelude.UTCTime)
timeRangeFilter_startTime = Lens.lens (\TimeRangeFilter' {startTime} -> startTime) (\s@TimeRangeFilter' {} a -> s {startTime = a} :: TimeRangeFilter) Prelude.. Lens.mapping Prelude._Time

-- | The end time of the time range filter
timeRangeFilter_endTime :: Lens.Lens' TimeRangeFilter (Prelude.Maybe Prelude.UTCTime)
timeRangeFilter_endTime = Lens.lens (\TimeRangeFilter' {endTime} -> endTime) (\s@TimeRangeFilter' {} a -> s {endTime = a} :: TimeRangeFilter) Prelude.. Lens.mapping Prelude._Time

instance Prelude.Hashable TimeRangeFilter

instance Prelude.NFData TimeRangeFilter

instance Prelude.ToQuery TimeRangeFilter where
  toQuery TimeRangeFilter' {..} =
    Prelude.mconcat
      [ "StartTime" Prelude.=: startTime,
        "EndTime" Prelude.=: endTime
      ]
