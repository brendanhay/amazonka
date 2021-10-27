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
-- Module      : Network.AWS.Kendra.Types.TimeRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.TimeRange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides a range of time.
--
-- /See:/ 'newTimeRange' smart constructor.
data TimeRange = TimeRange'
  { -- | The UNIX datetime of the beginning of the time range.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The UNIX datetime of the end of the time range.
    endTime :: Prelude.Maybe Core.POSIX
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
-- 'startTime', 'timeRange_startTime' - The UNIX datetime of the beginning of the time range.
--
-- 'endTime', 'timeRange_endTime' - The UNIX datetime of the end of the time range.
newTimeRange ::
  TimeRange
newTimeRange =
  TimeRange'
    { startTime = Prelude.Nothing,
      endTime = Prelude.Nothing
    }

-- | The UNIX datetime of the beginning of the time range.
timeRange_startTime :: Lens.Lens' TimeRange (Prelude.Maybe Prelude.UTCTime)
timeRange_startTime = Lens.lens (\TimeRange' {startTime} -> startTime) (\s@TimeRange' {} a -> s {startTime = a} :: TimeRange) Prelude.. Lens.mapping Core._Time

-- | The UNIX datetime of the end of the time range.
timeRange_endTime :: Lens.Lens' TimeRange (Prelude.Maybe Prelude.UTCTime)
timeRange_endTime = Lens.lens (\TimeRange' {endTime} -> endTime) (\s@TimeRange' {} a -> s {endTime = a} :: TimeRange) Prelude.. Lens.mapping Core._Time

instance Prelude.Hashable TimeRange

instance Prelude.NFData TimeRange

instance Core.ToJSON TimeRange where
  toJSON TimeRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StartTime" Core..=) Prelude.<$> startTime,
            ("EndTime" Core..=) Prelude.<$> endTime
          ]
      )
