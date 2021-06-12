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
-- Module      : Network.AWS.CodeDeploy.Types.TimeRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TimeRange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a time range.
--
-- /See:/ 'newTimeRange' smart constructor.
data TimeRange = TimeRange'
  { -- | The end time of the time range.
    --
    -- Specify null to leave the end time open-ended.
    end :: Core.Maybe Core.POSIX,
    -- | The start time of the time range.
    --
    -- Specify null to leave the start time open-ended.
    start :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'timeRange_end' - The end time of the time range.
--
-- Specify null to leave the end time open-ended.
--
-- 'start', 'timeRange_start' - The start time of the time range.
--
-- Specify null to leave the start time open-ended.
newTimeRange ::
  TimeRange
newTimeRange =
  TimeRange'
    { end = Core.Nothing,
      start = Core.Nothing
    }

-- | The end time of the time range.
--
-- Specify null to leave the end time open-ended.
timeRange_end :: Lens.Lens' TimeRange (Core.Maybe Core.UTCTime)
timeRange_end = Lens.lens (\TimeRange' {end} -> end) (\s@TimeRange' {} a -> s {end = a} :: TimeRange) Core.. Lens.mapping Core._Time

-- | The start time of the time range.
--
-- Specify null to leave the start time open-ended.
timeRange_start :: Lens.Lens' TimeRange (Core.Maybe Core.UTCTime)
timeRange_start = Lens.lens (\TimeRange' {start} -> start) (\s@TimeRange' {} a -> s {start = a} :: TimeRange) Core.. Lens.mapping Core._Time

instance Core.Hashable TimeRange

instance Core.NFData TimeRange

instance Core.ToJSON TimeRange where
  toJSON TimeRange' {..} =
    Core.object
      ( Core.catMaybes
          [ ("end" Core..=) Core.<$> end,
            ("start" Core..=) Core.<$> start
          ]
      )
