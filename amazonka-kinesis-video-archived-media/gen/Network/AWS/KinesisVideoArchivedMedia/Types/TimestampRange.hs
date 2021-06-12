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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The range of timestamps for which to return fragments.
--
-- /See:/ 'newTimestampRange' smart constructor.
data TimestampRange = TimestampRange'
  { -- | The starting timestamp in the range of timestamps for which to return
    -- fragments.
    startTimestamp :: Core.POSIX,
    -- | The ending timestamp in the range of timestamps for which to return
    -- fragments.
    endTimestamp :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TimestampRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTimestamp', 'timestampRange_startTimestamp' - The starting timestamp in the range of timestamps for which to return
-- fragments.
--
-- 'endTimestamp', 'timestampRange_endTimestamp' - The ending timestamp in the range of timestamps for which to return
-- fragments.
newTimestampRange ::
  -- | 'startTimestamp'
  Core.UTCTime ->
  -- | 'endTimestamp'
  Core.UTCTime ->
  TimestampRange
newTimestampRange pStartTimestamp_ pEndTimestamp_ =
  TimestampRange'
    { startTimestamp =
        Core._Time Lens.# pStartTimestamp_,
      endTimestamp = Core._Time Lens.# pEndTimestamp_
    }

-- | The starting timestamp in the range of timestamps for which to return
-- fragments.
timestampRange_startTimestamp :: Lens.Lens' TimestampRange Core.UTCTime
timestampRange_startTimestamp = Lens.lens (\TimestampRange' {startTimestamp} -> startTimestamp) (\s@TimestampRange' {} a -> s {startTimestamp = a} :: TimestampRange) Core.. Core._Time

-- | The ending timestamp in the range of timestamps for which to return
-- fragments.
timestampRange_endTimestamp :: Lens.Lens' TimestampRange Core.UTCTime
timestampRange_endTimestamp = Lens.lens (\TimestampRange' {endTimestamp} -> endTimestamp) (\s@TimestampRange' {} a -> s {endTimestamp = a} :: TimestampRange) Core.. Core._Time

instance Core.Hashable TimestampRange

instance Core.NFData TimestampRange

instance Core.ToJSON TimestampRange where
  toJSON TimestampRange' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StartTimestamp" Core..= startTimestamp),
            Core.Just ("EndTimestamp" Core..= endTimestamp)
          ]
      )
