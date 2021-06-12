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
-- Module      : Network.AWS.MediaConvert.Types.Timing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Timing where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about when jobs are submitted, started, and finished is
-- specified in Unix epoch format in seconds.
--
-- /See:/ 'newTiming' smart constructor.
data Timing = Timing'
  { -- | The time, in Unix epoch format, that the transcoding job finished
    finishTime :: Core.Maybe Core.POSIX,
    -- | The time, in Unix epoch format, that transcoding for the job began.
    startTime :: Core.Maybe Core.POSIX,
    -- | The time, in Unix epoch format, that you submitted the job.
    submitTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Timing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finishTime', 'timing_finishTime' - The time, in Unix epoch format, that the transcoding job finished
--
-- 'startTime', 'timing_startTime' - The time, in Unix epoch format, that transcoding for the job began.
--
-- 'submitTime', 'timing_submitTime' - The time, in Unix epoch format, that you submitted the job.
newTiming ::
  Timing
newTiming =
  Timing'
    { finishTime = Core.Nothing,
      startTime = Core.Nothing,
      submitTime = Core.Nothing
    }

-- | The time, in Unix epoch format, that the transcoding job finished
timing_finishTime :: Lens.Lens' Timing (Core.Maybe Core.UTCTime)
timing_finishTime = Lens.lens (\Timing' {finishTime} -> finishTime) (\s@Timing' {} a -> s {finishTime = a} :: Timing) Core.. Lens.mapping Core._Time

-- | The time, in Unix epoch format, that transcoding for the job began.
timing_startTime :: Lens.Lens' Timing (Core.Maybe Core.UTCTime)
timing_startTime = Lens.lens (\Timing' {startTime} -> startTime) (\s@Timing' {} a -> s {startTime = a} :: Timing) Core.. Lens.mapping Core._Time

-- | The time, in Unix epoch format, that you submitted the job.
timing_submitTime :: Lens.Lens' Timing (Core.Maybe Core.UTCTime)
timing_submitTime = Lens.lens (\Timing' {submitTime} -> submitTime) (\s@Timing' {} a -> s {submitTime = a} :: Timing) Core.. Lens.mapping Core._Time

instance Core.FromJSON Timing where
  parseJSON =
    Core.withObject
      "Timing"
      ( \x ->
          Timing'
            Core.<$> (x Core..:? "finishTime")
            Core.<*> (x Core..:? "startTime")
            Core.<*> (x Core..:? "submitTime")
      )

instance Core.Hashable Timing

instance Core.NFData Timing
