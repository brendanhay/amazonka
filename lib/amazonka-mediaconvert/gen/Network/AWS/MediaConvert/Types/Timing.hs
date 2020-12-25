{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Timing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Timing
  ( Timing (..),

    -- * Smart constructor
    mkTiming,

    -- * Lenses
    tFinishTime,
    tStartTime,
    tSubmitTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about when jobs are submitted, started, and finished is specified in Unix epoch format in seconds.
--
-- /See:/ 'mkTiming' smart constructor.
data Timing = Timing'
  { -- | The time, in Unix epoch format, that the transcoding job finished
    finishTime :: Core.Maybe Core.NominalDiffTime,
    -- | The time, in Unix epoch format, that transcoding for the job began.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The time, in Unix epoch format, that you submitted the job.
    submitTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Timing' value with any optional fields omitted.
mkTiming ::
  Timing
mkTiming =
  Timing'
    { finishTime = Core.Nothing,
      startTime = Core.Nothing,
      submitTime = Core.Nothing
    }

-- | The time, in Unix epoch format, that the transcoding job finished
--
-- /Note:/ Consider using 'finishTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tFinishTime :: Lens.Lens' Timing (Core.Maybe Core.NominalDiffTime)
tFinishTime = Lens.field @"finishTime"
{-# DEPRECATED tFinishTime "Use generic-lens or generic-optics with 'finishTime' instead." #-}

-- | The time, in Unix epoch format, that transcoding for the job began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStartTime :: Lens.Lens' Timing (Core.Maybe Core.NominalDiffTime)
tStartTime = Lens.field @"startTime"
{-# DEPRECATED tStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The time, in Unix epoch format, that you submitted the job.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSubmitTime :: Lens.Lens' Timing (Core.Maybe Core.NominalDiffTime)
tSubmitTime = Lens.field @"submitTime"
{-# DEPRECATED tSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

instance Core.FromJSON Timing where
  parseJSON =
    Core.withObject "Timing" Core.$
      \x ->
        Timing'
          Core.<$> (x Core..:? "finishTime")
          Core.<*> (x Core..:? "startTime")
          Core.<*> (x Core..:? "submitTime")
