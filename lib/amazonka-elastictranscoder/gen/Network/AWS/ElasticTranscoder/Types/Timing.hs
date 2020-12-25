{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Timing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Timing
  ( Timing (..),

    -- * Smart constructor
    mkTiming,

    -- * Lenses
    tFinishTimeMillis,
    tStartTimeMillis,
    tSubmitTimeMillis,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the timing of a job.
--
-- /See:/ 'mkTiming' smart constructor.
data Timing = Timing'
  { -- | The time the job finished transcoding, in epoch milliseconds.
    finishTimeMillis :: Core.Maybe Core.Integer,
    -- | The time the job began transcoding, in epoch milliseconds.
    startTimeMillis :: Core.Maybe Core.Integer,
    -- | The time the job was submitted to Elastic Transcoder, in epoch milliseconds.
    submitTimeMillis :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Timing' value with any optional fields omitted.
mkTiming ::
  Timing
mkTiming =
  Timing'
    { finishTimeMillis = Core.Nothing,
      startTimeMillis = Core.Nothing,
      submitTimeMillis = Core.Nothing
    }

-- | The time the job finished transcoding, in epoch milliseconds.
--
-- /Note:/ Consider using 'finishTimeMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tFinishTimeMillis :: Lens.Lens' Timing (Core.Maybe Core.Integer)
tFinishTimeMillis = Lens.field @"finishTimeMillis"
{-# DEPRECATED tFinishTimeMillis "Use generic-lens or generic-optics with 'finishTimeMillis' instead." #-}

-- | The time the job began transcoding, in epoch milliseconds.
--
-- /Note:/ Consider using 'startTimeMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStartTimeMillis :: Lens.Lens' Timing (Core.Maybe Core.Integer)
tStartTimeMillis = Lens.field @"startTimeMillis"
{-# DEPRECATED tStartTimeMillis "Use generic-lens or generic-optics with 'startTimeMillis' instead." #-}

-- | The time the job was submitted to Elastic Transcoder, in epoch milliseconds.
--
-- /Note:/ Consider using 'submitTimeMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSubmitTimeMillis :: Lens.Lens' Timing (Core.Maybe Core.Integer)
tSubmitTimeMillis = Lens.field @"submitTimeMillis"
{-# DEPRECATED tSubmitTimeMillis "Use generic-lens or generic-optics with 'submitTimeMillis' instead." #-}

instance Core.FromJSON Timing where
  parseJSON =
    Core.withObject "Timing" Core.$
      \x ->
        Timing'
          Core.<$> (x Core..:? "FinishTimeMillis")
          Core.<*> (x Core..:? "StartTimeMillis")
          Core.<*> (x Core..:? "SubmitTimeMillis")
