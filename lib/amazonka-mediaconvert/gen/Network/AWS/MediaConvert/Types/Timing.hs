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
    tStartTime,
    tFinishTime,
    tSubmitTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about when jobs are submitted, started, and finished is specified in Unix epoch format in seconds.
--
-- /See:/ 'mkTiming' smart constructor.
data Timing = Timing'
  { startTime :: Lude.Maybe Lude.Timestamp,
    finishTime :: Lude.Maybe Lude.Timestamp,
    submitTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Timing' with the minimum fields required to make a request.
--
-- * 'finishTime' - The time, in Unix epoch format, that the transcoding job finished
-- * 'startTime' - The time, in Unix epoch format, that transcoding for the job began.
-- * 'submitTime' - The time, in Unix epoch format, that you submitted the job.
mkTiming ::
  Timing
mkTiming =
  Timing'
    { startTime = Lude.Nothing,
      finishTime = Lude.Nothing,
      submitTime = Lude.Nothing
    }

-- | The time, in Unix epoch format, that transcoding for the job began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStartTime :: Lens.Lens' Timing (Lude.Maybe Lude.Timestamp)
tStartTime = Lens.lens (startTime :: Timing -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: Timing)
{-# DEPRECATED tStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The time, in Unix epoch format, that the transcoding job finished
--
-- /Note:/ Consider using 'finishTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tFinishTime :: Lens.Lens' Timing (Lude.Maybe Lude.Timestamp)
tFinishTime = Lens.lens (finishTime :: Timing -> Lude.Maybe Lude.Timestamp) (\s a -> s {finishTime = a} :: Timing)
{-# DEPRECATED tFinishTime "Use generic-lens or generic-optics with 'finishTime' instead." #-}

-- | The time, in Unix epoch format, that you submitted the job.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSubmitTime :: Lens.Lens' Timing (Lude.Maybe Lude.Timestamp)
tSubmitTime = Lens.lens (submitTime :: Timing -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTime = a} :: Timing)
{-# DEPRECATED tSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

instance Lude.FromJSON Timing where
  parseJSON =
    Lude.withObject
      "Timing"
      ( \x ->
          Timing'
            Lude.<$> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "finishTime")
            Lude.<*> (x Lude..:? "submitTime")
      )
