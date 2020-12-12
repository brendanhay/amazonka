{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings
  ( Scte35SpliceInsertScheduleActionSettings (..),

    -- * Smart constructor
    mkScte35SpliceInsertScheduleActionSettings,

    -- * Lenses
    ssisasDuration,
    ssisasSpliceEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for a SCTE-35 splice_insert message.
--
-- /See:/ 'mkScte35SpliceInsertScheduleActionSettings' smart constructor.
data Scte35SpliceInsertScheduleActionSettings = Scte35SpliceInsertScheduleActionSettings'
  { duration ::
      Lude.Maybe
        Lude.Natural,
    spliceEventId ::
      Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scte35SpliceInsertScheduleActionSettings' with the minimum fields required to make a request.
--
-- * 'duration' - Optional, the duration for the splice_insert, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. If you enter a duration, there is an expectation that the downstream system can read the duration and cue in at that time. If you do not enter a duration, the splice_insert will continue indefinitely and there is an expectation that you will enter a return_to_network to end the splice_insert at the appropriate time.
-- * 'spliceEventId' - The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
mkScte35SpliceInsertScheduleActionSettings ::
  -- | 'spliceEventId'
  Lude.Natural ->
  Scte35SpliceInsertScheduleActionSettings
mkScte35SpliceInsertScheduleActionSettings pSpliceEventId_ =
  Scte35SpliceInsertScheduleActionSettings'
    { duration =
        Lude.Nothing,
      spliceEventId = pSpliceEventId_
    }

-- | Optional, the duration for the splice_insert, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. If you enter a duration, there is an expectation that the downstream system can read the duration and cue in at that time. If you do not enter a duration, the splice_insert will continue indefinitely and there is an expectation that you will enter a return_to_network to end the splice_insert at the appropriate time.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssisasDuration :: Lens.Lens' Scte35SpliceInsertScheduleActionSettings (Lude.Maybe Lude.Natural)
ssisasDuration = Lens.lens (duration :: Scte35SpliceInsertScheduleActionSettings -> Lude.Maybe Lude.Natural) (\s a -> s {duration = a} :: Scte35SpliceInsertScheduleActionSettings)
{-# DEPRECATED ssisasDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
--
-- /Note:/ Consider using 'spliceEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssisasSpliceEventId :: Lens.Lens' Scte35SpliceInsertScheduleActionSettings Lude.Natural
ssisasSpliceEventId = Lens.lens (spliceEventId :: Scte35SpliceInsertScheduleActionSettings -> Lude.Natural) (\s a -> s {spliceEventId = a} :: Scte35SpliceInsertScheduleActionSettings)
{-# DEPRECATED ssisasSpliceEventId "Use generic-lens or generic-optics with 'spliceEventId' instead." #-}

instance Lude.FromJSON Scte35SpliceInsertScheduleActionSettings where
  parseJSON =
    Lude.withObject
      "Scte35SpliceInsertScheduleActionSettings"
      ( \x ->
          Scte35SpliceInsertScheduleActionSettings'
            Lude.<$> (x Lude..:? "duration") Lude.<*> (x Lude..: "spliceEventId")
      )

instance Lude.ToJSON Scte35SpliceInsertScheduleActionSettings where
  toJSON Scte35SpliceInsertScheduleActionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("duration" Lude..=) Lude.<$> duration,
            Lude.Just ("spliceEventId" Lude..= spliceEventId)
          ]
      )
