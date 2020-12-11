-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneySchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneySchedule
  ( JourneySchedule (..),

    -- * Smart constructor
    mkJourneySchedule,

    -- * Lenses
    jsStartTime,
    jsEndTime,
    jsTimezone,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the schedule settings for a journey.
--
-- /See:/ 'mkJourneySchedule' smart constructor.
data JourneySchedule = JourneySchedule'
  { startTime ::
      Lude.Maybe Lude.Timestamp,
    endTime :: Lude.Maybe Lude.Timestamp,
    timezone :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JourneySchedule' with the minimum fields required to make a request.
--
-- * 'endTime' - The scheduled time, in ISO 8601 format, when the journey ended or will end.
-- * 'startTime' - The scheduled time, in ISO 8601 format, when the journey began or will begin.
-- * 'timezone' - The starting UTC offset for the journey schedule, if the value of the journey's LocalTime property is true. Valid values are: UTC,
--
--                   UTC+01, UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05, UTC+05:30,
--                   UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+08:45, UTC+09, UTC+09:30,
--                   UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+12:45, UTC+13, UTC+13:45, UTC-02,
--                   UTC-02:30, UTC-03, UTC-03:30, UTC-04, UTC-05, UTC-06, UTC-07, UTC-08, UTC-09,
--                   UTC-09:30, UTC-10, and UTC-11.
mkJourneySchedule ::
  JourneySchedule
mkJourneySchedule =
  JourneySchedule'
    { startTime = Lude.Nothing,
      endTime = Lude.Nothing,
      timezone = Lude.Nothing
    }

-- | The scheduled time, in ISO 8601 format, when the journey began or will begin.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsStartTime :: Lens.Lens' JourneySchedule (Lude.Maybe Lude.Timestamp)
jsStartTime = Lens.lens (startTime :: JourneySchedule -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: JourneySchedule)
{-# DEPRECATED jsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The scheduled time, in ISO 8601 format, when the journey ended or will end.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsEndTime :: Lens.Lens' JourneySchedule (Lude.Maybe Lude.Timestamp)
jsEndTime = Lens.lens (endTime :: JourneySchedule -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: JourneySchedule)
{-# DEPRECATED jsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The starting UTC offset for the journey schedule, if the value of the journey's LocalTime property is true. Valid values are: UTC,
--
--                   UTC+01, UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05, UTC+05:30,
--                   UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+08:45, UTC+09, UTC+09:30,
--                   UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+12:45, UTC+13, UTC+13:45, UTC-02,
--                   UTC-02:30, UTC-03, UTC-03:30, UTC-04, UTC-05, UTC-06, UTC-07, UTC-08, UTC-09,
--                   UTC-09:30, UTC-10, and UTC-11.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsTimezone :: Lens.Lens' JourneySchedule (Lude.Maybe Lude.Text)
jsTimezone = Lens.lens (timezone :: JourneySchedule -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: JourneySchedule)
{-# DEPRECATED jsTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

instance Lude.FromJSON JourneySchedule where
  parseJSON =
    Lude.withObject
      "JourneySchedule"
      ( \x ->
          JourneySchedule'
            Lude.<$> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "Timezone")
      )

instance Lude.ToJSON JourneySchedule where
  toJSON JourneySchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StartTime" Lude..=) Lude.<$> startTime,
            ("EndTime" Lude..=) Lude.<$> endTime,
            ("Timezone" Lude..=) Lude.<$> timezone
          ]
      )
