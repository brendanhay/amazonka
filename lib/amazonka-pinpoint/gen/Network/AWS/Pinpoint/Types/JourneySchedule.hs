{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneySchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.JourneySchedule
  ( JourneySchedule (..)
  -- * Smart constructor
  , mkJourneySchedule
  -- * Lenses
  , jsEndTime
  , jsStartTime
  , jsTimezone
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the schedule settings for a journey.
--
-- /See:/ 'mkJourneySchedule' smart constructor.
data JourneySchedule = JourneySchedule'
  { endTime :: Core.Maybe Core.UTCTime
    -- ^ The scheduled time, in ISO 8601 format, when the journey ended or will end.
  , startTime :: Core.Maybe Core.UTCTime
    -- ^ The scheduled time, in ISO 8601 format, when the journey began or will begin.
  , timezone :: Core.Maybe Core.Text
    -- ^ The starting UTC offset for the journey schedule, if the value of the journey's LocalTime property is true. Valid values are: UTC,
--
--                   UTC+01, UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05, UTC+05:30,
--                   UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+08:45, UTC+09, UTC+09:30,
--                   UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+12:45, UTC+13, UTC+13:45, UTC-02,
--                   UTC-02:30, UTC-03, UTC-03:30, UTC-04, UTC-05, UTC-06, UTC-07, UTC-08, UTC-09,
--                   UTC-09:30, UTC-10, and UTC-11.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'JourneySchedule' value with any optional fields omitted.
mkJourneySchedule
    :: JourneySchedule
mkJourneySchedule
  = JourneySchedule'{endTime = Core.Nothing,
                     startTime = Core.Nothing, timezone = Core.Nothing}

-- | The scheduled time, in ISO 8601 format, when the journey ended or will end.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsEndTime :: Lens.Lens' JourneySchedule (Core.Maybe Core.UTCTime)
jsEndTime = Lens.field @"endTime"
{-# INLINEABLE jsEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The scheduled time, in ISO 8601 format, when the journey began or will begin.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsStartTime :: Lens.Lens' JourneySchedule (Core.Maybe Core.UTCTime)
jsStartTime = Lens.field @"startTime"
{-# INLINEABLE jsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The starting UTC offset for the journey schedule, if the value of the journey's LocalTime property is true. Valid values are: UTC,
--
--                   UTC+01, UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05, UTC+05:30,
--                   UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+08:45, UTC+09, UTC+09:30,
--                   UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+12:45, UTC+13, UTC+13:45, UTC-02,
--                   UTC-02:30, UTC-03, UTC-03:30, UTC-04, UTC-05, UTC-06, UTC-07, UTC-08, UTC-09,
--                   UTC-09:30, UTC-10, and UTC-11.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsTimezone :: Lens.Lens' JourneySchedule (Core.Maybe Core.Text)
jsTimezone = Lens.field @"timezone"
{-# INLINEABLE jsTimezone #-}
{-# DEPRECATED timezone "Use generic-lens or generic-optics with 'timezone' instead"  #-}

instance Core.FromJSON JourneySchedule where
        toJSON JourneySchedule{..}
          = Core.object
              (Core.catMaybes
                 [("EndTime" Core..=) Core.<$> endTime,
                  ("StartTime" Core..=) Core.<$> startTime,
                  ("Timezone" Core..=) Core.<$> timezone])

instance Core.FromJSON JourneySchedule where
        parseJSON
          = Core.withObject "JourneySchedule" Core.$
              \ x ->
                JourneySchedule' Core.<$>
                  (x Core..:? "EndTime") Core.<*> x Core..:? "StartTime" Core.<*>
                    x Core..:? "Timezone"
