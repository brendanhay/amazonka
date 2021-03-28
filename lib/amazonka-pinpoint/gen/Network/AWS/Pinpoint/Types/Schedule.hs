{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Schedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.Schedule
  ( Schedule (..)
  -- * Smart constructor
  , mkSchedule
  -- * Lenses
  , sStartTime
  , sEndTime
  , sEventFilter
  , sFrequency
  , sIsLocalTime
  , sQuietTime
  , sTimezone
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.CampaignEventFilter as Types
import qualified Network.AWS.Pinpoint.Types.Frequency as Types
import qualified Network.AWS.Pinpoint.Types.QuietTime as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the schedule settings for a campaign.
--
-- /See:/ 'mkSchedule' smart constructor.
data Schedule = Schedule'
  { startTime :: Core.Text
    -- ^ The scheduled time when the campaign began or will begin. Valid values are: IMMEDIATE, to start the campaign immediately; or, a specific time in ISO 8601 format.
  , endTime :: Core.Maybe Core.Text
    -- ^ The scheduled time, in ISO 8601 format, when the campaign ended or will end.
  , eventFilter :: Core.Maybe Types.CampaignEventFilter
    -- ^ The type of event that causes the campaign to be sent, if the value of the Frequency property is EVENT.
  , frequency :: Core.Maybe Types.Frequency
    -- ^ Specifies how often the campaign is sent or whether the campaign is sent in response to a specific event.
  , isLocalTime :: Core.Maybe Core.Bool
    -- ^ Specifies whether the start and end times for the campaign schedule use each recipient's local time. To base the schedule on each recipient's local time, set this value to true.
  , quietTime :: Core.Maybe Types.QuietTime
    -- ^ The default quiet time for the campaign. Quiet time is a specific time range when a campaign doesn't send messages to endpoints, if all the following conditions are met:
--
--
--     * The EndpointDemographic.Timezone property of the endpoint is set to a valid value.
--
--
--     * The current time in the endpoint's time zone is later than or equal to the time specified by the QuietTime.Start property for the campaign.
--
--
--     * The current time in the endpoint's time zone is earlier than or equal to the time specified by the QuietTime.End property for the campaign.
--
--
-- If any of the preceding conditions isn't met, the endpoint will receive messages from the campaign, even if quiet time is enabled.
  , timezone :: Core.Maybe Core.Text
    -- ^ The starting UTC offset for the campaign schedule, if the value of the IsLocalTime property is true. Valid values are: UTC, UTC+01, UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05,
--
--                   UTC+05:30, UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+09, UTC+09:30,
--                   UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+13, UTC-02, UTC-03, UTC-04, UTC-05, UTC-06,
--                   UTC-07, UTC-08, UTC-09, UTC-10, and UTC-11.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Schedule' value with any optional fields omitted.
mkSchedule
    :: Core.Text -- ^ 'startTime'
    -> Schedule
mkSchedule startTime
  = Schedule'{startTime, endTime = Core.Nothing,
              eventFilter = Core.Nothing, frequency = Core.Nothing,
              isLocalTime = Core.Nothing, quietTime = Core.Nothing,
              timezone = Core.Nothing}

-- | The scheduled time when the campaign began or will begin. Valid values are: IMMEDIATE, to start the campaign immediately; or, a specific time in ISO 8601 format.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' Schedule Core.Text
sStartTime = Lens.field @"startTime"
{-# INLINEABLE sStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The scheduled time, in ISO 8601 format, when the campaign ended or will end.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndTime :: Lens.Lens' Schedule (Core.Maybe Core.Text)
sEndTime = Lens.field @"endTime"
{-# INLINEABLE sEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The type of event that causes the campaign to be sent, if the value of the Frequency property is EVENT.
--
-- /Note:/ Consider using 'eventFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEventFilter :: Lens.Lens' Schedule (Core.Maybe Types.CampaignEventFilter)
sEventFilter = Lens.field @"eventFilter"
{-# INLINEABLE sEventFilter #-}
{-# DEPRECATED eventFilter "Use generic-lens or generic-optics with 'eventFilter' instead"  #-}

-- | Specifies how often the campaign is sent or whether the campaign is sent in response to a specific event.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFrequency :: Lens.Lens' Schedule (Core.Maybe Types.Frequency)
sFrequency = Lens.field @"frequency"
{-# INLINEABLE sFrequency #-}
{-# DEPRECATED frequency "Use generic-lens or generic-optics with 'frequency' instead"  #-}

-- | Specifies whether the start and end times for the campaign schedule use each recipient's local time. To base the schedule on each recipient's local time, set this value to true.
--
-- /Note:/ Consider using 'isLocalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sIsLocalTime :: Lens.Lens' Schedule (Core.Maybe Core.Bool)
sIsLocalTime = Lens.field @"isLocalTime"
{-# INLINEABLE sIsLocalTime #-}
{-# DEPRECATED isLocalTime "Use generic-lens or generic-optics with 'isLocalTime' instead"  #-}

-- | The default quiet time for the campaign. Quiet time is a specific time range when a campaign doesn't send messages to endpoints, if all the following conditions are met:
--
--
--     * The EndpointDemographic.Timezone property of the endpoint is set to a valid value.
--
--
--     * The current time in the endpoint's time zone is later than or equal to the time specified by the QuietTime.Start property for the campaign.
--
--
--     * The current time in the endpoint's time zone is earlier than or equal to the time specified by the QuietTime.End property for the campaign.
--
--
-- If any of the preceding conditions isn't met, the endpoint will receive messages from the campaign, even if quiet time is enabled.
--
-- /Note:/ Consider using 'quietTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sQuietTime :: Lens.Lens' Schedule (Core.Maybe Types.QuietTime)
sQuietTime = Lens.field @"quietTime"
{-# INLINEABLE sQuietTime #-}
{-# DEPRECATED quietTime "Use generic-lens or generic-optics with 'quietTime' instead"  #-}

-- | The starting UTC offset for the campaign schedule, if the value of the IsLocalTime property is true. Valid values are: UTC, UTC+01, UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05,
--
--                   UTC+05:30, UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+09, UTC+09:30,
--                   UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+13, UTC-02, UTC-03, UTC-04, UTC-05, UTC-06,
--                   UTC-07, UTC-08, UTC-09, UTC-10, and UTC-11.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTimezone :: Lens.Lens' Schedule (Core.Maybe Core.Text)
sTimezone = Lens.field @"timezone"
{-# INLINEABLE sTimezone #-}
{-# DEPRECATED timezone "Use generic-lens or generic-optics with 'timezone' instead"  #-}

instance Core.FromJSON Schedule where
        toJSON Schedule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StartTime" Core..= startTime),
                  ("EndTime" Core..=) Core.<$> endTime,
                  ("EventFilter" Core..=) Core.<$> eventFilter,
                  ("Frequency" Core..=) Core.<$> frequency,
                  ("IsLocalTime" Core..=) Core.<$> isLocalTime,
                  ("QuietTime" Core..=) Core.<$> quietTime,
                  ("Timezone" Core..=) Core.<$> timezone])

instance Core.FromJSON Schedule where
        parseJSON
          = Core.withObject "Schedule" Core.$
              \ x ->
                Schedule' Core.<$>
                  (x Core..: "StartTime") Core.<*> x Core..:? "EndTime" Core.<*>
                    x Core..:? "EventFilter"
                    Core.<*> x Core..:? "Frequency"
                    Core.<*> x Core..:? "IsLocalTime"
                    Core.<*> x Core..:? "QuietTime"
                    Core.<*> x Core..:? "Timezone"
