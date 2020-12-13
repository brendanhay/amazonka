{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Schedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Schedule
  ( Schedule (..),

    -- * Smart constructor
    mkSchedule,

    -- * Lenses
    sFrequency,
    sStartTime,
    sQuietTime,
    sEventFilter,
    sIsLocalTime,
    sEndTime,
    sTimezone,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignEventFilter
import Network.AWS.Pinpoint.Types.Frequency
import Network.AWS.Pinpoint.Types.QuietTime
import qualified Network.AWS.Prelude as Lude

-- | Specifies the schedule settings for a campaign.
--
-- /See:/ 'mkSchedule' smart constructor.
data Schedule = Schedule'
  { -- | Specifies how often the campaign is sent or whether the campaign is sent in response to a specific event.
    frequency :: Lude.Maybe Frequency,
    -- | The scheduled time when the campaign began or will begin. Valid values are: IMMEDIATE, to start the campaign immediately; or, a specific time in ISO 8601 format.
    startTime :: Lude.Text,
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
    quietTime :: Lude.Maybe QuietTime,
    -- | The type of event that causes the campaign to be sent, if the value of the Frequency property is EVENT.
    eventFilter :: Lude.Maybe CampaignEventFilter,
    -- | Specifies whether the start and end times for the campaign schedule use each recipient's local time. To base the schedule on each recipient's local time, set this value to true.
    isLocalTime :: Lude.Maybe Lude.Bool,
    -- | The scheduled time, in ISO 8601 format, when the campaign ended or will end.
    endTime :: Lude.Maybe Lude.Text,
    -- | The starting UTC offset for the campaign schedule, if the value of the IsLocalTime property is true. Valid values are: UTC, UTC+01, UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05,
    --
    --                   UTC+05:30, UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+09, UTC+09:30,
    --                   UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+13, UTC-02, UTC-03, UTC-04, UTC-05, UTC-06,
    --                   UTC-07, UTC-08, UTC-09, UTC-10, and UTC-11.
    timezone :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Schedule' with the minimum fields required to make a request.
--
-- * 'frequency' - Specifies how often the campaign is sent or whether the campaign is sent in response to a specific event.
-- * 'startTime' - The scheduled time when the campaign began or will begin. Valid values are: IMMEDIATE, to start the campaign immediately; or, a specific time in ISO 8601 format.
-- * 'quietTime' - The default quiet time for the campaign. Quiet time is a specific time range when a campaign doesn't send messages to endpoints, if all the following conditions are met:
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
-- * 'eventFilter' - The type of event that causes the campaign to be sent, if the value of the Frequency property is EVENT.
-- * 'isLocalTime' - Specifies whether the start and end times for the campaign schedule use each recipient's local time. To base the schedule on each recipient's local time, set this value to true.
-- * 'endTime' - The scheduled time, in ISO 8601 format, when the campaign ended or will end.
-- * 'timezone' - The starting UTC offset for the campaign schedule, if the value of the IsLocalTime property is true. Valid values are: UTC, UTC+01, UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05,
--
--                   UTC+05:30, UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+09, UTC+09:30,
--                   UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+13, UTC-02, UTC-03, UTC-04, UTC-05, UTC-06,
--                   UTC-07, UTC-08, UTC-09, UTC-10, and UTC-11.
mkSchedule ::
  -- | 'startTime'
  Lude.Text ->
  Schedule
mkSchedule pStartTime_ =
  Schedule'
    { frequency = Lude.Nothing,
      startTime = pStartTime_,
      quietTime = Lude.Nothing,
      eventFilter = Lude.Nothing,
      isLocalTime = Lude.Nothing,
      endTime = Lude.Nothing,
      timezone = Lude.Nothing
    }

-- | Specifies how often the campaign is sent or whether the campaign is sent in response to a specific event.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFrequency :: Lens.Lens' Schedule (Lude.Maybe Frequency)
sFrequency = Lens.lens (frequency :: Schedule -> Lude.Maybe Frequency) (\s a -> s {frequency = a} :: Schedule)
{-# DEPRECATED sFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The scheduled time when the campaign began or will begin. Valid values are: IMMEDIATE, to start the campaign immediately; or, a specific time in ISO 8601 format.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' Schedule Lude.Text
sStartTime = Lens.lens (startTime :: Schedule -> Lude.Text) (\s a -> s {startTime = a} :: Schedule)
{-# DEPRECATED sStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

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
sQuietTime :: Lens.Lens' Schedule (Lude.Maybe QuietTime)
sQuietTime = Lens.lens (quietTime :: Schedule -> Lude.Maybe QuietTime) (\s a -> s {quietTime = a} :: Schedule)
{-# DEPRECATED sQuietTime "Use generic-lens or generic-optics with 'quietTime' instead." #-}

-- | The type of event that causes the campaign to be sent, if the value of the Frequency property is EVENT.
--
-- /Note:/ Consider using 'eventFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEventFilter :: Lens.Lens' Schedule (Lude.Maybe CampaignEventFilter)
sEventFilter = Lens.lens (eventFilter :: Schedule -> Lude.Maybe CampaignEventFilter) (\s a -> s {eventFilter = a} :: Schedule)
{-# DEPRECATED sEventFilter "Use generic-lens or generic-optics with 'eventFilter' instead." #-}

-- | Specifies whether the start and end times for the campaign schedule use each recipient's local time. To base the schedule on each recipient's local time, set this value to true.
--
-- /Note:/ Consider using 'isLocalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sIsLocalTime :: Lens.Lens' Schedule (Lude.Maybe Lude.Bool)
sIsLocalTime = Lens.lens (isLocalTime :: Schedule -> Lude.Maybe Lude.Bool) (\s a -> s {isLocalTime = a} :: Schedule)
{-# DEPRECATED sIsLocalTime "Use generic-lens or generic-optics with 'isLocalTime' instead." #-}

-- | The scheduled time, in ISO 8601 format, when the campaign ended or will end.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndTime :: Lens.Lens' Schedule (Lude.Maybe Lude.Text)
sEndTime = Lens.lens (endTime :: Schedule -> Lude.Maybe Lude.Text) (\s a -> s {endTime = a} :: Schedule)
{-# DEPRECATED sEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The starting UTC offset for the campaign schedule, if the value of the IsLocalTime property is true. Valid values are: UTC, UTC+01, UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05,
--
--                   UTC+05:30, UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+09, UTC+09:30,
--                   UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+13, UTC-02, UTC-03, UTC-04, UTC-05, UTC-06,
--                   UTC-07, UTC-08, UTC-09, UTC-10, and UTC-11.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTimezone :: Lens.Lens' Schedule (Lude.Maybe Lude.Text)
sTimezone = Lens.lens (timezone :: Schedule -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: Schedule)
{-# DEPRECATED sTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

instance Lude.FromJSON Schedule where
  parseJSON =
    Lude.withObject
      "Schedule"
      ( \x ->
          Schedule'
            Lude.<$> (x Lude..:? "Frequency")
            Lude.<*> (x Lude..: "StartTime")
            Lude.<*> (x Lude..:? "QuietTime")
            Lude.<*> (x Lude..:? "EventFilter")
            Lude.<*> (x Lude..:? "IsLocalTime")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "Timezone")
      )

instance Lude.ToJSON Schedule where
  toJSON Schedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Frequency" Lude..=) Lude.<$> frequency,
            Lude.Just ("StartTime" Lude..= startTime),
            ("QuietTime" Lude..=) Lude.<$> quietTime,
            ("EventFilter" Lude..=) Lude.<$> eventFilter,
            ("IsLocalTime" Lude..=) Lude.<$> isLocalTime,
            ("EndTime" Lude..=) Lude.<$> endTime,
            ("Timezone" Lude..=) Lude.<$> timezone
          ]
      )
