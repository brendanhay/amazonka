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
-- Module      : Network.AWS.Pinpoint.Types.Schedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Schedule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignEventFilter
import Network.AWS.Pinpoint.Types.Frequency
import Network.AWS.Pinpoint.Types.QuietTime

-- | Specifies the schedule settings for a campaign.
--
-- /See:/ 'newSchedule' smart constructor.
data Schedule = Schedule'
  { -- | The type of event that causes the campaign to be sent, if the value of
    -- the Frequency property is EVENT.
    eventFilter :: Core.Maybe CampaignEventFilter,
    -- | Specifies whether the start and end times for the campaign schedule use
    -- each recipient\'s local time. To base the schedule on each recipient\'s
    -- local time, set this value to true.
    isLocalTime :: Core.Maybe Core.Bool,
    -- | The scheduled time, in ISO 8601 format, when the campaign ended or will
    -- end.
    endTime :: Core.Maybe Core.Text,
    -- | Specifies how often the campaign is sent or whether the campaign is sent
    -- in response to a specific event.
    frequency :: Core.Maybe Frequency,
    -- | The default quiet time for the campaign. Quiet time is a specific time
    -- range when a campaign doesn\'t send messages to endpoints, if all the
    -- following conditions are met:
    --
    -- -   The EndpointDemographic.Timezone property of the endpoint is set to
    --     a valid value.
    --
    -- -   The current time in the endpoint\'s time zone is later than or equal
    --     to the time specified by the QuietTime.Start property for the
    --     campaign.
    --
    -- -   The current time in the endpoint\'s time zone is earlier than or
    --     equal to the time specified by the QuietTime.End property for the
    --     campaign.
    --
    -- If any of the preceding conditions isn\'t met, the endpoint will receive
    -- messages from the campaign, even if quiet time is enabled.
    quietTime :: Core.Maybe QuietTime,
    -- | The starting UTC offset for the campaign schedule, if the value of the
    -- IsLocalTime property is true. Valid values are: UTC, UTC+01, UTC+02,
    -- UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05, UTC+05:30, UTC+05:45,
    -- UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+09, UTC+09:30, UTC+10, UTC+10:30,
    -- UTC+11, UTC+12, UTC+13, UTC-02, UTC-03, UTC-04, UTC-05, UTC-06, UTC-07,
    -- UTC-08, UTC-09, UTC-10, and UTC-11.
    timezone :: Core.Maybe Core.Text,
    -- | The scheduled time when the campaign began or will begin. Valid values
    -- are: IMMEDIATE, to start the campaign immediately; or, a specific time
    -- in ISO 8601 format.
    startTime :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Schedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventFilter', 'schedule_eventFilter' - The type of event that causes the campaign to be sent, if the value of
-- the Frequency property is EVENT.
--
-- 'isLocalTime', 'schedule_isLocalTime' - Specifies whether the start and end times for the campaign schedule use
-- each recipient\'s local time. To base the schedule on each recipient\'s
-- local time, set this value to true.
--
-- 'endTime', 'schedule_endTime' - The scheduled time, in ISO 8601 format, when the campaign ended or will
-- end.
--
-- 'frequency', 'schedule_frequency' - Specifies how often the campaign is sent or whether the campaign is sent
-- in response to a specific event.
--
-- 'quietTime', 'schedule_quietTime' - The default quiet time for the campaign. Quiet time is a specific time
-- range when a campaign doesn\'t send messages to endpoints, if all the
-- following conditions are met:
--
-- -   The EndpointDemographic.Timezone property of the endpoint is set to
--     a valid value.
--
-- -   The current time in the endpoint\'s time zone is later than or equal
--     to the time specified by the QuietTime.Start property for the
--     campaign.
--
-- -   The current time in the endpoint\'s time zone is earlier than or
--     equal to the time specified by the QuietTime.End property for the
--     campaign.
--
-- If any of the preceding conditions isn\'t met, the endpoint will receive
-- messages from the campaign, even if quiet time is enabled.
--
-- 'timezone', 'schedule_timezone' - The starting UTC offset for the campaign schedule, if the value of the
-- IsLocalTime property is true. Valid values are: UTC, UTC+01, UTC+02,
-- UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05, UTC+05:30, UTC+05:45,
-- UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+09, UTC+09:30, UTC+10, UTC+10:30,
-- UTC+11, UTC+12, UTC+13, UTC-02, UTC-03, UTC-04, UTC-05, UTC-06, UTC-07,
-- UTC-08, UTC-09, UTC-10, and UTC-11.
--
-- 'startTime', 'schedule_startTime' - The scheduled time when the campaign began or will begin. Valid values
-- are: IMMEDIATE, to start the campaign immediately; or, a specific time
-- in ISO 8601 format.
newSchedule ::
  -- | 'startTime'
  Core.Text ->
  Schedule
newSchedule pStartTime_ =
  Schedule'
    { eventFilter = Core.Nothing,
      isLocalTime = Core.Nothing,
      endTime = Core.Nothing,
      frequency = Core.Nothing,
      quietTime = Core.Nothing,
      timezone = Core.Nothing,
      startTime = pStartTime_
    }

-- | The type of event that causes the campaign to be sent, if the value of
-- the Frequency property is EVENT.
schedule_eventFilter :: Lens.Lens' Schedule (Core.Maybe CampaignEventFilter)
schedule_eventFilter = Lens.lens (\Schedule' {eventFilter} -> eventFilter) (\s@Schedule' {} a -> s {eventFilter = a} :: Schedule)

-- | Specifies whether the start and end times for the campaign schedule use
-- each recipient\'s local time. To base the schedule on each recipient\'s
-- local time, set this value to true.
schedule_isLocalTime :: Lens.Lens' Schedule (Core.Maybe Core.Bool)
schedule_isLocalTime = Lens.lens (\Schedule' {isLocalTime} -> isLocalTime) (\s@Schedule' {} a -> s {isLocalTime = a} :: Schedule)

-- | The scheduled time, in ISO 8601 format, when the campaign ended or will
-- end.
schedule_endTime :: Lens.Lens' Schedule (Core.Maybe Core.Text)
schedule_endTime = Lens.lens (\Schedule' {endTime} -> endTime) (\s@Schedule' {} a -> s {endTime = a} :: Schedule)

-- | Specifies how often the campaign is sent or whether the campaign is sent
-- in response to a specific event.
schedule_frequency :: Lens.Lens' Schedule (Core.Maybe Frequency)
schedule_frequency = Lens.lens (\Schedule' {frequency} -> frequency) (\s@Schedule' {} a -> s {frequency = a} :: Schedule)

-- | The default quiet time for the campaign. Quiet time is a specific time
-- range when a campaign doesn\'t send messages to endpoints, if all the
-- following conditions are met:
--
-- -   The EndpointDemographic.Timezone property of the endpoint is set to
--     a valid value.
--
-- -   The current time in the endpoint\'s time zone is later than or equal
--     to the time specified by the QuietTime.Start property for the
--     campaign.
--
-- -   The current time in the endpoint\'s time zone is earlier than or
--     equal to the time specified by the QuietTime.End property for the
--     campaign.
--
-- If any of the preceding conditions isn\'t met, the endpoint will receive
-- messages from the campaign, even if quiet time is enabled.
schedule_quietTime :: Lens.Lens' Schedule (Core.Maybe QuietTime)
schedule_quietTime = Lens.lens (\Schedule' {quietTime} -> quietTime) (\s@Schedule' {} a -> s {quietTime = a} :: Schedule)

-- | The starting UTC offset for the campaign schedule, if the value of the
-- IsLocalTime property is true. Valid values are: UTC, UTC+01, UTC+02,
-- UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05, UTC+05:30, UTC+05:45,
-- UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+09, UTC+09:30, UTC+10, UTC+10:30,
-- UTC+11, UTC+12, UTC+13, UTC-02, UTC-03, UTC-04, UTC-05, UTC-06, UTC-07,
-- UTC-08, UTC-09, UTC-10, and UTC-11.
schedule_timezone :: Lens.Lens' Schedule (Core.Maybe Core.Text)
schedule_timezone = Lens.lens (\Schedule' {timezone} -> timezone) (\s@Schedule' {} a -> s {timezone = a} :: Schedule)

-- | The scheduled time when the campaign began or will begin. Valid values
-- are: IMMEDIATE, to start the campaign immediately; or, a specific time
-- in ISO 8601 format.
schedule_startTime :: Lens.Lens' Schedule Core.Text
schedule_startTime = Lens.lens (\Schedule' {startTime} -> startTime) (\s@Schedule' {} a -> s {startTime = a} :: Schedule)

instance Core.FromJSON Schedule where
  parseJSON =
    Core.withObject
      "Schedule"
      ( \x ->
          Schedule'
            Core.<$> (x Core..:? "EventFilter")
            Core.<*> (x Core..:? "IsLocalTime")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "Frequency")
            Core.<*> (x Core..:? "QuietTime")
            Core.<*> (x Core..:? "Timezone")
            Core.<*> (x Core..: "StartTime")
      )

instance Core.Hashable Schedule

instance Core.NFData Schedule

instance Core.ToJSON Schedule where
  toJSON Schedule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EventFilter" Core..=) Core.<$> eventFilter,
            ("IsLocalTime" Core..=) Core.<$> isLocalTime,
            ("EndTime" Core..=) Core.<$> endTime,
            ("Frequency" Core..=) Core.<$> frequency,
            ("QuietTime" Core..=) Core.<$> quietTime,
            ("Timezone" Core..=) Core.<$> timezone,
            Core.Just ("StartTime" Core..= startTime)
          ]
      )
