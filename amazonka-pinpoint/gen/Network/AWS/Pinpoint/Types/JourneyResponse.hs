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
-- Module      : Network.AWS.Pinpoint.Types.JourneyResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Activity
import Network.AWS.Pinpoint.Types.JourneyLimits
import Network.AWS.Pinpoint.Types.JourneySchedule
import Network.AWS.Pinpoint.Types.QuietTime
import Network.AWS.Pinpoint.Types.StartCondition
import Network.AWS.Pinpoint.Types.State

-- | Provides information about the status, configuration, and other settings
-- for a journey.
--
-- /See:/ 'newJourneyResponse' smart constructor.
data JourneyResponse = JourneyResponse'
  { -- | The date, in ISO 8601 format, when the journey was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | A map that contains a set of Activity objects, one object for each
    -- activity in the journey. For each Activity object, the key is the unique
    -- identifier (string) for an activity and the value is the settings for
    -- the activity.
    activities :: Core.Maybe (Core.HashMap Core.Text Activity),
    -- | The date, in ISO 8601 format, when the journey was created.
    creationDate :: Core.Maybe Core.Text,
    -- | The current status of the journey. Possible values are:
    --
    -- -   DRAFT - The journey is being developed and hasn\'t been published
    --     yet.
    --
    -- -   ACTIVE - The journey has been developed and published. Depending on
    --     the journey\'s schedule, the journey may currently be running or
    --     scheduled to start running at a later time. If a journey\'s status
    --     is ACTIVE, you can\'t add, change, or remove activities from it.
    --
    -- -   COMPLETED - The journey has been published and has finished running.
    --     All participants have entered the journey and no participants are
    --     waiting to complete the journey or any activities in the journey.
    --
    -- -   CANCELLED - The journey has been stopped. If a journey\'s status is
    --     CANCELLED, you can\'t add, change, or remove activities or segment
    --     settings from the journey.
    --
    -- -   CLOSED - The journey has been published and has started running. It
    --     may have also passed its scheduled end time, or passed its scheduled
    --     start time and a refresh frequency hasn\'t been specified for it. If
    --     a journey\'s status is CLOSED, you can\'t add participants to it,
    --     and no existing participants can enter the journey for the first
    --     time. However, any existing participants who are currently waiting
    --     to start an activity may continue the journey.
    state :: Core.Maybe State,
    -- | This object is not used or supported.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The quiet time settings for the journey. Quiet time is a specific time
    -- range when a journey doesn\'t send messages to participants, if all the
    -- following conditions are met:
    --
    -- -   The EndpointDemographic.Timezone property of the endpoint for the
    --     participant is set to a valid value.
    --
    -- -   The current time in the participant\'s time zone is later than or
    --     equal to the time specified by the QuietTime.Start property for the
    --     journey.
    --
    -- -   The current time in the participant\'s time zone is earlier than or
    --     equal to the time specified by the QuietTime.End property for the
    --     journey.
    --
    -- If any of the preceding conditions isn\'t met, the participant will
    -- receive messages from the journey, even if quiet time is enabled.
    quietTime :: Core.Maybe QuietTime,
    -- | The frequency with which Amazon Pinpoint evaluates segment and event
    -- data for the journey, as a duration in ISO 8601 format.
    refreshFrequency :: Core.Maybe Core.Text,
    -- | The messaging and entry limits for the journey.
    limits :: Core.Maybe JourneyLimits,
    -- | The segment that defines which users are participants in the journey.
    startCondition :: Core.Maybe StartCondition,
    -- | Specifies whether the journey\'s scheduled start and end times use each
    -- participant\'s local time. If this value is true, the schedule uses each
    -- participant\'s local time.
    localTime :: Core.Maybe Core.Bool,
    -- | The unique identifier for the first activity in the journey.
    startActivity :: Core.Maybe Core.Text,
    -- | The schedule settings for the journey.
    schedule :: Core.Maybe JourneySchedule,
    -- | The name of the journey.
    name :: Core.Text,
    -- | The unique identifier for the journey.
    id :: Core.Text,
    -- | The unique identifier for the application that the journey applies to.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JourneyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'journeyResponse_lastModifiedDate' - The date, in ISO 8601 format, when the journey was last modified.
--
-- 'activities', 'journeyResponse_activities' - A map that contains a set of Activity objects, one object for each
-- activity in the journey. For each Activity object, the key is the unique
-- identifier (string) for an activity and the value is the settings for
-- the activity.
--
-- 'creationDate', 'journeyResponse_creationDate' - The date, in ISO 8601 format, when the journey was created.
--
-- 'state', 'journeyResponse_state' - The current status of the journey. Possible values are:
--
-- -   DRAFT - The journey is being developed and hasn\'t been published
--     yet.
--
-- -   ACTIVE - The journey has been developed and published. Depending on
--     the journey\'s schedule, the journey may currently be running or
--     scheduled to start running at a later time. If a journey\'s status
--     is ACTIVE, you can\'t add, change, or remove activities from it.
--
-- -   COMPLETED - The journey has been published and has finished running.
--     All participants have entered the journey and no participants are
--     waiting to complete the journey or any activities in the journey.
--
-- -   CANCELLED - The journey has been stopped. If a journey\'s status is
--     CANCELLED, you can\'t add, change, or remove activities or segment
--     settings from the journey.
--
-- -   CLOSED - The journey has been published and has started running. It
--     may have also passed its scheduled end time, or passed its scheduled
--     start time and a refresh frequency hasn\'t been specified for it. If
--     a journey\'s status is CLOSED, you can\'t add participants to it,
--     and no existing participants can enter the journey for the first
--     time. However, any existing participants who are currently waiting
--     to start an activity may continue the journey.
--
-- 'tags', 'journeyResponse_tags' - This object is not used or supported.
--
-- 'quietTime', 'journeyResponse_quietTime' - The quiet time settings for the journey. Quiet time is a specific time
-- range when a journey doesn\'t send messages to participants, if all the
-- following conditions are met:
--
-- -   The EndpointDemographic.Timezone property of the endpoint for the
--     participant is set to a valid value.
--
-- -   The current time in the participant\'s time zone is later than or
--     equal to the time specified by the QuietTime.Start property for the
--     journey.
--
-- -   The current time in the participant\'s time zone is earlier than or
--     equal to the time specified by the QuietTime.End property for the
--     journey.
--
-- If any of the preceding conditions isn\'t met, the participant will
-- receive messages from the journey, even if quiet time is enabled.
--
-- 'refreshFrequency', 'journeyResponse_refreshFrequency' - The frequency with which Amazon Pinpoint evaluates segment and event
-- data for the journey, as a duration in ISO 8601 format.
--
-- 'limits', 'journeyResponse_limits' - The messaging and entry limits for the journey.
--
-- 'startCondition', 'journeyResponse_startCondition' - The segment that defines which users are participants in the journey.
--
-- 'localTime', 'journeyResponse_localTime' - Specifies whether the journey\'s scheduled start and end times use each
-- participant\'s local time. If this value is true, the schedule uses each
-- participant\'s local time.
--
-- 'startActivity', 'journeyResponse_startActivity' - The unique identifier for the first activity in the journey.
--
-- 'schedule', 'journeyResponse_schedule' - The schedule settings for the journey.
--
-- 'name', 'journeyResponse_name' - The name of the journey.
--
-- 'id', 'journeyResponse_id' - The unique identifier for the journey.
--
-- 'applicationId', 'journeyResponse_applicationId' - The unique identifier for the application that the journey applies to.
newJourneyResponse ::
  -- | 'name'
  Core.Text ->
  -- | 'id'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  JourneyResponse
newJourneyResponse pName_ pId_ pApplicationId_ =
  JourneyResponse'
    { lastModifiedDate = Core.Nothing,
      activities = Core.Nothing,
      creationDate = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      quietTime = Core.Nothing,
      refreshFrequency = Core.Nothing,
      limits = Core.Nothing,
      startCondition = Core.Nothing,
      localTime = Core.Nothing,
      startActivity = Core.Nothing,
      schedule = Core.Nothing,
      name = pName_,
      id = pId_,
      applicationId = pApplicationId_
    }

-- | The date, in ISO 8601 format, when the journey was last modified.
journeyResponse_lastModifiedDate :: Lens.Lens' JourneyResponse (Core.Maybe Core.Text)
journeyResponse_lastModifiedDate = Lens.lens (\JourneyResponse' {lastModifiedDate} -> lastModifiedDate) (\s@JourneyResponse' {} a -> s {lastModifiedDate = a} :: JourneyResponse)

-- | A map that contains a set of Activity objects, one object for each
-- activity in the journey. For each Activity object, the key is the unique
-- identifier (string) for an activity and the value is the settings for
-- the activity.
journeyResponse_activities :: Lens.Lens' JourneyResponse (Core.Maybe (Core.HashMap Core.Text Activity))
journeyResponse_activities = Lens.lens (\JourneyResponse' {activities} -> activities) (\s@JourneyResponse' {} a -> s {activities = a} :: JourneyResponse) Core.. Lens.mapping Lens._Coerce

-- | The date, in ISO 8601 format, when the journey was created.
journeyResponse_creationDate :: Lens.Lens' JourneyResponse (Core.Maybe Core.Text)
journeyResponse_creationDate = Lens.lens (\JourneyResponse' {creationDate} -> creationDate) (\s@JourneyResponse' {} a -> s {creationDate = a} :: JourneyResponse)

-- | The current status of the journey. Possible values are:
--
-- -   DRAFT - The journey is being developed and hasn\'t been published
--     yet.
--
-- -   ACTIVE - The journey has been developed and published. Depending on
--     the journey\'s schedule, the journey may currently be running or
--     scheduled to start running at a later time. If a journey\'s status
--     is ACTIVE, you can\'t add, change, or remove activities from it.
--
-- -   COMPLETED - The journey has been published and has finished running.
--     All participants have entered the journey and no participants are
--     waiting to complete the journey or any activities in the journey.
--
-- -   CANCELLED - The journey has been stopped. If a journey\'s status is
--     CANCELLED, you can\'t add, change, or remove activities or segment
--     settings from the journey.
--
-- -   CLOSED - The journey has been published and has started running. It
--     may have also passed its scheduled end time, or passed its scheduled
--     start time and a refresh frequency hasn\'t been specified for it. If
--     a journey\'s status is CLOSED, you can\'t add participants to it,
--     and no existing participants can enter the journey for the first
--     time. However, any existing participants who are currently waiting
--     to start an activity may continue the journey.
journeyResponse_state :: Lens.Lens' JourneyResponse (Core.Maybe State)
journeyResponse_state = Lens.lens (\JourneyResponse' {state} -> state) (\s@JourneyResponse' {} a -> s {state = a} :: JourneyResponse)

-- | This object is not used or supported.
journeyResponse_tags :: Lens.Lens' JourneyResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
journeyResponse_tags = Lens.lens (\JourneyResponse' {tags} -> tags) (\s@JourneyResponse' {} a -> s {tags = a} :: JourneyResponse) Core.. Lens.mapping Lens._Coerce

-- | The quiet time settings for the journey. Quiet time is a specific time
-- range when a journey doesn\'t send messages to participants, if all the
-- following conditions are met:
--
-- -   The EndpointDemographic.Timezone property of the endpoint for the
--     participant is set to a valid value.
--
-- -   The current time in the participant\'s time zone is later than or
--     equal to the time specified by the QuietTime.Start property for the
--     journey.
--
-- -   The current time in the participant\'s time zone is earlier than or
--     equal to the time specified by the QuietTime.End property for the
--     journey.
--
-- If any of the preceding conditions isn\'t met, the participant will
-- receive messages from the journey, even if quiet time is enabled.
journeyResponse_quietTime :: Lens.Lens' JourneyResponse (Core.Maybe QuietTime)
journeyResponse_quietTime = Lens.lens (\JourneyResponse' {quietTime} -> quietTime) (\s@JourneyResponse' {} a -> s {quietTime = a} :: JourneyResponse)

-- | The frequency with which Amazon Pinpoint evaluates segment and event
-- data for the journey, as a duration in ISO 8601 format.
journeyResponse_refreshFrequency :: Lens.Lens' JourneyResponse (Core.Maybe Core.Text)
journeyResponse_refreshFrequency = Lens.lens (\JourneyResponse' {refreshFrequency} -> refreshFrequency) (\s@JourneyResponse' {} a -> s {refreshFrequency = a} :: JourneyResponse)

-- | The messaging and entry limits for the journey.
journeyResponse_limits :: Lens.Lens' JourneyResponse (Core.Maybe JourneyLimits)
journeyResponse_limits = Lens.lens (\JourneyResponse' {limits} -> limits) (\s@JourneyResponse' {} a -> s {limits = a} :: JourneyResponse)

-- | The segment that defines which users are participants in the journey.
journeyResponse_startCondition :: Lens.Lens' JourneyResponse (Core.Maybe StartCondition)
journeyResponse_startCondition = Lens.lens (\JourneyResponse' {startCondition} -> startCondition) (\s@JourneyResponse' {} a -> s {startCondition = a} :: JourneyResponse)

-- | Specifies whether the journey\'s scheduled start and end times use each
-- participant\'s local time. If this value is true, the schedule uses each
-- participant\'s local time.
journeyResponse_localTime :: Lens.Lens' JourneyResponse (Core.Maybe Core.Bool)
journeyResponse_localTime = Lens.lens (\JourneyResponse' {localTime} -> localTime) (\s@JourneyResponse' {} a -> s {localTime = a} :: JourneyResponse)

-- | The unique identifier for the first activity in the journey.
journeyResponse_startActivity :: Lens.Lens' JourneyResponse (Core.Maybe Core.Text)
journeyResponse_startActivity = Lens.lens (\JourneyResponse' {startActivity} -> startActivity) (\s@JourneyResponse' {} a -> s {startActivity = a} :: JourneyResponse)

-- | The schedule settings for the journey.
journeyResponse_schedule :: Lens.Lens' JourneyResponse (Core.Maybe JourneySchedule)
journeyResponse_schedule = Lens.lens (\JourneyResponse' {schedule} -> schedule) (\s@JourneyResponse' {} a -> s {schedule = a} :: JourneyResponse)

-- | The name of the journey.
journeyResponse_name :: Lens.Lens' JourneyResponse Core.Text
journeyResponse_name = Lens.lens (\JourneyResponse' {name} -> name) (\s@JourneyResponse' {} a -> s {name = a} :: JourneyResponse)

-- | The unique identifier for the journey.
journeyResponse_id :: Lens.Lens' JourneyResponse Core.Text
journeyResponse_id = Lens.lens (\JourneyResponse' {id} -> id) (\s@JourneyResponse' {} a -> s {id = a} :: JourneyResponse)

-- | The unique identifier for the application that the journey applies to.
journeyResponse_applicationId :: Lens.Lens' JourneyResponse Core.Text
journeyResponse_applicationId = Lens.lens (\JourneyResponse' {applicationId} -> applicationId) (\s@JourneyResponse' {} a -> s {applicationId = a} :: JourneyResponse)

instance Core.FromJSON JourneyResponse where
  parseJSON =
    Core.withObject
      "JourneyResponse"
      ( \x ->
          JourneyResponse'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "Activities" Core..!= Core.mempty)
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "QuietTime")
            Core.<*> (x Core..:? "RefreshFrequency")
            Core.<*> (x Core..:? "Limits")
            Core.<*> (x Core..:? "StartCondition")
            Core.<*> (x Core..:? "LocalTime")
            Core.<*> (x Core..:? "StartActivity")
            Core.<*> (x Core..:? "Schedule")
            Core.<*> (x Core..: "Name")
            Core.<*> (x Core..: "Id")
            Core.<*> (x Core..: "ApplicationId")
      )

instance Core.Hashable JourneyResponse

instance Core.NFData JourneyResponse
