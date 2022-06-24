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
-- Module      : Amazonka.Pinpoint.Types.JourneyResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneyResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.Activity
import Amazonka.Pinpoint.Types.JourneyChannelSettings
import Amazonka.Pinpoint.Types.JourneyLimits
import Amazonka.Pinpoint.Types.JourneySchedule
import Amazonka.Pinpoint.Types.QuietTime
import Amazonka.Pinpoint.Types.StartCondition
import Amazonka.Pinpoint.Types.State
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status, configuration, and other settings
-- for a journey.
--
-- /See:/ 'newJourneyResponse' smart constructor.
data JourneyResponse = JourneyResponse'
  { -- | This object is not used or supported.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The schedule settings for the journey.
    schedule :: Prelude.Maybe JourneySchedule,
    -- | A map that contains a set of Activity objects, one object for each
    -- activity in the journey. For each Activity object, the key is the unique
    -- identifier (string) for an activity and the value is the settings for
    -- the activity.
    activities :: Prelude.Maybe (Prelude.HashMap Prelude.Text Activity),
    -- | The date, in ISO 8601 format, when the journey was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether a journey should be refreshed on segment update.
    refreshOnSegmentUpdate :: Prelude.Maybe Prelude.Bool,
    -- | The messaging and entry limits for the journey.
    limits :: Prelude.Maybe JourneyLimits,
    -- | The segment that defines which users are participants in the journey.
    startCondition :: Prelude.Maybe StartCondition,
    -- | The unique identifier for the first activity in the journey.
    startActivity :: Prelude.Maybe Prelude.Text,
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
    state :: Prelude.Maybe State,
    -- | The date, in ISO 8601 format, when the journey was created.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The frequency with which Amazon Pinpoint evaluates segment and event
    -- data for the journey, as a duration in ISO 8601 format.
    refreshFrequency :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the journey\'s scheduled start and end times use each
    -- participant\'s local time. If this value is true, the schedule uses each
    -- participant\'s local time.
    localTime :: Prelude.Maybe Prelude.Bool,
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
    quietTime :: Prelude.Maybe QuietTime,
    -- | Specifies whether endpoints in quiet hours should enter a wait till the
    -- end of their quiet hours.
    waitForQuietTime :: Prelude.Maybe Prelude.Bool,
    -- | The channel-specific configurations for the journey.
    journeyChannelSettings :: Prelude.Maybe JourneyChannelSettings,
    -- | The name of the journey.
    name :: Prelude.Text,
    -- | The unique identifier for the journey.
    id :: Prelude.Text,
    -- | The unique identifier for the application that the journey applies to.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JourneyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'journeyResponse_tags' - This object is not used or supported.
--
-- 'schedule', 'journeyResponse_schedule' - The schedule settings for the journey.
--
-- 'activities', 'journeyResponse_activities' - A map that contains a set of Activity objects, one object for each
-- activity in the journey. For each Activity object, the key is the unique
-- identifier (string) for an activity and the value is the settings for
-- the activity.
--
-- 'lastModifiedDate', 'journeyResponse_lastModifiedDate' - The date, in ISO 8601 format, when the journey was last modified.
--
-- 'refreshOnSegmentUpdate', 'journeyResponse_refreshOnSegmentUpdate' - Specifies whether a journey should be refreshed on segment update.
--
-- 'limits', 'journeyResponse_limits' - The messaging and entry limits for the journey.
--
-- 'startCondition', 'journeyResponse_startCondition' - The segment that defines which users are participants in the journey.
--
-- 'startActivity', 'journeyResponse_startActivity' - The unique identifier for the first activity in the journey.
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
-- 'creationDate', 'journeyResponse_creationDate' - The date, in ISO 8601 format, when the journey was created.
--
-- 'refreshFrequency', 'journeyResponse_refreshFrequency' - The frequency with which Amazon Pinpoint evaluates segment and event
-- data for the journey, as a duration in ISO 8601 format.
--
-- 'localTime', 'journeyResponse_localTime' - Specifies whether the journey\'s scheduled start and end times use each
-- participant\'s local time. If this value is true, the schedule uses each
-- participant\'s local time.
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
-- 'waitForQuietTime', 'journeyResponse_waitForQuietTime' - Specifies whether endpoints in quiet hours should enter a wait till the
-- end of their quiet hours.
--
-- 'journeyChannelSettings', 'journeyResponse_journeyChannelSettings' - The channel-specific configurations for the journey.
--
-- 'name', 'journeyResponse_name' - The name of the journey.
--
-- 'id', 'journeyResponse_id' - The unique identifier for the journey.
--
-- 'applicationId', 'journeyResponse_applicationId' - The unique identifier for the application that the journey applies to.
newJourneyResponse ::
  -- | 'name'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  JourneyResponse
newJourneyResponse pName_ pId_ pApplicationId_ =
  JourneyResponse'
    { tags = Prelude.Nothing,
      schedule = Prelude.Nothing,
      activities = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      refreshOnSegmentUpdate = Prelude.Nothing,
      limits = Prelude.Nothing,
      startCondition = Prelude.Nothing,
      startActivity = Prelude.Nothing,
      state = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      refreshFrequency = Prelude.Nothing,
      localTime = Prelude.Nothing,
      quietTime = Prelude.Nothing,
      waitForQuietTime = Prelude.Nothing,
      journeyChannelSettings = Prelude.Nothing,
      name = pName_,
      id = pId_,
      applicationId = pApplicationId_
    }

-- | This object is not used or supported.
journeyResponse_tags :: Lens.Lens' JourneyResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
journeyResponse_tags = Lens.lens (\JourneyResponse' {tags} -> tags) (\s@JourneyResponse' {} a -> s {tags = a} :: JourneyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The schedule settings for the journey.
journeyResponse_schedule :: Lens.Lens' JourneyResponse (Prelude.Maybe JourneySchedule)
journeyResponse_schedule = Lens.lens (\JourneyResponse' {schedule} -> schedule) (\s@JourneyResponse' {} a -> s {schedule = a} :: JourneyResponse)

-- | A map that contains a set of Activity objects, one object for each
-- activity in the journey. For each Activity object, the key is the unique
-- identifier (string) for an activity and the value is the settings for
-- the activity.
journeyResponse_activities :: Lens.Lens' JourneyResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Activity))
journeyResponse_activities = Lens.lens (\JourneyResponse' {activities} -> activities) (\s@JourneyResponse' {} a -> s {activities = a} :: JourneyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date, in ISO 8601 format, when the journey was last modified.
journeyResponse_lastModifiedDate :: Lens.Lens' JourneyResponse (Prelude.Maybe Prelude.Text)
journeyResponse_lastModifiedDate = Lens.lens (\JourneyResponse' {lastModifiedDate} -> lastModifiedDate) (\s@JourneyResponse' {} a -> s {lastModifiedDate = a} :: JourneyResponse)

-- | Specifies whether a journey should be refreshed on segment update.
journeyResponse_refreshOnSegmentUpdate :: Lens.Lens' JourneyResponse (Prelude.Maybe Prelude.Bool)
journeyResponse_refreshOnSegmentUpdate = Lens.lens (\JourneyResponse' {refreshOnSegmentUpdate} -> refreshOnSegmentUpdate) (\s@JourneyResponse' {} a -> s {refreshOnSegmentUpdate = a} :: JourneyResponse)

-- | The messaging and entry limits for the journey.
journeyResponse_limits :: Lens.Lens' JourneyResponse (Prelude.Maybe JourneyLimits)
journeyResponse_limits = Lens.lens (\JourneyResponse' {limits} -> limits) (\s@JourneyResponse' {} a -> s {limits = a} :: JourneyResponse)

-- | The segment that defines which users are participants in the journey.
journeyResponse_startCondition :: Lens.Lens' JourneyResponse (Prelude.Maybe StartCondition)
journeyResponse_startCondition = Lens.lens (\JourneyResponse' {startCondition} -> startCondition) (\s@JourneyResponse' {} a -> s {startCondition = a} :: JourneyResponse)

-- | The unique identifier for the first activity in the journey.
journeyResponse_startActivity :: Lens.Lens' JourneyResponse (Prelude.Maybe Prelude.Text)
journeyResponse_startActivity = Lens.lens (\JourneyResponse' {startActivity} -> startActivity) (\s@JourneyResponse' {} a -> s {startActivity = a} :: JourneyResponse)

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
journeyResponse_state :: Lens.Lens' JourneyResponse (Prelude.Maybe State)
journeyResponse_state = Lens.lens (\JourneyResponse' {state} -> state) (\s@JourneyResponse' {} a -> s {state = a} :: JourneyResponse)

-- | The date, in ISO 8601 format, when the journey was created.
journeyResponse_creationDate :: Lens.Lens' JourneyResponse (Prelude.Maybe Prelude.Text)
journeyResponse_creationDate = Lens.lens (\JourneyResponse' {creationDate} -> creationDate) (\s@JourneyResponse' {} a -> s {creationDate = a} :: JourneyResponse)

-- | The frequency with which Amazon Pinpoint evaluates segment and event
-- data for the journey, as a duration in ISO 8601 format.
journeyResponse_refreshFrequency :: Lens.Lens' JourneyResponse (Prelude.Maybe Prelude.Text)
journeyResponse_refreshFrequency = Lens.lens (\JourneyResponse' {refreshFrequency} -> refreshFrequency) (\s@JourneyResponse' {} a -> s {refreshFrequency = a} :: JourneyResponse)

-- | Specifies whether the journey\'s scheduled start and end times use each
-- participant\'s local time. If this value is true, the schedule uses each
-- participant\'s local time.
journeyResponse_localTime :: Lens.Lens' JourneyResponse (Prelude.Maybe Prelude.Bool)
journeyResponse_localTime = Lens.lens (\JourneyResponse' {localTime} -> localTime) (\s@JourneyResponse' {} a -> s {localTime = a} :: JourneyResponse)

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
journeyResponse_quietTime :: Lens.Lens' JourneyResponse (Prelude.Maybe QuietTime)
journeyResponse_quietTime = Lens.lens (\JourneyResponse' {quietTime} -> quietTime) (\s@JourneyResponse' {} a -> s {quietTime = a} :: JourneyResponse)

-- | Specifies whether endpoints in quiet hours should enter a wait till the
-- end of their quiet hours.
journeyResponse_waitForQuietTime :: Lens.Lens' JourneyResponse (Prelude.Maybe Prelude.Bool)
journeyResponse_waitForQuietTime = Lens.lens (\JourneyResponse' {waitForQuietTime} -> waitForQuietTime) (\s@JourneyResponse' {} a -> s {waitForQuietTime = a} :: JourneyResponse)

-- | The channel-specific configurations for the journey.
journeyResponse_journeyChannelSettings :: Lens.Lens' JourneyResponse (Prelude.Maybe JourneyChannelSettings)
journeyResponse_journeyChannelSettings = Lens.lens (\JourneyResponse' {journeyChannelSettings} -> journeyChannelSettings) (\s@JourneyResponse' {} a -> s {journeyChannelSettings = a} :: JourneyResponse)

-- | The name of the journey.
journeyResponse_name :: Lens.Lens' JourneyResponse Prelude.Text
journeyResponse_name = Lens.lens (\JourneyResponse' {name} -> name) (\s@JourneyResponse' {} a -> s {name = a} :: JourneyResponse)

-- | The unique identifier for the journey.
journeyResponse_id :: Lens.Lens' JourneyResponse Prelude.Text
journeyResponse_id = Lens.lens (\JourneyResponse' {id} -> id) (\s@JourneyResponse' {} a -> s {id = a} :: JourneyResponse)

-- | The unique identifier for the application that the journey applies to.
journeyResponse_applicationId :: Lens.Lens' JourneyResponse Prelude.Text
journeyResponse_applicationId = Lens.lens (\JourneyResponse' {applicationId} -> applicationId) (\s@JourneyResponse' {} a -> s {applicationId = a} :: JourneyResponse)

instance Core.FromJSON JourneyResponse where
  parseJSON =
    Core.withObject
      "JourneyResponse"
      ( \x ->
          JourneyResponse'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Schedule")
            Prelude.<*> (x Core..:? "Activities" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "RefreshOnSegmentUpdate")
            Prelude.<*> (x Core..:? "Limits")
            Prelude.<*> (x Core..:? "StartCondition")
            Prelude.<*> (x Core..:? "StartActivity")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "RefreshFrequency")
            Prelude.<*> (x Core..:? "LocalTime")
            Prelude.<*> (x Core..:? "QuietTime")
            Prelude.<*> (x Core..:? "WaitForQuietTime")
            Prelude.<*> (x Core..:? "JourneyChannelSettings")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Id")
            Prelude.<*> (x Core..: "ApplicationId")
      )

instance Prelude.Hashable JourneyResponse where
  hashWithSalt _salt JourneyResponse' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` activities
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` refreshOnSegmentUpdate
      `Prelude.hashWithSalt` limits
      `Prelude.hashWithSalt` startCondition
      `Prelude.hashWithSalt` startActivity
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` refreshFrequency
      `Prelude.hashWithSalt` localTime
      `Prelude.hashWithSalt` quietTime
      `Prelude.hashWithSalt` waitForQuietTime
      `Prelude.hashWithSalt` journeyChannelSettings
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData JourneyResponse where
  rnf JourneyResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf activities
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf refreshOnSegmentUpdate
      `Prelude.seq` Prelude.rnf limits
      `Prelude.seq` Prelude.rnf startCondition
      `Prelude.seq` Prelude.rnf startActivity
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf refreshFrequency
      `Prelude.seq` Prelude.rnf localTime
      `Prelude.seq` Prelude.rnf quietTime
      `Prelude.seq` Prelude.rnf waitForQuietTime
      `Prelude.seq` Prelude.rnf journeyChannelSettings
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf applicationId
