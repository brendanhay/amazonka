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
-- Module      : Amazonka.Pinpoint.Types.WriteJourneyRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.WriteJourneyRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.Activity
import Amazonka.Pinpoint.Types.ClosedDays
import Amazonka.Pinpoint.Types.JourneyChannelSettings
import Amazonka.Pinpoint.Types.JourneyLimits
import Amazonka.Pinpoint.Types.JourneySchedule
import Amazonka.Pinpoint.Types.OpenHours
import Amazonka.Pinpoint.Types.QuietTime
import Amazonka.Pinpoint.Types.StartCondition
import Amazonka.Pinpoint.Types.State
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration and other settings for a journey.
--
-- /See:/ 'newWriteJourneyRequest' smart constructor.
data WriteJourneyRequest = WriteJourneyRequest'
  { -- | A map that contains a set of Activity objects, one object for each
    -- activity in the journey. For each Activity object, the key is the unique
    -- identifier (string) for an activity and the value is the settings for
    -- the activity. An activity identifier can contain a maximum of 100
    -- characters. The characters must be alphanumeric characters.
    activities :: Prelude.Maybe (Prelude.HashMap Prelude.Text Activity),
    -- | The time when journey will stop sending messages. QuietTime should be
    -- configured first and SendingSchedule should be set to true.
    closedDays :: Prelude.Maybe ClosedDays,
    -- | The date, in ISO 8601 format, when the journey was created.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The channel-specific configurations for the journey.
    journeyChannelSettings :: Prelude.Maybe JourneyChannelSettings,
    -- | The date, in ISO 8601 format, when the journey was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The messaging and entry limits for the journey.
    limits :: Prelude.Maybe JourneyLimits,
    -- | Specifies whether the journey\'s scheduled start and end times use each
    -- participant\'s local time. To base the schedule on each participant\'s
    -- local time, set this value to true.
    localTime :: Prelude.Maybe Prelude.Bool,
    -- | The time when journey allow to send messages. QuietTime should be
    -- configured first and SendingSchedule should be set to true.
    openHours :: Prelude.Maybe OpenHours,
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
    -- | The frequency with which Amazon Pinpoint evaluates segment and event
    -- data for the journey, as a duration in ISO 8601 format.
    refreshFrequency :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether a journey should be refreshed on segment update.
    refreshOnSegmentUpdate :: Prelude.Maybe Prelude.Bool,
    -- | The schedule settings for the journey.
    schedule :: Prelude.Maybe JourneySchedule,
    -- | Indicates if journey have Advance Quiet Time (OpenHours and ClosedDays).
    -- This flag should be set to true in order to allow (OpenHours and
    -- ClosedDays)
    sendingSchedule :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for the first activity in the journey. The
    -- identifier for this activity can contain a maximum of 128 characters.
    -- The characters must be alphanumeric characters.
    startActivity :: Prelude.Maybe Prelude.Text,
    -- | The segment that defines which users are participants in the journey.
    startCondition :: Prelude.Maybe StartCondition,
    -- | The status of the journey. Valid values are:
    --
    -- -   DRAFT - Saves the journey and doesn\'t publish it.
    --
    -- -   ACTIVE - Saves and publishes the journey. Depending on the
    --     journey\'s schedule, the journey starts running immediately or at
    --     the scheduled start time. If a journey\'s status is ACTIVE, you
    --     can\'t add, change, or remove activities from it.
    --
    -- PAUSED, CANCELLED, COMPLETED, and CLOSED states are not supported in
    -- requests to create or update a journey. To cancel, pause, or resume a
    -- journey, use the Journey State resource.
    state :: Prelude.Maybe State,
    -- | Specifies whether endpoints in quiet hours should enter a wait till the
    -- end of their quiet hours.
    waitForQuietTime :: Prelude.Maybe Prelude.Bool,
    -- | The name of the journey. A journey name can contain a maximum of 150
    -- characters. The characters can be alphanumeric characters or symbols,
    -- such as underscores (_) or hyphens (-). A journey name can\'t contain
    -- any spaces.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WriteJourneyRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activities', 'writeJourneyRequest_activities' - A map that contains a set of Activity objects, one object for each
-- activity in the journey. For each Activity object, the key is the unique
-- identifier (string) for an activity and the value is the settings for
-- the activity. An activity identifier can contain a maximum of 100
-- characters. The characters must be alphanumeric characters.
--
-- 'closedDays', 'writeJourneyRequest_closedDays' - The time when journey will stop sending messages. QuietTime should be
-- configured first and SendingSchedule should be set to true.
--
-- 'creationDate', 'writeJourneyRequest_creationDate' - The date, in ISO 8601 format, when the journey was created.
--
-- 'journeyChannelSettings', 'writeJourneyRequest_journeyChannelSettings' - The channel-specific configurations for the journey.
--
-- 'lastModifiedDate', 'writeJourneyRequest_lastModifiedDate' - The date, in ISO 8601 format, when the journey was last modified.
--
-- 'limits', 'writeJourneyRequest_limits' - The messaging and entry limits for the journey.
--
-- 'localTime', 'writeJourneyRequest_localTime' - Specifies whether the journey\'s scheduled start and end times use each
-- participant\'s local time. To base the schedule on each participant\'s
-- local time, set this value to true.
--
-- 'openHours', 'writeJourneyRequest_openHours' - The time when journey allow to send messages. QuietTime should be
-- configured first and SendingSchedule should be set to true.
--
-- 'quietTime', 'writeJourneyRequest_quietTime' - The quiet time settings for the journey. Quiet time is a specific time
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
-- 'refreshFrequency', 'writeJourneyRequest_refreshFrequency' - The frequency with which Amazon Pinpoint evaluates segment and event
-- data for the journey, as a duration in ISO 8601 format.
--
-- 'refreshOnSegmentUpdate', 'writeJourneyRequest_refreshOnSegmentUpdate' - Specifies whether a journey should be refreshed on segment update.
--
-- 'schedule', 'writeJourneyRequest_schedule' - The schedule settings for the journey.
--
-- 'sendingSchedule', 'writeJourneyRequest_sendingSchedule' - Indicates if journey have Advance Quiet Time (OpenHours and ClosedDays).
-- This flag should be set to true in order to allow (OpenHours and
-- ClosedDays)
--
-- 'startActivity', 'writeJourneyRequest_startActivity' - The unique identifier for the first activity in the journey. The
-- identifier for this activity can contain a maximum of 128 characters.
-- The characters must be alphanumeric characters.
--
-- 'startCondition', 'writeJourneyRequest_startCondition' - The segment that defines which users are participants in the journey.
--
-- 'state', 'writeJourneyRequest_state' - The status of the journey. Valid values are:
--
-- -   DRAFT - Saves the journey and doesn\'t publish it.
--
-- -   ACTIVE - Saves and publishes the journey. Depending on the
--     journey\'s schedule, the journey starts running immediately or at
--     the scheduled start time. If a journey\'s status is ACTIVE, you
--     can\'t add, change, or remove activities from it.
--
-- PAUSED, CANCELLED, COMPLETED, and CLOSED states are not supported in
-- requests to create or update a journey. To cancel, pause, or resume a
-- journey, use the Journey State resource.
--
-- 'waitForQuietTime', 'writeJourneyRequest_waitForQuietTime' - Specifies whether endpoints in quiet hours should enter a wait till the
-- end of their quiet hours.
--
-- 'name', 'writeJourneyRequest_name' - The name of the journey. A journey name can contain a maximum of 150
-- characters. The characters can be alphanumeric characters or symbols,
-- such as underscores (_) or hyphens (-). A journey name can\'t contain
-- any spaces.
newWriteJourneyRequest ::
  -- | 'name'
  Prelude.Text ->
  WriteJourneyRequest
newWriteJourneyRequest pName_ =
  WriteJourneyRequest'
    { activities = Prelude.Nothing,
      closedDays = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      journeyChannelSettings = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      limits = Prelude.Nothing,
      localTime = Prelude.Nothing,
      openHours = Prelude.Nothing,
      quietTime = Prelude.Nothing,
      refreshFrequency = Prelude.Nothing,
      refreshOnSegmentUpdate = Prelude.Nothing,
      schedule = Prelude.Nothing,
      sendingSchedule = Prelude.Nothing,
      startActivity = Prelude.Nothing,
      startCondition = Prelude.Nothing,
      state = Prelude.Nothing,
      waitForQuietTime = Prelude.Nothing,
      name = pName_
    }

-- | A map that contains a set of Activity objects, one object for each
-- activity in the journey. For each Activity object, the key is the unique
-- identifier (string) for an activity and the value is the settings for
-- the activity. An activity identifier can contain a maximum of 100
-- characters. The characters must be alphanumeric characters.
writeJourneyRequest_activities :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Activity))
writeJourneyRequest_activities = Lens.lens (\WriteJourneyRequest' {activities} -> activities) (\s@WriteJourneyRequest' {} a -> s {activities = a} :: WriteJourneyRequest) Prelude.. Lens.mapping Lens.coerced

-- | The time when journey will stop sending messages. QuietTime should be
-- configured first and SendingSchedule should be set to true.
writeJourneyRequest_closedDays :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe ClosedDays)
writeJourneyRequest_closedDays = Lens.lens (\WriteJourneyRequest' {closedDays} -> closedDays) (\s@WriteJourneyRequest' {} a -> s {closedDays = a} :: WriteJourneyRequest)

-- | The date, in ISO 8601 format, when the journey was created.
writeJourneyRequest_creationDate :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe Prelude.Text)
writeJourneyRequest_creationDate = Lens.lens (\WriteJourneyRequest' {creationDate} -> creationDate) (\s@WriteJourneyRequest' {} a -> s {creationDate = a} :: WriteJourneyRequest)

-- | The channel-specific configurations for the journey.
writeJourneyRequest_journeyChannelSettings :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe JourneyChannelSettings)
writeJourneyRequest_journeyChannelSettings = Lens.lens (\WriteJourneyRequest' {journeyChannelSettings} -> journeyChannelSettings) (\s@WriteJourneyRequest' {} a -> s {journeyChannelSettings = a} :: WriteJourneyRequest)

-- | The date, in ISO 8601 format, when the journey was last modified.
writeJourneyRequest_lastModifiedDate :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe Prelude.Text)
writeJourneyRequest_lastModifiedDate = Lens.lens (\WriteJourneyRequest' {lastModifiedDate} -> lastModifiedDate) (\s@WriteJourneyRequest' {} a -> s {lastModifiedDate = a} :: WriteJourneyRequest)

-- | The messaging and entry limits for the journey.
writeJourneyRequest_limits :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe JourneyLimits)
writeJourneyRequest_limits = Lens.lens (\WriteJourneyRequest' {limits} -> limits) (\s@WriteJourneyRequest' {} a -> s {limits = a} :: WriteJourneyRequest)

-- | Specifies whether the journey\'s scheduled start and end times use each
-- participant\'s local time. To base the schedule on each participant\'s
-- local time, set this value to true.
writeJourneyRequest_localTime :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe Prelude.Bool)
writeJourneyRequest_localTime = Lens.lens (\WriteJourneyRequest' {localTime} -> localTime) (\s@WriteJourneyRequest' {} a -> s {localTime = a} :: WriteJourneyRequest)

-- | The time when journey allow to send messages. QuietTime should be
-- configured first and SendingSchedule should be set to true.
writeJourneyRequest_openHours :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe OpenHours)
writeJourneyRequest_openHours = Lens.lens (\WriteJourneyRequest' {openHours} -> openHours) (\s@WriteJourneyRequest' {} a -> s {openHours = a} :: WriteJourneyRequest)

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
writeJourneyRequest_quietTime :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe QuietTime)
writeJourneyRequest_quietTime = Lens.lens (\WriteJourneyRequest' {quietTime} -> quietTime) (\s@WriteJourneyRequest' {} a -> s {quietTime = a} :: WriteJourneyRequest)

-- | The frequency with which Amazon Pinpoint evaluates segment and event
-- data for the journey, as a duration in ISO 8601 format.
writeJourneyRequest_refreshFrequency :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe Prelude.Text)
writeJourneyRequest_refreshFrequency = Lens.lens (\WriteJourneyRequest' {refreshFrequency} -> refreshFrequency) (\s@WriteJourneyRequest' {} a -> s {refreshFrequency = a} :: WriteJourneyRequest)

-- | Specifies whether a journey should be refreshed on segment update.
writeJourneyRequest_refreshOnSegmentUpdate :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe Prelude.Bool)
writeJourneyRequest_refreshOnSegmentUpdate = Lens.lens (\WriteJourneyRequest' {refreshOnSegmentUpdate} -> refreshOnSegmentUpdate) (\s@WriteJourneyRequest' {} a -> s {refreshOnSegmentUpdate = a} :: WriteJourneyRequest)

-- | The schedule settings for the journey.
writeJourneyRequest_schedule :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe JourneySchedule)
writeJourneyRequest_schedule = Lens.lens (\WriteJourneyRequest' {schedule} -> schedule) (\s@WriteJourneyRequest' {} a -> s {schedule = a} :: WriteJourneyRequest)

-- | Indicates if journey have Advance Quiet Time (OpenHours and ClosedDays).
-- This flag should be set to true in order to allow (OpenHours and
-- ClosedDays)
writeJourneyRequest_sendingSchedule :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe Prelude.Bool)
writeJourneyRequest_sendingSchedule = Lens.lens (\WriteJourneyRequest' {sendingSchedule} -> sendingSchedule) (\s@WriteJourneyRequest' {} a -> s {sendingSchedule = a} :: WriteJourneyRequest)

-- | The unique identifier for the first activity in the journey. The
-- identifier for this activity can contain a maximum of 128 characters.
-- The characters must be alphanumeric characters.
writeJourneyRequest_startActivity :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe Prelude.Text)
writeJourneyRequest_startActivity = Lens.lens (\WriteJourneyRequest' {startActivity} -> startActivity) (\s@WriteJourneyRequest' {} a -> s {startActivity = a} :: WriteJourneyRequest)

-- | The segment that defines which users are participants in the journey.
writeJourneyRequest_startCondition :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe StartCondition)
writeJourneyRequest_startCondition = Lens.lens (\WriteJourneyRequest' {startCondition} -> startCondition) (\s@WriteJourneyRequest' {} a -> s {startCondition = a} :: WriteJourneyRequest)

-- | The status of the journey. Valid values are:
--
-- -   DRAFT - Saves the journey and doesn\'t publish it.
--
-- -   ACTIVE - Saves and publishes the journey. Depending on the
--     journey\'s schedule, the journey starts running immediately or at
--     the scheduled start time. If a journey\'s status is ACTIVE, you
--     can\'t add, change, or remove activities from it.
--
-- PAUSED, CANCELLED, COMPLETED, and CLOSED states are not supported in
-- requests to create or update a journey. To cancel, pause, or resume a
-- journey, use the Journey State resource.
writeJourneyRequest_state :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe State)
writeJourneyRequest_state = Lens.lens (\WriteJourneyRequest' {state} -> state) (\s@WriteJourneyRequest' {} a -> s {state = a} :: WriteJourneyRequest)

-- | Specifies whether endpoints in quiet hours should enter a wait till the
-- end of their quiet hours.
writeJourneyRequest_waitForQuietTime :: Lens.Lens' WriteJourneyRequest (Prelude.Maybe Prelude.Bool)
writeJourneyRequest_waitForQuietTime = Lens.lens (\WriteJourneyRequest' {waitForQuietTime} -> waitForQuietTime) (\s@WriteJourneyRequest' {} a -> s {waitForQuietTime = a} :: WriteJourneyRequest)

-- | The name of the journey. A journey name can contain a maximum of 150
-- characters. The characters can be alphanumeric characters or symbols,
-- such as underscores (_) or hyphens (-). A journey name can\'t contain
-- any spaces.
writeJourneyRequest_name :: Lens.Lens' WriteJourneyRequest Prelude.Text
writeJourneyRequest_name = Lens.lens (\WriteJourneyRequest' {name} -> name) (\s@WriteJourneyRequest' {} a -> s {name = a} :: WriteJourneyRequest)

instance Prelude.Hashable WriteJourneyRequest where
  hashWithSalt _salt WriteJourneyRequest' {..} =
    _salt
      `Prelude.hashWithSalt` activities
      `Prelude.hashWithSalt` closedDays
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` journeyChannelSettings
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` limits
      `Prelude.hashWithSalt` localTime
      `Prelude.hashWithSalt` openHours
      `Prelude.hashWithSalt` quietTime
      `Prelude.hashWithSalt` refreshFrequency
      `Prelude.hashWithSalt` refreshOnSegmentUpdate
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` sendingSchedule
      `Prelude.hashWithSalt` startActivity
      `Prelude.hashWithSalt` startCondition
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` waitForQuietTime
      `Prelude.hashWithSalt` name

instance Prelude.NFData WriteJourneyRequest where
  rnf WriteJourneyRequest' {..} =
    Prelude.rnf activities `Prelude.seq`
      Prelude.rnf closedDays `Prelude.seq`
        Prelude.rnf creationDate `Prelude.seq`
          Prelude.rnf journeyChannelSettings `Prelude.seq`
            Prelude.rnf lastModifiedDate `Prelude.seq`
              Prelude.rnf limits `Prelude.seq`
                Prelude.rnf localTime `Prelude.seq`
                  Prelude.rnf openHours `Prelude.seq`
                    Prelude.rnf quietTime `Prelude.seq`
                      Prelude.rnf refreshFrequency `Prelude.seq`
                        Prelude.rnf refreshOnSegmentUpdate `Prelude.seq`
                          Prelude.rnf schedule `Prelude.seq`
                            Prelude.rnf sendingSchedule `Prelude.seq`
                              Prelude.rnf startActivity `Prelude.seq`
                                Prelude.rnf startCondition `Prelude.seq`
                                  Prelude.rnf state `Prelude.seq`
                                    Prelude.rnf waitForQuietTime `Prelude.seq`
                                      Prelude.rnf name

instance Data.ToJSON WriteJourneyRequest where
  toJSON WriteJourneyRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Activities" Data..=) Prelude.<$> activities,
            ("ClosedDays" Data..=) Prelude.<$> closedDays,
            ("CreationDate" Data..=) Prelude.<$> creationDate,
            ("JourneyChannelSettings" Data..=)
              Prelude.<$> journeyChannelSettings,
            ("LastModifiedDate" Data..=)
              Prelude.<$> lastModifiedDate,
            ("Limits" Data..=) Prelude.<$> limits,
            ("LocalTime" Data..=) Prelude.<$> localTime,
            ("OpenHours" Data..=) Prelude.<$> openHours,
            ("QuietTime" Data..=) Prelude.<$> quietTime,
            ("RefreshFrequency" Data..=)
              Prelude.<$> refreshFrequency,
            ("RefreshOnSegmentUpdate" Data..=)
              Prelude.<$> refreshOnSegmentUpdate,
            ("Schedule" Data..=) Prelude.<$> schedule,
            ("SendingSchedule" Data..=)
              Prelude.<$> sendingSchedule,
            ("StartActivity" Data..=) Prelude.<$> startActivity,
            ("StartCondition" Data..=)
              Prelude.<$> startCondition,
            ("State" Data..=) Prelude.<$> state,
            ("WaitForQuietTime" Data..=)
              Prelude.<$> waitForQuietTime,
            Prelude.Just ("Name" Data..= name)
          ]
      )
