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
-- Module      : Network.AWS.Pinpoint.Types.WriteJourneyRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteJourneyRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Activity
import Network.AWS.Pinpoint.Types.JourneyLimits
import Network.AWS.Pinpoint.Types.JourneySchedule
import Network.AWS.Pinpoint.Types.QuietTime
import Network.AWS.Pinpoint.Types.StartCondition
import Network.AWS.Pinpoint.Types.State

-- | Specifies the configuration and other settings for a journey.
--
-- /See:/ 'newWriteJourneyRequest' smart constructor.
data WriteJourneyRequest = WriteJourneyRequest'
  { -- | The date, in ISO 8601 format, when the journey was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | A map that contains a set of Activity objects, one object for each
    -- activity in the journey. For each Activity object, the key is the unique
    -- identifier (string) for an activity and the value is the settings for
    -- the activity. An activity identifier can contain a maximum of 100
    -- characters. The characters must be alphanumeric characters.
    activities :: Core.Maybe (Core.HashMap Core.Text Activity),
    -- | The date, in ISO 8601 format, when the journey was created.
    creationDate :: Core.Maybe Core.Text,
    -- | The status of the journey. Valid values are:
    --
    -- -   DRAFT - Saves the journey and doesn\'t publish it.
    --
    -- -   ACTIVE - Saves and publishes the journey. Depending on the
    --     journey\'s schedule, the journey starts running immediately or at
    --     the scheduled start time. If a journey\'s status is ACTIVE, you
    --     can\'t add, change, or remove activities from it.
    --
    -- The CANCELLED, COMPLETED, and CLOSED values are not supported in
    -- requests to create or update a journey. To cancel a journey, use the
    -- Journey State resource.
    state :: Core.Maybe State,
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
    -- participant\'s local time. To base the schedule on each participant\'s
    -- local time, set this value to true.
    localTime :: Core.Maybe Core.Bool,
    -- | The unique identifier for the first activity in the journey. The
    -- identifier for this activity can contain a maximum of 128 characters.
    -- The characters must be alphanumeric characters.
    startActivity :: Core.Maybe Core.Text,
    -- | The schedule settings for the journey.
    schedule :: Core.Maybe JourneySchedule,
    -- | The name of the journey. A journey name can contain a maximum of 150
    -- characters. The characters can be alphanumeric characters or symbols,
    -- such as underscores (_) or hyphens (-). A journey name can\'t contain
    -- any spaces.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WriteJourneyRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'writeJourneyRequest_lastModifiedDate' - The date, in ISO 8601 format, when the journey was last modified.
--
-- 'activities', 'writeJourneyRequest_activities' - A map that contains a set of Activity objects, one object for each
-- activity in the journey. For each Activity object, the key is the unique
-- identifier (string) for an activity and the value is the settings for
-- the activity. An activity identifier can contain a maximum of 100
-- characters. The characters must be alphanumeric characters.
--
-- 'creationDate', 'writeJourneyRequest_creationDate' - The date, in ISO 8601 format, when the journey was created.
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
-- The CANCELLED, COMPLETED, and CLOSED values are not supported in
-- requests to create or update a journey. To cancel a journey, use the
-- Journey State resource.
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
-- 'limits', 'writeJourneyRequest_limits' - The messaging and entry limits for the journey.
--
-- 'startCondition', 'writeJourneyRequest_startCondition' - The segment that defines which users are participants in the journey.
--
-- 'localTime', 'writeJourneyRequest_localTime' - Specifies whether the journey\'s scheduled start and end times use each
-- participant\'s local time. To base the schedule on each participant\'s
-- local time, set this value to true.
--
-- 'startActivity', 'writeJourneyRequest_startActivity' - The unique identifier for the first activity in the journey. The
-- identifier for this activity can contain a maximum of 128 characters.
-- The characters must be alphanumeric characters.
--
-- 'schedule', 'writeJourneyRequest_schedule' - The schedule settings for the journey.
--
-- 'name', 'writeJourneyRequest_name' - The name of the journey. A journey name can contain a maximum of 150
-- characters. The characters can be alphanumeric characters or symbols,
-- such as underscores (_) or hyphens (-). A journey name can\'t contain
-- any spaces.
newWriteJourneyRequest ::
  -- | 'name'
  Core.Text ->
  WriteJourneyRequest
newWriteJourneyRequest pName_ =
  WriteJourneyRequest'
    { lastModifiedDate =
        Core.Nothing,
      activities = Core.Nothing,
      creationDate = Core.Nothing,
      state = Core.Nothing,
      quietTime = Core.Nothing,
      refreshFrequency = Core.Nothing,
      limits = Core.Nothing,
      startCondition = Core.Nothing,
      localTime = Core.Nothing,
      startActivity = Core.Nothing,
      schedule = Core.Nothing,
      name = pName_
    }

-- | The date, in ISO 8601 format, when the journey was last modified.
writeJourneyRequest_lastModifiedDate :: Lens.Lens' WriteJourneyRequest (Core.Maybe Core.Text)
writeJourneyRequest_lastModifiedDate = Lens.lens (\WriteJourneyRequest' {lastModifiedDate} -> lastModifiedDate) (\s@WriteJourneyRequest' {} a -> s {lastModifiedDate = a} :: WriteJourneyRequest)

-- | A map that contains a set of Activity objects, one object for each
-- activity in the journey. For each Activity object, the key is the unique
-- identifier (string) for an activity and the value is the settings for
-- the activity. An activity identifier can contain a maximum of 100
-- characters. The characters must be alphanumeric characters.
writeJourneyRequest_activities :: Lens.Lens' WriteJourneyRequest (Core.Maybe (Core.HashMap Core.Text Activity))
writeJourneyRequest_activities = Lens.lens (\WriteJourneyRequest' {activities} -> activities) (\s@WriteJourneyRequest' {} a -> s {activities = a} :: WriteJourneyRequest) Core.. Lens.mapping Lens._Coerce

-- | The date, in ISO 8601 format, when the journey was created.
writeJourneyRequest_creationDate :: Lens.Lens' WriteJourneyRequest (Core.Maybe Core.Text)
writeJourneyRequest_creationDate = Lens.lens (\WriteJourneyRequest' {creationDate} -> creationDate) (\s@WriteJourneyRequest' {} a -> s {creationDate = a} :: WriteJourneyRequest)

-- | The status of the journey. Valid values are:
--
-- -   DRAFT - Saves the journey and doesn\'t publish it.
--
-- -   ACTIVE - Saves and publishes the journey. Depending on the
--     journey\'s schedule, the journey starts running immediately or at
--     the scheduled start time. If a journey\'s status is ACTIVE, you
--     can\'t add, change, or remove activities from it.
--
-- The CANCELLED, COMPLETED, and CLOSED values are not supported in
-- requests to create or update a journey. To cancel a journey, use the
-- Journey State resource.
writeJourneyRequest_state :: Lens.Lens' WriteJourneyRequest (Core.Maybe State)
writeJourneyRequest_state = Lens.lens (\WriteJourneyRequest' {state} -> state) (\s@WriteJourneyRequest' {} a -> s {state = a} :: WriteJourneyRequest)

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
writeJourneyRequest_quietTime :: Lens.Lens' WriteJourneyRequest (Core.Maybe QuietTime)
writeJourneyRequest_quietTime = Lens.lens (\WriteJourneyRequest' {quietTime} -> quietTime) (\s@WriteJourneyRequest' {} a -> s {quietTime = a} :: WriteJourneyRequest)

-- | The frequency with which Amazon Pinpoint evaluates segment and event
-- data for the journey, as a duration in ISO 8601 format.
writeJourneyRequest_refreshFrequency :: Lens.Lens' WriteJourneyRequest (Core.Maybe Core.Text)
writeJourneyRequest_refreshFrequency = Lens.lens (\WriteJourneyRequest' {refreshFrequency} -> refreshFrequency) (\s@WriteJourneyRequest' {} a -> s {refreshFrequency = a} :: WriteJourneyRequest)

-- | The messaging and entry limits for the journey.
writeJourneyRequest_limits :: Lens.Lens' WriteJourneyRequest (Core.Maybe JourneyLimits)
writeJourneyRequest_limits = Lens.lens (\WriteJourneyRequest' {limits} -> limits) (\s@WriteJourneyRequest' {} a -> s {limits = a} :: WriteJourneyRequest)

-- | The segment that defines which users are participants in the journey.
writeJourneyRequest_startCondition :: Lens.Lens' WriteJourneyRequest (Core.Maybe StartCondition)
writeJourneyRequest_startCondition = Lens.lens (\WriteJourneyRequest' {startCondition} -> startCondition) (\s@WriteJourneyRequest' {} a -> s {startCondition = a} :: WriteJourneyRequest)

-- | Specifies whether the journey\'s scheduled start and end times use each
-- participant\'s local time. To base the schedule on each participant\'s
-- local time, set this value to true.
writeJourneyRequest_localTime :: Lens.Lens' WriteJourneyRequest (Core.Maybe Core.Bool)
writeJourneyRequest_localTime = Lens.lens (\WriteJourneyRequest' {localTime} -> localTime) (\s@WriteJourneyRequest' {} a -> s {localTime = a} :: WriteJourneyRequest)

-- | The unique identifier for the first activity in the journey. The
-- identifier for this activity can contain a maximum of 128 characters.
-- The characters must be alphanumeric characters.
writeJourneyRequest_startActivity :: Lens.Lens' WriteJourneyRequest (Core.Maybe Core.Text)
writeJourneyRequest_startActivity = Lens.lens (\WriteJourneyRequest' {startActivity} -> startActivity) (\s@WriteJourneyRequest' {} a -> s {startActivity = a} :: WriteJourneyRequest)

-- | The schedule settings for the journey.
writeJourneyRequest_schedule :: Lens.Lens' WriteJourneyRequest (Core.Maybe JourneySchedule)
writeJourneyRequest_schedule = Lens.lens (\WriteJourneyRequest' {schedule} -> schedule) (\s@WriteJourneyRequest' {} a -> s {schedule = a} :: WriteJourneyRequest)

-- | The name of the journey. A journey name can contain a maximum of 150
-- characters. The characters can be alphanumeric characters or symbols,
-- such as underscores (_) or hyphens (-). A journey name can\'t contain
-- any spaces.
writeJourneyRequest_name :: Lens.Lens' WriteJourneyRequest Core.Text
writeJourneyRequest_name = Lens.lens (\WriteJourneyRequest' {name} -> name) (\s@WriteJourneyRequest' {} a -> s {name = a} :: WriteJourneyRequest)

instance Core.Hashable WriteJourneyRequest

instance Core.NFData WriteJourneyRequest

instance Core.ToJSON WriteJourneyRequest where
  toJSON WriteJourneyRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LastModifiedDate" Core..=)
              Core.<$> lastModifiedDate,
            ("Activities" Core..=) Core.<$> activities,
            ("CreationDate" Core..=) Core.<$> creationDate,
            ("State" Core..=) Core.<$> state,
            ("QuietTime" Core..=) Core.<$> quietTime,
            ("RefreshFrequency" Core..=)
              Core.<$> refreshFrequency,
            ("Limits" Core..=) Core.<$> limits,
            ("StartCondition" Core..=) Core.<$> startCondition,
            ("LocalTime" Core..=) Core.<$> localTime,
            ("StartActivity" Core..=) Core.<$> startActivity,
            ("Schedule" Core..=) Core.<$> schedule,
            Core.Just ("Name" Core..= name)
          ]
      )
