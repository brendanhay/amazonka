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
-- Module      : Amazonka.SWF.Types.ActivityTaskScheduledEventAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ActivityTaskScheduledEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.ActivityType
import Amazonka.SWF.Types.TaskList

-- | Provides the details of the @ActivityTaskScheduled@ event.
--
-- /See:/ 'newActivityTaskScheduledEventAttributes' smart constructor.
data ActivityTaskScheduledEventAttributes = ActivityTaskScheduledEventAttributes'
  { -- | Data attached to the event that can be used by the decider in subsequent
    -- workflow tasks. This data isn\'t sent to the activity.
    control :: Prelude.Maybe Prelude.Text,
    -- | The maximum time before which the worker processing this task must
    -- report progress by calling RecordActivityTaskHeartbeat. If the timeout
    -- is exceeded, the activity task is automatically timed out. If the worker
    -- subsequently attempts to record a heartbeat or return a result, it is
    -- ignored.
    heartbeatTimeout :: Prelude.Maybe Prelude.Text,
    -- | The input provided to the activity task.
    input :: Prelude.Maybe Prelude.Text,
    -- | The maximum amount of time for this activity task.
    scheduleToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The maximum amount of time the activity task can wait to be assigned to
    -- a worker.
    scheduleToStartTimeout :: Prelude.Maybe Prelude.Text,
    -- | The maximum amount of time a worker may take to process the activity
    -- task.
    startToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The priority to assign to the scheduled activity task. If set, this
    -- overrides any default priority value that was assigned when the activity
    -- type was registered.
    --
    -- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
    -- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
    -- indicate higher priority.
    --
    -- For more information about setting task priority, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
    -- in the /Amazon SWF Developer Guide/.
    taskPriority :: Prelude.Maybe Prelude.Text,
    -- | The type of the activity task.
    activityType :: ActivityType,
    -- | The unique ID of the activity task.
    activityId :: Prelude.Text,
    -- | The task list in which the activity task has been scheduled.
    taskList :: TaskList,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the
    -- decision that resulted in the scheduling of this activity task. This
    -- information can be useful for diagnosing problems by tracing back the
    -- chain of events leading up to this event.
    decisionTaskCompletedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivityTaskScheduledEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'control', 'activityTaskScheduledEventAttributes_control' - Data attached to the event that can be used by the decider in subsequent
-- workflow tasks. This data isn\'t sent to the activity.
--
-- 'heartbeatTimeout', 'activityTaskScheduledEventAttributes_heartbeatTimeout' - The maximum time before which the worker processing this task must
-- report progress by calling RecordActivityTaskHeartbeat. If the timeout
-- is exceeded, the activity task is automatically timed out. If the worker
-- subsequently attempts to record a heartbeat or return a result, it is
-- ignored.
--
-- 'input', 'activityTaskScheduledEventAttributes_input' - The input provided to the activity task.
--
-- 'scheduleToCloseTimeout', 'activityTaskScheduledEventAttributes_scheduleToCloseTimeout' - The maximum amount of time for this activity task.
--
-- 'scheduleToStartTimeout', 'activityTaskScheduledEventAttributes_scheduleToStartTimeout' - The maximum amount of time the activity task can wait to be assigned to
-- a worker.
--
-- 'startToCloseTimeout', 'activityTaskScheduledEventAttributes_startToCloseTimeout' - The maximum amount of time a worker may take to process the activity
-- task.
--
-- 'taskPriority', 'activityTaskScheduledEventAttributes_taskPriority' - The priority to assign to the scheduled activity task. If set, this
-- overrides any default priority value that was assigned when the activity
-- type was registered.
--
-- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
--
-- 'activityType', 'activityTaskScheduledEventAttributes_activityType' - The type of the activity task.
--
-- 'activityId', 'activityTaskScheduledEventAttributes_activityId' - The unique ID of the activity task.
--
-- 'taskList', 'activityTaskScheduledEventAttributes_taskList' - The task list in which the activity task has been scheduled.
--
-- 'decisionTaskCompletedEventId', 'activityTaskScheduledEventAttributes_decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision that resulted in the scheduling of this activity task. This
-- information can be useful for diagnosing problems by tracing back the
-- chain of events leading up to this event.
newActivityTaskScheduledEventAttributes ::
  -- | 'activityType'
  ActivityType ->
  -- | 'activityId'
  Prelude.Text ->
  -- | 'taskList'
  TaskList ->
  -- | 'decisionTaskCompletedEventId'
  Prelude.Integer ->
  ActivityTaskScheduledEventAttributes
newActivityTaskScheduledEventAttributes
  pActivityType_
  pActivityId_
  pTaskList_
  pDecisionTaskCompletedEventId_ =
    ActivityTaskScheduledEventAttributes'
      { control =
          Prelude.Nothing,
        heartbeatTimeout = Prelude.Nothing,
        input = Prelude.Nothing,
        scheduleToCloseTimeout =
          Prelude.Nothing,
        scheduleToStartTimeout =
          Prelude.Nothing,
        startToCloseTimeout = Prelude.Nothing,
        taskPriority = Prelude.Nothing,
        activityType = pActivityType_,
        activityId = pActivityId_,
        taskList = pTaskList_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that can be used by the decider in subsequent
-- workflow tasks. This data isn\'t sent to the activity.
activityTaskScheduledEventAttributes_control :: Lens.Lens' ActivityTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskScheduledEventAttributes_control = Lens.lens (\ActivityTaskScheduledEventAttributes' {control} -> control) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {control = a} :: ActivityTaskScheduledEventAttributes)

-- | The maximum time before which the worker processing this task must
-- report progress by calling RecordActivityTaskHeartbeat. If the timeout
-- is exceeded, the activity task is automatically timed out. If the worker
-- subsequently attempts to record a heartbeat or return a result, it is
-- ignored.
activityTaskScheduledEventAttributes_heartbeatTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskScheduledEventAttributes_heartbeatTimeout = Lens.lens (\ActivityTaskScheduledEventAttributes' {heartbeatTimeout} -> heartbeatTimeout) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {heartbeatTimeout = a} :: ActivityTaskScheduledEventAttributes)

-- | The input provided to the activity task.
activityTaskScheduledEventAttributes_input :: Lens.Lens' ActivityTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskScheduledEventAttributes_input = Lens.lens (\ActivityTaskScheduledEventAttributes' {input} -> input) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {input = a} :: ActivityTaskScheduledEventAttributes)

-- | The maximum amount of time for this activity task.
activityTaskScheduledEventAttributes_scheduleToCloseTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskScheduledEventAttributes_scheduleToCloseTimeout = Lens.lens (\ActivityTaskScheduledEventAttributes' {scheduleToCloseTimeout} -> scheduleToCloseTimeout) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {scheduleToCloseTimeout = a} :: ActivityTaskScheduledEventAttributes)

-- | The maximum amount of time the activity task can wait to be assigned to
-- a worker.
activityTaskScheduledEventAttributes_scheduleToStartTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskScheduledEventAttributes_scheduleToStartTimeout = Lens.lens (\ActivityTaskScheduledEventAttributes' {scheduleToStartTimeout} -> scheduleToStartTimeout) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {scheduleToStartTimeout = a} :: ActivityTaskScheduledEventAttributes)

-- | The maximum amount of time a worker may take to process the activity
-- task.
activityTaskScheduledEventAttributes_startToCloseTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskScheduledEventAttributes_startToCloseTimeout = Lens.lens (\ActivityTaskScheduledEventAttributes' {startToCloseTimeout} -> startToCloseTimeout) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {startToCloseTimeout = a} :: ActivityTaskScheduledEventAttributes)

-- | The priority to assign to the scheduled activity task. If set, this
-- overrides any default priority value that was assigned when the activity
-- type was registered.
--
-- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
activityTaskScheduledEventAttributes_taskPriority :: Lens.Lens' ActivityTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskScheduledEventAttributes_taskPriority = Lens.lens (\ActivityTaskScheduledEventAttributes' {taskPriority} -> taskPriority) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {taskPriority = a} :: ActivityTaskScheduledEventAttributes)

-- | The type of the activity task.
activityTaskScheduledEventAttributes_activityType :: Lens.Lens' ActivityTaskScheduledEventAttributes ActivityType
activityTaskScheduledEventAttributes_activityType = Lens.lens (\ActivityTaskScheduledEventAttributes' {activityType} -> activityType) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {activityType = a} :: ActivityTaskScheduledEventAttributes)

-- | The unique ID of the activity task.
activityTaskScheduledEventAttributes_activityId :: Lens.Lens' ActivityTaskScheduledEventAttributes Prelude.Text
activityTaskScheduledEventAttributes_activityId = Lens.lens (\ActivityTaskScheduledEventAttributes' {activityId} -> activityId) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {activityId = a} :: ActivityTaskScheduledEventAttributes)

-- | The task list in which the activity task has been scheduled.
activityTaskScheduledEventAttributes_taskList :: Lens.Lens' ActivityTaskScheduledEventAttributes TaskList
activityTaskScheduledEventAttributes_taskList = Lens.lens (\ActivityTaskScheduledEventAttributes' {taskList} -> taskList) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {taskList = a} :: ActivityTaskScheduledEventAttributes)

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the
-- decision that resulted in the scheduling of this activity task. This
-- information can be useful for diagnosing problems by tracing back the
-- chain of events leading up to this event.
activityTaskScheduledEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' ActivityTaskScheduledEventAttributes Prelude.Integer
activityTaskScheduledEventAttributes_decisionTaskCompletedEventId = Lens.lens (\ActivityTaskScheduledEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: ActivityTaskScheduledEventAttributes)

instance
  Data.FromJSON
    ActivityTaskScheduledEventAttributes
  where
  parseJSON =
    Data.withObject
      "ActivityTaskScheduledEventAttributes"
      ( \x ->
          ActivityTaskScheduledEventAttributes'
            Prelude.<$> (x Data..:? "control")
            Prelude.<*> (x Data..:? "heartbeatTimeout")
            Prelude.<*> (x Data..:? "input")
            Prelude.<*> (x Data..:? "scheduleToCloseTimeout")
            Prelude.<*> (x Data..:? "scheduleToStartTimeout")
            Prelude.<*> (x Data..:? "startToCloseTimeout")
            Prelude.<*> (x Data..:? "taskPriority")
            Prelude.<*> (x Data..: "activityType")
            Prelude.<*> (x Data..: "activityId")
            Prelude.<*> (x Data..: "taskList")
            Prelude.<*> (x Data..: "decisionTaskCompletedEventId")
      )

instance
  Prelude.Hashable
    ActivityTaskScheduledEventAttributes
  where
  hashWithSalt
    _salt
    ActivityTaskScheduledEventAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` control
        `Prelude.hashWithSalt` heartbeatTimeout
        `Prelude.hashWithSalt` input
        `Prelude.hashWithSalt` scheduleToCloseTimeout
        `Prelude.hashWithSalt` scheduleToStartTimeout
        `Prelude.hashWithSalt` startToCloseTimeout
        `Prelude.hashWithSalt` taskPriority
        `Prelude.hashWithSalt` activityType
        `Prelude.hashWithSalt` activityId
        `Prelude.hashWithSalt` taskList
        `Prelude.hashWithSalt` decisionTaskCompletedEventId

instance
  Prelude.NFData
    ActivityTaskScheduledEventAttributes
  where
  rnf ActivityTaskScheduledEventAttributes' {..} =
    Prelude.rnf control
      `Prelude.seq` Prelude.rnf heartbeatTimeout
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf scheduleToCloseTimeout
      `Prelude.seq` Prelude.rnf scheduleToStartTimeout
      `Prelude.seq` Prelude.rnf startToCloseTimeout
      `Prelude.seq` Prelude.rnf taskPriority
      `Prelude.seq` Prelude.rnf activityType
      `Prelude.seq` Prelude.rnf activityId
      `Prelude.seq` Prelude.rnf taskList
      `Prelude.seq` Prelude.rnf decisionTaskCompletedEventId
