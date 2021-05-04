{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SWF.Types.ActivityTaskScheduledEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskScheduledEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.ActivityType
import Network.AWS.SWF.Types.TaskList

-- | Provides the details of the @ActivityTaskScheduled@ event.
--
-- /See:/ 'newActivityTaskScheduledEventAttributes' smart constructor.
data ActivityTaskScheduledEventAttributes = ActivityTaskScheduledEventAttributes'
  { -- | The input provided to the activity task.
    input :: Prelude.Maybe Prelude.Text,
    -- | The maximum time before which the worker processing this task must
    -- report progress by calling RecordActivityTaskHeartbeat. If the timeout
    -- is exceeded, the activity task is automatically timed out. If the worker
    -- subsequently attempts to record a heartbeat or return a result, it is
    -- ignored.
    heartbeatTimeout :: Prelude.Maybe Prelude.Text,
    -- | The maximum amount of time for this activity task.
    scheduleToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The maximum amount of time the activity task can wait to be assigned to
    -- a worker.
    scheduleToStartTimeout :: Prelude.Maybe Prelude.Text,
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
    -- | Data attached to the event that can be used by the decider in subsequent
    -- workflow tasks. This data isn\'t sent to the activity.
    control :: Prelude.Maybe Prelude.Text,
    -- | The maximum amount of time a worker may take to process the activity
    -- task.
    startToCloseTimeout :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActivityTaskScheduledEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'activityTaskScheduledEventAttributes_input' - The input provided to the activity task.
--
-- 'heartbeatTimeout', 'activityTaskScheduledEventAttributes_heartbeatTimeout' - The maximum time before which the worker processing this task must
-- report progress by calling RecordActivityTaskHeartbeat. If the timeout
-- is exceeded, the activity task is automatically timed out. If the worker
-- subsequently attempts to record a heartbeat or return a result, it is
-- ignored.
--
-- 'scheduleToCloseTimeout', 'activityTaskScheduledEventAttributes_scheduleToCloseTimeout' - The maximum amount of time for this activity task.
--
-- 'scheduleToStartTimeout', 'activityTaskScheduledEventAttributes_scheduleToStartTimeout' - The maximum amount of time the activity task can wait to be assigned to
-- a worker.
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
-- 'control', 'activityTaskScheduledEventAttributes_control' - Data attached to the event that can be used by the decider in subsequent
-- workflow tasks. This data isn\'t sent to the activity.
--
-- 'startToCloseTimeout', 'activityTaskScheduledEventAttributes_startToCloseTimeout' - The maximum amount of time a worker may take to process the activity
-- task.
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
      { input =
          Prelude.Nothing,
        heartbeatTimeout = Prelude.Nothing,
        scheduleToCloseTimeout =
          Prelude.Nothing,
        scheduleToStartTimeout =
          Prelude.Nothing,
        taskPriority = Prelude.Nothing,
        control = Prelude.Nothing,
        startToCloseTimeout = Prelude.Nothing,
        activityType = pActivityType_,
        activityId = pActivityId_,
        taskList = pTaskList_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The input provided to the activity task.
activityTaskScheduledEventAttributes_input :: Lens.Lens' ActivityTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskScheduledEventAttributes_input = Lens.lens (\ActivityTaskScheduledEventAttributes' {input} -> input) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {input = a} :: ActivityTaskScheduledEventAttributes)

-- | The maximum time before which the worker processing this task must
-- report progress by calling RecordActivityTaskHeartbeat. If the timeout
-- is exceeded, the activity task is automatically timed out. If the worker
-- subsequently attempts to record a heartbeat or return a result, it is
-- ignored.
activityTaskScheduledEventAttributes_heartbeatTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskScheduledEventAttributes_heartbeatTimeout = Lens.lens (\ActivityTaskScheduledEventAttributes' {heartbeatTimeout} -> heartbeatTimeout) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {heartbeatTimeout = a} :: ActivityTaskScheduledEventAttributes)

-- | The maximum amount of time for this activity task.
activityTaskScheduledEventAttributes_scheduleToCloseTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskScheduledEventAttributes_scheduleToCloseTimeout = Lens.lens (\ActivityTaskScheduledEventAttributes' {scheduleToCloseTimeout} -> scheduleToCloseTimeout) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {scheduleToCloseTimeout = a} :: ActivityTaskScheduledEventAttributes)

-- | The maximum amount of time the activity task can wait to be assigned to
-- a worker.
activityTaskScheduledEventAttributes_scheduleToStartTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskScheduledEventAttributes_scheduleToStartTimeout = Lens.lens (\ActivityTaskScheduledEventAttributes' {scheduleToStartTimeout} -> scheduleToStartTimeout) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {scheduleToStartTimeout = a} :: ActivityTaskScheduledEventAttributes)

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

-- | Data attached to the event that can be used by the decider in subsequent
-- workflow tasks. This data isn\'t sent to the activity.
activityTaskScheduledEventAttributes_control :: Lens.Lens' ActivityTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskScheduledEventAttributes_control = Lens.lens (\ActivityTaskScheduledEventAttributes' {control} -> control) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {control = a} :: ActivityTaskScheduledEventAttributes)

-- | The maximum amount of time a worker may take to process the activity
-- task.
activityTaskScheduledEventAttributes_startToCloseTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskScheduledEventAttributes_startToCloseTimeout = Lens.lens (\ActivityTaskScheduledEventAttributes' {startToCloseTimeout} -> startToCloseTimeout) (\s@ActivityTaskScheduledEventAttributes' {} a -> s {startToCloseTimeout = a} :: ActivityTaskScheduledEventAttributes)

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
  Prelude.FromJSON
    ActivityTaskScheduledEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "ActivityTaskScheduledEventAttributes"
      ( \x ->
          ActivityTaskScheduledEventAttributes'
            Prelude.<$> (x Prelude..:? "input")
            Prelude.<*> (x Prelude..:? "heartbeatTimeout")
            Prelude.<*> (x Prelude..:? "scheduleToCloseTimeout")
            Prelude.<*> (x Prelude..:? "scheduleToStartTimeout")
            Prelude.<*> (x Prelude..:? "taskPriority")
            Prelude.<*> (x Prelude..:? "control")
            Prelude.<*> (x Prelude..:? "startToCloseTimeout")
            Prelude.<*> (x Prelude..: "activityType")
            Prelude.<*> (x Prelude..: "activityId")
            Prelude.<*> (x Prelude..: "taskList")
            Prelude.<*> (x Prelude..: "decisionTaskCompletedEventId")
      )

instance
  Prelude.Hashable
    ActivityTaskScheduledEventAttributes

instance
  Prelude.NFData
    ActivityTaskScheduledEventAttributes
