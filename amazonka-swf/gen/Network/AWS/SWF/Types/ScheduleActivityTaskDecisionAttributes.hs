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
-- Module      : Network.AWS.SWF.Types.ScheduleActivityTaskDecisionAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleActivityTaskDecisionAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.ActivityType
import Network.AWS.SWF.Types.TaskList

-- | Provides the details of the @ScheduleActivityTask@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--
--     -   @activityType.name@ – String constraint. The key is
--         @swf:activityType.name@.
--
--     -   @activityType.version@ – String constraint. The key is
--         @swf:activityType.version@.
--
--     -   @taskList@ – String constraint. The key is @swf:taskList.name@.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
--
-- /See:/ 'newScheduleActivityTaskDecisionAttributes' smart constructor.
data ScheduleActivityTaskDecisionAttributes = ScheduleActivityTaskDecisionAttributes'
  { -- | The input provided to the activity task.
    input :: Prelude.Maybe Prelude.Text,
    -- | If set, specifies the maximum time before which a worker processing a
    -- task of this type must report progress by calling
    -- RecordActivityTaskHeartbeat. If the timeout is exceeded, the activity
    -- task is automatically timed out. If the worker subsequently attempts to
    -- record a heartbeat or returns a result, it is ignored. This overrides
    -- the default heartbeat timeout specified when registering the activity
    -- type using RegisterActivityType.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    heartbeatTimeout :: Prelude.Maybe Prelude.Text,
    -- | The maximum duration for this activity task.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    --
    -- A schedule-to-close timeout for this activity task must be specified
    -- either as a default for the activity type or through this field. If
    -- neither this field is set nor a default schedule-to-close timeout was
    -- specified at registration time then a fault is returned.
    scheduleToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | If set, specifies the maximum duration the activity task can wait to be
    -- assigned to a worker. This overrides the default schedule-to-start
    -- timeout specified when registering the activity type using
    -- RegisterActivityType.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    --
    -- A schedule-to-start timeout for this activity task must be specified
    -- either as a default for the activity type or through this field. If
    -- neither this field is set nor a default schedule-to-start timeout was
    -- specified at registration time then a fault is returned.
    scheduleToStartTimeout :: Prelude.Maybe Prelude.Text,
    -- | If set, specifies the name of the task list in which to schedule the
    -- activity task. If not specified, the @defaultTaskList@ registered with
    -- the activity type is used.
    --
    -- A task list for this activity task must be specified either as a default
    -- for the activity type or through this field. If neither this field is
    -- set nor a default task list was specified at registration time then a
    -- fault is returned.
    --
    -- The specified string must not start or end with whitespace. It must not
    -- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
    -- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
    -- contain the literal string @arn@.
    taskList :: Prelude.Maybe TaskList,
    -- | If set, specifies the priority with which the activity task is to be
    -- assigned to a worker. This overrides the defaultTaskPriority specified
    -- when registering the activity type using RegisterActivityType. Valid
    -- values are integers that range from Java\'s @Integer.MIN_VALUE@
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
    -- | If set, specifies the maximum duration a worker may take to process this
    -- activity task. This overrides the default start-to-close timeout
    -- specified when registering the activity type using RegisterActivityType.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    --
    -- A start-to-close timeout for this activity task must be specified either
    -- as a default for the activity type or through this field. If neither
    -- this field is set nor a default start-to-close timeout was specified at
    -- registration time then a fault is returned.
    startToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The type of the activity task to schedule.
    activityType :: ActivityType,
    -- | The @activityId@ of the activity task.
    --
    -- The specified string must not start or end with whitespace. It must not
    -- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
    -- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
    -- contain the literal string @arn@.
    activityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScheduleActivityTaskDecisionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'scheduleActivityTaskDecisionAttributes_input' - The input provided to the activity task.
--
-- 'heartbeatTimeout', 'scheduleActivityTaskDecisionAttributes_heartbeatTimeout' - If set, specifies the maximum time before which a worker processing a
-- task of this type must report progress by calling
-- RecordActivityTaskHeartbeat. If the timeout is exceeded, the activity
-- task is automatically timed out. If the worker subsequently attempts to
-- record a heartbeat or returns a result, it is ignored. This overrides
-- the default heartbeat timeout specified when registering the activity
-- type using RegisterActivityType.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'scheduleToCloseTimeout', 'scheduleActivityTaskDecisionAttributes_scheduleToCloseTimeout' - The maximum duration for this activity task.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- A schedule-to-close timeout for this activity task must be specified
-- either as a default for the activity type or through this field. If
-- neither this field is set nor a default schedule-to-close timeout was
-- specified at registration time then a fault is returned.
--
-- 'scheduleToStartTimeout', 'scheduleActivityTaskDecisionAttributes_scheduleToStartTimeout' - If set, specifies the maximum duration the activity task can wait to be
-- assigned to a worker. This overrides the default schedule-to-start
-- timeout specified when registering the activity type using
-- RegisterActivityType.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- A schedule-to-start timeout for this activity task must be specified
-- either as a default for the activity type or through this field. If
-- neither this field is set nor a default schedule-to-start timeout was
-- specified at registration time then a fault is returned.
--
-- 'taskList', 'scheduleActivityTaskDecisionAttributes_taskList' - If set, specifies the name of the task list in which to schedule the
-- activity task. If not specified, the @defaultTaskList@ registered with
-- the activity type is used.
--
-- A task list for this activity task must be specified either as a default
-- for the activity type or through this field. If neither this field is
-- set nor a default task list was specified at registration time then a
-- fault is returned.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- contain the literal string @arn@.
--
-- 'taskPriority', 'scheduleActivityTaskDecisionAttributes_taskPriority' - If set, specifies the priority with which the activity task is to be
-- assigned to a worker. This overrides the defaultTaskPriority specified
-- when registering the activity type using RegisterActivityType. Valid
-- values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
--
-- 'control', 'scheduleActivityTaskDecisionAttributes_control' - Data attached to the event that can be used by the decider in subsequent
-- workflow tasks. This data isn\'t sent to the activity.
--
-- 'startToCloseTimeout', 'scheduleActivityTaskDecisionAttributes_startToCloseTimeout' - If set, specifies the maximum duration a worker may take to process this
-- activity task. This overrides the default start-to-close timeout
-- specified when registering the activity type using RegisterActivityType.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- A start-to-close timeout for this activity task must be specified either
-- as a default for the activity type or through this field. If neither
-- this field is set nor a default start-to-close timeout was specified at
-- registration time then a fault is returned.
--
-- 'activityType', 'scheduleActivityTaskDecisionAttributes_activityType' - The type of the activity task to schedule.
--
-- 'activityId', 'scheduleActivityTaskDecisionAttributes_activityId' - The @activityId@ of the activity task.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- contain the literal string @arn@.
newScheduleActivityTaskDecisionAttributes ::
  -- | 'activityType'
  ActivityType ->
  -- | 'activityId'
  Prelude.Text ->
  ScheduleActivityTaskDecisionAttributes
newScheduleActivityTaskDecisionAttributes
  pActivityType_
  pActivityId_ =
    ScheduleActivityTaskDecisionAttributes'
      { input =
          Prelude.Nothing,
        heartbeatTimeout = Prelude.Nothing,
        scheduleToCloseTimeout =
          Prelude.Nothing,
        scheduleToStartTimeout =
          Prelude.Nothing,
        taskList = Prelude.Nothing,
        taskPriority = Prelude.Nothing,
        control = Prelude.Nothing,
        startToCloseTimeout =
          Prelude.Nothing,
        activityType = pActivityType_,
        activityId = pActivityId_
      }

-- | The input provided to the activity task.
scheduleActivityTaskDecisionAttributes_input :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Prelude.Maybe Prelude.Text)
scheduleActivityTaskDecisionAttributes_input = Lens.lens (\ScheduleActivityTaskDecisionAttributes' {input} -> input) (\s@ScheduleActivityTaskDecisionAttributes' {} a -> s {input = a} :: ScheduleActivityTaskDecisionAttributes)

-- | If set, specifies the maximum time before which a worker processing a
-- task of this type must report progress by calling
-- RecordActivityTaskHeartbeat. If the timeout is exceeded, the activity
-- task is automatically timed out. If the worker subsequently attempts to
-- record a heartbeat or returns a result, it is ignored. This overrides
-- the default heartbeat timeout specified when registering the activity
-- type using RegisterActivityType.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
scheduleActivityTaskDecisionAttributes_heartbeatTimeout :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Prelude.Maybe Prelude.Text)
scheduleActivityTaskDecisionAttributes_heartbeatTimeout = Lens.lens (\ScheduleActivityTaskDecisionAttributes' {heartbeatTimeout} -> heartbeatTimeout) (\s@ScheduleActivityTaskDecisionAttributes' {} a -> s {heartbeatTimeout = a} :: ScheduleActivityTaskDecisionAttributes)

-- | The maximum duration for this activity task.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- A schedule-to-close timeout for this activity task must be specified
-- either as a default for the activity type or through this field. If
-- neither this field is set nor a default schedule-to-close timeout was
-- specified at registration time then a fault is returned.
scheduleActivityTaskDecisionAttributes_scheduleToCloseTimeout :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Prelude.Maybe Prelude.Text)
scheduleActivityTaskDecisionAttributes_scheduleToCloseTimeout = Lens.lens (\ScheduleActivityTaskDecisionAttributes' {scheduleToCloseTimeout} -> scheduleToCloseTimeout) (\s@ScheduleActivityTaskDecisionAttributes' {} a -> s {scheduleToCloseTimeout = a} :: ScheduleActivityTaskDecisionAttributes)

-- | If set, specifies the maximum duration the activity task can wait to be
-- assigned to a worker. This overrides the default schedule-to-start
-- timeout specified when registering the activity type using
-- RegisterActivityType.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- A schedule-to-start timeout for this activity task must be specified
-- either as a default for the activity type or through this field. If
-- neither this field is set nor a default schedule-to-start timeout was
-- specified at registration time then a fault is returned.
scheduleActivityTaskDecisionAttributes_scheduleToStartTimeout :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Prelude.Maybe Prelude.Text)
scheduleActivityTaskDecisionAttributes_scheduleToStartTimeout = Lens.lens (\ScheduleActivityTaskDecisionAttributes' {scheduleToStartTimeout} -> scheduleToStartTimeout) (\s@ScheduleActivityTaskDecisionAttributes' {} a -> s {scheduleToStartTimeout = a} :: ScheduleActivityTaskDecisionAttributes)

-- | If set, specifies the name of the task list in which to schedule the
-- activity task. If not specified, the @defaultTaskList@ registered with
-- the activity type is used.
--
-- A task list for this activity task must be specified either as a default
-- for the activity type or through this field. If neither this field is
-- set nor a default task list was specified at registration time then a
-- fault is returned.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- contain the literal string @arn@.
scheduleActivityTaskDecisionAttributes_taskList :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Prelude.Maybe TaskList)
scheduleActivityTaskDecisionAttributes_taskList = Lens.lens (\ScheduleActivityTaskDecisionAttributes' {taskList} -> taskList) (\s@ScheduleActivityTaskDecisionAttributes' {} a -> s {taskList = a} :: ScheduleActivityTaskDecisionAttributes)

-- | If set, specifies the priority with which the activity task is to be
-- assigned to a worker. This overrides the defaultTaskPriority specified
-- when registering the activity type using RegisterActivityType. Valid
-- values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
scheduleActivityTaskDecisionAttributes_taskPriority :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Prelude.Maybe Prelude.Text)
scheduleActivityTaskDecisionAttributes_taskPriority = Lens.lens (\ScheduleActivityTaskDecisionAttributes' {taskPriority} -> taskPriority) (\s@ScheduleActivityTaskDecisionAttributes' {} a -> s {taskPriority = a} :: ScheduleActivityTaskDecisionAttributes)

-- | Data attached to the event that can be used by the decider in subsequent
-- workflow tasks. This data isn\'t sent to the activity.
scheduleActivityTaskDecisionAttributes_control :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Prelude.Maybe Prelude.Text)
scheduleActivityTaskDecisionAttributes_control = Lens.lens (\ScheduleActivityTaskDecisionAttributes' {control} -> control) (\s@ScheduleActivityTaskDecisionAttributes' {} a -> s {control = a} :: ScheduleActivityTaskDecisionAttributes)

-- | If set, specifies the maximum duration a worker may take to process this
-- activity task. This overrides the default start-to-close timeout
-- specified when registering the activity type using RegisterActivityType.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- A start-to-close timeout for this activity task must be specified either
-- as a default for the activity type or through this field. If neither
-- this field is set nor a default start-to-close timeout was specified at
-- registration time then a fault is returned.
scheduleActivityTaskDecisionAttributes_startToCloseTimeout :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Prelude.Maybe Prelude.Text)
scheduleActivityTaskDecisionAttributes_startToCloseTimeout = Lens.lens (\ScheduleActivityTaskDecisionAttributes' {startToCloseTimeout} -> startToCloseTimeout) (\s@ScheduleActivityTaskDecisionAttributes' {} a -> s {startToCloseTimeout = a} :: ScheduleActivityTaskDecisionAttributes)

-- | The type of the activity task to schedule.
scheduleActivityTaskDecisionAttributes_activityType :: Lens.Lens' ScheduleActivityTaskDecisionAttributes ActivityType
scheduleActivityTaskDecisionAttributes_activityType = Lens.lens (\ScheduleActivityTaskDecisionAttributes' {activityType} -> activityType) (\s@ScheduleActivityTaskDecisionAttributes' {} a -> s {activityType = a} :: ScheduleActivityTaskDecisionAttributes)

-- | The @activityId@ of the activity task.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- contain the literal string @arn@.
scheduleActivityTaskDecisionAttributes_activityId :: Lens.Lens' ScheduleActivityTaskDecisionAttributes Prelude.Text
scheduleActivityTaskDecisionAttributes_activityId = Lens.lens (\ScheduleActivityTaskDecisionAttributes' {activityId} -> activityId) (\s@ScheduleActivityTaskDecisionAttributes' {} a -> s {activityId = a} :: ScheduleActivityTaskDecisionAttributes)

instance
  Prelude.Hashable
    ScheduleActivityTaskDecisionAttributes

instance
  Prelude.NFData
    ScheduleActivityTaskDecisionAttributes

instance
  Prelude.ToJSON
    ScheduleActivityTaskDecisionAttributes
  where
  toJSON ScheduleActivityTaskDecisionAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("input" Prelude..=) Prelude.<$> input,
            ("heartbeatTimeout" Prelude..=)
              Prelude.<$> heartbeatTimeout,
            ("scheduleToCloseTimeout" Prelude..=)
              Prelude.<$> scheduleToCloseTimeout,
            ("scheduleToStartTimeout" Prelude..=)
              Prelude.<$> scheduleToStartTimeout,
            ("taskList" Prelude..=) Prelude.<$> taskList,
            ("taskPriority" Prelude..=) Prelude.<$> taskPriority,
            ("control" Prelude..=) Prelude.<$> control,
            ("startToCloseTimeout" Prelude..=)
              Prelude.<$> startToCloseTimeout,
            Prelude.Just
              ("activityType" Prelude..= activityType),
            Prelude.Just ("activityId" Prelude..= activityId)
          ]
      )
