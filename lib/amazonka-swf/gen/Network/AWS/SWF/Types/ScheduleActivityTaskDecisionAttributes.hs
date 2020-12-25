{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ScheduleActivityTaskDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleActivityTaskDecisionAttributes
  ( ScheduleActivityTaskDecisionAttributes (..),

    -- * Smart constructor
    mkScheduleActivityTaskDecisionAttributes,

    -- * Lenses
    satdaActivityType,
    satdaActivityId,
    satdaControl,
    satdaHeartbeatTimeout,
    satdaInput,
    satdaScheduleToCloseTimeout,
    satdaScheduleToStartTimeout,
    satdaStartToCloseTimeout,
    satdaTaskList,
    satdaTaskPriority,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.ActivityId as Types
import qualified Network.AWS.SWF.Types.ActivityType as Types
import qualified Network.AWS.SWF.Types.Control as Types
import qualified Network.AWS.SWF.Types.HeartbeatTimeout as Types
import qualified Network.AWS.SWF.Types.Input as Types
import qualified Network.AWS.SWF.Types.ScheduleToCloseTimeout as Types
import qualified Network.AWS.SWF.Types.ScheduleToStartTimeout as Types
import qualified Network.AWS.SWF.Types.StartToCloseTimeout as Types
import qualified Network.AWS.SWF.Types.TaskList as Types
import qualified Network.AWS.SWF.Types.TaskPriority as Types

-- | Provides the details of the @ScheduleActivityTask@ decision.
--
-- __Access Control__
-- You can use IAM policies to control this decision's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @activityType.name@ – String constraint. The key is @swf:activityType.name@ .
--
--
--     * @activityType.version@ – String constraint. The key is @swf:activityType.version@ .
--
--
--     * @taskList@ – String constraint. The key is @swf:taskList.name@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- /See:/ 'mkScheduleActivityTaskDecisionAttributes' smart constructor.
data ScheduleActivityTaskDecisionAttributes = ScheduleActivityTaskDecisionAttributes'
  { -- | The type of the activity task to schedule.
    activityType :: Types.ActivityType,
    -- | The @activityId@ of the activity task.
    --
    -- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
    activityId :: Types.ActivityId,
    -- | Data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the activity.
    control :: Core.Maybe Types.Control,
    -- | If set, specifies the maximum time before which a worker processing a task of this type must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. If the worker subsequently attempts to record a heartbeat or returns a result, it is ignored. This overrides the default heartbeat timeout specified when registering the activity type using 'RegisterActivityType' .
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    heartbeatTimeout :: Core.Maybe Types.HeartbeatTimeout,
    -- | The input provided to the activity task.
    input :: Core.Maybe Types.Input,
    -- | The maximum duration for this activity task.
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    scheduleToCloseTimeout :: Core.Maybe Types.ScheduleToCloseTimeout,
    -- | If set, specifies the maximum duration the activity task can wait to be assigned to a worker. This overrides the default schedule-to-start timeout specified when registering the activity type using 'RegisterActivityType' .
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    scheduleToStartTimeout :: Core.Maybe Types.ScheduleToStartTimeout,
    -- | If set, specifies the maximum duration a worker may take to process this activity task. This overrides the default start-to-close timeout specified when registering the activity type using 'RegisterActivityType' .
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    startToCloseTimeout :: Core.Maybe Types.StartToCloseTimeout,
    -- | If set, specifies the name of the task list in which to schedule the activity task. If not specified, the @defaultTaskList@ registered with the activity type is used.
    --
    -- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
    taskList :: Core.Maybe Types.TaskList,
    -- | If set, specifies the priority with which the activity task is to be assigned to a worker. This overrides the defaultTaskPriority specified when registering the activity type using 'RegisterActivityType' . Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
    --
    -- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
    taskPriority :: Core.Maybe Types.TaskPriority
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduleActivityTaskDecisionAttributes' value with any optional fields omitted.
mkScheduleActivityTaskDecisionAttributes ::
  -- | 'activityType'
  Types.ActivityType ->
  -- | 'activityId'
  Types.ActivityId ->
  ScheduleActivityTaskDecisionAttributes
mkScheduleActivityTaskDecisionAttributes activityType activityId =
  ScheduleActivityTaskDecisionAttributes'
    { activityType,
      activityId,
      control = Core.Nothing,
      heartbeatTimeout = Core.Nothing,
      input = Core.Nothing,
      scheduleToCloseTimeout = Core.Nothing,
      scheduleToStartTimeout = Core.Nothing,
      startToCloseTimeout = Core.Nothing,
      taskList = Core.Nothing,
      taskPriority = Core.Nothing
    }

-- | The type of the activity task to schedule.
--
-- /Note:/ Consider using 'activityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaActivityType :: Lens.Lens' ScheduleActivityTaskDecisionAttributes Types.ActivityType
satdaActivityType = Lens.field @"activityType"
{-# DEPRECATED satdaActivityType "Use generic-lens or generic-optics with 'activityType' instead." #-}

-- | The @activityId@ of the activity task.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- /Note:/ Consider using 'activityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaActivityId :: Lens.Lens' ScheduleActivityTaskDecisionAttributes Types.ActivityId
satdaActivityId = Lens.field @"activityId"
{-# DEPRECATED satdaActivityId "Use generic-lens or generic-optics with 'activityId' instead." #-}

-- | Data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the activity.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaControl :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Core.Maybe Types.Control)
satdaControl = Lens.field @"control"
{-# DEPRECATED satdaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | If set, specifies the maximum time before which a worker processing a task of this type must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. If the worker subsequently attempts to record a heartbeat or returns a result, it is ignored. This overrides the default heartbeat timeout specified when registering the activity type using 'RegisterActivityType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'heartbeatTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaHeartbeatTimeout :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Core.Maybe Types.HeartbeatTimeout)
satdaHeartbeatTimeout = Lens.field @"heartbeatTimeout"
{-# DEPRECATED satdaHeartbeatTimeout "Use generic-lens or generic-optics with 'heartbeatTimeout' instead." #-}

-- | The input provided to the activity task.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaInput :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Core.Maybe Types.Input)
satdaInput = Lens.field @"input"
{-# DEPRECATED satdaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The maximum duration for this activity task.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'scheduleToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaScheduleToCloseTimeout :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Core.Maybe Types.ScheduleToCloseTimeout)
satdaScheduleToCloseTimeout = Lens.field @"scheduleToCloseTimeout"
{-# DEPRECATED satdaScheduleToCloseTimeout "Use generic-lens or generic-optics with 'scheduleToCloseTimeout' instead." #-}

-- | If set, specifies the maximum duration the activity task can wait to be assigned to a worker. This overrides the default schedule-to-start timeout specified when registering the activity type using 'RegisterActivityType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'scheduleToStartTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaScheduleToStartTimeout :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Core.Maybe Types.ScheduleToStartTimeout)
satdaScheduleToStartTimeout = Lens.field @"scheduleToStartTimeout"
{-# DEPRECATED satdaScheduleToStartTimeout "Use generic-lens or generic-optics with 'scheduleToStartTimeout' instead." #-}

-- | If set, specifies the maximum duration a worker may take to process this activity task. This overrides the default start-to-close timeout specified when registering the activity type using 'RegisterActivityType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'startToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaStartToCloseTimeout :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Core.Maybe Types.StartToCloseTimeout)
satdaStartToCloseTimeout = Lens.field @"startToCloseTimeout"
{-# DEPRECATED satdaStartToCloseTimeout "Use generic-lens or generic-optics with 'startToCloseTimeout' instead." #-}

-- | If set, specifies the name of the task list in which to schedule the activity task. If not specified, the @defaultTaskList@ registered with the activity type is used.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaTaskList :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Core.Maybe Types.TaskList)
satdaTaskList = Lens.field @"taskList"
{-# DEPRECATED satdaTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | If set, specifies the priority with which the activity task is to be assigned to a worker. This overrides the defaultTaskPriority specified when registering the activity type using 'RegisterActivityType' . Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaTaskPriority :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Core.Maybe Types.TaskPriority)
satdaTaskPriority = Lens.field @"taskPriority"
{-# DEPRECATED satdaTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

instance Core.FromJSON ScheduleActivityTaskDecisionAttributes where
  toJSON ScheduleActivityTaskDecisionAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("activityType" Core..= activityType),
            Core.Just ("activityId" Core..= activityId),
            ("control" Core..=) Core.<$> control,
            ("heartbeatTimeout" Core..=) Core.<$> heartbeatTimeout,
            ("input" Core..=) Core.<$> input,
            ("scheduleToCloseTimeout" Core..=) Core.<$> scheduleToCloseTimeout,
            ("scheduleToStartTimeout" Core..=) Core.<$> scheduleToStartTimeout,
            ("startToCloseTimeout" Core..=) Core.<$> startToCloseTimeout,
            ("taskList" Core..=) Core.<$> taskList,
            ("taskPriority" Core..=) Core.<$> taskPriority
          ]
      )
