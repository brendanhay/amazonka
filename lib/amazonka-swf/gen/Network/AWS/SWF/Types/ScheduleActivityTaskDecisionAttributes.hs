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
    satdaControl,
    satdaHeartbeatTimeout,
    satdaScheduleToCloseTimeout,
    satdaInput,
    satdaTaskList,
    satdaTaskPriority,
    satdaScheduleToStartTimeout,
    satdaStartToCloseTimeout,
    satdaActivityType,
    satdaActivityId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.ActivityType
import Network.AWS.SWF.Types.TaskList

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
  { control ::
      Lude.Maybe
        Lude.Text,
    heartbeatTimeout ::
      Lude.Maybe
        Lude.Text,
    scheduleToCloseTimeout ::
      Lude.Maybe
        Lude.Text,
    input ::
      Lude.Maybe
        Lude.Text,
    taskList ::
      Lude.Maybe
        TaskList,
    taskPriority ::
      Lude.Maybe
        Lude.Text,
    scheduleToStartTimeout ::
      Lude.Maybe
        Lude.Text,
    startToCloseTimeout ::
      Lude.Maybe
        Lude.Text,
    activityType ::
      ActivityType,
    activityId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduleActivityTaskDecisionAttributes' with the minimum fields required to make a request.
--
-- * 'activityId' - The @activityId@ of the activity task.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
-- * 'activityType' - The type of the activity task to schedule.
-- * 'control' - Data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the activity.
-- * 'heartbeatTimeout' - If set, specifies the maximum time before which a worker processing a task of this type must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. If the worker subsequently attempts to record a heartbeat or returns a result, it is ignored. This overrides the default heartbeat timeout specified when registering the activity type using 'RegisterActivityType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'input' - The input provided to the activity task.
-- * 'scheduleToCloseTimeout' - The maximum duration for this activity task.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'scheduleToStartTimeout' - If set, specifies the maximum duration the activity task can wait to be assigned to a worker. This overrides the default schedule-to-start timeout specified when registering the activity type using 'RegisterActivityType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'startToCloseTimeout' - If set, specifies the maximum duration a worker may take to process this activity task. This overrides the default start-to-close timeout specified when registering the activity type using 'RegisterActivityType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'taskList' - If set, specifies the name of the task list in which to schedule the activity task. If not specified, the @defaultTaskList@ registered with the activity type is used.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
-- * 'taskPriority' - If set, specifies the priority with which the activity task is to be assigned to a worker. This overrides the defaultTaskPriority specified when registering the activity type using 'RegisterActivityType' . Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
mkScheduleActivityTaskDecisionAttributes ::
  -- | 'activityType'
  ActivityType ->
  -- | 'activityId'
  Lude.Text ->
  ScheduleActivityTaskDecisionAttributes
mkScheduleActivityTaskDecisionAttributes
  pActivityType_
  pActivityId_ =
    ScheduleActivityTaskDecisionAttributes'
      { control = Lude.Nothing,
        heartbeatTimeout = Lude.Nothing,
        scheduleToCloseTimeout = Lude.Nothing,
        input = Lude.Nothing,
        taskList = Lude.Nothing,
        taskPriority = Lude.Nothing,
        scheduleToStartTimeout = Lude.Nothing,
        startToCloseTimeout = Lude.Nothing,
        activityType = pActivityType_,
        activityId = pActivityId_
      }

-- | Data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the activity.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaControl :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Lude.Maybe Lude.Text)
satdaControl = Lens.lens (control :: ScheduleActivityTaskDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {control = a} :: ScheduleActivityTaskDecisionAttributes)
{-# DEPRECATED satdaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | If set, specifies the maximum time before which a worker processing a task of this type must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. If the worker subsequently attempts to record a heartbeat or returns a result, it is ignored. This overrides the default heartbeat timeout specified when registering the activity type using 'RegisterActivityType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'heartbeatTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaHeartbeatTimeout :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Lude.Maybe Lude.Text)
satdaHeartbeatTimeout = Lens.lens (heartbeatTimeout :: ScheduleActivityTaskDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {heartbeatTimeout = a} :: ScheduleActivityTaskDecisionAttributes)
{-# DEPRECATED satdaHeartbeatTimeout "Use generic-lens or generic-optics with 'heartbeatTimeout' instead." #-}

-- | The maximum duration for this activity task.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'scheduleToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaScheduleToCloseTimeout :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Lude.Maybe Lude.Text)
satdaScheduleToCloseTimeout = Lens.lens (scheduleToCloseTimeout :: ScheduleActivityTaskDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {scheduleToCloseTimeout = a} :: ScheduleActivityTaskDecisionAttributes)
{-# DEPRECATED satdaScheduleToCloseTimeout "Use generic-lens or generic-optics with 'scheduleToCloseTimeout' instead." #-}

-- | The input provided to the activity task.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaInput :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Lude.Maybe Lude.Text)
satdaInput = Lens.lens (input :: ScheduleActivityTaskDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: ScheduleActivityTaskDecisionAttributes)
{-# DEPRECATED satdaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | If set, specifies the name of the task list in which to schedule the activity task. If not specified, the @defaultTaskList@ registered with the activity type is used.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaTaskList :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Lude.Maybe TaskList)
satdaTaskList = Lens.lens (taskList :: ScheduleActivityTaskDecisionAttributes -> Lude.Maybe TaskList) (\s a -> s {taskList = a} :: ScheduleActivityTaskDecisionAttributes)
{-# DEPRECATED satdaTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | If set, specifies the priority with which the activity task is to be assigned to a worker. This overrides the defaultTaskPriority specified when registering the activity type using 'RegisterActivityType' . Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaTaskPriority :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Lude.Maybe Lude.Text)
satdaTaskPriority = Lens.lens (taskPriority :: ScheduleActivityTaskDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {taskPriority = a} :: ScheduleActivityTaskDecisionAttributes)
{-# DEPRECATED satdaTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

-- | If set, specifies the maximum duration the activity task can wait to be assigned to a worker. This overrides the default schedule-to-start timeout specified when registering the activity type using 'RegisterActivityType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'scheduleToStartTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaScheduleToStartTimeout :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Lude.Maybe Lude.Text)
satdaScheduleToStartTimeout = Lens.lens (scheduleToStartTimeout :: ScheduleActivityTaskDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {scheduleToStartTimeout = a} :: ScheduleActivityTaskDecisionAttributes)
{-# DEPRECATED satdaScheduleToStartTimeout "Use generic-lens or generic-optics with 'scheduleToStartTimeout' instead." #-}

-- | If set, specifies the maximum duration a worker may take to process this activity task. This overrides the default start-to-close timeout specified when registering the activity type using 'RegisterActivityType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'startToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaStartToCloseTimeout :: Lens.Lens' ScheduleActivityTaskDecisionAttributes (Lude.Maybe Lude.Text)
satdaStartToCloseTimeout = Lens.lens (startToCloseTimeout :: ScheduleActivityTaskDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {startToCloseTimeout = a} :: ScheduleActivityTaskDecisionAttributes)
{-# DEPRECATED satdaStartToCloseTimeout "Use generic-lens or generic-optics with 'startToCloseTimeout' instead." #-}

-- | The type of the activity task to schedule.
--
-- /Note:/ Consider using 'activityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaActivityType :: Lens.Lens' ScheduleActivityTaskDecisionAttributes ActivityType
satdaActivityType = Lens.lens (activityType :: ScheduleActivityTaskDecisionAttributes -> ActivityType) (\s a -> s {activityType = a} :: ScheduleActivityTaskDecisionAttributes)
{-# DEPRECATED satdaActivityType "Use generic-lens or generic-optics with 'activityType' instead." #-}

-- | The @activityId@ of the activity task.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- /Note:/ Consider using 'activityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
satdaActivityId :: Lens.Lens' ScheduleActivityTaskDecisionAttributes Lude.Text
satdaActivityId = Lens.lens (activityId :: ScheduleActivityTaskDecisionAttributes -> Lude.Text) (\s a -> s {activityId = a} :: ScheduleActivityTaskDecisionAttributes)
{-# DEPRECATED satdaActivityId "Use generic-lens or generic-optics with 'activityId' instead." #-}

instance Lude.ToJSON ScheduleActivityTaskDecisionAttributes where
  toJSON ScheduleActivityTaskDecisionAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("control" Lude..=) Lude.<$> control,
            ("heartbeatTimeout" Lude..=) Lude.<$> heartbeatTimeout,
            ("scheduleToCloseTimeout" Lude..=) Lude.<$> scheduleToCloseTimeout,
            ("input" Lude..=) Lude.<$> input,
            ("taskList" Lude..=) Lude.<$> taskList,
            ("taskPriority" Lude..=) Lude.<$> taskPriority,
            ("scheduleToStartTimeout" Lude..=) Lude.<$> scheduleToStartTimeout,
            ("startToCloseTimeout" Lude..=) Lude.<$> startToCloseTimeout,
            Lude.Just ("activityType" Lude..= activityType),
            Lude.Just ("activityId" Lude..= activityId)
          ]
      )
