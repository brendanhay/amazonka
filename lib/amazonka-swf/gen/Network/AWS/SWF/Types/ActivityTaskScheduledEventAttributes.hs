{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskScheduledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskScheduledEventAttributes
  ( ActivityTaskScheduledEventAttributes (..),

    -- * Smart constructor
    mkActivityTaskScheduledEventAttributes,

    -- * Lenses
    atseaControl,
    atseaHeartbeatTimeout,
    atseaScheduleToCloseTimeout,
    atseaInput,
    atseaTaskPriority,
    atseaScheduleToStartTimeout,
    atseaStartToCloseTimeout,
    atseaActivityType,
    atseaActivityId,
    atseaTaskList,
    atseaDecisionTaskCompletedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.ActivityType
import Network.AWS.SWF.Types.TaskList

-- | Provides the details of the @ActivityTaskScheduled@ event.
--
-- /See:/ 'mkActivityTaskScheduledEventAttributes' smart constructor.
data ActivityTaskScheduledEventAttributes = ActivityTaskScheduledEventAttributes'
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
      Lude.Text,
    taskList ::
      TaskList,
    decisionTaskCompletedEventId ::
      Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityTaskScheduledEventAttributes' with the minimum fields required to make a request.
--
-- * 'activityId' - The unique ID of the activity task.
-- * 'activityType' - The type of the activity task.
-- * 'control' - Data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the activity.
-- * 'decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision that resulted in the scheduling of this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'heartbeatTimeout' - The maximum time before which the worker processing this task must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. If the worker subsequently attempts to record a heartbeat or return a result, it is ignored.
-- * 'input' - The input provided to the activity task.
-- * 'scheduleToCloseTimeout' - The maximum amount of time for this activity task.
-- * 'scheduleToStartTimeout' - The maximum amount of time the activity task can wait to be assigned to a worker.
-- * 'startToCloseTimeout' - The maximum amount of time a worker may take to process the activity task.
-- * 'taskList' - The task list in which the activity task has been scheduled.
-- * 'taskPriority' - The priority to assign to the scheduled activity task. If set, this overrides any default priority value that was assigned when the activity type was registered.
--
-- Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
mkActivityTaskScheduledEventAttributes ::
  -- | 'activityType'
  ActivityType ->
  -- | 'activityId'
  Lude.Text ->
  -- | 'taskList'
  TaskList ->
  -- | 'decisionTaskCompletedEventId'
  Lude.Integer ->
  ActivityTaskScheduledEventAttributes
mkActivityTaskScheduledEventAttributes
  pActivityType_
  pActivityId_
  pTaskList_
  pDecisionTaskCompletedEventId_ =
    ActivityTaskScheduledEventAttributes'
      { control = Lude.Nothing,
        heartbeatTimeout = Lude.Nothing,
        scheduleToCloseTimeout = Lude.Nothing,
        input = Lude.Nothing,
        taskPriority = Lude.Nothing,
        scheduleToStartTimeout = Lude.Nothing,
        startToCloseTimeout = Lude.Nothing,
        activityType = pActivityType_,
        activityId = pActivityId_,
        taskList = pTaskList_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the activity.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaControl :: Lens.Lens' ActivityTaskScheduledEventAttributes (Lude.Maybe Lude.Text)
atseaControl = Lens.lens (control :: ActivityTaskScheduledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {control = a} :: ActivityTaskScheduledEventAttributes)
{-# DEPRECATED atseaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The maximum time before which the worker processing this task must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. If the worker subsequently attempts to record a heartbeat or return a result, it is ignored.
--
-- /Note:/ Consider using 'heartbeatTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaHeartbeatTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Lude.Maybe Lude.Text)
atseaHeartbeatTimeout = Lens.lens (heartbeatTimeout :: ActivityTaskScheduledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {heartbeatTimeout = a} :: ActivityTaskScheduledEventAttributes)
{-# DEPRECATED atseaHeartbeatTimeout "Use generic-lens or generic-optics with 'heartbeatTimeout' instead." #-}

-- | The maximum amount of time for this activity task.
--
-- /Note:/ Consider using 'scheduleToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaScheduleToCloseTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Lude.Maybe Lude.Text)
atseaScheduleToCloseTimeout = Lens.lens (scheduleToCloseTimeout :: ActivityTaskScheduledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {scheduleToCloseTimeout = a} :: ActivityTaskScheduledEventAttributes)
{-# DEPRECATED atseaScheduleToCloseTimeout "Use generic-lens or generic-optics with 'scheduleToCloseTimeout' instead." #-}

-- | The input provided to the activity task.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaInput :: Lens.Lens' ActivityTaskScheduledEventAttributes (Lude.Maybe Lude.Text)
atseaInput = Lens.lens (input :: ActivityTaskScheduledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: ActivityTaskScheduledEventAttributes)
{-# DEPRECATED atseaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The priority to assign to the scheduled activity task. If set, this overrides any default priority value that was assigned when the activity type was registered.
--
-- Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaTaskPriority :: Lens.Lens' ActivityTaskScheduledEventAttributes (Lude.Maybe Lude.Text)
atseaTaskPriority = Lens.lens (taskPriority :: ActivityTaskScheduledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {taskPriority = a} :: ActivityTaskScheduledEventAttributes)
{-# DEPRECATED atseaTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

-- | The maximum amount of time the activity task can wait to be assigned to a worker.
--
-- /Note:/ Consider using 'scheduleToStartTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaScheduleToStartTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Lude.Maybe Lude.Text)
atseaScheduleToStartTimeout = Lens.lens (scheduleToStartTimeout :: ActivityTaskScheduledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {scheduleToStartTimeout = a} :: ActivityTaskScheduledEventAttributes)
{-# DEPRECATED atseaScheduleToStartTimeout "Use generic-lens or generic-optics with 'scheduleToStartTimeout' instead." #-}

-- | The maximum amount of time a worker may take to process the activity task.
--
-- /Note:/ Consider using 'startToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaStartToCloseTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Lude.Maybe Lude.Text)
atseaStartToCloseTimeout = Lens.lens (startToCloseTimeout :: ActivityTaskScheduledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {startToCloseTimeout = a} :: ActivityTaskScheduledEventAttributes)
{-# DEPRECATED atseaStartToCloseTimeout "Use generic-lens or generic-optics with 'startToCloseTimeout' instead." #-}

-- | The type of the activity task.
--
-- /Note:/ Consider using 'activityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaActivityType :: Lens.Lens' ActivityTaskScheduledEventAttributes ActivityType
atseaActivityType = Lens.lens (activityType :: ActivityTaskScheduledEventAttributes -> ActivityType) (\s a -> s {activityType = a} :: ActivityTaskScheduledEventAttributes)
{-# DEPRECATED atseaActivityType "Use generic-lens or generic-optics with 'activityType' instead." #-}

-- | The unique ID of the activity task.
--
-- /Note:/ Consider using 'activityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaActivityId :: Lens.Lens' ActivityTaskScheduledEventAttributes Lude.Text
atseaActivityId = Lens.lens (activityId :: ActivityTaskScheduledEventAttributes -> Lude.Text) (\s a -> s {activityId = a} :: ActivityTaskScheduledEventAttributes)
{-# DEPRECATED atseaActivityId "Use generic-lens or generic-optics with 'activityId' instead." #-}

-- | The task list in which the activity task has been scheduled.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaTaskList :: Lens.Lens' ActivityTaskScheduledEventAttributes TaskList
atseaTaskList = Lens.lens (taskList :: ActivityTaskScheduledEventAttributes -> TaskList) (\s a -> s {taskList = a} :: ActivityTaskScheduledEventAttributes)
{-# DEPRECATED atseaTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision that resulted in the scheduling of this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaDecisionTaskCompletedEventId :: Lens.Lens' ActivityTaskScheduledEventAttributes Lude.Integer
atseaDecisionTaskCompletedEventId = Lens.lens (decisionTaskCompletedEventId :: ActivityTaskScheduledEventAttributes -> Lude.Integer) (\s a -> s {decisionTaskCompletedEventId = a} :: ActivityTaskScheduledEventAttributes)
{-# DEPRECATED atseaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

instance Lude.FromJSON ActivityTaskScheduledEventAttributes where
  parseJSON =
    Lude.withObject
      "ActivityTaskScheduledEventAttributes"
      ( \x ->
          ActivityTaskScheduledEventAttributes'
            Lude.<$> (x Lude..:? "control")
            Lude.<*> (x Lude..:? "heartbeatTimeout")
            Lude.<*> (x Lude..:? "scheduleToCloseTimeout")
            Lude.<*> (x Lude..:? "input")
            Lude.<*> (x Lude..:? "taskPriority")
            Lude.<*> (x Lude..:? "scheduleToStartTimeout")
            Lude.<*> (x Lude..:? "startToCloseTimeout")
            Lude.<*> (x Lude..: "activityType")
            Lude.<*> (x Lude..: "activityId")
            Lude.<*> (x Lude..: "taskList")
            Lude.<*> (x Lude..: "decisionTaskCompletedEventId")
      )
