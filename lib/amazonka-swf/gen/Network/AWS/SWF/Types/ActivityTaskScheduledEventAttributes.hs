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
    atseaActivityType,
    atseaActivityId,
    atseaTaskList,
    atseaDecisionTaskCompletedEventId,
    atseaControl,
    atseaHeartbeatTimeout,
    atseaInput,
    atseaScheduleToCloseTimeout,
    atseaScheduleToStartTimeout,
    atseaStartToCloseTimeout,
    atseaTaskPriority,
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

-- | Provides the details of the @ActivityTaskScheduled@ event.
--
-- /See:/ 'mkActivityTaskScheduledEventAttributes' smart constructor.
data ActivityTaskScheduledEventAttributes = ActivityTaskScheduledEventAttributes'
  { -- | The type of the activity task.
    activityType :: Types.ActivityType,
    -- | The unique ID of the activity task.
    activityId :: Types.ActivityId,
    -- | The task list in which the activity task has been scheduled.
    taskList :: Types.TaskList,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision that resulted in the scheduling of this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Core.Integer,
    -- | Data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the activity.
    control :: Core.Maybe Types.Control,
    -- | The maximum time before which the worker processing this task must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. If the worker subsequently attempts to record a heartbeat or return a result, it is ignored.
    heartbeatTimeout :: Core.Maybe Types.HeartbeatTimeout,
    -- | The input provided to the activity task.
    input :: Core.Maybe Types.Input,
    -- | The maximum amount of time for this activity task.
    scheduleToCloseTimeout :: Core.Maybe Types.ScheduleToCloseTimeout,
    -- | The maximum amount of time the activity task can wait to be assigned to a worker.
    scheduleToStartTimeout :: Core.Maybe Types.ScheduleToStartTimeout,
    -- | The maximum amount of time a worker may take to process the activity task.
    startToCloseTimeout :: Core.Maybe Types.StartToCloseTimeout,
    -- | The priority to assign to the scheduled activity task. If set, this overrides any default priority value that was assigned when the activity type was registered.
    --
    -- Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
    -- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
    taskPriority :: Core.Maybe Types.TaskPriority
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivityTaskScheduledEventAttributes' value with any optional fields omitted.
mkActivityTaskScheduledEventAttributes ::
  -- | 'activityType'
  Types.ActivityType ->
  -- | 'activityId'
  Types.ActivityId ->
  -- | 'taskList'
  Types.TaskList ->
  -- | 'decisionTaskCompletedEventId'
  Core.Integer ->
  ActivityTaskScheduledEventAttributes
mkActivityTaskScheduledEventAttributes
  activityType
  activityId
  taskList
  decisionTaskCompletedEventId =
    ActivityTaskScheduledEventAttributes'
      { activityType,
        activityId,
        taskList,
        decisionTaskCompletedEventId,
        control = Core.Nothing,
        heartbeatTimeout = Core.Nothing,
        input = Core.Nothing,
        scheduleToCloseTimeout = Core.Nothing,
        scheduleToStartTimeout = Core.Nothing,
        startToCloseTimeout = Core.Nothing,
        taskPriority = Core.Nothing
      }

-- | The type of the activity task.
--
-- /Note:/ Consider using 'activityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaActivityType :: Lens.Lens' ActivityTaskScheduledEventAttributes Types.ActivityType
atseaActivityType = Lens.field @"activityType"
{-# DEPRECATED atseaActivityType "Use generic-lens or generic-optics with 'activityType' instead." #-}

-- | The unique ID of the activity task.
--
-- /Note:/ Consider using 'activityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaActivityId :: Lens.Lens' ActivityTaskScheduledEventAttributes Types.ActivityId
atseaActivityId = Lens.field @"activityId"
{-# DEPRECATED atseaActivityId "Use generic-lens or generic-optics with 'activityId' instead." #-}

-- | The task list in which the activity task has been scheduled.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaTaskList :: Lens.Lens' ActivityTaskScheduledEventAttributes Types.TaskList
atseaTaskList = Lens.field @"taskList"
{-# DEPRECATED atseaTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision that resulted in the scheduling of this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaDecisionTaskCompletedEventId :: Lens.Lens' ActivityTaskScheduledEventAttributes Core.Integer
atseaDecisionTaskCompletedEventId = Lens.field @"decisionTaskCompletedEventId"
{-# DEPRECATED atseaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

-- | Data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the activity.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaControl :: Lens.Lens' ActivityTaskScheduledEventAttributes (Core.Maybe Types.Control)
atseaControl = Lens.field @"control"
{-# DEPRECATED atseaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The maximum time before which the worker processing this task must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. If the worker subsequently attempts to record a heartbeat or return a result, it is ignored.
--
-- /Note:/ Consider using 'heartbeatTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaHeartbeatTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Core.Maybe Types.HeartbeatTimeout)
atseaHeartbeatTimeout = Lens.field @"heartbeatTimeout"
{-# DEPRECATED atseaHeartbeatTimeout "Use generic-lens or generic-optics with 'heartbeatTimeout' instead." #-}

-- | The input provided to the activity task.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaInput :: Lens.Lens' ActivityTaskScheduledEventAttributes (Core.Maybe Types.Input)
atseaInput = Lens.field @"input"
{-# DEPRECATED atseaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The maximum amount of time for this activity task.
--
-- /Note:/ Consider using 'scheduleToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaScheduleToCloseTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Core.Maybe Types.ScheduleToCloseTimeout)
atseaScheduleToCloseTimeout = Lens.field @"scheduleToCloseTimeout"
{-# DEPRECATED atseaScheduleToCloseTimeout "Use generic-lens or generic-optics with 'scheduleToCloseTimeout' instead." #-}

-- | The maximum amount of time the activity task can wait to be assigned to a worker.
--
-- /Note:/ Consider using 'scheduleToStartTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaScheduleToStartTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Core.Maybe Types.ScheduleToStartTimeout)
atseaScheduleToStartTimeout = Lens.field @"scheduleToStartTimeout"
{-# DEPRECATED atseaScheduleToStartTimeout "Use generic-lens or generic-optics with 'scheduleToStartTimeout' instead." #-}

-- | The maximum amount of time a worker may take to process the activity task.
--
-- /Note:/ Consider using 'startToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaStartToCloseTimeout :: Lens.Lens' ActivityTaskScheduledEventAttributes (Core.Maybe Types.StartToCloseTimeout)
atseaStartToCloseTimeout = Lens.field @"startToCloseTimeout"
{-# DEPRECATED atseaStartToCloseTimeout "Use generic-lens or generic-optics with 'startToCloseTimeout' instead." #-}

-- | The priority to assign to the scheduled activity task. If set, this overrides any default priority value that was assigned when the activity type was registered.
--
-- Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atseaTaskPriority :: Lens.Lens' ActivityTaskScheduledEventAttributes (Core.Maybe Types.TaskPriority)
atseaTaskPriority = Lens.field @"taskPriority"
{-# DEPRECATED atseaTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

instance Core.FromJSON ActivityTaskScheduledEventAttributes where
  parseJSON =
    Core.withObject "ActivityTaskScheduledEventAttributes" Core.$
      \x ->
        ActivityTaskScheduledEventAttributes'
          Core.<$> (x Core..: "activityType")
          Core.<*> (x Core..: "activityId")
          Core.<*> (x Core..: "taskList")
          Core.<*> (x Core..: "decisionTaskCompletedEventId")
          Core.<*> (x Core..:? "control")
          Core.<*> (x Core..:? "heartbeatTimeout")
          Core.<*> (x Core..:? "input")
          Core.<*> (x Core..:? "scheduleToCloseTimeout")
          Core.<*> (x Core..:? "scheduleToStartTimeout")
          Core.<*> (x Core..:? "startToCloseTimeout")
          Core.<*> (x Core..:? "taskPriority")
