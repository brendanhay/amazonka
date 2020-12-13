{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.HistoryEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.HistoryEvent
  ( HistoryEvent (..),

    -- * Smart constructor
    mkHistoryEvent,

    -- * Lenses
    heMapStateStartedEventDetails,
    heTaskSubmitFailedEventDetails,
    heTaskStartedEventDetails,
    heActivityStartedEventDetails,
    heTaskSubmittedEventDetails,
    heLambdaFunctionStartFailedEventDetails,
    heTaskStartFailedEventDetails,
    heStateExitedEventDetails,
    heLambdaFunctionSucceededEventDetails,
    heTaskSucceededEventDetails,
    heActivitySucceededEventDetails,
    heMapIterationAbortedEventDetails,
    heMapIterationSucceededEventDetails,
    heMapIterationStartedEventDetails,
    heLambdaFunctionTimedOutEventDetails,
    heTaskTimedOutEventDetails,
    heActivityTimedOutEventDetails,
    heExecutionFailedEventDetails,
    heExecutionAbortedEventDetails,
    heExecutionSucceededEventDetails,
    heLambdaFunctionScheduledEventDetails,
    heTaskScheduledEventDetails,
    heActivityScheduledEventDetails,
    heExecutionStartedEventDetails,
    heId,
    heActivityScheduleFailedEventDetails,
    heLambdaFunctionScheduleFailedEventDetails,
    heStateEnteredEventDetails,
    hePreviousEventId,
    heActivityFailedEventDetails,
    heTaskFailedEventDetails,
    heLambdaFunctionFailedEventDetails,
    heType,
    heExecutionTimedOutEventDetails,
    heMapIterationFailedEventDetails,
    heTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StepFunctions.Types.ActivityFailedEventDetails
import Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails
import Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails
import Network.AWS.StepFunctions.Types.ActivityStartedEventDetails
import Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails
import Network.AWS.StepFunctions.Types.ActivityTimedOutEventDetails
import Network.AWS.StepFunctions.Types.ExecutionAbortedEventDetails
import Network.AWS.StepFunctions.Types.ExecutionFailedEventDetails
import Network.AWS.StepFunctions.Types.ExecutionStartedEventDetails
import Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails
import Network.AWS.StepFunctions.Types.ExecutionTimedOutEventDetails
import Network.AWS.StepFunctions.Types.HistoryEventType
import Network.AWS.StepFunctions.Types.LambdaFunctionFailedEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionScheduledEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionStartFailedEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionTimedOutEventDetails
import Network.AWS.StepFunctions.Types.MapIterationEventDetails
import Network.AWS.StepFunctions.Types.MapStateStartedEventDetails
import Network.AWS.StepFunctions.Types.StateEnteredEventDetails
import Network.AWS.StepFunctions.Types.StateExitedEventDetails
import Network.AWS.StepFunctions.Types.TaskFailedEventDetails
import Network.AWS.StepFunctions.Types.TaskScheduledEventDetails
import Network.AWS.StepFunctions.Types.TaskStartFailedEventDetails
import Network.AWS.StepFunctions.Types.TaskStartedEventDetails
import Network.AWS.StepFunctions.Types.TaskSubmitFailedEventDetails
import Network.AWS.StepFunctions.Types.TaskSubmittedEventDetails
import Network.AWS.StepFunctions.Types.TaskSucceededEventDetails
import Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails

-- | Contains details about the events of an execution.
--
-- /See:/ 'mkHistoryEvent' smart constructor.
data HistoryEvent = HistoryEvent'
  { -- | Contains details about Map state that was started.
    mapStateStartedEventDetails :: Lude.Maybe MapStateStartedEventDetails,
    -- | Contains details about a task that where the submit failed.
    taskSubmitFailedEventDetails :: Lude.Maybe TaskSubmitFailedEventDetails,
    -- | Contains details about a task that was started.
    taskStartedEventDetails :: Lude.Maybe TaskStartedEventDetails,
    activityStartedEventDetails :: Lude.Maybe ActivityStartedEventDetails,
    -- | Contains details about a submitted task.
    taskSubmittedEventDetails :: Lude.Maybe TaskSubmittedEventDetails,
    -- | Contains details about a lambda function that failed to start during an execution.
    lambdaFunctionStartFailedEventDetails :: Lude.Maybe LambdaFunctionStartFailedEventDetails,
    -- | Contains details about a task that failed to start.
    taskStartFailedEventDetails :: Lude.Maybe TaskStartFailedEventDetails,
    stateExitedEventDetails :: Lude.Maybe StateExitedEventDetails,
    -- | Contains details about a lambda function that terminated successfully during an execution.
    lambdaFunctionSucceededEventDetails :: Lude.Maybe LambdaFunctionSucceededEventDetails,
    -- | Contains details about a task that succeeded.
    taskSucceededEventDetails :: Lude.Maybe TaskSucceededEventDetails,
    activitySucceededEventDetails :: Lude.Maybe ActivitySucceededEventDetails,
    -- | Contains details about an iteration of a Map state that was aborted.
    mapIterationAbortedEventDetails :: Lude.Maybe MapIterationEventDetails,
    -- | Contains details about an iteration of a Map state that succeeded.
    mapIterationSucceededEventDetails :: Lude.Maybe MapIterationEventDetails,
    -- | Contains details about an iteration of a Map state that was started.
    mapIterationStartedEventDetails :: Lude.Maybe MapIterationEventDetails,
    lambdaFunctionTimedOutEventDetails :: Lude.Maybe LambdaFunctionTimedOutEventDetails,
    -- | Contains details about a task that timed out.
    taskTimedOutEventDetails :: Lude.Maybe TaskTimedOutEventDetails,
    activityTimedOutEventDetails :: Lude.Maybe ActivityTimedOutEventDetails,
    executionFailedEventDetails :: Lude.Maybe ExecutionFailedEventDetails,
    executionAbortedEventDetails :: Lude.Maybe ExecutionAbortedEventDetails,
    executionSucceededEventDetails :: Lude.Maybe ExecutionSucceededEventDetails,
    lambdaFunctionScheduledEventDetails :: Lude.Maybe LambdaFunctionScheduledEventDetails,
    -- | Contains details about a task that was scheduled.
    taskScheduledEventDetails :: Lude.Maybe TaskScheduledEventDetails,
    activityScheduledEventDetails :: Lude.Maybe ActivityScheduledEventDetails,
    executionStartedEventDetails :: Lude.Maybe ExecutionStartedEventDetails,
    -- | The id of the event. Events are numbered sequentially, starting at one.
    id :: Lude.Integer,
    -- | Contains details about an activity schedule event that failed during an execution.
    activityScheduleFailedEventDetails :: Lude.Maybe ActivityScheduleFailedEventDetails,
    lambdaFunctionScheduleFailedEventDetails :: Lude.Maybe LambdaFunctionScheduleFailedEventDetails,
    stateEnteredEventDetails :: Lude.Maybe StateEnteredEventDetails,
    -- | The id of the previous event.
    previousEventId :: Lude.Maybe Lude.Integer,
    activityFailedEventDetails :: Lude.Maybe ActivityFailedEventDetails,
    -- | Contains details about the failure of a task.
    taskFailedEventDetails :: Lude.Maybe TaskFailedEventDetails,
    lambdaFunctionFailedEventDetails :: Lude.Maybe LambdaFunctionFailedEventDetails,
    -- | The type of the event.
    type' :: HistoryEventType,
    executionTimedOutEventDetails :: Lude.Maybe ExecutionTimedOutEventDetails,
    -- | Contains details about an iteration of a Map state that failed.
    mapIterationFailedEventDetails :: Lude.Maybe MapIterationEventDetails,
    -- | The date and time the event occurred.
    timestamp :: Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HistoryEvent' with the minimum fields required to make a request.
--
-- * 'mapStateStartedEventDetails' - Contains details about Map state that was started.
-- * 'taskSubmitFailedEventDetails' - Contains details about a task that where the submit failed.
-- * 'taskStartedEventDetails' - Contains details about a task that was started.
-- * 'activityStartedEventDetails' -
-- * 'taskSubmittedEventDetails' - Contains details about a submitted task.
-- * 'lambdaFunctionStartFailedEventDetails' - Contains details about a lambda function that failed to start during an execution.
-- * 'taskStartFailedEventDetails' - Contains details about a task that failed to start.
-- * 'stateExitedEventDetails' -
-- * 'lambdaFunctionSucceededEventDetails' - Contains details about a lambda function that terminated successfully during an execution.
-- * 'taskSucceededEventDetails' - Contains details about a task that succeeded.
-- * 'activitySucceededEventDetails' -
-- * 'mapIterationAbortedEventDetails' - Contains details about an iteration of a Map state that was aborted.
-- * 'mapIterationSucceededEventDetails' - Contains details about an iteration of a Map state that succeeded.
-- * 'mapIterationStartedEventDetails' - Contains details about an iteration of a Map state that was started.
-- * 'lambdaFunctionTimedOutEventDetails' -
-- * 'taskTimedOutEventDetails' - Contains details about a task that timed out.
-- * 'activityTimedOutEventDetails' -
-- * 'executionFailedEventDetails' -
-- * 'executionAbortedEventDetails' -
-- * 'executionSucceededEventDetails' -
-- * 'lambdaFunctionScheduledEventDetails' -
-- * 'taskScheduledEventDetails' - Contains details about a task that was scheduled.
-- * 'activityScheduledEventDetails' -
-- * 'executionStartedEventDetails' -
-- * 'id' - The id of the event. Events are numbered sequentially, starting at one.
-- * 'activityScheduleFailedEventDetails' - Contains details about an activity schedule event that failed during an execution.
-- * 'lambdaFunctionScheduleFailedEventDetails' -
-- * 'stateEnteredEventDetails' -
-- * 'previousEventId' - The id of the previous event.
-- * 'activityFailedEventDetails' -
-- * 'taskFailedEventDetails' - Contains details about the failure of a task.
-- * 'lambdaFunctionFailedEventDetails' -
-- * 'type'' - The type of the event.
-- * 'executionTimedOutEventDetails' -
-- * 'mapIterationFailedEventDetails' - Contains details about an iteration of a Map state that failed.
-- * 'timestamp' - The date and time the event occurred.
mkHistoryEvent ::
  -- | 'id'
  Lude.Integer ->
  -- | 'type''
  HistoryEventType ->
  -- | 'timestamp'
  Lude.Timestamp ->
  HistoryEvent
mkHistoryEvent pId_ pType_ pTimestamp_ =
  HistoryEvent'
    { mapStateStartedEventDetails = Lude.Nothing,
      taskSubmitFailedEventDetails = Lude.Nothing,
      taskStartedEventDetails = Lude.Nothing,
      activityStartedEventDetails = Lude.Nothing,
      taskSubmittedEventDetails = Lude.Nothing,
      lambdaFunctionStartFailedEventDetails = Lude.Nothing,
      taskStartFailedEventDetails = Lude.Nothing,
      stateExitedEventDetails = Lude.Nothing,
      lambdaFunctionSucceededEventDetails = Lude.Nothing,
      taskSucceededEventDetails = Lude.Nothing,
      activitySucceededEventDetails = Lude.Nothing,
      mapIterationAbortedEventDetails = Lude.Nothing,
      mapIterationSucceededEventDetails = Lude.Nothing,
      mapIterationStartedEventDetails = Lude.Nothing,
      lambdaFunctionTimedOutEventDetails = Lude.Nothing,
      taskTimedOutEventDetails = Lude.Nothing,
      activityTimedOutEventDetails = Lude.Nothing,
      executionFailedEventDetails = Lude.Nothing,
      executionAbortedEventDetails = Lude.Nothing,
      executionSucceededEventDetails = Lude.Nothing,
      lambdaFunctionScheduledEventDetails = Lude.Nothing,
      taskScheduledEventDetails = Lude.Nothing,
      activityScheduledEventDetails = Lude.Nothing,
      executionStartedEventDetails = Lude.Nothing,
      id = pId_,
      activityScheduleFailedEventDetails = Lude.Nothing,
      lambdaFunctionScheduleFailedEventDetails = Lude.Nothing,
      stateEnteredEventDetails = Lude.Nothing,
      previousEventId = Lude.Nothing,
      activityFailedEventDetails = Lude.Nothing,
      taskFailedEventDetails = Lude.Nothing,
      lambdaFunctionFailedEventDetails = Lude.Nothing,
      type' = pType_,
      executionTimedOutEventDetails = Lude.Nothing,
      mapIterationFailedEventDetails = Lude.Nothing,
      timestamp = pTimestamp_
    }

-- | Contains details about Map state that was started.
--
-- /Note:/ Consider using 'mapStateStartedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heMapStateStartedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe MapStateStartedEventDetails)
heMapStateStartedEventDetails = Lens.lens (mapStateStartedEventDetails :: HistoryEvent -> Lude.Maybe MapStateStartedEventDetails) (\s a -> s {mapStateStartedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heMapStateStartedEventDetails "Use generic-lens or generic-optics with 'mapStateStartedEventDetails' instead." #-}

-- | Contains details about a task that where the submit failed.
--
-- /Note:/ Consider using 'taskSubmitFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskSubmitFailedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe TaskSubmitFailedEventDetails)
heTaskSubmitFailedEventDetails = Lens.lens (taskSubmitFailedEventDetails :: HistoryEvent -> Lude.Maybe TaskSubmitFailedEventDetails) (\s a -> s {taskSubmitFailedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heTaskSubmitFailedEventDetails "Use generic-lens or generic-optics with 'taskSubmitFailedEventDetails' instead." #-}

-- | Contains details about a task that was started.
--
-- /Note:/ Consider using 'taskStartedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskStartedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe TaskStartedEventDetails)
heTaskStartedEventDetails = Lens.lens (taskStartedEventDetails :: HistoryEvent -> Lude.Maybe TaskStartedEventDetails) (\s a -> s {taskStartedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heTaskStartedEventDetails "Use generic-lens or generic-optics with 'taskStartedEventDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'activityStartedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityStartedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe ActivityStartedEventDetails)
heActivityStartedEventDetails = Lens.lens (activityStartedEventDetails :: HistoryEvent -> Lude.Maybe ActivityStartedEventDetails) (\s a -> s {activityStartedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heActivityStartedEventDetails "Use generic-lens or generic-optics with 'activityStartedEventDetails' instead." #-}

-- | Contains details about a submitted task.
--
-- /Note:/ Consider using 'taskSubmittedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskSubmittedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe TaskSubmittedEventDetails)
heTaskSubmittedEventDetails = Lens.lens (taskSubmittedEventDetails :: HistoryEvent -> Lude.Maybe TaskSubmittedEventDetails) (\s a -> s {taskSubmittedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heTaskSubmittedEventDetails "Use generic-lens or generic-optics with 'taskSubmittedEventDetails' instead." #-}

-- | Contains details about a lambda function that failed to start during an execution.
--
-- /Note:/ Consider using 'lambdaFunctionStartFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionStartFailedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe LambdaFunctionStartFailedEventDetails)
heLambdaFunctionStartFailedEventDetails = Lens.lens (lambdaFunctionStartFailedEventDetails :: HistoryEvent -> Lude.Maybe LambdaFunctionStartFailedEventDetails) (\s a -> s {lambdaFunctionStartFailedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heLambdaFunctionStartFailedEventDetails "Use generic-lens or generic-optics with 'lambdaFunctionStartFailedEventDetails' instead." #-}

-- | Contains details about a task that failed to start.
--
-- /Note:/ Consider using 'taskStartFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskStartFailedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe TaskStartFailedEventDetails)
heTaskStartFailedEventDetails = Lens.lens (taskStartFailedEventDetails :: HistoryEvent -> Lude.Maybe TaskStartFailedEventDetails) (\s a -> s {taskStartFailedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heTaskStartFailedEventDetails "Use generic-lens or generic-optics with 'taskStartFailedEventDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'stateExitedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heStateExitedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe StateExitedEventDetails)
heStateExitedEventDetails = Lens.lens (stateExitedEventDetails :: HistoryEvent -> Lude.Maybe StateExitedEventDetails) (\s a -> s {stateExitedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heStateExitedEventDetails "Use generic-lens or generic-optics with 'stateExitedEventDetails' instead." #-}

-- | Contains details about a lambda function that terminated successfully during an execution.
--
-- /Note:/ Consider using 'lambdaFunctionSucceededEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionSucceededEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe LambdaFunctionSucceededEventDetails)
heLambdaFunctionSucceededEventDetails = Lens.lens (lambdaFunctionSucceededEventDetails :: HistoryEvent -> Lude.Maybe LambdaFunctionSucceededEventDetails) (\s a -> s {lambdaFunctionSucceededEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heLambdaFunctionSucceededEventDetails "Use generic-lens or generic-optics with 'lambdaFunctionSucceededEventDetails' instead." #-}

-- | Contains details about a task that succeeded.
--
-- /Note:/ Consider using 'taskSucceededEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskSucceededEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe TaskSucceededEventDetails)
heTaskSucceededEventDetails = Lens.lens (taskSucceededEventDetails :: HistoryEvent -> Lude.Maybe TaskSucceededEventDetails) (\s a -> s {taskSucceededEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heTaskSucceededEventDetails "Use generic-lens or generic-optics with 'taskSucceededEventDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'activitySucceededEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivitySucceededEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe ActivitySucceededEventDetails)
heActivitySucceededEventDetails = Lens.lens (activitySucceededEventDetails :: HistoryEvent -> Lude.Maybe ActivitySucceededEventDetails) (\s a -> s {activitySucceededEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heActivitySucceededEventDetails "Use generic-lens or generic-optics with 'activitySucceededEventDetails' instead." #-}

-- | Contains details about an iteration of a Map state that was aborted.
--
-- /Note:/ Consider using 'mapIterationAbortedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heMapIterationAbortedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe MapIterationEventDetails)
heMapIterationAbortedEventDetails = Lens.lens (mapIterationAbortedEventDetails :: HistoryEvent -> Lude.Maybe MapIterationEventDetails) (\s a -> s {mapIterationAbortedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heMapIterationAbortedEventDetails "Use generic-lens or generic-optics with 'mapIterationAbortedEventDetails' instead." #-}

-- | Contains details about an iteration of a Map state that succeeded.
--
-- /Note:/ Consider using 'mapIterationSucceededEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heMapIterationSucceededEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe MapIterationEventDetails)
heMapIterationSucceededEventDetails = Lens.lens (mapIterationSucceededEventDetails :: HistoryEvent -> Lude.Maybe MapIterationEventDetails) (\s a -> s {mapIterationSucceededEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heMapIterationSucceededEventDetails "Use generic-lens or generic-optics with 'mapIterationSucceededEventDetails' instead." #-}

-- | Contains details about an iteration of a Map state that was started.
--
-- /Note:/ Consider using 'mapIterationStartedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heMapIterationStartedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe MapIterationEventDetails)
heMapIterationStartedEventDetails = Lens.lens (mapIterationStartedEventDetails :: HistoryEvent -> Lude.Maybe MapIterationEventDetails) (\s a -> s {mapIterationStartedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heMapIterationStartedEventDetails "Use generic-lens or generic-optics with 'mapIterationStartedEventDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lambdaFunctionTimedOutEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionTimedOutEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe LambdaFunctionTimedOutEventDetails)
heLambdaFunctionTimedOutEventDetails = Lens.lens (lambdaFunctionTimedOutEventDetails :: HistoryEvent -> Lude.Maybe LambdaFunctionTimedOutEventDetails) (\s a -> s {lambdaFunctionTimedOutEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heLambdaFunctionTimedOutEventDetails "Use generic-lens or generic-optics with 'lambdaFunctionTimedOutEventDetails' instead." #-}

-- | Contains details about a task that timed out.
--
-- /Note:/ Consider using 'taskTimedOutEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskTimedOutEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe TaskTimedOutEventDetails)
heTaskTimedOutEventDetails = Lens.lens (taskTimedOutEventDetails :: HistoryEvent -> Lude.Maybe TaskTimedOutEventDetails) (\s a -> s {taskTimedOutEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heTaskTimedOutEventDetails "Use generic-lens or generic-optics with 'taskTimedOutEventDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'activityTimedOutEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTimedOutEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe ActivityTimedOutEventDetails)
heActivityTimedOutEventDetails = Lens.lens (activityTimedOutEventDetails :: HistoryEvent -> Lude.Maybe ActivityTimedOutEventDetails) (\s a -> s {activityTimedOutEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heActivityTimedOutEventDetails "Use generic-lens or generic-optics with 'activityTimedOutEventDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'executionFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heExecutionFailedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe ExecutionFailedEventDetails)
heExecutionFailedEventDetails = Lens.lens (executionFailedEventDetails :: HistoryEvent -> Lude.Maybe ExecutionFailedEventDetails) (\s a -> s {executionFailedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heExecutionFailedEventDetails "Use generic-lens or generic-optics with 'executionFailedEventDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'executionAbortedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heExecutionAbortedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe ExecutionAbortedEventDetails)
heExecutionAbortedEventDetails = Lens.lens (executionAbortedEventDetails :: HistoryEvent -> Lude.Maybe ExecutionAbortedEventDetails) (\s a -> s {executionAbortedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heExecutionAbortedEventDetails "Use generic-lens or generic-optics with 'executionAbortedEventDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'executionSucceededEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heExecutionSucceededEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe ExecutionSucceededEventDetails)
heExecutionSucceededEventDetails = Lens.lens (executionSucceededEventDetails :: HistoryEvent -> Lude.Maybe ExecutionSucceededEventDetails) (\s a -> s {executionSucceededEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heExecutionSucceededEventDetails "Use generic-lens or generic-optics with 'executionSucceededEventDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lambdaFunctionScheduledEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionScheduledEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe LambdaFunctionScheduledEventDetails)
heLambdaFunctionScheduledEventDetails = Lens.lens (lambdaFunctionScheduledEventDetails :: HistoryEvent -> Lude.Maybe LambdaFunctionScheduledEventDetails) (\s a -> s {lambdaFunctionScheduledEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heLambdaFunctionScheduledEventDetails "Use generic-lens or generic-optics with 'lambdaFunctionScheduledEventDetails' instead." #-}

-- | Contains details about a task that was scheduled.
--
-- /Note:/ Consider using 'taskScheduledEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskScheduledEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe TaskScheduledEventDetails)
heTaskScheduledEventDetails = Lens.lens (taskScheduledEventDetails :: HistoryEvent -> Lude.Maybe TaskScheduledEventDetails) (\s a -> s {taskScheduledEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heTaskScheduledEventDetails "Use generic-lens or generic-optics with 'taskScheduledEventDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'activityScheduledEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityScheduledEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe ActivityScheduledEventDetails)
heActivityScheduledEventDetails = Lens.lens (activityScheduledEventDetails :: HistoryEvent -> Lude.Maybe ActivityScheduledEventDetails) (\s a -> s {activityScheduledEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heActivityScheduledEventDetails "Use generic-lens or generic-optics with 'activityScheduledEventDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'executionStartedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heExecutionStartedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe ExecutionStartedEventDetails)
heExecutionStartedEventDetails = Lens.lens (executionStartedEventDetails :: HistoryEvent -> Lude.Maybe ExecutionStartedEventDetails) (\s a -> s {executionStartedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heExecutionStartedEventDetails "Use generic-lens or generic-optics with 'executionStartedEventDetails' instead." #-}

-- | The id of the event. Events are numbered sequentially, starting at one.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heId :: Lens.Lens' HistoryEvent Lude.Integer
heId = Lens.lens (id :: HistoryEvent -> Lude.Integer) (\s a -> s {id = a} :: HistoryEvent)
{-# DEPRECATED heId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Contains details about an activity schedule event that failed during an execution.
--
-- /Note:/ Consider using 'activityScheduleFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityScheduleFailedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe ActivityScheduleFailedEventDetails)
heActivityScheduleFailedEventDetails = Lens.lens (activityScheduleFailedEventDetails :: HistoryEvent -> Lude.Maybe ActivityScheduleFailedEventDetails) (\s a -> s {activityScheduleFailedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heActivityScheduleFailedEventDetails "Use generic-lens or generic-optics with 'activityScheduleFailedEventDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lambdaFunctionScheduleFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionScheduleFailedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe LambdaFunctionScheduleFailedEventDetails)
heLambdaFunctionScheduleFailedEventDetails = Lens.lens (lambdaFunctionScheduleFailedEventDetails :: HistoryEvent -> Lude.Maybe LambdaFunctionScheduleFailedEventDetails) (\s a -> s {lambdaFunctionScheduleFailedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heLambdaFunctionScheduleFailedEventDetails "Use generic-lens or generic-optics with 'lambdaFunctionScheduleFailedEventDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'stateEnteredEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heStateEnteredEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe StateEnteredEventDetails)
heStateEnteredEventDetails = Lens.lens (stateEnteredEventDetails :: HistoryEvent -> Lude.Maybe StateEnteredEventDetails) (\s a -> s {stateEnteredEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heStateEnteredEventDetails "Use generic-lens or generic-optics with 'stateEnteredEventDetails' instead." #-}

-- | The id of the previous event.
--
-- /Note:/ Consider using 'previousEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hePreviousEventId :: Lens.Lens' HistoryEvent (Lude.Maybe Lude.Integer)
hePreviousEventId = Lens.lens (previousEventId :: HistoryEvent -> Lude.Maybe Lude.Integer) (\s a -> s {previousEventId = a} :: HistoryEvent)
{-# DEPRECATED hePreviousEventId "Use generic-lens or generic-optics with 'previousEventId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'activityFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityFailedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe ActivityFailedEventDetails)
heActivityFailedEventDetails = Lens.lens (activityFailedEventDetails :: HistoryEvent -> Lude.Maybe ActivityFailedEventDetails) (\s a -> s {activityFailedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heActivityFailedEventDetails "Use generic-lens or generic-optics with 'activityFailedEventDetails' instead." #-}

-- | Contains details about the failure of a task.
--
-- /Note:/ Consider using 'taskFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskFailedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe TaskFailedEventDetails)
heTaskFailedEventDetails = Lens.lens (taskFailedEventDetails :: HistoryEvent -> Lude.Maybe TaskFailedEventDetails) (\s a -> s {taskFailedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heTaskFailedEventDetails "Use generic-lens or generic-optics with 'taskFailedEventDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lambdaFunctionFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionFailedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe LambdaFunctionFailedEventDetails)
heLambdaFunctionFailedEventDetails = Lens.lens (lambdaFunctionFailedEventDetails :: HistoryEvent -> Lude.Maybe LambdaFunctionFailedEventDetails) (\s a -> s {lambdaFunctionFailedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heLambdaFunctionFailedEventDetails "Use generic-lens or generic-optics with 'lambdaFunctionFailedEventDetails' instead." #-}

-- | The type of the event.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heType :: Lens.Lens' HistoryEvent HistoryEventType
heType = Lens.lens (type' :: HistoryEvent -> HistoryEventType) (\s a -> s {type' = a} :: HistoryEvent)
{-# DEPRECATED heType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'executionTimedOutEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heExecutionTimedOutEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe ExecutionTimedOutEventDetails)
heExecutionTimedOutEventDetails = Lens.lens (executionTimedOutEventDetails :: HistoryEvent -> Lude.Maybe ExecutionTimedOutEventDetails) (\s a -> s {executionTimedOutEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heExecutionTimedOutEventDetails "Use generic-lens or generic-optics with 'executionTimedOutEventDetails' instead." #-}

-- | Contains details about an iteration of a Map state that failed.
--
-- /Note:/ Consider using 'mapIterationFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heMapIterationFailedEventDetails :: Lens.Lens' HistoryEvent (Lude.Maybe MapIterationEventDetails)
heMapIterationFailedEventDetails = Lens.lens (mapIterationFailedEventDetails :: HistoryEvent -> Lude.Maybe MapIterationEventDetails) (\s a -> s {mapIterationFailedEventDetails = a} :: HistoryEvent)
{-# DEPRECATED heMapIterationFailedEventDetails "Use generic-lens or generic-optics with 'mapIterationFailedEventDetails' instead." #-}

-- | The date and time the event occurred.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTimestamp :: Lens.Lens' HistoryEvent Lude.Timestamp
heTimestamp = Lens.lens (timestamp :: HistoryEvent -> Lude.Timestamp) (\s a -> s {timestamp = a} :: HistoryEvent)
{-# DEPRECATED heTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON HistoryEvent where
  parseJSON =
    Lude.withObject
      "HistoryEvent"
      ( \x ->
          HistoryEvent'
            Lude.<$> (x Lude..:? "mapStateStartedEventDetails")
            Lude.<*> (x Lude..:? "taskSubmitFailedEventDetails")
            Lude.<*> (x Lude..:? "taskStartedEventDetails")
            Lude.<*> (x Lude..:? "activityStartedEventDetails")
            Lude.<*> (x Lude..:? "taskSubmittedEventDetails")
            Lude.<*> (x Lude..:? "lambdaFunctionStartFailedEventDetails")
            Lude.<*> (x Lude..:? "taskStartFailedEventDetails")
            Lude.<*> (x Lude..:? "stateExitedEventDetails")
            Lude.<*> (x Lude..:? "lambdaFunctionSucceededEventDetails")
            Lude.<*> (x Lude..:? "taskSucceededEventDetails")
            Lude.<*> (x Lude..:? "activitySucceededEventDetails")
            Lude.<*> (x Lude..:? "mapIterationAbortedEventDetails")
            Lude.<*> (x Lude..:? "mapIterationSucceededEventDetails")
            Lude.<*> (x Lude..:? "mapIterationStartedEventDetails")
            Lude.<*> (x Lude..:? "lambdaFunctionTimedOutEventDetails")
            Lude.<*> (x Lude..:? "taskTimedOutEventDetails")
            Lude.<*> (x Lude..:? "activityTimedOutEventDetails")
            Lude.<*> (x Lude..:? "executionFailedEventDetails")
            Lude.<*> (x Lude..:? "executionAbortedEventDetails")
            Lude.<*> (x Lude..:? "executionSucceededEventDetails")
            Lude.<*> (x Lude..:? "lambdaFunctionScheduledEventDetails")
            Lude.<*> (x Lude..:? "taskScheduledEventDetails")
            Lude.<*> (x Lude..:? "activityScheduledEventDetails")
            Lude.<*> (x Lude..:? "executionStartedEventDetails")
            Lude.<*> (x Lude..: "id")
            Lude.<*> (x Lude..:? "activityScheduleFailedEventDetails")
            Lude.<*> (x Lude..:? "lambdaFunctionScheduleFailedEventDetails")
            Lude.<*> (x Lude..:? "stateEnteredEventDetails")
            Lude.<*> (x Lude..:? "previousEventId")
            Lude.<*> (x Lude..:? "activityFailedEventDetails")
            Lude.<*> (x Lude..:? "taskFailedEventDetails")
            Lude.<*> (x Lude..:? "lambdaFunctionFailedEventDetails")
            Lude.<*> (x Lude..: "type")
            Lude.<*> (x Lude..:? "executionTimedOutEventDetails")
            Lude.<*> (x Lude..:? "mapIterationFailedEventDetails")
            Lude.<*> (x Lude..: "timestamp")
      )
