{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.HistoryEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.HistoryEvent
  ( HistoryEvent (..)
  -- * Smart constructor
  , mkHistoryEvent
  -- * Lenses
  , heTimestamp
  , heType
  , heId
  , heActivityFailedEventDetails
  , heActivityScheduleFailedEventDetails
  , heActivityScheduledEventDetails
  , heActivityStartedEventDetails
  , heActivitySucceededEventDetails
  , heActivityTimedOutEventDetails
  , heExecutionAbortedEventDetails
  , heExecutionFailedEventDetails
  , heExecutionStartedEventDetails
  , heExecutionSucceededEventDetails
  , heExecutionTimedOutEventDetails
  , heLambdaFunctionFailedEventDetails
  , heLambdaFunctionScheduleFailedEventDetails
  , heLambdaFunctionScheduledEventDetails
  , heLambdaFunctionStartFailedEventDetails
  , heLambdaFunctionSucceededEventDetails
  , heLambdaFunctionTimedOutEventDetails
  , heMapIterationAbortedEventDetails
  , heMapIterationFailedEventDetails
  , heMapIterationStartedEventDetails
  , heMapIterationSucceededEventDetails
  , heMapStateStartedEventDetails
  , hePreviousEventId
  , heStateEnteredEventDetails
  , heStateExitedEventDetails
  , heTaskFailedEventDetails
  , heTaskScheduledEventDetails
  , heTaskStartFailedEventDetails
  , heTaskStartedEventDetails
  , heTaskSubmitFailedEventDetails
  , heTaskSubmittedEventDetails
  , heTaskSucceededEventDetails
  , heTaskTimedOutEventDetails
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.ActivityFailedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.ActivityStartedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.ActivityTimedOutEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.ExecutionAbortedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.ExecutionFailedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.ExecutionStartedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.ExecutionTimedOutEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.HistoryEventType as Types
import qualified Network.AWS.StepFunctions.Types.LambdaFunctionFailedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.LambdaFunctionScheduledEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.LambdaFunctionStartFailedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.LambdaFunctionTimedOutEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.MapIterationEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.MapStateStartedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.StateEnteredEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.StateExitedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.TaskFailedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.TaskScheduledEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.TaskStartFailedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.TaskStartedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.TaskSubmitFailedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.TaskSubmittedEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.TaskSucceededEventDetails as Types
import qualified Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails as Types

-- | Contains details about the events of an execution.
--
-- /See:/ 'mkHistoryEvent' smart constructor.
data HistoryEvent = HistoryEvent'
  { timestamp :: Core.NominalDiffTime
    -- ^ The date and time the event occurred.
  , type' :: Types.HistoryEventType
    -- ^ The type of the event.
  , id :: Core.Integer
    -- ^ The id of the event. Events are numbered sequentially, starting at one.
  , activityFailedEventDetails :: Core.Maybe Types.ActivityFailedEventDetails
  , activityScheduleFailedEventDetails :: Core.Maybe Types.ActivityScheduleFailedEventDetails
    -- ^ Contains details about an activity schedule event that failed during an execution.
  , activityScheduledEventDetails :: Core.Maybe Types.ActivityScheduledEventDetails
  , activityStartedEventDetails :: Core.Maybe Types.ActivityStartedEventDetails
  , activitySucceededEventDetails :: Core.Maybe Types.ActivitySucceededEventDetails
  , activityTimedOutEventDetails :: Core.Maybe Types.ActivityTimedOutEventDetails
  , executionAbortedEventDetails :: Core.Maybe Types.ExecutionAbortedEventDetails
  , executionFailedEventDetails :: Core.Maybe Types.ExecutionFailedEventDetails
  , executionStartedEventDetails :: Core.Maybe Types.ExecutionStartedEventDetails
  , executionSucceededEventDetails :: Core.Maybe Types.ExecutionSucceededEventDetails
  , executionTimedOutEventDetails :: Core.Maybe Types.ExecutionTimedOutEventDetails
  , lambdaFunctionFailedEventDetails :: Core.Maybe Types.LambdaFunctionFailedEventDetails
  , lambdaFunctionScheduleFailedEventDetails :: Core.Maybe Types.LambdaFunctionScheduleFailedEventDetails
  , lambdaFunctionScheduledEventDetails :: Core.Maybe Types.LambdaFunctionScheduledEventDetails
  , lambdaFunctionStartFailedEventDetails :: Core.Maybe Types.LambdaFunctionStartFailedEventDetails
    -- ^ Contains details about a lambda function that failed to start during an execution.
  , lambdaFunctionSucceededEventDetails :: Core.Maybe Types.LambdaFunctionSucceededEventDetails
    -- ^ Contains details about a lambda function that terminated successfully during an execution.
  , lambdaFunctionTimedOutEventDetails :: Core.Maybe Types.LambdaFunctionTimedOutEventDetails
  , mapIterationAbortedEventDetails :: Core.Maybe Types.MapIterationEventDetails
    -- ^ Contains details about an iteration of a Map state that was aborted.
  , mapIterationFailedEventDetails :: Core.Maybe Types.MapIterationEventDetails
    -- ^ Contains details about an iteration of a Map state that failed.
  , mapIterationStartedEventDetails :: Core.Maybe Types.MapIterationEventDetails
    -- ^ Contains details about an iteration of a Map state that was started.
  , mapIterationSucceededEventDetails :: Core.Maybe Types.MapIterationEventDetails
    -- ^ Contains details about an iteration of a Map state that succeeded.
  , mapStateStartedEventDetails :: Core.Maybe Types.MapStateStartedEventDetails
    -- ^ Contains details about Map state that was started.
  , previousEventId :: Core.Maybe Core.Integer
    -- ^ The id of the previous event.
  , stateEnteredEventDetails :: Core.Maybe Types.StateEnteredEventDetails
  , stateExitedEventDetails :: Core.Maybe Types.StateExitedEventDetails
  , taskFailedEventDetails :: Core.Maybe Types.TaskFailedEventDetails
    -- ^ Contains details about the failure of a task.
  , taskScheduledEventDetails :: Core.Maybe Types.TaskScheduledEventDetails
    -- ^ Contains details about a task that was scheduled.
  , taskStartFailedEventDetails :: Core.Maybe Types.TaskStartFailedEventDetails
    -- ^ Contains details about a task that failed to start.
  , taskStartedEventDetails :: Core.Maybe Types.TaskStartedEventDetails
    -- ^ Contains details about a task that was started.
  , taskSubmitFailedEventDetails :: Core.Maybe Types.TaskSubmitFailedEventDetails
    -- ^ Contains details about a task that where the submit failed.
  , taskSubmittedEventDetails :: Core.Maybe Types.TaskSubmittedEventDetails
    -- ^ Contains details about a submitted task.
  , taskSucceededEventDetails :: Core.Maybe Types.TaskSucceededEventDetails
    -- ^ Contains details about a task that succeeded.
  , taskTimedOutEventDetails :: Core.Maybe Types.TaskTimedOutEventDetails
    -- ^ Contains details about a task that timed out.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'HistoryEvent' value with any optional fields omitted.
mkHistoryEvent
    :: Core.NominalDiffTime -- ^ 'timestamp'
    -> Types.HistoryEventType -- ^ 'type\''
    -> Core.Integer -- ^ 'id'
    -> HistoryEvent
mkHistoryEvent timestamp type' id
  = HistoryEvent'{timestamp, type', id,
                  activityFailedEventDetails = Core.Nothing,
                  activityScheduleFailedEventDetails = Core.Nothing,
                  activityScheduledEventDetails = Core.Nothing,
                  activityStartedEventDetails = Core.Nothing,
                  activitySucceededEventDetails = Core.Nothing,
                  activityTimedOutEventDetails = Core.Nothing,
                  executionAbortedEventDetails = Core.Nothing,
                  executionFailedEventDetails = Core.Nothing,
                  executionStartedEventDetails = Core.Nothing,
                  executionSucceededEventDetails = Core.Nothing,
                  executionTimedOutEventDetails = Core.Nothing,
                  lambdaFunctionFailedEventDetails = Core.Nothing,
                  lambdaFunctionScheduleFailedEventDetails = Core.Nothing,
                  lambdaFunctionScheduledEventDetails = Core.Nothing,
                  lambdaFunctionStartFailedEventDetails = Core.Nothing,
                  lambdaFunctionSucceededEventDetails = Core.Nothing,
                  lambdaFunctionTimedOutEventDetails = Core.Nothing,
                  mapIterationAbortedEventDetails = Core.Nothing,
                  mapIterationFailedEventDetails = Core.Nothing,
                  mapIterationStartedEventDetails = Core.Nothing,
                  mapIterationSucceededEventDetails = Core.Nothing,
                  mapStateStartedEventDetails = Core.Nothing,
                  previousEventId = Core.Nothing,
                  stateEnteredEventDetails = Core.Nothing,
                  stateExitedEventDetails = Core.Nothing,
                  taskFailedEventDetails = Core.Nothing,
                  taskScheduledEventDetails = Core.Nothing,
                  taskStartFailedEventDetails = Core.Nothing,
                  taskStartedEventDetails = Core.Nothing,
                  taskSubmitFailedEventDetails = Core.Nothing,
                  taskSubmittedEventDetails = Core.Nothing,
                  taskSucceededEventDetails = Core.Nothing,
                  taskTimedOutEventDetails = Core.Nothing}

-- | The date and time the event occurred.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTimestamp :: Lens.Lens' HistoryEvent Core.NominalDiffTime
heTimestamp = Lens.field @"timestamp"
{-# INLINEABLE heTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

-- | The type of the event.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heType :: Lens.Lens' HistoryEvent Types.HistoryEventType
heType = Lens.field @"type'"
{-# INLINEABLE heType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The id of the event. Events are numbered sequentially, starting at one.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heId :: Lens.Lens' HistoryEvent Core.Integer
heId = Lens.field @"id"
{-# INLINEABLE heId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'activityFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.ActivityFailedEventDetails)
heActivityFailedEventDetails = Lens.field @"activityFailedEventDetails"
{-# INLINEABLE heActivityFailedEventDetails #-}
{-# DEPRECATED activityFailedEventDetails "Use generic-lens or generic-optics with 'activityFailedEventDetails' instead"  #-}

-- | Contains details about an activity schedule event that failed during an execution.
--
-- /Note:/ Consider using 'activityScheduleFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityScheduleFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.ActivityScheduleFailedEventDetails)
heActivityScheduleFailedEventDetails = Lens.field @"activityScheduleFailedEventDetails"
{-# INLINEABLE heActivityScheduleFailedEventDetails #-}
{-# DEPRECATED activityScheduleFailedEventDetails "Use generic-lens or generic-optics with 'activityScheduleFailedEventDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'activityScheduledEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityScheduledEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.ActivityScheduledEventDetails)
heActivityScheduledEventDetails = Lens.field @"activityScheduledEventDetails"
{-# INLINEABLE heActivityScheduledEventDetails #-}
{-# DEPRECATED activityScheduledEventDetails "Use generic-lens or generic-optics with 'activityScheduledEventDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'activityStartedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityStartedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.ActivityStartedEventDetails)
heActivityStartedEventDetails = Lens.field @"activityStartedEventDetails"
{-# INLINEABLE heActivityStartedEventDetails #-}
{-# DEPRECATED activityStartedEventDetails "Use generic-lens or generic-optics with 'activityStartedEventDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'activitySucceededEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivitySucceededEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.ActivitySucceededEventDetails)
heActivitySucceededEventDetails = Lens.field @"activitySucceededEventDetails"
{-# INLINEABLE heActivitySucceededEventDetails #-}
{-# DEPRECATED activitySucceededEventDetails "Use generic-lens or generic-optics with 'activitySucceededEventDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'activityTimedOutEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTimedOutEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.ActivityTimedOutEventDetails)
heActivityTimedOutEventDetails = Lens.field @"activityTimedOutEventDetails"
{-# INLINEABLE heActivityTimedOutEventDetails #-}
{-# DEPRECATED activityTimedOutEventDetails "Use generic-lens or generic-optics with 'activityTimedOutEventDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'executionAbortedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heExecutionAbortedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.ExecutionAbortedEventDetails)
heExecutionAbortedEventDetails = Lens.field @"executionAbortedEventDetails"
{-# INLINEABLE heExecutionAbortedEventDetails #-}
{-# DEPRECATED executionAbortedEventDetails "Use generic-lens or generic-optics with 'executionAbortedEventDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'executionFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heExecutionFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.ExecutionFailedEventDetails)
heExecutionFailedEventDetails = Lens.field @"executionFailedEventDetails"
{-# INLINEABLE heExecutionFailedEventDetails #-}
{-# DEPRECATED executionFailedEventDetails "Use generic-lens or generic-optics with 'executionFailedEventDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'executionStartedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heExecutionStartedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.ExecutionStartedEventDetails)
heExecutionStartedEventDetails = Lens.field @"executionStartedEventDetails"
{-# INLINEABLE heExecutionStartedEventDetails #-}
{-# DEPRECATED executionStartedEventDetails "Use generic-lens or generic-optics with 'executionStartedEventDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'executionSucceededEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heExecutionSucceededEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.ExecutionSucceededEventDetails)
heExecutionSucceededEventDetails = Lens.field @"executionSucceededEventDetails"
{-# INLINEABLE heExecutionSucceededEventDetails #-}
{-# DEPRECATED executionSucceededEventDetails "Use generic-lens or generic-optics with 'executionSucceededEventDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'executionTimedOutEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heExecutionTimedOutEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.ExecutionTimedOutEventDetails)
heExecutionTimedOutEventDetails = Lens.field @"executionTimedOutEventDetails"
{-# INLINEABLE heExecutionTimedOutEventDetails #-}
{-# DEPRECATED executionTimedOutEventDetails "Use generic-lens or generic-optics with 'executionTimedOutEventDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lambdaFunctionFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.LambdaFunctionFailedEventDetails)
heLambdaFunctionFailedEventDetails = Lens.field @"lambdaFunctionFailedEventDetails"
{-# INLINEABLE heLambdaFunctionFailedEventDetails #-}
{-# DEPRECATED lambdaFunctionFailedEventDetails "Use generic-lens or generic-optics with 'lambdaFunctionFailedEventDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lambdaFunctionScheduleFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionScheduleFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.LambdaFunctionScheduleFailedEventDetails)
heLambdaFunctionScheduleFailedEventDetails = Lens.field @"lambdaFunctionScheduleFailedEventDetails"
{-# INLINEABLE heLambdaFunctionScheduleFailedEventDetails #-}
{-# DEPRECATED lambdaFunctionScheduleFailedEventDetails "Use generic-lens or generic-optics with 'lambdaFunctionScheduleFailedEventDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lambdaFunctionScheduledEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionScheduledEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.LambdaFunctionScheduledEventDetails)
heLambdaFunctionScheduledEventDetails = Lens.field @"lambdaFunctionScheduledEventDetails"
{-# INLINEABLE heLambdaFunctionScheduledEventDetails #-}
{-# DEPRECATED lambdaFunctionScheduledEventDetails "Use generic-lens or generic-optics with 'lambdaFunctionScheduledEventDetails' instead"  #-}

-- | Contains details about a lambda function that failed to start during an execution.
--
-- /Note:/ Consider using 'lambdaFunctionStartFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionStartFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.LambdaFunctionStartFailedEventDetails)
heLambdaFunctionStartFailedEventDetails = Lens.field @"lambdaFunctionStartFailedEventDetails"
{-# INLINEABLE heLambdaFunctionStartFailedEventDetails #-}
{-# DEPRECATED lambdaFunctionStartFailedEventDetails "Use generic-lens or generic-optics with 'lambdaFunctionStartFailedEventDetails' instead"  #-}

-- | Contains details about a lambda function that terminated successfully during an execution.
--
-- /Note:/ Consider using 'lambdaFunctionSucceededEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionSucceededEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.LambdaFunctionSucceededEventDetails)
heLambdaFunctionSucceededEventDetails = Lens.field @"lambdaFunctionSucceededEventDetails"
{-# INLINEABLE heLambdaFunctionSucceededEventDetails #-}
{-# DEPRECATED lambdaFunctionSucceededEventDetails "Use generic-lens or generic-optics with 'lambdaFunctionSucceededEventDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lambdaFunctionTimedOutEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionTimedOutEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.LambdaFunctionTimedOutEventDetails)
heLambdaFunctionTimedOutEventDetails = Lens.field @"lambdaFunctionTimedOutEventDetails"
{-# INLINEABLE heLambdaFunctionTimedOutEventDetails #-}
{-# DEPRECATED lambdaFunctionTimedOutEventDetails "Use generic-lens or generic-optics with 'lambdaFunctionTimedOutEventDetails' instead"  #-}

-- | Contains details about an iteration of a Map state that was aborted.
--
-- /Note:/ Consider using 'mapIterationAbortedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heMapIterationAbortedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.MapIterationEventDetails)
heMapIterationAbortedEventDetails = Lens.field @"mapIterationAbortedEventDetails"
{-# INLINEABLE heMapIterationAbortedEventDetails #-}
{-# DEPRECATED mapIterationAbortedEventDetails "Use generic-lens or generic-optics with 'mapIterationAbortedEventDetails' instead"  #-}

-- | Contains details about an iteration of a Map state that failed.
--
-- /Note:/ Consider using 'mapIterationFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heMapIterationFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.MapIterationEventDetails)
heMapIterationFailedEventDetails = Lens.field @"mapIterationFailedEventDetails"
{-# INLINEABLE heMapIterationFailedEventDetails #-}
{-# DEPRECATED mapIterationFailedEventDetails "Use generic-lens or generic-optics with 'mapIterationFailedEventDetails' instead"  #-}

-- | Contains details about an iteration of a Map state that was started.
--
-- /Note:/ Consider using 'mapIterationStartedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heMapIterationStartedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.MapIterationEventDetails)
heMapIterationStartedEventDetails = Lens.field @"mapIterationStartedEventDetails"
{-# INLINEABLE heMapIterationStartedEventDetails #-}
{-# DEPRECATED mapIterationStartedEventDetails "Use generic-lens or generic-optics with 'mapIterationStartedEventDetails' instead"  #-}

-- | Contains details about an iteration of a Map state that succeeded.
--
-- /Note:/ Consider using 'mapIterationSucceededEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heMapIterationSucceededEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.MapIterationEventDetails)
heMapIterationSucceededEventDetails = Lens.field @"mapIterationSucceededEventDetails"
{-# INLINEABLE heMapIterationSucceededEventDetails #-}
{-# DEPRECATED mapIterationSucceededEventDetails "Use generic-lens or generic-optics with 'mapIterationSucceededEventDetails' instead"  #-}

-- | Contains details about Map state that was started.
--
-- /Note:/ Consider using 'mapStateStartedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heMapStateStartedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.MapStateStartedEventDetails)
heMapStateStartedEventDetails = Lens.field @"mapStateStartedEventDetails"
{-# INLINEABLE heMapStateStartedEventDetails #-}
{-# DEPRECATED mapStateStartedEventDetails "Use generic-lens or generic-optics with 'mapStateStartedEventDetails' instead"  #-}

-- | The id of the previous event.
--
-- /Note:/ Consider using 'previousEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hePreviousEventId :: Lens.Lens' HistoryEvent (Core.Maybe Core.Integer)
hePreviousEventId = Lens.field @"previousEventId"
{-# INLINEABLE hePreviousEventId #-}
{-# DEPRECATED previousEventId "Use generic-lens or generic-optics with 'previousEventId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'stateEnteredEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heStateEnteredEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.StateEnteredEventDetails)
heStateEnteredEventDetails = Lens.field @"stateEnteredEventDetails"
{-# INLINEABLE heStateEnteredEventDetails #-}
{-# DEPRECATED stateEnteredEventDetails "Use generic-lens or generic-optics with 'stateEnteredEventDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'stateExitedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heStateExitedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.StateExitedEventDetails)
heStateExitedEventDetails = Lens.field @"stateExitedEventDetails"
{-# INLINEABLE heStateExitedEventDetails #-}
{-# DEPRECATED stateExitedEventDetails "Use generic-lens or generic-optics with 'stateExitedEventDetails' instead"  #-}

-- | Contains details about the failure of a task.
--
-- /Note:/ Consider using 'taskFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.TaskFailedEventDetails)
heTaskFailedEventDetails = Lens.field @"taskFailedEventDetails"
{-# INLINEABLE heTaskFailedEventDetails #-}
{-# DEPRECATED taskFailedEventDetails "Use generic-lens or generic-optics with 'taskFailedEventDetails' instead"  #-}

-- | Contains details about a task that was scheduled.
--
-- /Note:/ Consider using 'taskScheduledEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskScheduledEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.TaskScheduledEventDetails)
heTaskScheduledEventDetails = Lens.field @"taskScheduledEventDetails"
{-# INLINEABLE heTaskScheduledEventDetails #-}
{-# DEPRECATED taskScheduledEventDetails "Use generic-lens or generic-optics with 'taskScheduledEventDetails' instead"  #-}

-- | Contains details about a task that failed to start.
--
-- /Note:/ Consider using 'taskStartFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskStartFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.TaskStartFailedEventDetails)
heTaskStartFailedEventDetails = Lens.field @"taskStartFailedEventDetails"
{-# INLINEABLE heTaskStartFailedEventDetails #-}
{-# DEPRECATED taskStartFailedEventDetails "Use generic-lens or generic-optics with 'taskStartFailedEventDetails' instead"  #-}

-- | Contains details about a task that was started.
--
-- /Note:/ Consider using 'taskStartedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskStartedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.TaskStartedEventDetails)
heTaskStartedEventDetails = Lens.field @"taskStartedEventDetails"
{-# INLINEABLE heTaskStartedEventDetails #-}
{-# DEPRECATED taskStartedEventDetails "Use generic-lens or generic-optics with 'taskStartedEventDetails' instead"  #-}

-- | Contains details about a task that where the submit failed.
--
-- /Note:/ Consider using 'taskSubmitFailedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskSubmitFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.TaskSubmitFailedEventDetails)
heTaskSubmitFailedEventDetails = Lens.field @"taskSubmitFailedEventDetails"
{-# INLINEABLE heTaskSubmitFailedEventDetails #-}
{-# DEPRECATED taskSubmitFailedEventDetails "Use generic-lens or generic-optics with 'taskSubmitFailedEventDetails' instead"  #-}

-- | Contains details about a submitted task.
--
-- /Note:/ Consider using 'taskSubmittedEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskSubmittedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.TaskSubmittedEventDetails)
heTaskSubmittedEventDetails = Lens.field @"taskSubmittedEventDetails"
{-# INLINEABLE heTaskSubmittedEventDetails #-}
{-# DEPRECATED taskSubmittedEventDetails "Use generic-lens or generic-optics with 'taskSubmittedEventDetails' instead"  #-}

-- | Contains details about a task that succeeded.
--
-- /Note:/ Consider using 'taskSucceededEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskSucceededEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.TaskSucceededEventDetails)
heTaskSucceededEventDetails = Lens.field @"taskSucceededEventDetails"
{-# INLINEABLE heTaskSucceededEventDetails #-}
{-# DEPRECATED taskSucceededEventDetails "Use generic-lens or generic-optics with 'taskSucceededEventDetails' instead"  #-}

-- | Contains details about a task that timed out.
--
-- /Note:/ Consider using 'taskTimedOutEventDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTaskTimedOutEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe Types.TaskTimedOutEventDetails)
heTaskTimedOutEventDetails = Lens.field @"taskTimedOutEventDetails"
{-# INLINEABLE heTaskTimedOutEventDetails #-}
{-# DEPRECATED taskTimedOutEventDetails "Use generic-lens or generic-optics with 'taskTimedOutEventDetails' instead"  #-}

instance Core.FromJSON HistoryEvent where
        parseJSON
          = Core.withObject "HistoryEvent" Core.$
              \ x ->
                HistoryEvent' Core.<$>
                  (x Core..: "timestamp") Core.<*> x Core..: "type" Core.<*>
                    x Core..: "id"
                    Core.<*> x Core..:? "activityFailedEventDetails"
                    Core.<*> x Core..:? "activityScheduleFailedEventDetails"
                    Core.<*> x Core..:? "activityScheduledEventDetails"
                    Core.<*> x Core..:? "activityStartedEventDetails"
                    Core.<*> x Core..:? "activitySucceededEventDetails"
                    Core.<*> x Core..:? "activityTimedOutEventDetails"
                    Core.<*> x Core..:? "executionAbortedEventDetails"
                    Core.<*> x Core..:? "executionFailedEventDetails"
                    Core.<*> x Core..:? "executionStartedEventDetails"
                    Core.<*> x Core..:? "executionSucceededEventDetails"
                    Core.<*> x Core..:? "executionTimedOutEventDetails"
                    Core.<*> x Core..:? "lambdaFunctionFailedEventDetails"
                    Core.<*> x Core..:? "lambdaFunctionScheduleFailedEventDetails"
                    Core.<*> x Core..:? "lambdaFunctionScheduledEventDetails"
                    Core.<*> x Core..:? "lambdaFunctionStartFailedEventDetails"
                    Core.<*> x Core..:? "lambdaFunctionSucceededEventDetails"
                    Core.<*> x Core..:? "lambdaFunctionTimedOutEventDetails"
                    Core.<*> x Core..:? "mapIterationAbortedEventDetails"
                    Core.<*> x Core..:? "mapIterationFailedEventDetails"
                    Core.<*> x Core..:? "mapIterationStartedEventDetails"
                    Core.<*> x Core..:? "mapIterationSucceededEventDetails"
                    Core.<*> x Core..:? "mapStateStartedEventDetails"
                    Core.<*> x Core..:? "previousEventId"
                    Core.<*> x Core..:? "stateEnteredEventDetails"
                    Core.<*> x Core..:? "stateExitedEventDetails"
                    Core.<*> x Core..:? "taskFailedEventDetails"
                    Core.<*> x Core..:? "taskScheduledEventDetails"
                    Core.<*> x Core..:? "taskStartFailedEventDetails"
                    Core.<*> x Core..:? "taskStartedEventDetails"
                    Core.<*> x Core..:? "taskSubmitFailedEventDetails"
                    Core.<*> x Core..:? "taskSubmittedEventDetails"
                    Core.<*> x Core..:? "taskSucceededEventDetails"
                    Core.<*> x Core..:? "taskTimedOutEventDetails"
