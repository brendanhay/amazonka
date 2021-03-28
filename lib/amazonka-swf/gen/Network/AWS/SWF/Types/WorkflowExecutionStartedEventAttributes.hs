{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.WorkflowExecutionStartedEventAttributes
  ( WorkflowExecutionStartedEventAttributes (..)
  -- * Smart constructor
  , mkWorkflowExecutionStartedEventAttributes
  -- * Lenses
  , wChildPolicy
  , wTaskList
  , wWorkflowType
  , wContinuedExecutionRunId
  , wExecutionStartToCloseTimeout
  , wInput
  , wLambdaRole
  , wParentInitiatedEventId
  , wParentWorkflowExecution
  , wTagList
  , wTaskPriority
  , wTaskStartToCloseTimeout
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Arn as Types
import qualified Network.AWS.SWF.Types.ChildPolicy as Types
import qualified Network.AWS.SWF.Types.ContinuedExecutionRunId as Types
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.ExecutionStartToCloseTimeout as Types
import qualified Network.AWS.SWF.Types.Tag as Types
import qualified Network.AWS.SWF.Types.TaskList as Types
import qualified Network.AWS.SWF.Types.TaskPriority as Types
import qualified Network.AWS.SWF.Types.TaskStartToCloseTimeout as Types
import qualified Network.AWS.SWF.Types.WorkflowExecution as Types
import qualified Network.AWS.SWF.Types.WorkflowType as Types

-- | Provides details of @WorkflowExecutionStarted@ event.
--
-- /See:/ 'mkWorkflowExecutionStartedEventAttributes' smart constructor.
data WorkflowExecutionStartedEventAttributes = WorkflowExecutionStartedEventAttributes'
  { childPolicy :: Types.ChildPolicy
    -- ^ The policy to use for the child workflow executions if this workflow execution is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout.
--
-- The supported child policies are:
--
--     * @TERMINATE@ – The child executions are terminated.
--
--
--     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.
--
--
--     * @ABANDON@ – No action is taken. The child executions continue to run.
--
--
  , taskList :: Types.TaskList
    -- ^ The name of the task list for scheduling the decision tasks for this workflow execution.
  , workflowType :: Types.WorkflowType
    -- ^ The workflow type of this execution.
  , continuedExecutionRunId :: Core.Maybe Types.ContinuedExecutionRunId
    -- ^ If this workflow execution was started due to a @ContinueAsNewWorkflowExecution@ decision, then it contains the @runId@ of the previous workflow execution that was closed and continued as this execution.
  , executionStartToCloseTimeout :: Core.Maybe Types.ExecutionStartToCloseTimeout
    -- ^ The maximum duration for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
  , input :: Core.Maybe Types.Data
    -- ^ The input provided to the workflow execution.
  , lambdaRole :: Core.Maybe Types.Arn
    -- ^ The IAM role attached to the workflow execution.
  , parentInitiatedEventId :: Core.Maybe Core.Integer
    -- ^ The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this workflow execution. The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , parentWorkflowExecution :: Core.Maybe Types.WorkflowExecution
    -- ^ The source workflow execution that started this workflow execution. The member isn't set if the workflow execution was not started by a workflow.
  , tagList :: Core.Maybe [Types.Tag]
    -- ^ The list of tags associated with this workflow execution. An execution can have up to 5 tags.
  , taskPriority :: Core.Maybe Types.TaskPriority
    -- ^ The priority of the decision tasks in the workflow execution.
  , taskStartToCloseTimeout :: Core.Maybe Types.TaskStartToCloseTimeout
    -- ^ The maximum duration of decision tasks for this workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowExecutionStartedEventAttributes' value with any optional fields omitted.
mkWorkflowExecutionStartedEventAttributes
    :: Types.ChildPolicy -- ^ 'childPolicy'
    -> Types.TaskList -- ^ 'taskList'
    -> Types.WorkflowType -- ^ 'workflowType'
    -> WorkflowExecutionStartedEventAttributes
mkWorkflowExecutionStartedEventAttributes childPolicy taskList
  workflowType
  = WorkflowExecutionStartedEventAttributes'{childPolicy, taskList,
                                             workflowType, continuedExecutionRunId = Core.Nothing,
                                             executionStartToCloseTimeout = Core.Nothing,
                                             input = Core.Nothing, lambdaRole = Core.Nothing,
                                             parentInitiatedEventId = Core.Nothing,
                                             parentWorkflowExecution = Core.Nothing,
                                             tagList = Core.Nothing, taskPriority = Core.Nothing,
                                             taskStartToCloseTimeout = Core.Nothing}

-- | The policy to use for the child workflow executions if this workflow execution is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout.
--
-- The supported child policies are:
--
--     * @TERMINATE@ – The child executions are terminated.
--
--
--     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.
--
--
--     * @ABANDON@ – No action is taken. The child executions continue to run.
--
--
--
-- /Note:/ Consider using 'childPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wChildPolicy :: Lens.Lens' WorkflowExecutionStartedEventAttributes Types.ChildPolicy
wChildPolicy = Lens.field @"childPolicy"
{-# INLINEABLE wChildPolicy #-}
{-# DEPRECATED childPolicy "Use generic-lens or generic-optics with 'childPolicy' instead"  #-}

-- | The name of the task list for scheduling the decision tasks for this workflow execution.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wTaskList :: Lens.Lens' WorkflowExecutionStartedEventAttributes Types.TaskList
wTaskList = Lens.field @"taskList"
{-# INLINEABLE wTaskList #-}
{-# DEPRECATED taskList "Use generic-lens or generic-optics with 'taskList' instead"  #-}

-- | The workflow type of this execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wWorkflowType :: Lens.Lens' WorkflowExecutionStartedEventAttributes Types.WorkflowType
wWorkflowType = Lens.field @"workflowType"
{-# INLINEABLE wWorkflowType #-}
{-# DEPRECATED workflowType "Use generic-lens or generic-optics with 'workflowType' instead"  #-}

-- | If this workflow execution was started due to a @ContinueAsNewWorkflowExecution@ decision, then it contains the @runId@ of the previous workflow execution that was closed and continued as this execution.
--
-- /Note:/ Consider using 'continuedExecutionRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wContinuedExecutionRunId :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Types.ContinuedExecutionRunId)
wContinuedExecutionRunId = Lens.field @"continuedExecutionRunId"
{-# INLINEABLE wContinuedExecutionRunId #-}
{-# DEPRECATED continuedExecutionRunId "Use generic-lens or generic-optics with 'continuedExecutionRunId' instead"  #-}

-- | The maximum duration for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'executionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wExecutionStartToCloseTimeout :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Types.ExecutionStartToCloseTimeout)
wExecutionStartToCloseTimeout = Lens.field @"executionStartToCloseTimeout"
{-# INLINEABLE wExecutionStartToCloseTimeout #-}
{-# DEPRECATED executionStartToCloseTimeout "Use generic-lens or generic-optics with 'executionStartToCloseTimeout' instead"  #-}

-- | The input provided to the workflow execution.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wInput :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Types.Data)
wInput = Lens.field @"input"
{-# INLINEABLE wInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

-- | The IAM role attached to the workflow execution.
--
-- /Note:/ Consider using 'lambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wLambdaRole :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Types.Arn)
wLambdaRole = Lens.field @"lambdaRole"
{-# INLINEABLE wLambdaRole #-}
{-# DEPRECATED lambdaRole "Use generic-lens or generic-optics with 'lambdaRole' instead"  #-}

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this workflow execution. The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'parentInitiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wParentInitiatedEventId :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Core.Integer)
wParentInitiatedEventId = Lens.field @"parentInitiatedEventId"
{-# INLINEABLE wParentInitiatedEventId #-}
{-# DEPRECATED parentInitiatedEventId "Use generic-lens or generic-optics with 'parentInitiatedEventId' instead"  #-}

-- | The source workflow execution that started this workflow execution. The member isn't set if the workflow execution was not started by a workflow.
--
-- /Note:/ Consider using 'parentWorkflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wParentWorkflowExecution :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Types.WorkflowExecution)
wParentWorkflowExecution = Lens.field @"parentWorkflowExecution"
{-# INLINEABLE wParentWorkflowExecution #-}
{-# DEPRECATED parentWorkflowExecution "Use generic-lens or generic-optics with 'parentWorkflowExecution' instead"  #-}

-- | The list of tags associated with this workflow execution. An execution can have up to 5 tags.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wTagList :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe [Types.Tag])
wTagList = Lens.field @"tagList"
{-# INLINEABLE wTagList #-}
{-# DEPRECATED tagList "Use generic-lens or generic-optics with 'tagList' instead"  #-}

-- | The priority of the decision tasks in the workflow execution.
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wTaskPriority :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Types.TaskPriority)
wTaskPriority = Lens.field @"taskPriority"
{-# INLINEABLE wTaskPriority #-}
{-# DEPRECATED taskPriority "Use generic-lens or generic-optics with 'taskPriority' instead"  #-}

-- | The maximum duration of decision tasks for this workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'taskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wTaskStartToCloseTimeout :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Core.Maybe Types.TaskStartToCloseTimeout)
wTaskStartToCloseTimeout = Lens.field @"taskStartToCloseTimeout"
{-# INLINEABLE wTaskStartToCloseTimeout #-}
{-# DEPRECATED taskStartToCloseTimeout "Use generic-lens or generic-optics with 'taskStartToCloseTimeout' instead"  #-}

instance Core.FromJSON WorkflowExecutionStartedEventAttributes
         where
        parseJSON
          = Core.withObject "WorkflowExecutionStartedEventAttributes" Core.$
              \ x ->
                WorkflowExecutionStartedEventAttributes' Core.<$>
                  (x Core..: "childPolicy") Core.<*> x Core..: "taskList" Core.<*>
                    x Core..: "workflowType"
                    Core.<*> x Core..:? "continuedExecutionRunId"
                    Core.<*> x Core..:? "executionStartToCloseTimeout"
                    Core.<*> x Core..:? "input"
                    Core.<*> x Core..:? "lambdaRole"
                    Core.<*> x Core..:? "parentInitiatedEventId"
                    Core.<*> x Core..:? "parentWorkflowExecution"
                    Core.<*> x Core..:? "tagList"
                    Core.<*> x Core..:? "taskPriority"
                    Core.<*> x Core..:? "taskStartToCloseTimeout"
