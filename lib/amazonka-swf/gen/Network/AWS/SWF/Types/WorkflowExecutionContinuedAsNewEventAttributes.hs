{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionContinuedAsNewEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionContinuedAsNewEventAttributes
  ( WorkflowExecutionContinuedAsNewEventAttributes (..),

    -- * Smart constructor
    mkWorkflowExecutionContinuedAsNewEventAttributes,

    -- * Lenses
    wecaneaDecisionTaskCompletedEventId,
    wecaneaNewExecutionRunId,
    wecaneaTaskList,
    wecaneaChildPolicy,
    wecaneaWorkflowType,
    wecaneaExecutionStartToCloseTimeout,
    wecaneaInput,
    wecaneaLambdaRole,
    wecaneaTagList,
    wecaneaTaskPriority,
    wecaneaTaskStartToCloseTimeout,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Arn as Types
import qualified Network.AWS.SWF.Types.ChildPolicy as Types
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.DurationInSecondsOptional as Types
import qualified Network.AWS.SWF.Types.Tag as Types
import qualified Network.AWS.SWF.Types.TaskList as Types
import qualified Network.AWS.SWF.Types.TaskPriority as Types
import qualified Network.AWS.SWF.Types.WorkflowRunId as Types
import qualified Network.AWS.SWF.Types.WorkflowType as Types

-- | Provides the details of the @WorkflowExecutionContinuedAsNew@ event.
--
-- /See:/ 'mkWorkflowExecutionContinuedAsNewEventAttributes' smart constructor.
data WorkflowExecutionContinuedAsNewEventAttributes = WorkflowExecutionContinuedAsNewEventAttributes'
  { -- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @ContinueAsNewWorkflowExecution@ decision that started this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Core.Integer,
    -- | The @runId@ of the new workflow execution.
    newExecutionRunId :: Types.WorkflowRunId,
    -- | The task list to use for the decisions of the new (continued) workflow execution.
    taskList :: Types.TaskList,
    -- | The policy to use for the child workflow executions of the new execution if it is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout.
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
    childPolicy :: Types.ChildPolicy,
    -- | The workflow type of this execution.
    workflowType :: Types.WorkflowType,
    -- | The total duration allowed for the new workflow execution.
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    executionStartToCloseTimeout :: Core.Maybe Types.DurationInSecondsOptional,
    -- | The input provided to the new workflow execution.
    input :: Core.Maybe Types.Data,
    -- | The IAM role to attach to the new (continued) workflow execution.
    lambdaRole :: Core.Maybe Types.Arn,
    -- | The list of tags associated with the new workflow execution.
    tagList :: Core.Maybe [Types.Tag],
    -- | The priority of the task to use for the decisions of the new (continued) workflow execution.
    taskPriority :: Core.Maybe Types.TaskPriority,
    -- | The maximum duration of decision tasks for the new workflow execution.
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    taskStartToCloseTimeout :: Core.Maybe Types.DurationInSecondsOptional
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowExecutionContinuedAsNewEventAttributes' value with any optional fields omitted.
mkWorkflowExecutionContinuedAsNewEventAttributes ::
  -- | 'decisionTaskCompletedEventId'
  Core.Integer ->
  -- | 'newExecutionRunId'
  Types.WorkflowRunId ->
  -- | 'taskList'
  Types.TaskList ->
  -- | 'childPolicy'
  Types.ChildPolicy ->
  -- | 'workflowType'
  Types.WorkflowType ->
  WorkflowExecutionContinuedAsNewEventAttributes
mkWorkflowExecutionContinuedAsNewEventAttributes
  decisionTaskCompletedEventId
  newExecutionRunId
  taskList
  childPolicy
  workflowType =
    WorkflowExecutionContinuedAsNewEventAttributes'
      { decisionTaskCompletedEventId,
        newExecutionRunId,
        taskList,
        childPolicy,
        workflowType,
        executionStartToCloseTimeout = Core.Nothing,
        input = Core.Nothing,
        lambdaRole = Core.Nothing,
        tagList = Core.Nothing,
        taskPriority = Core.Nothing,
        taskStartToCloseTimeout = Core.Nothing
      }

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @ContinueAsNewWorkflowExecution@ decision that started this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecaneaDecisionTaskCompletedEventId :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes Core.Integer
wecaneaDecisionTaskCompletedEventId = Lens.field @"decisionTaskCompletedEventId"
{-# DEPRECATED wecaneaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

-- | The @runId@ of the new workflow execution.
--
-- /Note:/ Consider using 'newExecutionRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecaneaNewExecutionRunId :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes Types.WorkflowRunId
wecaneaNewExecutionRunId = Lens.field @"newExecutionRunId"
{-# DEPRECATED wecaneaNewExecutionRunId "Use generic-lens or generic-optics with 'newExecutionRunId' instead." #-}

-- | The task list to use for the decisions of the new (continued) workflow execution.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecaneaTaskList :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes Types.TaskList
wecaneaTaskList = Lens.field @"taskList"
{-# DEPRECATED wecaneaTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | The policy to use for the child workflow executions of the new execution if it is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout.
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
wecaneaChildPolicy :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes Types.ChildPolicy
wecaneaChildPolicy = Lens.field @"childPolicy"
{-# DEPRECATED wecaneaChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

-- | The workflow type of this execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecaneaWorkflowType :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes Types.WorkflowType
wecaneaWorkflowType = Lens.field @"workflowType"
{-# DEPRECATED wecaneaWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | The total duration allowed for the new workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'executionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecaneaExecutionStartToCloseTimeout :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes (Core.Maybe Types.DurationInSecondsOptional)
wecaneaExecutionStartToCloseTimeout = Lens.field @"executionStartToCloseTimeout"
{-# DEPRECATED wecaneaExecutionStartToCloseTimeout "Use generic-lens or generic-optics with 'executionStartToCloseTimeout' instead." #-}

-- | The input provided to the new workflow execution.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecaneaInput :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes (Core.Maybe Types.Data)
wecaneaInput = Lens.field @"input"
{-# DEPRECATED wecaneaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The IAM role to attach to the new (continued) workflow execution.
--
-- /Note:/ Consider using 'lambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecaneaLambdaRole :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes (Core.Maybe Types.Arn)
wecaneaLambdaRole = Lens.field @"lambdaRole"
{-# DEPRECATED wecaneaLambdaRole "Use generic-lens or generic-optics with 'lambdaRole' instead." #-}

-- | The list of tags associated with the new workflow execution.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecaneaTagList :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes (Core.Maybe [Types.Tag])
wecaneaTagList = Lens.field @"tagList"
{-# DEPRECATED wecaneaTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The priority of the task to use for the decisions of the new (continued) workflow execution.
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecaneaTaskPriority :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes (Core.Maybe Types.TaskPriority)
wecaneaTaskPriority = Lens.field @"taskPriority"
{-# DEPRECATED wecaneaTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

-- | The maximum duration of decision tasks for the new workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'taskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecaneaTaskStartToCloseTimeout :: Lens.Lens' WorkflowExecutionContinuedAsNewEventAttributes (Core.Maybe Types.DurationInSecondsOptional)
wecaneaTaskStartToCloseTimeout = Lens.field @"taskStartToCloseTimeout"
{-# DEPRECATED wecaneaTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'taskStartToCloseTimeout' instead." #-}

instance
  Core.FromJSON
    WorkflowExecutionContinuedAsNewEventAttributes
  where
  parseJSON =
    Core.withObject "WorkflowExecutionContinuedAsNewEventAttributes" Core.$
      \x ->
        WorkflowExecutionContinuedAsNewEventAttributes'
          Core.<$> (x Core..: "decisionTaskCompletedEventId")
          Core.<*> (x Core..: "newExecutionRunId")
          Core.<*> (x Core..: "taskList")
          Core.<*> (x Core..: "childPolicy")
          Core.<*> (x Core..: "workflowType")
          Core.<*> (x Core..:? "executionStartToCloseTimeout")
          Core.<*> (x Core..:? "input")
          Core.<*> (x Core..:? "lambdaRole")
          Core.<*> (x Core..:? "tagList")
          Core.<*> (x Core..:? "taskPriority")
          Core.<*> (x Core..:? "taskStartToCloseTimeout")
