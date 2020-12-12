{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionStartedEventAttributes
  ( WorkflowExecutionStartedEventAttributes (..),

    -- * Smart constructor
    mkWorkflowExecutionStartedEventAttributes,

    -- * Lenses
    weseaParentInitiatedEventId,
    weseaTagList,
    weseaTaskStartToCloseTimeout,
    weseaLambdaRole,
    weseaInput,
    weseaExecutionStartToCloseTimeout,
    weseaTaskPriority,
    weseaParentWorkflowExecution,
    weseaContinuedExecutionRunId,
    weseaChildPolicy,
    weseaTaskList,
    weseaWorkflowType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provides details of @WorkflowExecutionStarted@ event.
--
-- /See:/ 'mkWorkflowExecutionStartedEventAttributes' smart constructor.
data WorkflowExecutionStartedEventAttributes = WorkflowExecutionStartedEventAttributes'
  { parentInitiatedEventId ::
      Lude.Maybe
        Lude.Integer,
    tagList ::
      Lude.Maybe
        [Lude.Text],
    taskStartToCloseTimeout ::
      Lude.Maybe
        Lude.Text,
    lambdaRole ::
      Lude.Maybe
        Lude.Text,
    input ::
      Lude.Maybe
        Lude.Text,
    executionStartToCloseTimeout ::
      Lude.Maybe
        Lude.Text,
    taskPriority ::
      Lude.Maybe
        Lude.Text,
    parentWorkflowExecution ::
      Lude.Maybe
        WorkflowExecution,
    continuedExecutionRunId ::
      Lude.Maybe
        Lude.Text,
    childPolicy ::
      ChildPolicy,
    taskList ::
      TaskList,
    workflowType ::
      WorkflowType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowExecutionStartedEventAttributes' with the minimum fields required to make a request.
--
-- * 'childPolicy' - The policy to use for the child workflow executions if this workflow execution is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout.
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
-- * 'continuedExecutionRunId' - If this workflow execution was started due to a @ContinueAsNewWorkflowExecution@ decision, then it contains the @runId@ of the previous workflow execution that was closed and continued as this execution.
-- * 'executionStartToCloseTimeout' - The maximum duration for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'input' - The input provided to the workflow execution.
-- * 'lambdaRole' - The IAM role attached to the workflow execution.
-- * 'parentInitiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this workflow execution. The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'parentWorkflowExecution' - The source workflow execution that started this workflow execution. The member isn't set if the workflow execution was not started by a workflow.
-- * 'tagList' - The list of tags associated with this workflow execution. An execution can have up to 5 tags.
-- * 'taskList' - The name of the task list for scheduling the decision tasks for this workflow execution.
-- * 'taskPriority' - The priority of the decision tasks in the workflow execution.
-- * 'taskStartToCloseTimeout' - The maximum duration of decision tasks for this workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'workflowType' - The workflow type of this execution.
mkWorkflowExecutionStartedEventAttributes ::
  -- | 'childPolicy'
  ChildPolicy ->
  -- | 'taskList'
  TaskList ->
  -- | 'workflowType'
  WorkflowType ->
  WorkflowExecutionStartedEventAttributes
mkWorkflowExecutionStartedEventAttributes
  pChildPolicy_
  pTaskList_
  pWorkflowType_ =
    WorkflowExecutionStartedEventAttributes'
      { parentInitiatedEventId =
          Lude.Nothing,
        tagList = Lude.Nothing,
        taskStartToCloseTimeout = Lude.Nothing,
        lambdaRole = Lude.Nothing,
        input = Lude.Nothing,
        executionStartToCloseTimeout = Lude.Nothing,
        taskPriority = Lude.Nothing,
        parentWorkflowExecution = Lude.Nothing,
        continuedExecutionRunId = Lude.Nothing,
        childPolicy = pChildPolicy_,
        taskList = pTaskList_,
        workflowType = pWorkflowType_
      }

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this workflow execution. The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'parentInitiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaParentInitiatedEventId :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Lude.Maybe Lude.Integer)
weseaParentInitiatedEventId = Lens.lens (parentInitiatedEventId :: WorkflowExecutionStartedEventAttributes -> Lude.Maybe Lude.Integer) (\s a -> s {parentInitiatedEventId = a} :: WorkflowExecutionStartedEventAttributes)
{-# DEPRECATED weseaParentInitiatedEventId "Use generic-lens or generic-optics with 'parentInitiatedEventId' instead." #-}

-- | The list of tags associated with this workflow execution. An execution can have up to 5 tags.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaTagList :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Lude.Maybe [Lude.Text])
weseaTagList = Lens.lens (tagList :: WorkflowExecutionStartedEventAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {tagList = a} :: WorkflowExecutionStartedEventAttributes)
{-# DEPRECATED weseaTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The maximum duration of decision tasks for this workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'taskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaTaskStartToCloseTimeout :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Lude.Maybe Lude.Text)
weseaTaskStartToCloseTimeout = Lens.lens (taskStartToCloseTimeout :: WorkflowExecutionStartedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {taskStartToCloseTimeout = a} :: WorkflowExecutionStartedEventAttributes)
{-# DEPRECATED weseaTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'taskStartToCloseTimeout' instead." #-}

-- | The IAM role attached to the workflow execution.
--
-- /Note:/ Consider using 'lambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaLambdaRole :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Lude.Maybe Lude.Text)
weseaLambdaRole = Lens.lens (lambdaRole :: WorkflowExecutionStartedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {lambdaRole = a} :: WorkflowExecutionStartedEventAttributes)
{-# DEPRECATED weseaLambdaRole "Use generic-lens or generic-optics with 'lambdaRole' instead." #-}

-- | The input provided to the workflow execution.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaInput :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Lude.Maybe Lude.Text)
weseaInput = Lens.lens (input :: WorkflowExecutionStartedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: WorkflowExecutionStartedEventAttributes)
{-# DEPRECATED weseaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The maximum duration for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'executionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaExecutionStartToCloseTimeout :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Lude.Maybe Lude.Text)
weseaExecutionStartToCloseTimeout = Lens.lens (executionStartToCloseTimeout :: WorkflowExecutionStartedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {executionStartToCloseTimeout = a} :: WorkflowExecutionStartedEventAttributes)
{-# DEPRECATED weseaExecutionStartToCloseTimeout "Use generic-lens or generic-optics with 'executionStartToCloseTimeout' instead." #-}

-- | The priority of the decision tasks in the workflow execution.
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaTaskPriority :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Lude.Maybe Lude.Text)
weseaTaskPriority = Lens.lens (taskPriority :: WorkflowExecutionStartedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {taskPriority = a} :: WorkflowExecutionStartedEventAttributes)
{-# DEPRECATED weseaTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

-- | The source workflow execution that started this workflow execution. The member isn't set if the workflow execution was not started by a workflow.
--
-- /Note:/ Consider using 'parentWorkflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaParentWorkflowExecution :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Lude.Maybe WorkflowExecution)
weseaParentWorkflowExecution = Lens.lens (parentWorkflowExecution :: WorkflowExecutionStartedEventAttributes -> Lude.Maybe WorkflowExecution) (\s a -> s {parentWorkflowExecution = a} :: WorkflowExecutionStartedEventAttributes)
{-# DEPRECATED weseaParentWorkflowExecution "Use generic-lens or generic-optics with 'parentWorkflowExecution' instead." #-}

-- | If this workflow execution was started due to a @ContinueAsNewWorkflowExecution@ decision, then it contains the @runId@ of the previous workflow execution that was closed and continued as this execution.
--
-- /Note:/ Consider using 'continuedExecutionRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaContinuedExecutionRunId :: Lens.Lens' WorkflowExecutionStartedEventAttributes (Lude.Maybe Lude.Text)
weseaContinuedExecutionRunId = Lens.lens (continuedExecutionRunId :: WorkflowExecutionStartedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {continuedExecutionRunId = a} :: WorkflowExecutionStartedEventAttributes)
{-# DEPRECATED weseaContinuedExecutionRunId "Use generic-lens or generic-optics with 'continuedExecutionRunId' instead." #-}

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
weseaChildPolicy :: Lens.Lens' WorkflowExecutionStartedEventAttributes ChildPolicy
weseaChildPolicy = Lens.lens (childPolicy :: WorkflowExecutionStartedEventAttributes -> ChildPolicy) (\s a -> s {childPolicy = a} :: WorkflowExecutionStartedEventAttributes)
{-# DEPRECATED weseaChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

-- | The name of the task list for scheduling the decision tasks for this workflow execution.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaTaskList :: Lens.Lens' WorkflowExecutionStartedEventAttributes TaskList
weseaTaskList = Lens.lens (taskList :: WorkflowExecutionStartedEventAttributes -> TaskList) (\s a -> s {taskList = a} :: WorkflowExecutionStartedEventAttributes)
{-# DEPRECATED weseaTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | The workflow type of this execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weseaWorkflowType :: Lens.Lens' WorkflowExecutionStartedEventAttributes WorkflowType
weseaWorkflowType = Lens.lens (workflowType :: WorkflowExecutionStartedEventAttributes -> WorkflowType) (\s a -> s {workflowType = a} :: WorkflowExecutionStartedEventAttributes)
{-# DEPRECATED weseaWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

instance Lude.FromJSON WorkflowExecutionStartedEventAttributes where
  parseJSON =
    Lude.withObject
      "WorkflowExecutionStartedEventAttributes"
      ( \x ->
          WorkflowExecutionStartedEventAttributes'
            Lude.<$> (x Lude..:? "parentInitiatedEventId")
            Lude.<*> (x Lude..:? "tagList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "taskStartToCloseTimeout")
            Lude.<*> (x Lude..:? "lambdaRole")
            Lude.<*> (x Lude..:? "input")
            Lude.<*> (x Lude..:? "executionStartToCloseTimeout")
            Lude.<*> (x Lude..:? "taskPriority")
            Lude.<*> (x Lude..:? "parentWorkflowExecution")
            Lude.<*> (x Lude..:? "continuedExecutionRunId")
            Lude.<*> (x Lude..: "childPolicy")
            Lude.<*> (x Lude..: "taskList")
            Lude.<*> (x Lude..: "workflowType")
      )
