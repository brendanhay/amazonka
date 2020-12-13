{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes
  ( StartChildWorkflowExecutionInitiatedEventAttributes (..),

    -- * Smart constructor
    mkStartChildWorkflowExecutionInitiatedEventAttributes,

    -- * Lenses
    scweieaControl,
    scweieaTagList,
    scweieaTaskStartToCloseTimeout,
    scweieaLambdaRole,
    scweieaWorkflowType,
    scweieaInput,
    scweieaExecutionStartToCloseTimeout,
    scweieaTaskList,
    scweieaTaskPriority,
    scweieaChildPolicy,
    scweieaWorkflowId,
    scweieaDecisionTaskCompletedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @StartChildWorkflowExecutionInitiated@ event.
--
-- /See:/ 'mkStartChildWorkflowExecutionInitiatedEventAttributes' smart constructor.
data StartChildWorkflowExecutionInitiatedEventAttributes = StartChildWorkflowExecutionInitiatedEventAttributes'
  { -- | Data attached to the event that can be used by the decider in subsequent decision tasks. This data isn't sent to the activity.
    control :: Lude.Maybe Lude.Text,
    -- | The list of tags to associated with the child workflow execution.
    tagList :: Lude.Maybe [Lude.Text],
    -- | The maximum duration allowed for the decision tasks for this workflow execution.
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    taskStartToCloseTimeout :: Lude.Maybe Lude.Text,
    -- | The IAM role to attach to the child workflow execution.
    lambdaRole :: Lude.Maybe Lude.Text,
    -- | The type of the child workflow execution.
    workflowType :: WorkflowType,
    -- | The inputs provided to the child workflow execution.
    input :: Lude.Maybe Lude.Text,
    -- | The maximum duration for the child workflow execution. If the workflow execution isn't closed within this duration, it is timed out and force-terminated.
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    executionStartToCloseTimeout :: Lude.Maybe Lude.Text,
    -- | The name of the task list used for the decision tasks of the child workflow execution.
    taskList :: TaskList,
    -- | The priority assigned for the decision tasks for this workflow execution. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
    --
    -- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
    taskPriority :: Lude.Maybe Lude.Text,
    -- | The policy to use for the child workflow executions if this execution gets terminated by explicitly calling the 'TerminateWorkflowExecution' action or due to an expired timeout.
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
    childPolicy :: ChildPolicy,
    -- | The @workflowId@ of the child workflow execution.
    workflowId :: Lude.Text,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @StartChildWorkflowExecution@ 'Decision' to request this child workflow execution. This information can be useful for diagnosing problems by tracing back the cause of events.
    decisionTaskCompletedEventId :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartChildWorkflowExecutionInitiatedEventAttributes' with the minimum fields required to make a request.
--
-- * 'control' - Data attached to the event that can be used by the decider in subsequent decision tasks. This data isn't sent to the activity.
-- * 'tagList' - The list of tags to associated with the child workflow execution.
-- * 'taskStartToCloseTimeout' - The maximum duration allowed for the decision tasks for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'lambdaRole' - The IAM role to attach to the child workflow execution.
-- * 'workflowType' - The type of the child workflow execution.
-- * 'input' - The inputs provided to the child workflow execution.
-- * 'executionStartToCloseTimeout' - The maximum duration for the child workflow execution. If the workflow execution isn't closed within this duration, it is timed out and force-terminated.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'taskList' - The name of the task list used for the decision tasks of the child workflow execution.
-- * 'taskPriority' - The priority assigned for the decision tasks for this workflow execution. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
-- * 'childPolicy' - The policy to use for the child workflow executions if this execution gets terminated by explicitly calling the 'TerminateWorkflowExecution' action or due to an expired timeout.
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
-- * 'workflowId' - The @workflowId@ of the child workflow execution.
-- * 'decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @StartChildWorkflowExecution@ 'Decision' to request this child workflow execution. This information can be useful for diagnosing problems by tracing back the cause of events.
mkStartChildWorkflowExecutionInitiatedEventAttributes ::
  -- | 'workflowType'
  WorkflowType ->
  -- | 'taskList'
  TaskList ->
  -- | 'childPolicy'
  ChildPolicy ->
  -- | 'workflowId'
  Lude.Text ->
  -- | 'decisionTaskCompletedEventId'
  Lude.Integer ->
  StartChildWorkflowExecutionInitiatedEventAttributes
mkStartChildWorkflowExecutionInitiatedEventAttributes
  pWorkflowType_
  pTaskList_
  pChildPolicy_
  pWorkflowId_
  pDecisionTaskCompletedEventId_ =
    StartChildWorkflowExecutionInitiatedEventAttributes'
      { control =
          Lude.Nothing,
        tagList = Lude.Nothing,
        taskStartToCloseTimeout = Lude.Nothing,
        lambdaRole = Lude.Nothing,
        workflowType = pWorkflowType_,
        input = Lude.Nothing,
        executionStartToCloseTimeout =
          Lude.Nothing,
        taskList = pTaskList_,
        taskPriority = Lude.Nothing,
        childPolicy = pChildPolicy_,
        workflowId = pWorkflowId_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that can be used by the decider in subsequent decision tasks. This data isn't sent to the activity.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaControl :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Lude.Maybe Lude.Text)
scweieaControl = Lens.lens (control :: StartChildWorkflowExecutionInitiatedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {control = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED scweieaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The list of tags to associated with the child workflow execution.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaTagList :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Lude.Maybe [Lude.Text])
scweieaTagList = Lens.lens (tagList :: StartChildWorkflowExecutionInitiatedEventAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {tagList = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED scweieaTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The maximum duration allowed for the decision tasks for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'taskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaTaskStartToCloseTimeout :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Lude.Maybe Lude.Text)
scweieaTaskStartToCloseTimeout = Lens.lens (taskStartToCloseTimeout :: StartChildWorkflowExecutionInitiatedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {taskStartToCloseTimeout = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED scweieaTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'taskStartToCloseTimeout' instead." #-}

-- | The IAM role to attach to the child workflow execution.
--
-- /Note:/ Consider using 'lambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaLambdaRole :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Lude.Maybe Lude.Text)
scweieaLambdaRole = Lens.lens (lambdaRole :: StartChildWorkflowExecutionInitiatedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {lambdaRole = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED scweieaLambdaRole "Use generic-lens or generic-optics with 'lambdaRole' instead." #-}

-- | The type of the child workflow execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaWorkflowType :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes WorkflowType
scweieaWorkflowType = Lens.lens (workflowType :: StartChildWorkflowExecutionInitiatedEventAttributes -> WorkflowType) (\s a -> s {workflowType = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED scweieaWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | The inputs provided to the child workflow execution.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaInput :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Lude.Maybe Lude.Text)
scweieaInput = Lens.lens (input :: StartChildWorkflowExecutionInitiatedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED scweieaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The maximum duration for the child workflow execution. If the workflow execution isn't closed within this duration, it is timed out and force-terminated.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'executionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaExecutionStartToCloseTimeout :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Lude.Maybe Lude.Text)
scweieaExecutionStartToCloseTimeout = Lens.lens (executionStartToCloseTimeout :: StartChildWorkflowExecutionInitiatedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {executionStartToCloseTimeout = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED scweieaExecutionStartToCloseTimeout "Use generic-lens or generic-optics with 'executionStartToCloseTimeout' instead." #-}

-- | The name of the task list used for the decision tasks of the child workflow execution.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaTaskList :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes TaskList
scweieaTaskList = Lens.lens (taskList :: StartChildWorkflowExecutionInitiatedEventAttributes -> TaskList) (\s a -> s {taskList = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED scweieaTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | The priority assigned for the decision tasks for this workflow execution. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaTaskPriority :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Lude.Maybe Lude.Text)
scweieaTaskPriority = Lens.lens (taskPriority :: StartChildWorkflowExecutionInitiatedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {taskPriority = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED scweieaTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

-- | The policy to use for the child workflow executions if this execution gets terminated by explicitly calling the 'TerminateWorkflowExecution' action or due to an expired timeout.
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
scweieaChildPolicy :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes ChildPolicy
scweieaChildPolicy = Lens.lens (childPolicy :: StartChildWorkflowExecutionInitiatedEventAttributes -> ChildPolicy) (\s a -> s {childPolicy = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED scweieaChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

-- | The @workflowId@ of the child workflow execution.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaWorkflowId :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes Lude.Text
scweieaWorkflowId = Lens.lens (workflowId :: StartChildWorkflowExecutionInitiatedEventAttributes -> Lude.Text) (\s a -> s {workflowId = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED scweieaWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @StartChildWorkflowExecution@ 'Decision' to request this child workflow execution. This information can be useful for diagnosing problems by tracing back the cause of events.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scweieaDecisionTaskCompletedEventId :: Lens.Lens' StartChildWorkflowExecutionInitiatedEventAttributes Lude.Integer
scweieaDecisionTaskCompletedEventId = Lens.lens (decisionTaskCompletedEventId :: StartChildWorkflowExecutionInitiatedEventAttributes -> Lude.Integer) (\s a -> s {decisionTaskCompletedEventId = a} :: StartChildWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED scweieaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

instance
  Lude.FromJSON
    StartChildWorkflowExecutionInitiatedEventAttributes
  where
  parseJSON =
    Lude.withObject
      "StartChildWorkflowExecutionInitiatedEventAttributes"
      ( \x ->
          StartChildWorkflowExecutionInitiatedEventAttributes'
            Lude.<$> (x Lude..:? "control")
            Lude.<*> (x Lude..:? "tagList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "taskStartToCloseTimeout")
            Lude.<*> (x Lude..:? "lambdaRole")
            Lude.<*> (x Lude..: "workflowType")
            Lude.<*> (x Lude..:? "input")
            Lude.<*> (x Lude..:? "executionStartToCloseTimeout")
            Lude.<*> (x Lude..: "taskList")
            Lude.<*> (x Lude..:? "taskPriority")
            Lude.<*> (x Lude..: "childPolicy")
            Lude.<*> (x Lude..: "workflowId")
            Lude.<*> (x Lude..: "decisionTaskCompletedEventId")
      )
