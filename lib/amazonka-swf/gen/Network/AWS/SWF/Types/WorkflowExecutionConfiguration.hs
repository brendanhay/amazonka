{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionConfiguration
  ( WorkflowExecutionConfiguration (..),

    -- * Smart constructor
    mkWorkflowExecutionConfiguration,

    -- * Lenses
    wecLambdaRole,
    wecTaskPriority,
    wecTaskStartToCloseTimeout,
    wecExecutionStartToCloseTimeout,
    wecTaskList,
    wecChildPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList

-- | The configuration settings for a workflow execution including timeout values, tasklist etc. These configuration settings are determined from the defaults specified when registering the workflow type and those specified when starting the workflow execution.
--
-- /See:/ 'mkWorkflowExecutionConfiguration' smart constructor.
data WorkflowExecutionConfiguration = WorkflowExecutionConfiguration'
  { lambdaRole ::
      Lude.Maybe Lude.Text,
    taskPriority ::
      Lude.Maybe Lude.Text,
    taskStartToCloseTimeout ::
      Lude.Text,
    executionStartToCloseTimeout ::
      Lude.Text,
    taskList :: TaskList,
    childPolicy :: ChildPolicy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowExecutionConfiguration' with the minimum fields required to make a request.
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
-- * 'executionStartToCloseTimeout' - The total duration for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'lambdaRole' - The IAM role attached to the child workflow execution.
-- * 'taskList' - The task list used for the decision tasks generated for this workflow execution.
-- * 'taskPriority' - The priority assigned to decision tasks for this workflow execution. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
-- * 'taskStartToCloseTimeout' - The maximum duration allowed for decision tasks for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
mkWorkflowExecutionConfiguration ::
  -- | 'taskStartToCloseTimeout'
  Lude.Text ->
  -- | 'executionStartToCloseTimeout'
  Lude.Text ->
  -- | 'taskList'
  TaskList ->
  -- | 'childPolicy'
  ChildPolicy ->
  WorkflowExecutionConfiguration
mkWorkflowExecutionConfiguration
  pTaskStartToCloseTimeout_
  pExecutionStartToCloseTimeout_
  pTaskList_
  pChildPolicy_ =
    WorkflowExecutionConfiguration'
      { lambdaRole = Lude.Nothing,
        taskPriority = Lude.Nothing,
        taskStartToCloseTimeout = pTaskStartToCloseTimeout_,
        executionStartToCloseTimeout = pExecutionStartToCloseTimeout_,
        taskList = pTaskList_,
        childPolicy = pChildPolicy_
      }

-- | The IAM role attached to the child workflow execution.
--
-- /Note:/ Consider using 'lambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecLambdaRole :: Lens.Lens' WorkflowExecutionConfiguration (Lude.Maybe Lude.Text)
wecLambdaRole = Lens.lens (lambdaRole :: WorkflowExecutionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {lambdaRole = a} :: WorkflowExecutionConfiguration)
{-# DEPRECATED wecLambdaRole "Use generic-lens or generic-optics with 'lambdaRole' instead." #-}

-- | The priority assigned to decision tasks for this workflow execution. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecTaskPriority :: Lens.Lens' WorkflowExecutionConfiguration (Lude.Maybe Lude.Text)
wecTaskPriority = Lens.lens (taskPriority :: WorkflowExecutionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {taskPriority = a} :: WorkflowExecutionConfiguration)
{-# DEPRECATED wecTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

-- | The maximum duration allowed for decision tasks for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'taskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecTaskStartToCloseTimeout :: Lens.Lens' WorkflowExecutionConfiguration Lude.Text
wecTaskStartToCloseTimeout = Lens.lens (taskStartToCloseTimeout :: WorkflowExecutionConfiguration -> Lude.Text) (\s a -> s {taskStartToCloseTimeout = a} :: WorkflowExecutionConfiguration)
{-# DEPRECATED wecTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'taskStartToCloseTimeout' instead." #-}

-- | The total duration for this workflow execution.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'executionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecExecutionStartToCloseTimeout :: Lens.Lens' WorkflowExecutionConfiguration Lude.Text
wecExecutionStartToCloseTimeout = Lens.lens (executionStartToCloseTimeout :: WorkflowExecutionConfiguration -> Lude.Text) (\s a -> s {executionStartToCloseTimeout = a} :: WorkflowExecutionConfiguration)
{-# DEPRECATED wecExecutionStartToCloseTimeout "Use generic-lens or generic-optics with 'executionStartToCloseTimeout' instead." #-}

-- | The task list used for the decision tasks generated for this workflow execution.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecTaskList :: Lens.Lens' WorkflowExecutionConfiguration TaskList
wecTaskList = Lens.lens (taskList :: WorkflowExecutionConfiguration -> TaskList) (\s a -> s {taskList = a} :: WorkflowExecutionConfiguration)
{-# DEPRECATED wecTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

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
wecChildPolicy :: Lens.Lens' WorkflowExecutionConfiguration ChildPolicy
wecChildPolicy = Lens.lens (childPolicy :: WorkflowExecutionConfiguration -> ChildPolicy) (\s a -> s {childPolicy = a} :: WorkflowExecutionConfiguration)
{-# DEPRECATED wecChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

instance Lude.FromJSON WorkflowExecutionConfiguration where
  parseJSON =
    Lude.withObject
      "WorkflowExecutionConfiguration"
      ( \x ->
          WorkflowExecutionConfiguration'
            Lude.<$> (x Lude..:? "lambdaRole")
            Lude.<*> (x Lude..:? "taskPriority")
            Lude.<*> (x Lude..: "taskStartToCloseTimeout")
            Lude.<*> (x Lude..: "executionStartToCloseTimeout")
            Lude.<*> (x Lude..: "taskList")
            Lude.<*> (x Lude..: "childPolicy")
      )
