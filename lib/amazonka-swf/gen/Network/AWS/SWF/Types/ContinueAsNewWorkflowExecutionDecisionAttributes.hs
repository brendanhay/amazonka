-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionDecisionAttributes
  ( ContinueAsNewWorkflowExecutionDecisionAttributes (..),

    -- * Smart constructor
    mkContinueAsNewWorkflowExecutionDecisionAttributes,

    -- * Lenses
    canwedaTagList,
    canwedaTaskStartToCloseTimeout,
    canwedaLambdaRole,
    canwedaInput,
    canwedaWorkflowTypeVersion,
    canwedaExecutionStartToCloseTimeout,
    canwedaTaskList,
    canwedaTaskPriority,
    canwedaChildPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList

-- | Provides the details of the @ContinueAsNewWorkflowExecution@ decision.
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
--     * @tag@ – A tag used to identify the workflow execution
--
--
--     * @taskList@ – String constraint. The key is @swf:taskList.name@ .
--
--
--     * @workflowType.version@ – String constraint. The key is @swf:workflowType.version@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- /See:/ 'mkContinueAsNewWorkflowExecutionDecisionAttributes' smart constructor.
data ContinueAsNewWorkflowExecutionDecisionAttributes = ContinueAsNewWorkflowExecutionDecisionAttributes'
  { tagList ::
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
    workflowTypeVersion ::
      Lude.Maybe
        Lude.Text,
    executionStartToCloseTimeout ::
      Lude.Maybe
        Lude.Text,
    taskList ::
      Lude.Maybe
        TaskList,
    taskPriority ::
      Lude.Maybe
        Lude.Text,
    childPolicy ::
      Lude.Maybe
        ChildPolicy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'ContinueAsNewWorkflowExecutionDecisionAttributes' with the minimum fields required to make a request.
--
-- * 'childPolicy' - If set, specifies the policy to use for the child workflow executions of the new execution if it is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' .
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
-- * 'executionStartToCloseTimeout' - If set, specifies the total duration for this workflow execution. This overrides the @defaultExecutionStartToCloseTimeout@ specified when registering the workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'input' - The input provided to the new workflow execution.
-- * 'lambdaRole' - The IAM role to attach to the new (continued) execution.
-- * 'tagList' - The list of tags to associate with the new workflow execution. A maximum of 5 tags can be specified. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
-- * 'taskList' - The task list to use for the decisions of the new (continued) workflow execution.
-- * 'taskPriority' - The task priority that, if set, specifies the priority for the decision tasks for this workflow execution. This overrides the defaultTaskPriority specified when registering the workflow type. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
-- * 'taskStartToCloseTimeout' - Specifies the maximum duration of decision tasks for the new workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'workflowTypeVersion' - The version of the workflow to start.
mkContinueAsNewWorkflowExecutionDecisionAttributes ::
  ContinueAsNewWorkflowExecutionDecisionAttributes
mkContinueAsNewWorkflowExecutionDecisionAttributes =
  ContinueAsNewWorkflowExecutionDecisionAttributes'
    { tagList =
        Lude.Nothing,
      taskStartToCloseTimeout = Lude.Nothing,
      lambdaRole = Lude.Nothing,
      input = Lude.Nothing,
      workflowTypeVersion = Lude.Nothing,
      executionStartToCloseTimeout = Lude.Nothing,
      taskList = Lude.Nothing,
      taskPriority = Lude.Nothing,
      childPolicy = Lude.Nothing
    }

-- | The list of tags to associate with the new workflow execution. A maximum of 5 tags can be specified. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaTagList :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Lude.Maybe [Lude.Text])
canwedaTagList = Lens.lens (tagList :: ContinueAsNewWorkflowExecutionDecisionAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {tagList = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)
{-# DEPRECATED canwedaTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | Specifies the maximum duration of decision tasks for the new workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'taskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaTaskStartToCloseTimeout :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
canwedaTaskStartToCloseTimeout = Lens.lens (taskStartToCloseTimeout :: ContinueAsNewWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {taskStartToCloseTimeout = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)
{-# DEPRECATED canwedaTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'taskStartToCloseTimeout' instead." #-}

-- | The IAM role to attach to the new (continued) execution.
--
-- /Note:/ Consider using 'lambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaLambdaRole :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
canwedaLambdaRole = Lens.lens (lambdaRole :: ContinueAsNewWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {lambdaRole = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)
{-# DEPRECATED canwedaLambdaRole "Use generic-lens or generic-optics with 'lambdaRole' instead." #-}

-- | The input provided to the new workflow execution.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaInput :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
canwedaInput = Lens.lens (input :: ContinueAsNewWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)
{-# DEPRECATED canwedaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The version of the workflow to start.
--
-- /Note:/ Consider using 'workflowTypeVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaWorkflowTypeVersion :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
canwedaWorkflowTypeVersion = Lens.lens (workflowTypeVersion :: ContinueAsNewWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {workflowTypeVersion = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)
{-# DEPRECATED canwedaWorkflowTypeVersion "Use generic-lens or generic-optics with 'workflowTypeVersion' instead." #-}

-- | If set, specifies the total duration for this workflow execution. This overrides the @defaultExecutionStartToCloseTimeout@ specified when registering the workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'executionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaExecutionStartToCloseTimeout :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
canwedaExecutionStartToCloseTimeout = Lens.lens (executionStartToCloseTimeout :: ContinueAsNewWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {executionStartToCloseTimeout = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)
{-# DEPRECATED canwedaExecutionStartToCloseTimeout "Use generic-lens or generic-optics with 'executionStartToCloseTimeout' instead." #-}

-- | The task list to use for the decisions of the new (continued) workflow execution.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaTaskList :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Lude.Maybe TaskList)
canwedaTaskList = Lens.lens (taskList :: ContinueAsNewWorkflowExecutionDecisionAttributes -> Lude.Maybe TaskList) (\s a -> s {taskList = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)
{-# DEPRECATED canwedaTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | The task priority that, if set, specifies the priority for the decision tasks for this workflow execution. This overrides the defaultTaskPriority specified when registering the workflow type. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canwedaTaskPriority :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
canwedaTaskPriority = Lens.lens (taskPriority :: ContinueAsNewWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {taskPriority = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)
{-# DEPRECATED canwedaTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

-- | If set, specifies the policy to use for the child workflow executions of the new execution if it is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' .
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
canwedaChildPolicy :: Lens.Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Lude.Maybe ChildPolicy)
canwedaChildPolicy = Lens.lens (childPolicy :: ContinueAsNewWorkflowExecutionDecisionAttributes -> Lude.Maybe ChildPolicy) (\s a -> s {childPolicy = a} :: ContinueAsNewWorkflowExecutionDecisionAttributes)
{-# DEPRECATED canwedaChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

instance
  Lude.ToJSON
    ContinueAsNewWorkflowExecutionDecisionAttributes
  where
  toJSON ContinueAsNewWorkflowExecutionDecisionAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("tagList" Lude..=) Lude.<$> tagList,
            ("taskStartToCloseTimeout" Lude..=)
              Lude.<$> taskStartToCloseTimeout,
            ("lambdaRole" Lude..=) Lude.<$> lambdaRole,
            ("input" Lude..=) Lude.<$> input,
            ("workflowTypeVersion" Lude..=) Lude.<$> workflowTypeVersion,
            ("executionStartToCloseTimeout" Lude..=)
              Lude.<$> executionStartToCloseTimeout,
            ("taskList" Lude..=) Lude.<$> taskList,
            ("taskPriority" Lude..=) Lude.<$> taskPriority,
            ("childPolicy" Lude..=) Lude.<$> childPolicy
          ]
      )
