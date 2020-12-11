-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartChildWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartChildWorkflowExecutionDecisionAttributes
  ( StartChildWorkflowExecutionDecisionAttributes (..),

    -- * Smart constructor
    mkStartChildWorkflowExecutionDecisionAttributes,

    -- * Lenses
    scwedaControl,
    scwedaTagList,
    scwedaTaskStartToCloseTimeout,
    scwedaLambdaRole,
    scwedaInput,
    scwedaExecutionStartToCloseTimeout,
    scwedaTaskList,
    scwedaTaskPriority,
    scwedaChildPolicy,
    scwedaWorkflowType,
    scwedaWorkflowId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.TaskList
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @StartChildWorkflowExecution@ decision.
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
--     * @tagList.member.N@ – The key is "swf:tagList.N" where N is the tag number from 0 to 4, inclusive.
--
--
--     * @taskList@ – String constraint. The key is @swf:taskList.name@ .
--
--
--     * @workflowType.name@ – String constraint. The key is @swf:workflowType.name@ .
--
--
--     * @workflowType.version@ – String constraint. The key is @swf:workflowType.version@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- /See:/ 'mkStartChildWorkflowExecutionDecisionAttributes' smart constructor.
data StartChildWorkflowExecutionDecisionAttributes = StartChildWorkflowExecutionDecisionAttributes'
  { control ::
      Lude.Maybe
        Lude.Text,
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
    taskList ::
      Lude.Maybe
        TaskList,
    taskPriority ::
      Lude.Maybe
        Lude.Text,
    childPolicy ::
      Lude.Maybe
        ChildPolicy,
    workflowType ::
      WorkflowType,
    workflowId ::
      Lude.Text
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

-- | Creates a value of 'StartChildWorkflowExecutionDecisionAttributes' with the minimum fields required to make a request.
--
-- * 'childPolicy' - If set, specifies the policy to use for the child workflow executions if the workflow execution being started is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' .
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
-- * 'control' - The data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the child workflow execution.
-- * 'executionStartToCloseTimeout' - The total duration for this workflow execution. This overrides the defaultExecutionStartToCloseTimeout specified when registering the workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'input' - The input to be provided to the workflow execution.
-- * 'lambdaRole' - The IAM role attached to the child workflow execution.
-- * 'tagList' - The list of tags to associate with the child workflow execution. A maximum of 5 tags can be specified. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
-- * 'taskList' - The name of the task list to be used for decision tasks of the child workflow execution.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
-- * 'taskPriority' - A task priority that, if set, specifies the priority for a decision task of this workflow execution. This overrides the defaultTaskPriority specified when registering the workflow type. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
-- * 'taskStartToCloseTimeout' - Specifies the maximum duration of decision tasks for this workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'workflowId' - The @workflowId@ of the workflow execution.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
-- * 'workflowType' - The type of the workflow execution to be started.
mkStartChildWorkflowExecutionDecisionAttributes ::
  -- | 'workflowType'
  WorkflowType ->
  -- | 'workflowId'
  Lude.Text ->
  StartChildWorkflowExecutionDecisionAttributes
mkStartChildWorkflowExecutionDecisionAttributes
  pWorkflowType_
  pWorkflowId_ =
    StartChildWorkflowExecutionDecisionAttributes'
      { control =
          Lude.Nothing,
        tagList = Lude.Nothing,
        taskStartToCloseTimeout = Lude.Nothing,
        lambdaRole = Lude.Nothing,
        input = Lude.Nothing,
        executionStartToCloseTimeout = Lude.Nothing,
        taskList = Lude.Nothing,
        taskPriority = Lude.Nothing,
        childPolicy = Lude.Nothing,
        workflowType = pWorkflowType_,
        workflowId = pWorkflowId_
      }

-- | The data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the child workflow execution.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaControl :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
scwedaControl = Lens.lens (control :: StartChildWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {control = a} :: StartChildWorkflowExecutionDecisionAttributes)
{-# DEPRECATED scwedaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The list of tags to associate with the child workflow execution. A maximum of 5 tags can be specified. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaTagList :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Lude.Maybe [Lude.Text])
scwedaTagList = Lens.lens (tagList :: StartChildWorkflowExecutionDecisionAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {tagList = a} :: StartChildWorkflowExecutionDecisionAttributes)
{-# DEPRECATED scwedaTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | Specifies the maximum duration of decision tasks for this workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'taskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaTaskStartToCloseTimeout :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
scwedaTaskStartToCloseTimeout = Lens.lens (taskStartToCloseTimeout :: StartChildWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {taskStartToCloseTimeout = a} :: StartChildWorkflowExecutionDecisionAttributes)
{-# DEPRECATED scwedaTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'taskStartToCloseTimeout' instead." #-}

-- | The IAM role attached to the child workflow execution.
--
-- /Note:/ Consider using 'lambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaLambdaRole :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
scwedaLambdaRole = Lens.lens (lambdaRole :: StartChildWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {lambdaRole = a} :: StartChildWorkflowExecutionDecisionAttributes)
{-# DEPRECATED scwedaLambdaRole "Use generic-lens or generic-optics with 'lambdaRole' instead." #-}

-- | The input to be provided to the workflow execution.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaInput :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
scwedaInput = Lens.lens (input :: StartChildWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: StartChildWorkflowExecutionDecisionAttributes)
{-# DEPRECATED scwedaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The total duration for this workflow execution. This overrides the defaultExecutionStartToCloseTimeout specified when registering the workflow type.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'executionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaExecutionStartToCloseTimeout :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
scwedaExecutionStartToCloseTimeout = Lens.lens (executionStartToCloseTimeout :: StartChildWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {executionStartToCloseTimeout = a} :: StartChildWorkflowExecutionDecisionAttributes)
{-# DEPRECATED scwedaExecutionStartToCloseTimeout "Use generic-lens or generic-optics with 'executionStartToCloseTimeout' instead." #-}

-- | The name of the task list to be used for decision tasks of the child workflow execution.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaTaskList :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Lude.Maybe TaskList)
scwedaTaskList = Lens.lens (taskList :: StartChildWorkflowExecutionDecisionAttributes -> Lude.Maybe TaskList) (\s a -> s {taskList = a} :: StartChildWorkflowExecutionDecisionAttributes)
{-# DEPRECATED scwedaTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | A task priority that, if set, specifies the priority for a decision task of this workflow execution. This overrides the defaultTaskPriority specified when registering the workflow type. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaTaskPriority :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
scwedaTaskPriority = Lens.lens (taskPriority :: StartChildWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {taskPriority = a} :: StartChildWorkflowExecutionDecisionAttributes)
{-# DEPRECATED scwedaTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

-- | If set, specifies the policy to use for the child workflow executions if the workflow execution being started is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' .
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
scwedaChildPolicy :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes (Lude.Maybe ChildPolicy)
scwedaChildPolicy = Lens.lens (childPolicy :: StartChildWorkflowExecutionDecisionAttributes -> Lude.Maybe ChildPolicy) (\s a -> s {childPolicy = a} :: StartChildWorkflowExecutionDecisionAttributes)
{-# DEPRECATED scwedaChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

-- | The type of the workflow execution to be started.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaWorkflowType :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes WorkflowType
scwedaWorkflowType = Lens.lens (workflowType :: StartChildWorkflowExecutionDecisionAttributes -> WorkflowType) (\s a -> s {workflowType = a} :: StartChildWorkflowExecutionDecisionAttributes)
{-# DEPRECATED scwedaWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | The @workflowId@ of the workflow execution.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scwedaWorkflowId :: Lens.Lens' StartChildWorkflowExecutionDecisionAttributes Lude.Text
scwedaWorkflowId = Lens.lens (workflowId :: StartChildWorkflowExecutionDecisionAttributes -> Lude.Text) (\s a -> s {workflowId = a} :: StartChildWorkflowExecutionDecisionAttributes)
{-# DEPRECATED scwedaWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

instance Lude.ToJSON StartChildWorkflowExecutionDecisionAttributes where
  toJSON StartChildWorkflowExecutionDecisionAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("control" Lude..=) Lude.<$> control,
            ("tagList" Lude..=) Lude.<$> tagList,
            ("taskStartToCloseTimeout" Lude..=)
              Lude.<$> taskStartToCloseTimeout,
            ("lambdaRole" Lude..=) Lude.<$> lambdaRole,
            ("input" Lude..=) Lude.<$> input,
            ("executionStartToCloseTimeout" Lude..=)
              Lude.<$> executionStartToCloseTimeout,
            ("taskList" Lude..=) Lude.<$> taskList,
            ("taskPriority" Lude..=) Lude.<$> taskPriority,
            ("childPolicy" Lude..=) Lude.<$> childPolicy,
            Lude.Just ("workflowType" Lude..= workflowType),
            Lude.Just ("workflowId" Lude..= workflowId)
          ]
      )
