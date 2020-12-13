{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.StartWorkflowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an execution of the workflow type in the specified domain using the provided @workflowId@ and input data.
--
-- This action returns the newly started workflow execution.
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @tagList.member.0@ : The key is @swf:tagList.member.0@ .
--
--
--     * @tagList.member.1@ : The key is @swf:tagList.member.1@ .
--
--
--     * @tagList.member.2@ : The key is @swf:tagList.member.2@ .
--
--
--     * @tagList.member.3@ : The key is @swf:tagList.member.3@ .
--
--
--     * @tagList.member.4@ : The key is @swf:tagList.member.4@ .
--
--
--     * @taskList@ : String constraint. The key is @swf:taskList.name@ .
--
--
--     * @workflowType.name@ : String constraint. The key is @swf:workflowType.name@ .
--
--
--     * @workflowType.version@ : String constraint. The key is @swf:workflowType.version@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.StartWorkflowExecution
  ( -- * Creating a request
    StartWorkflowExecution (..),
    mkStartWorkflowExecution,

    -- ** Request lenses
    sweTagList,
    sweDomain,
    sweTaskStartToCloseTimeout,
    sweLambdaRole,
    sweWorkflowType,
    sweInput,
    sweExecutionStartToCloseTimeout,
    sweTaskList,
    sweTaskPriority,
    sweChildPolicy,
    sweWorkflowId,

    -- * Destructuring the response
    StartWorkflowExecutionResponse (..),
    mkStartWorkflowExecutionResponse,

    -- ** Response lenses
    swersRunId,
    swersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkStartWorkflowExecution' smart constructor.
data StartWorkflowExecution = StartWorkflowExecution'
  { -- | The list of tags to associate with the workflow execution. You can specify a maximum of 5 tags. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
    tagList :: Lude.Maybe [Lude.Text],
    -- | The name of the domain in which the workflow execution is created.
    domain :: Lude.Text,
    -- | Specifies the maximum duration of decision tasks for this workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' .
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    taskStartToCloseTimeout :: Lude.Maybe Lude.Text,
    -- | The IAM role to attach to this workflow execution.
    lambdaRole :: Lude.Maybe Lude.Text,
    -- | The type of the workflow to start.
    workflowType :: WorkflowType,
    -- | The input for the workflow execution. This is a free form string which should be meaningful to the workflow you are starting. This @input@ is made available to the new workflow execution in the @WorkflowExecutionStarted@ history event.
    input :: Lude.Maybe Lude.Text,
    -- | The total duration for this workflow execution. This overrides the defaultExecutionStartToCloseTimeout specified when registering the workflow type.
    --
    -- The duration is specified in seconds; an integer greater than or equal to @0@ . Exceeding this limit causes the workflow execution to time out. Unlike some of the other timeout parameters in Amazon SWF, you cannot specify a value of "NONE" for this timeout; there is a one-year max limit on the time that a workflow execution can run.
    executionStartToCloseTimeout :: Lude.Maybe Lude.Text,
    -- | The task list to use for the decision tasks generated for this workflow execution. This overrides the @defaultTaskList@ specified when registering the workflow type.
    --
    -- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
    taskList :: Lude.Maybe TaskList,
    -- | The task priority to use for this workflow execution. This overrides any default priority that was assigned when the workflow type was registered. If not set, then the default task priority for the workflow type is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
    --
    -- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
    taskPriority :: Lude.Maybe Lude.Text,
    -- | If set, specifies the policy to use for the child workflow executions of this workflow execution if it is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' .
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
    childPolicy :: Lude.Maybe ChildPolicy,
    -- | The user defined identifier associated with the workflow execution. You can use this to associate a custom identifier with the workflow execution. You may specify the same identifier if a workflow execution is logically a /restart/ of a previous execution. You cannot have two open workflow executions with the same @workflowId@ at the same time within the same domain.
    --
    -- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
    workflowId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartWorkflowExecution' with the minimum fields required to make a request.
--
-- * 'tagList' - The list of tags to associate with the workflow execution. You can specify a maximum of 5 tags. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
-- * 'domain' - The name of the domain in which the workflow execution is created.
-- * 'taskStartToCloseTimeout' - Specifies the maximum duration of decision tasks for this workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'lambdaRole' - The IAM role to attach to this workflow execution.
-- * 'workflowType' - The type of the workflow to start.
-- * 'input' - The input for the workflow execution. This is a free form string which should be meaningful to the workflow you are starting. This @input@ is made available to the new workflow execution in the @WorkflowExecutionStarted@ history event.
-- * 'executionStartToCloseTimeout' - The total duration for this workflow execution. This overrides the defaultExecutionStartToCloseTimeout specified when registering the workflow type.
--
-- The duration is specified in seconds; an integer greater than or equal to @0@ . Exceeding this limit causes the workflow execution to time out. Unlike some of the other timeout parameters in Amazon SWF, you cannot specify a value of "NONE" for this timeout; there is a one-year max limit on the time that a workflow execution can run.
-- * 'taskList' - The task list to use for the decision tasks generated for this workflow execution. This overrides the @defaultTaskList@ specified when registering the workflow type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
-- * 'taskPriority' - The task priority to use for this workflow execution. This overrides any default priority that was assigned when the workflow type was registered. If not set, then the default task priority for the workflow type is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
-- * 'childPolicy' - If set, specifies the policy to use for the child workflow executions of this workflow execution if it is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' .
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
-- * 'workflowId' - The user defined identifier associated with the workflow execution. You can use this to associate a custom identifier with the workflow execution. You may specify the same identifier if a workflow execution is logically a /restart/ of a previous execution. You cannot have two open workflow executions with the same @workflowId@ at the same time within the same domain.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
mkStartWorkflowExecution ::
  -- | 'domain'
  Lude.Text ->
  -- | 'workflowType'
  WorkflowType ->
  -- | 'workflowId'
  Lude.Text ->
  StartWorkflowExecution
mkStartWorkflowExecution pDomain_ pWorkflowType_ pWorkflowId_ =
  StartWorkflowExecution'
    { tagList = Lude.Nothing,
      domain = pDomain_,
      taskStartToCloseTimeout = Lude.Nothing,
      lambdaRole = Lude.Nothing,
      workflowType = pWorkflowType_,
      input = Lude.Nothing,
      executionStartToCloseTimeout = Lude.Nothing,
      taskList = Lude.Nothing,
      taskPriority = Lude.Nothing,
      childPolicy = Lude.Nothing,
      workflowId = pWorkflowId_
    }

-- | The list of tags to associate with the workflow execution. You can specify a maximum of 5 tags. You can list workflow executions with a specific tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and specifying a 'TagFilter' .
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweTagList :: Lens.Lens' StartWorkflowExecution (Lude.Maybe [Lude.Text])
sweTagList = Lens.lens (tagList :: StartWorkflowExecution -> Lude.Maybe [Lude.Text]) (\s a -> s {tagList = a} :: StartWorkflowExecution)
{-# DEPRECATED sweTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The name of the domain in which the workflow execution is created.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweDomain :: Lens.Lens' StartWorkflowExecution Lude.Text
sweDomain = Lens.lens (domain :: StartWorkflowExecution -> Lude.Text) (\s a -> s {domain = a} :: StartWorkflowExecution)
{-# DEPRECATED sweDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Specifies the maximum duration of decision tasks for this workflow execution. This parameter overrides the @defaultTaskStartToCloseTimout@ specified when registering the workflow type using 'RegisterWorkflowType' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'taskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweTaskStartToCloseTimeout :: Lens.Lens' StartWorkflowExecution (Lude.Maybe Lude.Text)
sweTaskStartToCloseTimeout = Lens.lens (taskStartToCloseTimeout :: StartWorkflowExecution -> Lude.Maybe Lude.Text) (\s a -> s {taskStartToCloseTimeout = a} :: StartWorkflowExecution)
{-# DEPRECATED sweTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'taskStartToCloseTimeout' instead." #-}

-- | The IAM role to attach to this workflow execution.
--
-- /Note:/ Consider using 'lambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweLambdaRole :: Lens.Lens' StartWorkflowExecution (Lude.Maybe Lude.Text)
sweLambdaRole = Lens.lens (lambdaRole :: StartWorkflowExecution -> Lude.Maybe Lude.Text) (\s a -> s {lambdaRole = a} :: StartWorkflowExecution)
{-# DEPRECATED sweLambdaRole "Use generic-lens or generic-optics with 'lambdaRole' instead." #-}

-- | The type of the workflow to start.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweWorkflowType :: Lens.Lens' StartWorkflowExecution WorkflowType
sweWorkflowType = Lens.lens (workflowType :: StartWorkflowExecution -> WorkflowType) (\s a -> s {workflowType = a} :: StartWorkflowExecution)
{-# DEPRECATED sweWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | The input for the workflow execution. This is a free form string which should be meaningful to the workflow you are starting. This @input@ is made available to the new workflow execution in the @WorkflowExecutionStarted@ history event.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweInput :: Lens.Lens' StartWorkflowExecution (Lude.Maybe Lude.Text)
sweInput = Lens.lens (input :: StartWorkflowExecution -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: StartWorkflowExecution)
{-# DEPRECATED sweInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The total duration for this workflow execution. This overrides the defaultExecutionStartToCloseTimeout specified when registering the workflow type.
--
-- The duration is specified in seconds; an integer greater than or equal to @0@ . Exceeding this limit causes the workflow execution to time out. Unlike some of the other timeout parameters in Amazon SWF, you cannot specify a value of "NONE" for this timeout; there is a one-year max limit on the time that a workflow execution can run.
--
-- /Note:/ Consider using 'executionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweExecutionStartToCloseTimeout :: Lens.Lens' StartWorkflowExecution (Lude.Maybe Lude.Text)
sweExecutionStartToCloseTimeout = Lens.lens (executionStartToCloseTimeout :: StartWorkflowExecution -> Lude.Maybe Lude.Text) (\s a -> s {executionStartToCloseTimeout = a} :: StartWorkflowExecution)
{-# DEPRECATED sweExecutionStartToCloseTimeout "Use generic-lens or generic-optics with 'executionStartToCloseTimeout' instead." #-}

-- | The task list to use for the decision tasks generated for this workflow execution. This overrides the @defaultTaskList@ specified when registering the workflow type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweTaskList :: Lens.Lens' StartWorkflowExecution (Lude.Maybe TaskList)
sweTaskList = Lens.lens (taskList :: StartWorkflowExecution -> Lude.Maybe TaskList) (\s a -> s {taskList = a} :: StartWorkflowExecution)
{-# DEPRECATED sweTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

-- | The task priority to use for this workflow execution. This overrides any default priority that was assigned when the workflow type was registered. If not set, then the default task priority for the workflow type is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'taskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweTaskPriority :: Lens.Lens' StartWorkflowExecution (Lude.Maybe Lude.Text)
sweTaskPriority = Lens.lens (taskPriority :: StartWorkflowExecution -> Lude.Maybe Lude.Text) (\s a -> s {taskPriority = a} :: StartWorkflowExecution)
{-# DEPRECATED sweTaskPriority "Use generic-lens or generic-optics with 'taskPriority' instead." #-}

-- | If set, specifies the policy to use for the child workflow executions of this workflow execution if it is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the default child policy specified when registering the workflow type using 'RegisterWorkflowType' .
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
sweChildPolicy :: Lens.Lens' StartWorkflowExecution (Lude.Maybe ChildPolicy)
sweChildPolicy = Lens.lens (childPolicy :: StartWorkflowExecution -> Lude.Maybe ChildPolicy) (\s a -> s {childPolicy = a} :: StartWorkflowExecution)
{-# DEPRECATED sweChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

-- | The user defined identifier associated with the workflow execution. You can use this to associate a custom identifier with the workflow execution. You may specify the same identifier if a workflow execution is logically a /restart/ of a previous execution. You cannot have two open workflow executions with the same @workflowId@ at the same time within the same domain.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweWorkflowId :: Lens.Lens' StartWorkflowExecution Lude.Text
sweWorkflowId = Lens.lens (workflowId :: StartWorkflowExecution -> Lude.Text) (\s a -> s {workflowId = a} :: StartWorkflowExecution)
{-# DEPRECATED sweWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

instance Lude.AWSRequest StartWorkflowExecution where
  type Rs StartWorkflowExecution = StartWorkflowExecutionResponse
  request = Req.postJSON swfService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartWorkflowExecutionResponse'
            Lude.<$> (x Lude..?> "runId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartWorkflowExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SimpleWorkflowService.StartWorkflowExecution" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartWorkflowExecution where
  toJSON StartWorkflowExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("tagList" Lude..=) Lude.<$> tagList,
            Lude.Just ("domain" Lude..= domain),
            ("taskStartToCloseTimeout" Lude..=)
              Lude.<$> taskStartToCloseTimeout,
            ("lambdaRole" Lude..=) Lude.<$> lambdaRole,
            Lude.Just ("workflowType" Lude..= workflowType),
            ("input" Lude..=) Lude.<$> input,
            ("executionStartToCloseTimeout" Lude..=)
              Lude.<$> executionStartToCloseTimeout,
            ("taskList" Lude..=) Lude.<$> taskList,
            ("taskPriority" Lude..=) Lude.<$> taskPriority,
            ("childPolicy" Lude..=) Lude.<$> childPolicy,
            Lude.Just ("workflowId" Lude..= workflowId)
          ]
      )

instance Lude.ToPath StartWorkflowExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery StartWorkflowExecution where
  toQuery = Lude.const Lude.mempty

-- | Specifies the @runId@ of a workflow execution.
--
-- /See:/ 'mkStartWorkflowExecutionResponse' smart constructor.
data StartWorkflowExecutionResponse = StartWorkflowExecutionResponse'
  { -- | The @runId@ of a workflow execution. This ID is generated by the service and can be used to uniquely identify the workflow execution within a domain.
    runId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartWorkflowExecutionResponse' with the minimum fields required to make a request.
--
-- * 'runId' - The @runId@ of a workflow execution. This ID is generated by the service and can be used to uniquely identify the workflow execution within a domain.
-- * 'responseStatus' - The response status code.
mkStartWorkflowExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartWorkflowExecutionResponse
mkStartWorkflowExecutionResponse pResponseStatus_ =
  StartWorkflowExecutionResponse'
    { runId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @runId@ of a workflow execution. This ID is generated by the service and can be used to uniquely identify the workflow execution within a domain.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swersRunId :: Lens.Lens' StartWorkflowExecutionResponse (Lude.Maybe Lude.Text)
swersRunId = Lens.lens (runId :: StartWorkflowExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {runId = a} :: StartWorkflowExecutionResponse)
{-# DEPRECATED swersRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swersResponseStatus :: Lens.Lens' StartWorkflowExecutionResponse Lude.Int
swersResponseStatus = Lens.lens (responseStatus :: StartWorkflowExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartWorkflowExecutionResponse)
{-# DEPRECATED swersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
