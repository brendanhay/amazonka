{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.RegisterWorkflowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new /workflow type/ and its configuration settings in the specified domain.
--
-- The retention period for the workflow history is set by the 'RegisterDomain' action.
-- /Important:/ If the type already exists, then a @TypeAlreadyExists@ fault is returned. You cannot change the configuration settings of a workflow type once it is registered and it must be registered as a new version.
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
--     * @defaultTaskList.name@ : String constraint. The key is @swf:defaultTaskList.name@ .
--
--
--     * @name@ : String constraint. The key is @swf:name@ .
--
--
--     * @version@ : String constraint. The key is @swf:version@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.RegisterWorkflowType
  ( -- * Creating a request
    RegisterWorkflowType (..),
    mkRegisterWorkflowType,

    -- ** Request lenses
    rwtDefaultLambdaRole,
    rwtDomain,
    rwtDefaultChildPolicy,
    rwtDefaultTaskList,
    rwtDefaultTaskPriority,
    rwtName,
    rwtDefaultExecutionStartToCloseTimeout,
    rwtVersion,
    rwtDefaultTaskStartToCloseTimeout,
    rwtDescription,

    -- * Destructuring the response
    RegisterWorkflowTypeResponse (..),
    mkRegisterWorkflowTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkRegisterWorkflowType' smart constructor.
data RegisterWorkflowType = RegisterWorkflowType'
  { -- | The default IAM role attached to this workflow type.
    defaultLambdaRole :: Lude.Maybe Lude.Text,
    -- | The name of the domain in which to register the workflow type.
    domain :: Lude.Text,
    -- | If set, specifies the default policy to use for the child workflow executions when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
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
    defaultChildPolicy :: Lude.Maybe ChildPolicy,
    -- | If set, specifies the default task list to use for scheduling decision tasks for executions of this workflow type. This default is used only if a task list isn't provided when starting the execution through the 'StartWorkflowExecution' Action or @StartChildWorkflowExecution@ 'Decision' .
    defaultTaskList :: Lude.Maybe TaskList,
    -- | The default task priority to assign to the workflow type. If not assigned, then @0@ is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
    --
    -- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
    defaultTaskPriority :: Lude.Maybe Lude.Text,
    -- | The name of the workflow type.
    --
    -- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
    name :: Lude.Text,
    -- | If set, specifies the default maximum duration for executions of this workflow type. You can override this default when starting an execution through the 'StartWorkflowExecution' Action or @StartChildWorkflowExecution@ 'Decision' .
    --
    -- The duration is specified in seconds; an integer greater than or equal to 0. Unlike some of the other timeout parameters in Amazon SWF, you cannot specify a value of "NONE" for @defaultExecutionStartToCloseTimeout@ ; there is a one-year max limit on the time that a workflow execution can run. Exceeding this limit always causes the workflow execution to time out.
    defaultExecutionStartToCloseTimeout :: Lude.Maybe Lude.Text,
    -- | The version of the workflow type.
    --
    -- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
    version :: Lude.Text,
    -- | If set, specifies the default maximum duration of decision tasks for this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    defaultTaskStartToCloseTimeout :: Lude.Maybe Lude.Text,
    -- | Textual description of the workflow type.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterWorkflowType' with the minimum fields required to make a request.
--
-- * 'defaultLambdaRole' - The default IAM role attached to this workflow type.
-- * 'domain' - The name of the domain in which to register the workflow type.
-- * 'defaultChildPolicy' - If set, specifies the default policy to use for the child workflow executions when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
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
-- * 'defaultTaskList' - If set, specifies the default task list to use for scheduling decision tasks for executions of this workflow type. This default is used only if a task list isn't provided when starting the execution through the 'StartWorkflowExecution' Action or @StartChildWorkflowExecution@ 'Decision' .
-- * 'defaultTaskPriority' - The default task priority to assign to the workflow type. If not assigned, then @0@ is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
-- * 'name' - The name of the workflow type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
-- * 'defaultExecutionStartToCloseTimeout' - If set, specifies the default maximum duration for executions of this workflow type. You can override this default when starting an execution through the 'StartWorkflowExecution' Action or @StartChildWorkflowExecution@ 'Decision' .
--
-- The duration is specified in seconds; an integer greater than or equal to 0. Unlike some of the other timeout parameters in Amazon SWF, you cannot specify a value of "NONE" for @defaultExecutionStartToCloseTimeout@ ; there is a one-year max limit on the time that a workflow execution can run. Exceeding this limit always causes the workflow execution to time out.
-- * 'version' - The version of the workflow type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
-- * 'defaultTaskStartToCloseTimeout' - If set, specifies the default maximum duration of decision tasks for this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'description' - Textual description of the workflow type.
mkRegisterWorkflowType ::
  -- | 'domain'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  RegisterWorkflowType
mkRegisterWorkflowType pDomain_ pName_ pVersion_ =
  RegisterWorkflowType'
    { defaultLambdaRole = Lude.Nothing,
      domain = pDomain_,
      defaultChildPolicy = Lude.Nothing,
      defaultTaskList = Lude.Nothing,
      defaultTaskPriority = Lude.Nothing,
      name = pName_,
      defaultExecutionStartToCloseTimeout = Lude.Nothing,
      version = pVersion_,
      defaultTaskStartToCloseTimeout = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The default IAM role attached to this workflow type.
--
-- /Note:/ Consider using 'defaultLambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDefaultLambdaRole :: Lens.Lens' RegisterWorkflowType (Lude.Maybe Lude.Text)
rwtDefaultLambdaRole = Lens.lens (defaultLambdaRole :: RegisterWorkflowType -> Lude.Maybe Lude.Text) (\s a -> s {defaultLambdaRole = a} :: RegisterWorkflowType)
{-# DEPRECATED rwtDefaultLambdaRole "Use generic-lens or generic-optics with 'defaultLambdaRole' instead." #-}

-- | The name of the domain in which to register the workflow type.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDomain :: Lens.Lens' RegisterWorkflowType Lude.Text
rwtDomain = Lens.lens (domain :: RegisterWorkflowType -> Lude.Text) (\s a -> s {domain = a} :: RegisterWorkflowType)
{-# DEPRECATED rwtDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | If set, specifies the default policy to use for the child workflow executions when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
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
-- /Note:/ Consider using 'defaultChildPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDefaultChildPolicy :: Lens.Lens' RegisterWorkflowType (Lude.Maybe ChildPolicy)
rwtDefaultChildPolicy = Lens.lens (defaultChildPolicy :: RegisterWorkflowType -> Lude.Maybe ChildPolicy) (\s a -> s {defaultChildPolicy = a} :: RegisterWorkflowType)
{-# DEPRECATED rwtDefaultChildPolicy "Use generic-lens or generic-optics with 'defaultChildPolicy' instead." #-}

-- | If set, specifies the default task list to use for scheduling decision tasks for executions of this workflow type. This default is used only if a task list isn't provided when starting the execution through the 'StartWorkflowExecution' Action or @StartChildWorkflowExecution@ 'Decision' .
--
-- /Note:/ Consider using 'defaultTaskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDefaultTaskList :: Lens.Lens' RegisterWorkflowType (Lude.Maybe TaskList)
rwtDefaultTaskList = Lens.lens (defaultTaskList :: RegisterWorkflowType -> Lude.Maybe TaskList) (\s a -> s {defaultTaskList = a} :: RegisterWorkflowType)
{-# DEPRECATED rwtDefaultTaskList "Use generic-lens or generic-optics with 'defaultTaskList' instead." #-}

-- | The default task priority to assign to the workflow type. If not assigned, then @0@ is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'defaultTaskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDefaultTaskPriority :: Lens.Lens' RegisterWorkflowType (Lude.Maybe Lude.Text)
rwtDefaultTaskPriority = Lens.lens (defaultTaskPriority :: RegisterWorkflowType -> Lude.Maybe Lude.Text) (\s a -> s {defaultTaskPriority = a} :: RegisterWorkflowType)
{-# DEPRECATED rwtDefaultTaskPriority "Use generic-lens or generic-optics with 'defaultTaskPriority' instead." #-}

-- | The name of the workflow type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtName :: Lens.Lens' RegisterWorkflowType Lude.Text
rwtName = Lens.lens (name :: RegisterWorkflowType -> Lude.Text) (\s a -> s {name = a} :: RegisterWorkflowType)
{-# DEPRECATED rwtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | If set, specifies the default maximum duration for executions of this workflow type. You can override this default when starting an execution through the 'StartWorkflowExecution' Action or @StartChildWorkflowExecution@ 'Decision' .
--
-- The duration is specified in seconds; an integer greater than or equal to 0. Unlike some of the other timeout parameters in Amazon SWF, you cannot specify a value of "NONE" for @defaultExecutionStartToCloseTimeout@ ; there is a one-year max limit on the time that a workflow execution can run. Exceeding this limit always causes the workflow execution to time out.
--
-- /Note:/ Consider using 'defaultExecutionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDefaultExecutionStartToCloseTimeout :: Lens.Lens' RegisterWorkflowType (Lude.Maybe Lude.Text)
rwtDefaultExecutionStartToCloseTimeout = Lens.lens (defaultExecutionStartToCloseTimeout :: RegisterWorkflowType -> Lude.Maybe Lude.Text) (\s a -> s {defaultExecutionStartToCloseTimeout = a} :: RegisterWorkflowType)
{-# DEPRECATED rwtDefaultExecutionStartToCloseTimeout "Use generic-lens or generic-optics with 'defaultExecutionStartToCloseTimeout' instead." #-}

-- | The version of the workflow type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtVersion :: Lens.Lens' RegisterWorkflowType Lude.Text
rwtVersion = Lens.lens (version :: RegisterWorkflowType -> Lude.Text) (\s a -> s {version = a} :: RegisterWorkflowType)
{-# DEPRECATED rwtVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | If set, specifies the default maximum duration of decision tasks for this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDefaultTaskStartToCloseTimeout :: Lens.Lens' RegisterWorkflowType (Lude.Maybe Lude.Text)
rwtDefaultTaskStartToCloseTimeout = Lens.lens (defaultTaskStartToCloseTimeout :: RegisterWorkflowType -> Lude.Maybe Lude.Text) (\s a -> s {defaultTaskStartToCloseTimeout = a} :: RegisterWorkflowType)
{-# DEPRECATED rwtDefaultTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'defaultTaskStartToCloseTimeout' instead." #-}

-- | Textual description of the workflow type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDescription :: Lens.Lens' RegisterWorkflowType (Lude.Maybe Lude.Text)
rwtDescription = Lens.lens (description :: RegisterWorkflowType -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RegisterWorkflowType)
{-# DEPRECATED rwtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest RegisterWorkflowType where
  type Rs RegisterWorkflowType = RegisterWorkflowTypeResponse
  request = Req.postJSON swfService
  response = Res.receiveNull RegisterWorkflowTypeResponse'

instance Lude.ToHeaders RegisterWorkflowType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.RegisterWorkflowType" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterWorkflowType where
  toJSON RegisterWorkflowType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("defaultLambdaRole" Lude..=) Lude.<$> defaultLambdaRole,
            Lude.Just ("domain" Lude..= domain),
            ("defaultChildPolicy" Lude..=) Lude.<$> defaultChildPolicy,
            ("defaultTaskList" Lude..=) Lude.<$> defaultTaskList,
            ("defaultTaskPriority" Lude..=) Lude.<$> defaultTaskPriority,
            Lude.Just ("name" Lude..= name),
            ("defaultExecutionStartToCloseTimeout" Lude..=)
              Lude.<$> defaultExecutionStartToCloseTimeout,
            Lude.Just ("version" Lude..= version),
            ("defaultTaskStartToCloseTimeout" Lude..=)
              Lude.<$> defaultTaskStartToCloseTimeout,
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath RegisterWorkflowType where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterWorkflowType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterWorkflowTypeResponse' smart constructor.
data RegisterWorkflowTypeResponse = RegisterWorkflowTypeResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterWorkflowTypeResponse' with the minimum fields required to make a request.
mkRegisterWorkflowTypeResponse ::
  RegisterWorkflowTypeResponse
mkRegisterWorkflowTypeResponse = RegisterWorkflowTypeResponse'
