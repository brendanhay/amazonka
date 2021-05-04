{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.RegisterWorkflowType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new /workflow type/ and its configuration settings in the
-- specified domain.
--
-- The retention period for the workflow history is set by the
-- RegisterDomain action.
--
-- If the type already exists, then a @TypeAlreadyExists@ fault is
-- returned. You cannot change the configuration settings of a workflow
-- type once it is registered and it must be registered as a new version.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--
--     -   @defaultTaskList.name@: String constraint. The key is
--         @swf:defaultTaskList.name@.
--
--     -   @name@: String constraint. The key is @swf:name@.
--
--     -   @version@: String constraint. The key is @swf:version@.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
module Network.AWS.SWF.RegisterWorkflowType
  ( -- * Creating a Request
    RegisterWorkflowType (..),
    newRegisterWorkflowType,

    -- * Request Lenses
    registerWorkflowType_defaultExecutionStartToCloseTimeout,
    registerWorkflowType_defaultTaskPriority,
    registerWorkflowType_defaultTaskList,
    registerWorkflowType_defaultChildPolicy,
    registerWorkflowType_description,
    registerWorkflowType_defaultLambdaRole,
    registerWorkflowType_defaultTaskStartToCloseTimeout,
    registerWorkflowType_domain,
    registerWorkflowType_name,
    registerWorkflowType_version,

    -- * Destructuring the Response
    RegisterWorkflowTypeResponse (..),
    newRegisterWorkflowTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newRegisterWorkflowType' smart constructor.
data RegisterWorkflowType = RegisterWorkflowType'
  { -- | If set, specifies the default maximum duration for executions of this
    -- workflow type. You can override this default when starting an execution
    -- through the StartWorkflowExecution Action or
    -- @StartChildWorkflowExecution@ Decision.
    --
    -- The duration is specified in seconds; an integer greater than or equal
    -- to 0. Unlike some of the other timeout parameters in Amazon SWF, you
    -- cannot specify a value of \"NONE\" for
    -- @defaultExecutionStartToCloseTimeout@; there is a one-year max limit on
    -- the time that a workflow execution can run. Exceeding this limit always
    -- causes the workflow execution to time out.
    defaultExecutionStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The default task priority to assign to the workflow type. If not
    -- assigned, then @0@ is used. Valid values are integers that range from
    -- Java\'s @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@
    -- (2147483647). Higher numbers indicate higher priority.
    --
    -- For more information about setting task priority, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
    -- in the /Amazon SWF Developer Guide/.
    defaultTaskPriority :: Prelude.Maybe Prelude.Text,
    -- | If set, specifies the default task list to use for scheduling decision
    -- tasks for executions of this workflow type. This default is used only if
    -- a task list isn\'t provided when starting the execution through the
    -- StartWorkflowExecution Action or @StartChildWorkflowExecution@ Decision.
    defaultTaskList :: Prelude.Maybe TaskList,
    -- | If set, specifies the default policy to use for the child workflow
    -- executions when a workflow execution of this type is terminated, by
    -- calling the TerminateWorkflowExecution action explicitly or due to an
    -- expired timeout. This default can be overridden when starting a workflow
    -- execution using the StartWorkflowExecution action or the
    -- @StartChildWorkflowExecution@ Decision.
    --
    -- The supported child policies are:
    --
    -- -   @TERMINATE@ – The child executions are terminated.
    --
    -- -   @REQUEST_CANCEL@ – A request to cancel is attempted for each child
    --     execution by recording a @WorkflowExecutionCancelRequested@ event in
    --     its history. It is up to the decider to take appropriate actions
    --     when it receives an execution history with this event.
    --
    -- -   @ABANDON@ – No action is taken. The child executions continue to
    --     run.
    defaultChildPolicy :: Prelude.Maybe ChildPolicy,
    -- | Textual description of the workflow type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The default IAM role attached to this workflow type.
    --
    -- Executions of this workflow type need IAM roles to invoke Lambda
    -- functions. If you don\'t specify an IAM role when you start this
    -- workflow type, the default Lambda role is attached to the execution. For
    -- more information, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html>
    -- in the /Amazon SWF Developer Guide/.
    defaultLambdaRole :: Prelude.Maybe Prelude.Text,
    -- | If set, specifies the default maximum duration of decision tasks for
    -- this workflow type. This default can be overridden when starting a
    -- workflow execution using the StartWorkflowExecution action or the
    -- @StartChildWorkflowExecution@ Decision.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    defaultTaskStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain in which to register the workflow type.
    domain :: Prelude.Text,
    -- | The name of the workflow type.
    --
    -- The specified string must not start or end with whitespace. It must not
    -- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
    -- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
    -- /be/ the literal string @arn@.
    name :: Prelude.Text,
    -- | The version of the workflow type.
    --
    -- The workflow type consists of the name and version, the combination of
    -- which must be unique within the domain. To get a list of all currently
    -- registered workflow types, use the ListWorkflowTypes action.
    --
    -- The specified string must not start or end with whitespace. It must not
    -- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
    -- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
    -- /be/ the literal string @arn@.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterWorkflowType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultExecutionStartToCloseTimeout', 'registerWorkflowType_defaultExecutionStartToCloseTimeout' - If set, specifies the default maximum duration for executions of this
-- workflow type. You can override this default when starting an execution
-- through the StartWorkflowExecution Action or
-- @StartChildWorkflowExecution@ Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. Unlike some of the other timeout parameters in Amazon SWF, you
-- cannot specify a value of \"NONE\" for
-- @defaultExecutionStartToCloseTimeout@; there is a one-year max limit on
-- the time that a workflow execution can run. Exceeding this limit always
-- causes the workflow execution to time out.
--
-- 'defaultTaskPriority', 'registerWorkflowType_defaultTaskPriority' - The default task priority to assign to the workflow type. If not
-- assigned, then @0@ is used. Valid values are integers that range from
-- Java\'s @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@
-- (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
--
-- 'defaultTaskList', 'registerWorkflowType_defaultTaskList' - If set, specifies the default task list to use for scheduling decision
-- tasks for executions of this workflow type. This default is used only if
-- a task list isn\'t provided when starting the execution through the
-- StartWorkflowExecution Action or @StartChildWorkflowExecution@ Decision.
--
-- 'defaultChildPolicy', 'registerWorkflowType_defaultChildPolicy' - If set, specifies the default policy to use for the child workflow
-- executions when a workflow execution of this type is terminated, by
-- calling the TerminateWorkflowExecution action explicitly or due to an
-- expired timeout. This default can be overridden when starting a workflow
-- execution using the StartWorkflowExecution action or the
-- @StartChildWorkflowExecution@ Decision.
--
-- The supported child policies are:
--
-- -   @TERMINATE@ – The child executions are terminated.
--
-- -   @REQUEST_CANCEL@ – A request to cancel is attempted for each child
--     execution by recording a @WorkflowExecutionCancelRequested@ event in
--     its history. It is up to the decider to take appropriate actions
--     when it receives an execution history with this event.
--
-- -   @ABANDON@ – No action is taken. The child executions continue to
--     run.
--
-- 'description', 'registerWorkflowType_description' - Textual description of the workflow type.
--
-- 'defaultLambdaRole', 'registerWorkflowType_defaultLambdaRole' - The default IAM role attached to this workflow type.
--
-- Executions of this workflow type need IAM roles to invoke Lambda
-- functions. If you don\'t specify an IAM role when you start this
-- workflow type, the default Lambda role is attached to the execution. For
-- more information, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html>
-- in the /Amazon SWF Developer Guide/.
--
-- 'defaultTaskStartToCloseTimeout', 'registerWorkflowType_defaultTaskStartToCloseTimeout' - If set, specifies the default maximum duration of decision tasks for
-- this workflow type. This default can be overridden when starting a
-- workflow execution using the StartWorkflowExecution action or the
-- @StartChildWorkflowExecution@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'domain', 'registerWorkflowType_domain' - The name of the domain in which to register the workflow type.
--
-- 'name', 'registerWorkflowType_name' - The name of the workflow type.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
--
-- 'version', 'registerWorkflowType_version' - The version of the workflow type.
--
-- The workflow type consists of the name and version, the combination of
-- which must be unique within the domain. To get a list of all currently
-- registered workflow types, use the ListWorkflowTypes action.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
newRegisterWorkflowType ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  RegisterWorkflowType
newRegisterWorkflowType pDomain_ pName_ pVersion_ =
  RegisterWorkflowType'
    { defaultExecutionStartToCloseTimeout =
        Prelude.Nothing,
      defaultTaskPriority = Prelude.Nothing,
      defaultTaskList = Prelude.Nothing,
      defaultChildPolicy = Prelude.Nothing,
      description = Prelude.Nothing,
      defaultLambdaRole = Prelude.Nothing,
      defaultTaskStartToCloseTimeout = Prelude.Nothing,
      domain = pDomain_,
      name = pName_,
      version = pVersion_
    }

-- | If set, specifies the default maximum duration for executions of this
-- workflow type. You can override this default when starting an execution
-- through the StartWorkflowExecution Action or
-- @StartChildWorkflowExecution@ Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. Unlike some of the other timeout parameters in Amazon SWF, you
-- cannot specify a value of \"NONE\" for
-- @defaultExecutionStartToCloseTimeout@; there is a one-year max limit on
-- the time that a workflow execution can run. Exceeding this limit always
-- causes the workflow execution to time out.
registerWorkflowType_defaultExecutionStartToCloseTimeout :: Lens.Lens' RegisterWorkflowType (Prelude.Maybe Prelude.Text)
registerWorkflowType_defaultExecutionStartToCloseTimeout = Lens.lens (\RegisterWorkflowType' {defaultExecutionStartToCloseTimeout} -> defaultExecutionStartToCloseTimeout) (\s@RegisterWorkflowType' {} a -> s {defaultExecutionStartToCloseTimeout = a} :: RegisterWorkflowType)

-- | The default task priority to assign to the workflow type. If not
-- assigned, then @0@ is used. Valid values are integers that range from
-- Java\'s @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@
-- (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
registerWorkflowType_defaultTaskPriority :: Lens.Lens' RegisterWorkflowType (Prelude.Maybe Prelude.Text)
registerWorkflowType_defaultTaskPriority = Lens.lens (\RegisterWorkflowType' {defaultTaskPriority} -> defaultTaskPriority) (\s@RegisterWorkflowType' {} a -> s {defaultTaskPriority = a} :: RegisterWorkflowType)

-- | If set, specifies the default task list to use for scheduling decision
-- tasks for executions of this workflow type. This default is used only if
-- a task list isn\'t provided when starting the execution through the
-- StartWorkflowExecution Action or @StartChildWorkflowExecution@ Decision.
registerWorkflowType_defaultTaskList :: Lens.Lens' RegisterWorkflowType (Prelude.Maybe TaskList)
registerWorkflowType_defaultTaskList = Lens.lens (\RegisterWorkflowType' {defaultTaskList} -> defaultTaskList) (\s@RegisterWorkflowType' {} a -> s {defaultTaskList = a} :: RegisterWorkflowType)

-- | If set, specifies the default policy to use for the child workflow
-- executions when a workflow execution of this type is terminated, by
-- calling the TerminateWorkflowExecution action explicitly or due to an
-- expired timeout. This default can be overridden when starting a workflow
-- execution using the StartWorkflowExecution action or the
-- @StartChildWorkflowExecution@ Decision.
--
-- The supported child policies are:
--
-- -   @TERMINATE@ – The child executions are terminated.
--
-- -   @REQUEST_CANCEL@ – A request to cancel is attempted for each child
--     execution by recording a @WorkflowExecutionCancelRequested@ event in
--     its history. It is up to the decider to take appropriate actions
--     when it receives an execution history with this event.
--
-- -   @ABANDON@ – No action is taken. The child executions continue to
--     run.
registerWorkflowType_defaultChildPolicy :: Lens.Lens' RegisterWorkflowType (Prelude.Maybe ChildPolicy)
registerWorkflowType_defaultChildPolicy = Lens.lens (\RegisterWorkflowType' {defaultChildPolicy} -> defaultChildPolicy) (\s@RegisterWorkflowType' {} a -> s {defaultChildPolicy = a} :: RegisterWorkflowType)

-- | Textual description of the workflow type.
registerWorkflowType_description :: Lens.Lens' RegisterWorkflowType (Prelude.Maybe Prelude.Text)
registerWorkflowType_description = Lens.lens (\RegisterWorkflowType' {description} -> description) (\s@RegisterWorkflowType' {} a -> s {description = a} :: RegisterWorkflowType)

-- | The default IAM role attached to this workflow type.
--
-- Executions of this workflow type need IAM roles to invoke Lambda
-- functions. If you don\'t specify an IAM role when you start this
-- workflow type, the default Lambda role is attached to the execution. For
-- more information, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/lambda-task.html>
-- in the /Amazon SWF Developer Guide/.
registerWorkflowType_defaultLambdaRole :: Lens.Lens' RegisterWorkflowType (Prelude.Maybe Prelude.Text)
registerWorkflowType_defaultLambdaRole = Lens.lens (\RegisterWorkflowType' {defaultLambdaRole} -> defaultLambdaRole) (\s@RegisterWorkflowType' {} a -> s {defaultLambdaRole = a} :: RegisterWorkflowType)

-- | If set, specifies the default maximum duration of decision tasks for
-- this workflow type. This default can be overridden when starting a
-- workflow execution using the StartWorkflowExecution action or the
-- @StartChildWorkflowExecution@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
registerWorkflowType_defaultTaskStartToCloseTimeout :: Lens.Lens' RegisterWorkflowType (Prelude.Maybe Prelude.Text)
registerWorkflowType_defaultTaskStartToCloseTimeout = Lens.lens (\RegisterWorkflowType' {defaultTaskStartToCloseTimeout} -> defaultTaskStartToCloseTimeout) (\s@RegisterWorkflowType' {} a -> s {defaultTaskStartToCloseTimeout = a} :: RegisterWorkflowType)

-- | The name of the domain in which to register the workflow type.
registerWorkflowType_domain :: Lens.Lens' RegisterWorkflowType Prelude.Text
registerWorkflowType_domain = Lens.lens (\RegisterWorkflowType' {domain} -> domain) (\s@RegisterWorkflowType' {} a -> s {domain = a} :: RegisterWorkflowType)

-- | The name of the workflow type.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
registerWorkflowType_name :: Lens.Lens' RegisterWorkflowType Prelude.Text
registerWorkflowType_name = Lens.lens (\RegisterWorkflowType' {name} -> name) (\s@RegisterWorkflowType' {} a -> s {name = a} :: RegisterWorkflowType)

-- | The version of the workflow type.
--
-- The workflow type consists of the name and version, the combination of
-- which must be unique within the domain. To get a list of all currently
-- registered workflow types, use the ListWorkflowTypes action.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
registerWorkflowType_version :: Lens.Lens' RegisterWorkflowType Prelude.Text
registerWorkflowType_version = Lens.lens (\RegisterWorkflowType' {version} -> version) (\s@RegisterWorkflowType' {} a -> s {version = a} :: RegisterWorkflowType)

instance Prelude.AWSRequest RegisterWorkflowType where
  type
    Rs RegisterWorkflowType =
      RegisterWorkflowTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull RegisterWorkflowTypeResponse'

instance Prelude.Hashable RegisterWorkflowType

instance Prelude.NFData RegisterWorkflowType

instance Prelude.ToHeaders RegisterWorkflowType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SimpleWorkflowService.RegisterWorkflowType" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RegisterWorkflowType where
  toJSON RegisterWorkflowType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("defaultExecutionStartToCloseTimeout" Prelude..=)
              Prelude.<$> defaultExecutionStartToCloseTimeout,
            ("defaultTaskPriority" Prelude..=)
              Prelude.<$> defaultTaskPriority,
            ("defaultTaskList" Prelude..=)
              Prelude.<$> defaultTaskList,
            ("defaultChildPolicy" Prelude..=)
              Prelude.<$> defaultChildPolicy,
            ("description" Prelude..=) Prelude.<$> description,
            ("defaultLambdaRole" Prelude..=)
              Prelude.<$> defaultLambdaRole,
            ("defaultTaskStartToCloseTimeout" Prelude..=)
              Prelude.<$> defaultTaskStartToCloseTimeout,
            Prelude.Just ("domain" Prelude..= domain),
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("version" Prelude..= version)
          ]
      )

instance Prelude.ToPath RegisterWorkflowType where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RegisterWorkflowType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterWorkflowTypeResponse' smart constructor.
data RegisterWorkflowTypeResponse = RegisterWorkflowTypeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterWorkflowTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRegisterWorkflowTypeResponse ::
  RegisterWorkflowTypeResponse
newRegisterWorkflowTypeResponse =
  RegisterWorkflowTypeResponse'

instance Prelude.NFData RegisterWorkflowTypeResponse
