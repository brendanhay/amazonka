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
-- Module      : Amazonka.SWF.PollForActivityTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by workers to get an ActivityTask from the specified activity
-- @taskList@. This initiates a long poll, where the service holds the HTTP
-- connection open and responds as soon as a task becomes available. The
-- maximum time the service holds on to the request before responding is 60
-- seconds. If no task is available within 60 seconds, the poll returns an
-- empty result. An empty result, in this context, means that an
-- ActivityTask is returned, but that the value of taskToken is an empty
-- string. If a task is returned, the worker should use its type to
-- identify and process it correctly.
--
-- Workers should set their client side socket timeout to at least 70
-- seconds (10 seconds higher than the maximum time service may hold the
-- poll request).
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
-- -   Constrain the @taskList.name@ parameter by using a @Condition@
--     element with the @swf:taskList.name@ key to allow the action to
--     access only certain task lists.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
module Amazonka.SWF.PollForActivityTask
  ( -- * Creating a Request
    PollForActivityTask (..),
    newPollForActivityTask,

    -- * Request Lenses
    pollForActivityTask_identity,
    pollForActivityTask_domain,
    pollForActivityTask_taskList,

    -- * Destructuring the Response
    PollForActivityTaskResponse (..),
    newPollForActivityTaskResponse,

    -- * Response Lenses
    pollForActivityTaskResponse_activityId,
    pollForActivityTaskResponse_activityType,
    pollForActivityTaskResponse_input,
    pollForActivityTaskResponse_taskToken,
    pollForActivityTaskResponse_workflowExecution,
    pollForActivityTaskResponse_httpStatus,
    pollForActivityTaskResponse_startedEventId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newPollForActivityTask' smart constructor.
data PollForActivityTask = PollForActivityTask'
  { -- | Identity of the worker making the request, recorded in the
    -- @ActivityTaskStarted@ event in the workflow history. This enables
    -- diagnostic tracing when problems arise. The form of this identity is
    -- user defined.
    identity :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the task lists being polled.
    domain :: Prelude.Text,
    -- | Specifies the task list to poll for activity tasks.
    --
    -- The specified string must not start or end with whitespace. It must not
    -- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
    -- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
    -- /be/ the literal string @arn@.
    taskList :: TaskList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PollForActivityTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identity', 'pollForActivityTask_identity' - Identity of the worker making the request, recorded in the
-- @ActivityTaskStarted@ event in the workflow history. This enables
-- diagnostic tracing when problems arise. The form of this identity is
-- user defined.
--
-- 'domain', 'pollForActivityTask_domain' - The name of the domain that contains the task lists being polled.
--
-- 'taskList', 'pollForActivityTask_taskList' - Specifies the task list to poll for activity tasks.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
newPollForActivityTask ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'taskList'
  TaskList ->
  PollForActivityTask
newPollForActivityTask pDomain_ pTaskList_ =
  PollForActivityTask'
    { identity = Prelude.Nothing,
      domain = pDomain_,
      taskList = pTaskList_
    }

-- | Identity of the worker making the request, recorded in the
-- @ActivityTaskStarted@ event in the workflow history. This enables
-- diagnostic tracing when problems arise. The form of this identity is
-- user defined.
pollForActivityTask_identity :: Lens.Lens' PollForActivityTask (Prelude.Maybe Prelude.Text)
pollForActivityTask_identity = Lens.lens (\PollForActivityTask' {identity} -> identity) (\s@PollForActivityTask' {} a -> s {identity = a} :: PollForActivityTask)

-- | The name of the domain that contains the task lists being polled.
pollForActivityTask_domain :: Lens.Lens' PollForActivityTask Prelude.Text
pollForActivityTask_domain = Lens.lens (\PollForActivityTask' {domain} -> domain) (\s@PollForActivityTask' {} a -> s {domain = a} :: PollForActivityTask)

-- | Specifies the task list to poll for activity tasks.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
pollForActivityTask_taskList :: Lens.Lens' PollForActivityTask TaskList
pollForActivityTask_taskList = Lens.lens (\PollForActivityTask' {taskList} -> taskList) (\s@PollForActivityTask' {} a -> s {taskList = a} :: PollForActivityTask)

instance Core.AWSRequest PollForActivityTask where
  type
    AWSResponse PollForActivityTask =
      PollForActivityTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PollForActivityTaskResponse'
            Prelude.<$> (x Data..?> "activityId")
            Prelude.<*> (x Data..?> "activityType")
            Prelude.<*> (x Data..?> "input")
            Prelude.<*> (x Data..?> "taskToken")
            Prelude.<*> (x Data..?> "workflowExecution")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "startedEventId")
      )

instance Prelude.Hashable PollForActivityTask where
  hashWithSalt _salt PollForActivityTask' {..} =
    _salt
      `Prelude.hashWithSalt` identity
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` taskList

instance Prelude.NFData PollForActivityTask where
  rnf PollForActivityTask' {..} =
    Prelude.rnf identity `Prelude.seq`
      Prelude.rnf domain `Prelude.seq`
        Prelude.rnf taskList

instance Data.ToHeaders PollForActivityTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SimpleWorkflowService.PollForActivityTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PollForActivityTask where
  toJSON PollForActivityTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("identity" Data..=) Prelude.<$> identity,
            Prelude.Just ("domain" Data..= domain),
            Prelude.Just ("taskList" Data..= taskList)
          ]
      )

instance Data.ToPath PollForActivityTask where
  toPath = Prelude.const "/"

instance Data.ToQuery PollForActivityTask where
  toQuery = Prelude.const Prelude.mempty

-- | Unit of work sent to an activity worker.
--
-- /See:/ 'newPollForActivityTaskResponse' smart constructor.
data PollForActivityTaskResponse = PollForActivityTaskResponse'
  { -- | The unique ID of the task.
    activityId :: Prelude.Maybe Prelude.Text,
    -- | The type of this activity task.
    activityType :: Prelude.Maybe ActivityType,
    -- | The inputs provided when the activity task was scheduled. The form of
    -- the input is user defined and should be meaningful to the activity
    -- implementation.
    input :: Prelude.Maybe Prelude.Text,
    -- | The opaque string used as a handle on the task. This token is used by
    -- workers to communicate progress and response information back to the
    -- system about the task.
    taskToken :: Prelude.Maybe Prelude.Text,
    -- | The workflow execution that started this activity task.
    workflowExecution :: Prelude.Maybe WorkflowExecution,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the @ActivityTaskStarted@ event recorded in the history.
    startedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PollForActivityTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activityId', 'pollForActivityTaskResponse_activityId' - The unique ID of the task.
--
-- 'activityType', 'pollForActivityTaskResponse_activityType' - The type of this activity task.
--
-- 'input', 'pollForActivityTaskResponse_input' - The inputs provided when the activity task was scheduled. The form of
-- the input is user defined and should be meaningful to the activity
-- implementation.
--
-- 'taskToken', 'pollForActivityTaskResponse_taskToken' - The opaque string used as a handle on the task. This token is used by
-- workers to communicate progress and response information back to the
-- system about the task.
--
-- 'workflowExecution', 'pollForActivityTaskResponse_workflowExecution' - The workflow execution that started this activity task.
--
-- 'httpStatus', 'pollForActivityTaskResponse_httpStatus' - The response's http status code.
--
-- 'startedEventId', 'pollForActivityTaskResponse_startedEventId' - The ID of the @ActivityTaskStarted@ event recorded in the history.
newPollForActivityTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'startedEventId'
  Prelude.Integer ->
  PollForActivityTaskResponse
newPollForActivityTaskResponse
  pHttpStatus_
  pStartedEventId_ =
    PollForActivityTaskResponse'
      { activityId =
          Prelude.Nothing,
        activityType = Prelude.Nothing,
        input = Prelude.Nothing,
        taskToken = Prelude.Nothing,
        workflowExecution = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        startedEventId = pStartedEventId_
      }

-- | The unique ID of the task.
pollForActivityTaskResponse_activityId :: Lens.Lens' PollForActivityTaskResponse (Prelude.Maybe Prelude.Text)
pollForActivityTaskResponse_activityId = Lens.lens (\PollForActivityTaskResponse' {activityId} -> activityId) (\s@PollForActivityTaskResponse' {} a -> s {activityId = a} :: PollForActivityTaskResponse)

-- | The type of this activity task.
pollForActivityTaskResponse_activityType :: Lens.Lens' PollForActivityTaskResponse (Prelude.Maybe ActivityType)
pollForActivityTaskResponse_activityType = Lens.lens (\PollForActivityTaskResponse' {activityType} -> activityType) (\s@PollForActivityTaskResponse' {} a -> s {activityType = a} :: PollForActivityTaskResponse)

-- | The inputs provided when the activity task was scheduled. The form of
-- the input is user defined and should be meaningful to the activity
-- implementation.
pollForActivityTaskResponse_input :: Lens.Lens' PollForActivityTaskResponse (Prelude.Maybe Prelude.Text)
pollForActivityTaskResponse_input = Lens.lens (\PollForActivityTaskResponse' {input} -> input) (\s@PollForActivityTaskResponse' {} a -> s {input = a} :: PollForActivityTaskResponse)

-- | The opaque string used as a handle on the task. This token is used by
-- workers to communicate progress and response information back to the
-- system about the task.
pollForActivityTaskResponse_taskToken :: Lens.Lens' PollForActivityTaskResponse (Prelude.Maybe Prelude.Text)
pollForActivityTaskResponse_taskToken = Lens.lens (\PollForActivityTaskResponse' {taskToken} -> taskToken) (\s@PollForActivityTaskResponse' {} a -> s {taskToken = a} :: PollForActivityTaskResponse)

-- | The workflow execution that started this activity task.
pollForActivityTaskResponse_workflowExecution :: Lens.Lens' PollForActivityTaskResponse (Prelude.Maybe WorkflowExecution)
pollForActivityTaskResponse_workflowExecution = Lens.lens (\PollForActivityTaskResponse' {workflowExecution} -> workflowExecution) (\s@PollForActivityTaskResponse' {} a -> s {workflowExecution = a} :: PollForActivityTaskResponse)

-- | The response's http status code.
pollForActivityTaskResponse_httpStatus :: Lens.Lens' PollForActivityTaskResponse Prelude.Int
pollForActivityTaskResponse_httpStatus = Lens.lens (\PollForActivityTaskResponse' {httpStatus} -> httpStatus) (\s@PollForActivityTaskResponse' {} a -> s {httpStatus = a} :: PollForActivityTaskResponse)

-- | The ID of the @ActivityTaskStarted@ event recorded in the history.
pollForActivityTaskResponse_startedEventId :: Lens.Lens' PollForActivityTaskResponse Prelude.Integer
pollForActivityTaskResponse_startedEventId = Lens.lens (\PollForActivityTaskResponse' {startedEventId} -> startedEventId) (\s@PollForActivityTaskResponse' {} a -> s {startedEventId = a} :: PollForActivityTaskResponse)

instance Prelude.NFData PollForActivityTaskResponse where
  rnf PollForActivityTaskResponse' {..} =
    Prelude.rnf activityId `Prelude.seq`
      Prelude.rnf activityType `Prelude.seq`
        Prelude.rnf input `Prelude.seq`
          Prelude.rnf taskToken `Prelude.seq`
            Prelude.rnf workflowExecution `Prelude.seq`
              Prelude.rnf httpStatus `Prelude.seq`
                Prelude.rnf startedEventId
