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
-- Module      : Network.AWS.SWF.PollForDecisionTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by deciders to get a DecisionTask from the specified decision
-- @taskList@. A decision task may be returned for any open workflow
-- execution that is using the specified task list. The task includes a
-- paginated view of the history of the workflow execution. The decider
-- should use the workflow type and the history to determine how to
-- properly handle the task.
--
-- This action initiates a long poll, where the service holds the HTTP
-- connection open and responds as soon a task becomes available. If no
-- decision task is available in the specified task list before the timeout
-- of 60 seconds expires, an empty result is returned. An empty result, in
-- this context, means that a DecisionTask is returned, but that the value
-- of taskToken is an empty string.
--
-- Deciders should set their client side socket timeout to at least 70
-- seconds (10 seconds higher than the timeout).
--
-- Because the number of workflow history events for a single workflow
-- execution might be very large, the result returned might be split up
-- across a number of pages. To retrieve subsequent pages, make additional
-- calls to @PollForDecisionTask@ using the @nextPageToken@ returned by the
-- initial call. Note that you do /not/ call @GetWorkflowExecutionHistory@
-- with this @nextPageToken@. Instead, call @PollForDecisionTask@ again.
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
--
-- This operation returns paginated results.
module Network.AWS.SWF.PollForDecisionTask
  ( -- * Creating a Request
    PollForDecisionTask (..),
    newPollForDecisionTask,

    -- * Request Lenses
    pollForDecisionTask_identity,
    pollForDecisionTask_nextPageToken,
    pollForDecisionTask_maximumPageSize,
    pollForDecisionTask_reverseOrder,
    pollForDecisionTask_domain,
    pollForDecisionTask_taskList,

    -- * Destructuring the Response
    PollForDecisionTaskResponse (..),
    newPollForDecisionTaskResponse,

    -- * Response Lenses
    pollForDecisionTaskResponse_previousStartedEventId,
    pollForDecisionTaskResponse_workflowExecution,
    pollForDecisionTaskResponse_workflowType,
    pollForDecisionTaskResponse_nextPageToken,
    pollForDecisionTaskResponse_events,
    pollForDecisionTaskResponse_taskToken,
    pollForDecisionTaskResponse_httpStatus,
    pollForDecisionTaskResponse_startedEventId,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newPollForDecisionTask' smart constructor.
data PollForDecisionTask = PollForDecisionTask'
  { -- | Identity of the decider making the request, which is recorded in the
    -- DecisionTaskStarted event in the workflow history. This enables
    -- diagnostic tracing when problems arise. The form of this identity is
    -- user defined.
    identity :: Prelude.Maybe Prelude.Text,
    -- | If @NextPageToken@ is returned there are more results available. The
    -- value of @NextPageToken@ is a unique pagination token for each page.
    -- Make the call again using the returned token to retrieve the next page.
    -- Keep all other arguments unchanged. Each pagination token expires after
    -- 60 seconds. Using an expired pagination token will return a @400@ error:
    -- \"@Specified token has exceeded its maximum lifetime@\".
    --
    -- The configured @maximumPageSize@ determines how many results can be
    -- returned in a single call.
    --
    -- The @nextPageToken@ returned by this action cannot be used with
    -- GetWorkflowExecutionHistory to get the next page. You must call
    -- PollForDecisionTask again (with the @nextPageToken@) to retrieve the
    -- next page of history records. Calling PollForDecisionTask with a
    -- @nextPageToken@ doesn\'t return a new decision task.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that are returned per call. Use
    -- @nextPageToken@ to obtain further pages of results.
    --
    -- This is an upper limit only; the actual number of results returned per
    -- call may be fewer than the specified maximum.
    maximumPageSize :: Prelude.Maybe Prelude.Natural,
    -- | When set to @true@, returns the events in reverse order. By default the
    -- results are returned in ascending order of the @eventTimestamp@ of the
    -- events.
    reverseOrder :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain containing the task lists to poll.
    domain :: Prelude.Text,
    -- | Specifies the task list to poll for decision tasks.
    --
    -- The specified string must not start or end with whitespace. It must not
    -- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
    -- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
    -- /be/ the literal string @arn@.
    taskList :: TaskList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PollForDecisionTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identity', 'pollForDecisionTask_identity' - Identity of the decider making the request, which is recorded in the
-- DecisionTaskStarted event in the workflow history. This enables
-- diagnostic tracing when problems arise. The form of this identity is
-- user defined.
--
-- 'nextPageToken', 'pollForDecisionTask_nextPageToken' - If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 60 seconds. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- The @nextPageToken@ returned by this action cannot be used with
-- GetWorkflowExecutionHistory to get the next page. You must call
-- PollForDecisionTask again (with the @nextPageToken@) to retrieve the
-- next page of history records. Calling PollForDecisionTask with a
-- @nextPageToken@ doesn\'t return a new decision task.
--
-- 'maximumPageSize', 'pollForDecisionTask_maximumPageSize' - The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
--
-- This is an upper limit only; the actual number of results returned per
-- call may be fewer than the specified maximum.
--
-- 'reverseOrder', 'pollForDecisionTask_reverseOrder' - When set to @true@, returns the events in reverse order. By default the
-- results are returned in ascending order of the @eventTimestamp@ of the
-- events.
--
-- 'domain', 'pollForDecisionTask_domain' - The name of the domain containing the task lists to poll.
--
-- 'taskList', 'pollForDecisionTask_taskList' - Specifies the task list to poll for decision tasks.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
newPollForDecisionTask ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'taskList'
  TaskList ->
  PollForDecisionTask
newPollForDecisionTask pDomain_ pTaskList_ =
  PollForDecisionTask'
    { identity = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      maximumPageSize = Prelude.Nothing,
      reverseOrder = Prelude.Nothing,
      domain = pDomain_,
      taskList = pTaskList_
    }

-- | Identity of the decider making the request, which is recorded in the
-- DecisionTaskStarted event in the workflow history. This enables
-- diagnostic tracing when problems arise. The form of this identity is
-- user defined.
pollForDecisionTask_identity :: Lens.Lens' PollForDecisionTask (Prelude.Maybe Prelude.Text)
pollForDecisionTask_identity = Lens.lens (\PollForDecisionTask' {identity} -> identity) (\s@PollForDecisionTask' {} a -> s {identity = a} :: PollForDecisionTask)

-- | If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 60 seconds. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- The @nextPageToken@ returned by this action cannot be used with
-- GetWorkflowExecutionHistory to get the next page. You must call
-- PollForDecisionTask again (with the @nextPageToken@) to retrieve the
-- next page of history records. Calling PollForDecisionTask with a
-- @nextPageToken@ doesn\'t return a new decision task.
pollForDecisionTask_nextPageToken :: Lens.Lens' PollForDecisionTask (Prelude.Maybe Prelude.Text)
pollForDecisionTask_nextPageToken = Lens.lens (\PollForDecisionTask' {nextPageToken} -> nextPageToken) (\s@PollForDecisionTask' {} a -> s {nextPageToken = a} :: PollForDecisionTask)

-- | The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
--
-- This is an upper limit only; the actual number of results returned per
-- call may be fewer than the specified maximum.
pollForDecisionTask_maximumPageSize :: Lens.Lens' PollForDecisionTask (Prelude.Maybe Prelude.Natural)
pollForDecisionTask_maximumPageSize = Lens.lens (\PollForDecisionTask' {maximumPageSize} -> maximumPageSize) (\s@PollForDecisionTask' {} a -> s {maximumPageSize = a} :: PollForDecisionTask)

-- | When set to @true@, returns the events in reverse order. By default the
-- results are returned in ascending order of the @eventTimestamp@ of the
-- events.
pollForDecisionTask_reverseOrder :: Lens.Lens' PollForDecisionTask (Prelude.Maybe Prelude.Bool)
pollForDecisionTask_reverseOrder = Lens.lens (\PollForDecisionTask' {reverseOrder} -> reverseOrder) (\s@PollForDecisionTask' {} a -> s {reverseOrder = a} :: PollForDecisionTask)

-- | The name of the domain containing the task lists to poll.
pollForDecisionTask_domain :: Lens.Lens' PollForDecisionTask Prelude.Text
pollForDecisionTask_domain = Lens.lens (\PollForDecisionTask' {domain} -> domain) (\s@PollForDecisionTask' {} a -> s {domain = a} :: PollForDecisionTask)

-- | Specifies the task list to poll for decision tasks.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
pollForDecisionTask_taskList :: Lens.Lens' PollForDecisionTask TaskList
pollForDecisionTask_taskList = Lens.lens (\PollForDecisionTask' {taskList} -> taskList) (\s@PollForDecisionTask' {} a -> s {taskList = a} :: PollForDecisionTask)

instance Core.AWSPager PollForDecisionTask where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? pollForDecisionTaskResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? pollForDecisionTaskResponse_events
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& pollForDecisionTask_nextPageToken
          Lens..~ rs
          Lens.^? pollForDecisionTaskResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest PollForDecisionTask where
  type
    AWSResponse PollForDecisionTask =
      PollForDecisionTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PollForDecisionTaskResponse'
            Prelude.<$> (x Core..?> "previousStartedEventId")
            Prelude.<*> (x Core..?> "workflowExecution")
            Prelude.<*> (x Core..?> "workflowType")
            Prelude.<*> (x Core..?> "nextPageToken")
            Prelude.<*> (x Core..?> "events" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "taskToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "startedEventId")
      )

instance Prelude.Hashable PollForDecisionTask

instance Prelude.NFData PollForDecisionTask

instance Core.ToHeaders PollForDecisionTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SimpleWorkflowService.PollForDecisionTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PollForDecisionTask where
  toJSON PollForDecisionTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("identity" Core..=) Prelude.<$> identity,
            ("nextPageToken" Core..=) Prelude.<$> nextPageToken,
            ("maximumPageSize" Core..=)
              Prelude.<$> maximumPageSize,
            ("reverseOrder" Core..=) Prelude.<$> reverseOrder,
            Prelude.Just ("domain" Core..= domain),
            Prelude.Just ("taskList" Core..= taskList)
          ]
      )

instance Core.ToPath PollForDecisionTask where
  toPath = Prelude.const "/"

instance Core.ToQuery PollForDecisionTask where
  toQuery = Prelude.const Prelude.mempty

-- | A structure that represents a decision task. Decision tasks are sent to
-- deciders in order for them to make decisions.
--
-- /See:/ 'newPollForDecisionTaskResponse' smart constructor.
data PollForDecisionTaskResponse = PollForDecisionTaskResponse'
  { -- | The ID of the DecisionTaskStarted event of the previous decision task of
    -- this workflow execution that was processed by the decider. This can be
    -- used to determine the events in the history new since the last decision
    -- task received by the decider.
    previousStartedEventId :: Prelude.Maybe Prelude.Integer,
    -- | The workflow execution for which this decision task was created.
    workflowExecution :: Prelude.Maybe WorkflowExecution,
    -- | The type of the workflow execution for which this decision task was
    -- created.
    workflowType :: Prelude.Maybe WorkflowType,
    -- | If a @NextPageToken@ was returned by a previous call, there are more
    -- results available. To retrieve the next page of results, make the call
    -- again using the returned token in @nextPageToken@. Keep all other
    -- arguments unchanged.
    --
    -- The configured @maximumPageSize@ determines how many results can be
    -- returned in a single call.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | A paginated list of history events of the workflow execution. The
    -- decider uses this during the processing of the decision task.
    events :: Prelude.Maybe [HistoryEvent],
    -- | The opaque string used as a handle on the task. This token is used by
    -- workers to communicate progress and response information back to the
    -- system about the task.
    taskToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the @DecisionTaskStarted@ event recorded in the history.
    startedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PollForDecisionTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'previousStartedEventId', 'pollForDecisionTaskResponse_previousStartedEventId' - The ID of the DecisionTaskStarted event of the previous decision task of
-- this workflow execution that was processed by the decider. This can be
-- used to determine the events in the history new since the last decision
-- task received by the decider.
--
-- 'workflowExecution', 'pollForDecisionTaskResponse_workflowExecution' - The workflow execution for which this decision task was created.
--
-- 'workflowType', 'pollForDecisionTaskResponse_workflowType' - The type of the workflow execution for which this decision task was
-- created.
--
-- 'nextPageToken', 'pollForDecisionTaskResponse_nextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- 'events', 'pollForDecisionTaskResponse_events' - A paginated list of history events of the workflow execution. The
-- decider uses this during the processing of the decision task.
--
-- 'taskToken', 'pollForDecisionTaskResponse_taskToken' - The opaque string used as a handle on the task. This token is used by
-- workers to communicate progress and response information back to the
-- system about the task.
--
-- 'httpStatus', 'pollForDecisionTaskResponse_httpStatus' - The response's http status code.
--
-- 'startedEventId', 'pollForDecisionTaskResponse_startedEventId' - The ID of the @DecisionTaskStarted@ event recorded in the history.
newPollForDecisionTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'startedEventId'
  Prelude.Integer ->
  PollForDecisionTaskResponse
newPollForDecisionTaskResponse
  pHttpStatus_
  pStartedEventId_ =
    PollForDecisionTaskResponse'
      { previousStartedEventId =
          Prelude.Nothing,
        workflowExecution = Prelude.Nothing,
        workflowType = Prelude.Nothing,
        nextPageToken = Prelude.Nothing,
        events = Prelude.Nothing,
        taskToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        startedEventId = pStartedEventId_
      }

-- | The ID of the DecisionTaskStarted event of the previous decision task of
-- this workflow execution that was processed by the decider. This can be
-- used to determine the events in the history new since the last decision
-- task received by the decider.
pollForDecisionTaskResponse_previousStartedEventId :: Lens.Lens' PollForDecisionTaskResponse (Prelude.Maybe Prelude.Integer)
pollForDecisionTaskResponse_previousStartedEventId = Lens.lens (\PollForDecisionTaskResponse' {previousStartedEventId} -> previousStartedEventId) (\s@PollForDecisionTaskResponse' {} a -> s {previousStartedEventId = a} :: PollForDecisionTaskResponse)

-- | The workflow execution for which this decision task was created.
pollForDecisionTaskResponse_workflowExecution :: Lens.Lens' PollForDecisionTaskResponse (Prelude.Maybe WorkflowExecution)
pollForDecisionTaskResponse_workflowExecution = Lens.lens (\PollForDecisionTaskResponse' {workflowExecution} -> workflowExecution) (\s@PollForDecisionTaskResponse' {} a -> s {workflowExecution = a} :: PollForDecisionTaskResponse)

-- | The type of the workflow execution for which this decision task was
-- created.
pollForDecisionTaskResponse_workflowType :: Lens.Lens' PollForDecisionTaskResponse (Prelude.Maybe WorkflowType)
pollForDecisionTaskResponse_workflowType = Lens.lens (\PollForDecisionTaskResponse' {workflowType} -> workflowType) (\s@PollForDecisionTaskResponse' {} a -> s {workflowType = a} :: PollForDecisionTaskResponse)

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
pollForDecisionTaskResponse_nextPageToken :: Lens.Lens' PollForDecisionTaskResponse (Prelude.Maybe Prelude.Text)
pollForDecisionTaskResponse_nextPageToken = Lens.lens (\PollForDecisionTaskResponse' {nextPageToken} -> nextPageToken) (\s@PollForDecisionTaskResponse' {} a -> s {nextPageToken = a} :: PollForDecisionTaskResponse)

-- | A paginated list of history events of the workflow execution. The
-- decider uses this during the processing of the decision task.
pollForDecisionTaskResponse_events :: Lens.Lens' PollForDecisionTaskResponse (Prelude.Maybe [HistoryEvent])
pollForDecisionTaskResponse_events = Lens.lens (\PollForDecisionTaskResponse' {events} -> events) (\s@PollForDecisionTaskResponse' {} a -> s {events = a} :: PollForDecisionTaskResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The opaque string used as a handle on the task. This token is used by
-- workers to communicate progress and response information back to the
-- system about the task.
pollForDecisionTaskResponse_taskToken :: Lens.Lens' PollForDecisionTaskResponse (Prelude.Maybe Prelude.Text)
pollForDecisionTaskResponse_taskToken = Lens.lens (\PollForDecisionTaskResponse' {taskToken} -> taskToken) (\s@PollForDecisionTaskResponse' {} a -> s {taskToken = a} :: PollForDecisionTaskResponse)

-- | The response's http status code.
pollForDecisionTaskResponse_httpStatus :: Lens.Lens' PollForDecisionTaskResponse Prelude.Int
pollForDecisionTaskResponse_httpStatus = Lens.lens (\PollForDecisionTaskResponse' {httpStatus} -> httpStatus) (\s@PollForDecisionTaskResponse' {} a -> s {httpStatus = a} :: PollForDecisionTaskResponse)

-- | The ID of the @DecisionTaskStarted@ event recorded in the history.
pollForDecisionTaskResponse_startedEventId :: Lens.Lens' PollForDecisionTaskResponse Prelude.Integer
pollForDecisionTaskResponse_startedEventId = Lens.lens (\PollForDecisionTaskResponse' {startedEventId} -> startedEventId) (\s@PollForDecisionTaskResponse' {} a -> s {startedEventId = a} :: PollForDecisionTaskResponse)

instance Prelude.NFData PollForDecisionTaskResponse
