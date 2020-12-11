{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.PollForDecisionTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by deciders to get a 'DecisionTask' from the specified decision @taskList@ . A decision task may be returned for any open workflow execution that is using the specified task list. The task includes a paginated view of the history of the workflow execution. The decider should use the workflow type and the history to determine how to properly handle the task.
--
-- This action initiates a long poll, where the service holds the HTTP connection open and responds as soon a task becomes available. If no decision task is available in the specified task list before the timeout of 60 seconds expires, an empty result is returned. An empty result, in this context, means that a DecisionTask is returned, but that the value of taskToken is an empty string.
-- /Important:/ Deciders should set their client side socket timeout to at least 70 seconds (10 seconds higher than the timeout).
-- /Important:/ Because the number of workflow history events for a single workflow execution might be very large, the result returned might be split up across a number of pages. To retrieve subsequent pages, make additional calls to @PollForDecisionTask@ using the @nextPageToken@ returned by the initial call. Note that you do /not/ call @GetWorkflowExecutionHistory@ with this @nextPageToken@ . Instead, call @PollForDecisionTask@ again.
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * Constrain the @taskList.name@ parameter by using a @Condition@ element with the @swf:taskList.name@ key to allow the action to access only certain task lists.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.SWF.PollForDecisionTask
  ( -- * Creating a request
    PollForDecisionTask (..),
    mkPollForDecisionTask,

    -- ** Request lenses
    pfdtNextPageToken,
    pfdtReverseOrder,
    pfdtMaximumPageSize,
    pfdtIdentity,
    pfdtDomain,
    pfdtTaskList,

    -- * Destructuring the response
    PollForDecisionTaskResponse (..),
    mkPollForDecisionTaskResponse,

    -- ** Response lenses
    pfdtrsNextPageToken,
    pfdtrsWorkflowType,
    pfdtrsPreviousStartedEventId,
    pfdtrsEvents,
    pfdtrsTaskToken,
    pfdtrsWorkflowExecution,
    pfdtrsResponseStatus,
    pfdtrsStartedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkPollForDecisionTask' smart constructor.
data PollForDecisionTask = PollForDecisionTask'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    reverseOrder :: Lude.Maybe Lude.Bool,
    maximumPageSize :: Lude.Maybe Lude.Natural,
    identity :: Lude.Maybe Lude.Text,
    domain :: Lude.Text,
    taskList :: TaskList
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PollForDecisionTask' with the minimum fields required to make a request.
--
-- * 'domain' - The name of the domain containing the task lists to poll.
-- * 'identity' - Identity of the decider making the request, which is recorded in the DecisionTaskStarted event in the workflow history. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
-- * 'maximumPageSize' - The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
--
-- This is an upper limit only; the actual number of results returned per call may be fewer than the specified maximum.
-- * 'nextPageToken' - If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
-- * 'reverseOrder' - When set to @true@ , returns the events in reverse order. By default the results are returned in ascending order of the @eventTimestamp@ of the events.
-- * 'taskList' - Specifies the task list to poll for decision tasks.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
mkPollForDecisionTask ::
  -- | 'domain'
  Lude.Text ->
  -- | 'taskList'
  TaskList ->
  PollForDecisionTask
mkPollForDecisionTask pDomain_ pTaskList_ =
  PollForDecisionTask'
    { nextPageToken = Lude.Nothing,
      reverseOrder = Lude.Nothing,
      maximumPageSize = Lude.Nothing,
      identity = Lude.Nothing,
      domain = pDomain_,
      taskList = pTaskList_
    }

-- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtNextPageToken :: Lens.Lens' PollForDecisionTask (Lude.Maybe Lude.Text)
pfdtNextPageToken = Lens.lens (nextPageToken :: PollForDecisionTask -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: PollForDecisionTask)
{-# DEPRECATED pfdtNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | When set to @true@ , returns the events in reverse order. By default the results are returned in ascending order of the @eventTimestamp@ of the events.
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtReverseOrder :: Lens.Lens' PollForDecisionTask (Lude.Maybe Lude.Bool)
pfdtReverseOrder = Lens.lens (reverseOrder :: PollForDecisionTask -> Lude.Maybe Lude.Bool) (\s a -> s {reverseOrder = a} :: PollForDecisionTask)
{-# DEPRECATED pfdtReverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead." #-}

-- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
--
-- This is an upper limit only; the actual number of results returned per call may be fewer than the specified maximum.
--
-- /Note:/ Consider using 'maximumPageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtMaximumPageSize :: Lens.Lens' PollForDecisionTask (Lude.Maybe Lude.Natural)
pfdtMaximumPageSize = Lens.lens (maximumPageSize :: PollForDecisionTask -> Lude.Maybe Lude.Natural) (\s a -> s {maximumPageSize = a} :: PollForDecisionTask)
{-# DEPRECATED pfdtMaximumPageSize "Use generic-lens or generic-optics with 'maximumPageSize' instead." #-}

-- | Identity of the decider making the request, which is recorded in the DecisionTaskStarted event in the workflow history. This enables diagnostic tracing when problems arise. The form of this identity is user defined.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtIdentity :: Lens.Lens' PollForDecisionTask (Lude.Maybe Lude.Text)
pfdtIdentity = Lens.lens (identity :: PollForDecisionTask -> Lude.Maybe Lude.Text) (\s a -> s {identity = a} :: PollForDecisionTask)
{-# DEPRECATED pfdtIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | The name of the domain containing the task lists to poll.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtDomain :: Lens.Lens' PollForDecisionTask Lude.Text
pfdtDomain = Lens.lens (domain :: PollForDecisionTask -> Lude.Text) (\s a -> s {domain = a} :: PollForDecisionTask)
{-# DEPRECATED pfdtDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Specifies the task list to poll for decision tasks.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtTaskList :: Lens.Lens' PollForDecisionTask TaskList
pfdtTaskList = Lens.lens (taskList :: PollForDecisionTask -> TaskList) (\s a -> s {taskList = a} :: PollForDecisionTask)
{-# DEPRECATED pfdtTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

instance Page.AWSPager PollForDecisionTask where
  page rq rs
    | Page.stop (rs Lens.^. pfdtrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. pfdtrsEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& pfdtNextPageToken Lens..~ rs Lens.^. pfdtrsNextPageToken

instance Lude.AWSRequest PollForDecisionTask where
  type Rs PollForDecisionTask = PollForDecisionTaskResponse
  request = Req.postJSON swfService
  response =
    Res.receiveJSON
      ( \s h x ->
          PollForDecisionTaskResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "workflowType")
            Lude.<*> (x Lude..?> "previousStartedEventId")
            Lude.<*> (x Lude..?> "events" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "taskToken")
            Lude.<*> (x Lude..?> "workflowExecution")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "startedEventId")
      )

instance Lude.ToHeaders PollForDecisionTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.PollForDecisionTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PollForDecisionTask where
  toJSON PollForDecisionTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("reverseOrder" Lude..=) Lude.<$> reverseOrder,
            ("maximumPageSize" Lude..=) Lude.<$> maximumPageSize,
            ("identity" Lude..=) Lude.<$> identity,
            Lude.Just ("domain" Lude..= domain),
            Lude.Just ("taskList" Lude..= taskList)
          ]
      )

instance Lude.ToPath PollForDecisionTask where
  toPath = Lude.const "/"

instance Lude.ToQuery PollForDecisionTask where
  toQuery = Lude.const Lude.mempty

-- | A structure that represents a decision task. Decision tasks are sent to deciders in order for them to make decisions.
--
-- /See:/ 'mkPollForDecisionTaskResponse' smart constructor.
data PollForDecisionTaskResponse = PollForDecisionTaskResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    workflowType ::
      Lude.Maybe WorkflowType,
    previousStartedEventId ::
      Lude.Maybe Lude.Integer,
    events :: Lude.Maybe [HistoryEvent],
    taskToken :: Lude.Maybe Lude.Text,
    workflowExecution ::
      Lude.Maybe WorkflowExecution,
    responseStatus :: Lude.Int,
    startedEventId :: Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PollForDecisionTaskResponse' with the minimum fields required to make a request.
--
-- * 'events' - A paginated list of history events of the workflow execution. The decider uses this during the processing of the decision task.
-- * 'nextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
-- * 'previousStartedEventId' - The ID of the DecisionTaskStarted event of the previous decision task of this workflow execution that was processed by the decider. This can be used to determine the events in the history new since the last decision task received by the decider.
-- * 'responseStatus' - The response status code.
-- * 'startedEventId' - The ID of the @DecisionTaskStarted@ event recorded in the history.
-- * 'taskToken' - The opaque string used as a handle on the task. This token is used by workers to communicate progress and response information back to the system about the task.
-- * 'workflowExecution' - The workflow execution for which this decision task was created.
-- * 'workflowType' - The type of the workflow execution for which this decision task was created.
mkPollForDecisionTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'startedEventId'
  Lude.Integer ->
  PollForDecisionTaskResponse
mkPollForDecisionTaskResponse pResponseStatus_ pStartedEventId_ =
  PollForDecisionTaskResponse'
    { nextPageToken = Lude.Nothing,
      workflowType = Lude.Nothing,
      previousStartedEventId = Lude.Nothing,
      events = Lude.Nothing,
      taskToken = Lude.Nothing,
      workflowExecution = Lude.Nothing,
      responseStatus = pResponseStatus_,
      startedEventId = pStartedEventId_
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrsNextPageToken :: Lens.Lens' PollForDecisionTaskResponse (Lude.Maybe Lude.Text)
pfdtrsNextPageToken = Lens.lens (nextPageToken :: PollForDecisionTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: PollForDecisionTaskResponse)
{-# DEPRECATED pfdtrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The type of the workflow execution for which this decision task was created.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrsWorkflowType :: Lens.Lens' PollForDecisionTaskResponse (Lude.Maybe WorkflowType)
pfdtrsWorkflowType = Lens.lens (workflowType :: PollForDecisionTaskResponse -> Lude.Maybe WorkflowType) (\s a -> s {workflowType = a} :: PollForDecisionTaskResponse)
{-# DEPRECATED pfdtrsWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | The ID of the DecisionTaskStarted event of the previous decision task of this workflow execution that was processed by the decider. This can be used to determine the events in the history new since the last decision task received by the decider.
--
-- /Note:/ Consider using 'previousStartedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrsPreviousStartedEventId :: Lens.Lens' PollForDecisionTaskResponse (Lude.Maybe Lude.Integer)
pfdtrsPreviousStartedEventId = Lens.lens (previousStartedEventId :: PollForDecisionTaskResponse -> Lude.Maybe Lude.Integer) (\s a -> s {previousStartedEventId = a} :: PollForDecisionTaskResponse)
{-# DEPRECATED pfdtrsPreviousStartedEventId "Use generic-lens or generic-optics with 'previousStartedEventId' instead." #-}

-- | A paginated list of history events of the workflow execution. The decider uses this during the processing of the decision task.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrsEvents :: Lens.Lens' PollForDecisionTaskResponse (Lude.Maybe [HistoryEvent])
pfdtrsEvents = Lens.lens (events :: PollForDecisionTaskResponse -> Lude.Maybe [HistoryEvent]) (\s a -> s {events = a} :: PollForDecisionTaskResponse)
{-# DEPRECATED pfdtrsEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The opaque string used as a handle on the task. This token is used by workers to communicate progress and response information back to the system about the task.
--
-- /Note:/ Consider using 'taskToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrsTaskToken :: Lens.Lens' PollForDecisionTaskResponse (Lude.Maybe Lude.Text)
pfdtrsTaskToken = Lens.lens (taskToken :: PollForDecisionTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskToken = a} :: PollForDecisionTaskResponse)
{-# DEPRECATED pfdtrsTaskToken "Use generic-lens or generic-optics with 'taskToken' instead." #-}

-- | The workflow execution for which this decision task was created.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrsWorkflowExecution :: Lens.Lens' PollForDecisionTaskResponse (Lude.Maybe WorkflowExecution)
pfdtrsWorkflowExecution = Lens.lens (workflowExecution :: PollForDecisionTaskResponse -> Lude.Maybe WorkflowExecution) (\s a -> s {workflowExecution = a} :: PollForDecisionTaskResponse)
{-# DEPRECATED pfdtrsWorkflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrsResponseStatus :: Lens.Lens' PollForDecisionTaskResponse Lude.Int
pfdtrsResponseStatus = Lens.lens (responseStatus :: PollForDecisionTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PollForDecisionTaskResponse)
{-# DEPRECATED pfdtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The ID of the @DecisionTaskStarted@ event recorded in the history.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfdtrsStartedEventId :: Lens.Lens' PollForDecisionTaskResponse Lude.Integer
pfdtrsStartedEventId = Lens.lens (startedEventId :: PollForDecisionTaskResponse -> Lude.Integer) (\s a -> s {startedEventId = a} :: PollForDecisionTaskResponse)
{-# DEPRECATED pfdtrsStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}
