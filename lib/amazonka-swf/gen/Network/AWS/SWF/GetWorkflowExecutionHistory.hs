{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.GetWorkflowExecutionHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the history of the specified workflow execution. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the @nextPageToken@ returned by the initial call.
--
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.SWF.GetWorkflowExecutionHistory
  ( -- * Creating a request
    GetWorkflowExecutionHistory (..),
    mkGetWorkflowExecutionHistory,

    -- ** Request lenses
    gwehNextPageToken,
    gwehReverseOrder,
    gwehMaximumPageSize,
    gwehDomain,
    gwehExecution,

    -- * Destructuring the response
    GetWorkflowExecutionHistoryResponse (..),
    mkGetWorkflowExecutionHistoryResponse,

    -- ** Response lenses
    gwehrsNextPageToken,
    gwehrsResponseStatus,
    gwehrsEvents,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkGetWorkflowExecutionHistory' smart constructor.
data GetWorkflowExecutionHistory = GetWorkflowExecutionHistory'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    reverseOrder ::
      Lude.Maybe Lude.Bool,
    maximumPageSize ::
      Lude.Maybe Lude.Natural,
    domain :: Lude.Text,
    execution :: WorkflowExecution
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetWorkflowExecutionHistory' with the minimum fields required to make a request.
--
-- * 'domain' - The name of the domain containing the workflow execution.
-- * 'execution' - Specifies the workflow execution for which to return the history.
-- * 'maximumPageSize' - The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
-- * 'nextPageToken' - If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
-- * 'reverseOrder' - When set to @true@ , returns the events in reverse order. By default the results are returned in ascending order of the @eventTimeStamp@ of the events.
mkGetWorkflowExecutionHistory ::
  -- | 'domain'
  Lude.Text ->
  -- | 'execution'
  WorkflowExecution ->
  GetWorkflowExecutionHistory
mkGetWorkflowExecutionHistory pDomain_ pExecution_ =
  GetWorkflowExecutionHistory'
    { nextPageToken = Lude.Nothing,
      reverseOrder = Lude.Nothing,
      maximumPageSize = Lude.Nothing,
      domain = pDomain_,
      execution = pExecution_
    }

-- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehNextPageToken :: Lens.Lens' GetWorkflowExecutionHistory (Lude.Maybe Lude.Text)
gwehNextPageToken = Lens.lens (nextPageToken :: GetWorkflowExecutionHistory -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetWorkflowExecutionHistory)
{-# DEPRECATED gwehNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | When set to @true@ , returns the events in reverse order. By default the results are returned in ascending order of the @eventTimeStamp@ of the events.
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehReverseOrder :: Lens.Lens' GetWorkflowExecutionHistory (Lude.Maybe Lude.Bool)
gwehReverseOrder = Lens.lens (reverseOrder :: GetWorkflowExecutionHistory -> Lude.Maybe Lude.Bool) (\s a -> s {reverseOrder = a} :: GetWorkflowExecutionHistory)
{-# DEPRECATED gwehReverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead." #-}

-- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
--
-- /Note:/ Consider using 'maximumPageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehMaximumPageSize :: Lens.Lens' GetWorkflowExecutionHistory (Lude.Maybe Lude.Natural)
gwehMaximumPageSize = Lens.lens (maximumPageSize :: GetWorkflowExecutionHistory -> Lude.Maybe Lude.Natural) (\s a -> s {maximumPageSize = a} :: GetWorkflowExecutionHistory)
{-# DEPRECATED gwehMaximumPageSize "Use generic-lens or generic-optics with 'maximumPageSize' instead." #-}

-- | The name of the domain containing the workflow execution.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehDomain :: Lens.Lens' GetWorkflowExecutionHistory Lude.Text
gwehDomain = Lens.lens (domain :: GetWorkflowExecutionHistory -> Lude.Text) (\s a -> s {domain = a} :: GetWorkflowExecutionHistory)
{-# DEPRECATED gwehDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Specifies the workflow execution for which to return the history.
--
-- /Note:/ Consider using 'execution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehExecution :: Lens.Lens' GetWorkflowExecutionHistory WorkflowExecution
gwehExecution = Lens.lens (execution :: GetWorkflowExecutionHistory -> WorkflowExecution) (\s a -> s {execution = a} :: GetWorkflowExecutionHistory)
{-# DEPRECATED gwehExecution "Use generic-lens or generic-optics with 'execution' instead." #-}

instance Page.AWSPager GetWorkflowExecutionHistory where
  page rq rs
    | Page.stop (rs Lens.^. gwehrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gwehrsEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gwehNextPageToken Lens..~ rs Lens.^. gwehrsNextPageToken

instance Lude.AWSRequest GetWorkflowExecutionHistory where
  type
    Rs GetWorkflowExecutionHistory =
      GetWorkflowExecutionHistoryResponse
  request = Req.postJSON swfService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetWorkflowExecutionHistoryResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "events" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders GetWorkflowExecutionHistory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SimpleWorkflowService.GetWorkflowExecutionHistory" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetWorkflowExecutionHistory where
  toJSON GetWorkflowExecutionHistory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("reverseOrder" Lude..=) Lude.<$> reverseOrder,
            ("maximumPageSize" Lude..=) Lude.<$> maximumPageSize,
            Lude.Just ("domain" Lude..= domain),
            Lude.Just ("execution" Lude..= execution)
          ]
      )

instance Lude.ToPath GetWorkflowExecutionHistory where
  toPath = Lude.const "/"

instance Lude.ToQuery GetWorkflowExecutionHistory where
  toQuery = Lude.const Lude.mempty

-- | Paginated representation of a workflow history for a workflow execution. This is the up to date, complete and authoritative record of the events related to all tasks and events in the life of the workflow execution.
--
-- /See:/ 'mkGetWorkflowExecutionHistoryResponse' smart constructor.
data GetWorkflowExecutionHistoryResponse = GetWorkflowExecutionHistoryResponse'
  { nextPageToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int,
    events ::
      [HistoryEvent]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetWorkflowExecutionHistoryResponse' with the minimum fields required to make a request.
--
-- * 'events' - The list of history events.
-- * 'nextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
-- * 'responseStatus' - The response status code.
mkGetWorkflowExecutionHistoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetWorkflowExecutionHistoryResponse
mkGetWorkflowExecutionHistoryResponse pResponseStatus_ =
  GetWorkflowExecutionHistoryResponse'
    { nextPageToken =
        Lude.Nothing,
      responseStatus = pResponseStatus_,
      events = Lude.mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehrsNextPageToken :: Lens.Lens' GetWorkflowExecutionHistoryResponse (Lude.Maybe Lude.Text)
gwehrsNextPageToken = Lens.lens (nextPageToken :: GetWorkflowExecutionHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetWorkflowExecutionHistoryResponse)
{-# DEPRECATED gwehrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehrsResponseStatus :: Lens.Lens' GetWorkflowExecutionHistoryResponse Lude.Int
gwehrsResponseStatus = Lens.lens (responseStatus :: GetWorkflowExecutionHistoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetWorkflowExecutionHistoryResponse)
{-# DEPRECATED gwehrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The list of history events.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwehrsEvents :: Lens.Lens' GetWorkflowExecutionHistoryResponse [HistoryEvent]
gwehrsEvents = Lens.lens (events :: GetWorkflowExecutionHistoryResponse -> [HistoryEvent]) (\s a -> s {events = a} :: GetWorkflowExecutionHistoryResponse)
{-# DEPRECATED gwehrsEvents "Use generic-lens or generic-optics with 'events' instead." #-}
