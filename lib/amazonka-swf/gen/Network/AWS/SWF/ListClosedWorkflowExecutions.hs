{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.ListClosedWorkflowExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of closed workflow executions in the specified domain that meet the filtering criteria. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.
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
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @tagFilter.tag@ : String constraint. The key is @swf:tagFilter.tag@ .
--
--
--     * @typeFilter.name@ : String constraint. The key is @swf:typeFilter.name@ .
--
--
--     * @typeFilter.version@ : String constraint. The key is @swf:typeFilter.version@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.SWF.ListClosedWorkflowExecutions
  ( -- * Creating a request
    ListClosedWorkflowExecutions (..),
    mkListClosedWorkflowExecutions,

    -- ** Request lenses
    lcweNextPageToken,
    lcweExecutionFilter,
    lcweCloseStatusFilter,
    lcweTypeFilter,
    lcweDomain,
    lcweCloseTimeFilter,
    lcweReverseOrder,
    lcweTagFilter,
    lcweStartTimeFilter,
    lcweMaximumPageSize,

    -- * Destructuring the response
    WorkflowExecutionInfos (..),
    mkWorkflowExecutionInfos,

    -- ** Response lenses
    weiExecutionInfos,
    weiNextPageToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkListClosedWorkflowExecutions' smart constructor.
data ListClosedWorkflowExecutions = ListClosedWorkflowExecutions'
  { -- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
    --
    -- The configured @maximumPageSize@ determines how many results can be returned in a single call.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | If specified, only workflow executions matching the workflow ID specified in the filter are returned.
    executionFilter :: Lude.Maybe WorkflowExecutionFilter,
    -- | If specified, only workflow executions that match this /close status/ are listed. For example, if TERMINATED is specified, then only TERMINATED workflow executions are listed.
    closeStatusFilter :: Lude.Maybe CloseStatusFilter,
    -- | If specified, only executions of the type specified in the filter are returned.
    typeFilter :: Lude.Maybe WorkflowTypeFilter,
    -- | The name of the domain that contains the workflow executions to list.
    domain :: Lude.Text,
    -- | If specified, the workflow executions are included in the returned results based on whether their close times are within the range specified by this filter. Also, if this parameter is specified, the returned results are ordered by their close times.
    closeTimeFilter :: Lude.Maybe ExecutionTimeFilter,
    -- | When set to @true@ , returns the results in reverse order. By default the results are returned in descending order of the start or the close time of the executions.
    reverseOrder :: Lude.Maybe Lude.Bool,
    -- | If specified, only executions that have the matching tag are listed.
    tagFilter :: Lude.Maybe TagFilter,
    -- | If specified, the workflow executions are included in the returned results based on whether their start times are within the range specified by this filter. Also, if this parameter is specified, the returned results are ordered by their start times.
    startTimeFilter :: Lude.Maybe ExecutionTimeFilter,
    -- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
    maximumPageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListClosedWorkflowExecutions' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
-- * 'executionFilter' - If specified, only workflow executions matching the workflow ID specified in the filter are returned.
-- * 'closeStatusFilter' - If specified, only workflow executions that match this /close status/ are listed. For example, if TERMINATED is specified, then only TERMINATED workflow executions are listed.
-- * 'typeFilter' - If specified, only executions of the type specified in the filter are returned.
-- * 'domain' - The name of the domain that contains the workflow executions to list.
-- * 'closeTimeFilter' - If specified, the workflow executions are included in the returned results based on whether their close times are within the range specified by this filter. Also, if this parameter is specified, the returned results are ordered by their close times.
-- * 'reverseOrder' - When set to @true@ , returns the results in reverse order. By default the results are returned in descending order of the start or the close time of the executions.
-- * 'tagFilter' - If specified, only executions that have the matching tag are listed.
-- * 'startTimeFilter' - If specified, the workflow executions are included in the returned results based on whether their start times are within the range specified by this filter. Also, if this parameter is specified, the returned results are ordered by their start times.
-- * 'maximumPageSize' - The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
mkListClosedWorkflowExecutions ::
  -- | 'domain'
  Lude.Text ->
  ListClosedWorkflowExecutions
mkListClosedWorkflowExecutions pDomain_ =
  ListClosedWorkflowExecutions'
    { nextPageToken = Lude.Nothing,
      executionFilter = Lude.Nothing,
      closeStatusFilter = Lude.Nothing,
      typeFilter = Lude.Nothing,
      domain = pDomain_,
      closeTimeFilter = Lude.Nothing,
      reverseOrder = Lude.Nothing,
      tagFilter = Lude.Nothing,
      startTimeFilter = Lude.Nothing,
      maximumPageSize = Lude.Nothing
    }

-- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcweNextPageToken :: Lens.Lens' ListClosedWorkflowExecutions (Lude.Maybe Lude.Text)
lcweNextPageToken = Lens.lens (nextPageToken :: ListClosedWorkflowExecutions -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListClosedWorkflowExecutions)
{-# DEPRECATED lcweNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | If specified, only workflow executions matching the workflow ID specified in the filter are returned.
--
-- /Note:/ Consider using 'executionFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcweExecutionFilter :: Lens.Lens' ListClosedWorkflowExecutions (Lude.Maybe WorkflowExecutionFilter)
lcweExecutionFilter = Lens.lens (executionFilter :: ListClosedWorkflowExecutions -> Lude.Maybe WorkflowExecutionFilter) (\s a -> s {executionFilter = a} :: ListClosedWorkflowExecutions)
{-# DEPRECATED lcweExecutionFilter "Use generic-lens or generic-optics with 'executionFilter' instead." #-}

-- | If specified, only workflow executions that match this /close status/ are listed. For example, if TERMINATED is specified, then only TERMINATED workflow executions are listed.
--
-- /Note:/ Consider using 'closeStatusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcweCloseStatusFilter :: Lens.Lens' ListClosedWorkflowExecutions (Lude.Maybe CloseStatusFilter)
lcweCloseStatusFilter = Lens.lens (closeStatusFilter :: ListClosedWorkflowExecutions -> Lude.Maybe CloseStatusFilter) (\s a -> s {closeStatusFilter = a} :: ListClosedWorkflowExecutions)
{-# DEPRECATED lcweCloseStatusFilter "Use generic-lens or generic-optics with 'closeStatusFilter' instead." #-}

-- | If specified, only executions of the type specified in the filter are returned.
--
-- /Note:/ Consider using 'typeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcweTypeFilter :: Lens.Lens' ListClosedWorkflowExecutions (Lude.Maybe WorkflowTypeFilter)
lcweTypeFilter = Lens.lens (typeFilter :: ListClosedWorkflowExecutions -> Lude.Maybe WorkflowTypeFilter) (\s a -> s {typeFilter = a} :: ListClosedWorkflowExecutions)
{-# DEPRECATED lcweTypeFilter "Use generic-lens or generic-optics with 'typeFilter' instead." #-}

-- | The name of the domain that contains the workflow executions to list.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcweDomain :: Lens.Lens' ListClosedWorkflowExecutions Lude.Text
lcweDomain = Lens.lens (domain :: ListClosedWorkflowExecutions -> Lude.Text) (\s a -> s {domain = a} :: ListClosedWorkflowExecutions)
{-# DEPRECATED lcweDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | If specified, the workflow executions are included in the returned results based on whether their close times are within the range specified by this filter. Also, if this parameter is specified, the returned results are ordered by their close times.
--
-- /Note:/ Consider using 'closeTimeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcweCloseTimeFilter :: Lens.Lens' ListClosedWorkflowExecutions (Lude.Maybe ExecutionTimeFilter)
lcweCloseTimeFilter = Lens.lens (closeTimeFilter :: ListClosedWorkflowExecutions -> Lude.Maybe ExecutionTimeFilter) (\s a -> s {closeTimeFilter = a} :: ListClosedWorkflowExecutions)
{-# DEPRECATED lcweCloseTimeFilter "Use generic-lens or generic-optics with 'closeTimeFilter' instead." #-}

-- | When set to @true@ , returns the results in reverse order. By default the results are returned in descending order of the start or the close time of the executions.
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcweReverseOrder :: Lens.Lens' ListClosedWorkflowExecutions (Lude.Maybe Lude.Bool)
lcweReverseOrder = Lens.lens (reverseOrder :: ListClosedWorkflowExecutions -> Lude.Maybe Lude.Bool) (\s a -> s {reverseOrder = a} :: ListClosedWorkflowExecutions)
{-# DEPRECATED lcweReverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead." #-}

-- | If specified, only executions that have the matching tag are listed.
--
-- /Note:/ Consider using 'tagFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcweTagFilter :: Lens.Lens' ListClosedWorkflowExecutions (Lude.Maybe TagFilter)
lcweTagFilter = Lens.lens (tagFilter :: ListClosedWorkflowExecutions -> Lude.Maybe TagFilter) (\s a -> s {tagFilter = a} :: ListClosedWorkflowExecutions)
{-# DEPRECATED lcweTagFilter "Use generic-lens or generic-optics with 'tagFilter' instead." #-}

-- | If specified, the workflow executions are included in the returned results based on whether their start times are within the range specified by this filter. Also, if this parameter is specified, the returned results are ordered by their start times.
--
-- /Note:/ Consider using 'startTimeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcweStartTimeFilter :: Lens.Lens' ListClosedWorkflowExecutions (Lude.Maybe ExecutionTimeFilter)
lcweStartTimeFilter = Lens.lens (startTimeFilter :: ListClosedWorkflowExecutions -> Lude.Maybe ExecutionTimeFilter) (\s a -> s {startTimeFilter = a} :: ListClosedWorkflowExecutions)
{-# DEPRECATED lcweStartTimeFilter "Use generic-lens or generic-optics with 'startTimeFilter' instead." #-}

-- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
--
-- /Note:/ Consider using 'maximumPageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcweMaximumPageSize :: Lens.Lens' ListClosedWorkflowExecutions (Lude.Maybe Lude.Natural)
lcweMaximumPageSize = Lens.lens (maximumPageSize :: ListClosedWorkflowExecutions -> Lude.Maybe Lude.Natural) (\s a -> s {maximumPageSize = a} :: ListClosedWorkflowExecutions)
{-# DEPRECATED lcweMaximumPageSize "Use generic-lens or generic-optics with 'maximumPageSize' instead." #-}

instance Page.AWSPager ListClosedWorkflowExecutions where
  page rq rs
    | Page.stop (rs Lens.^. weiNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. weiExecutionInfos) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcweNextPageToken Lens..~ rs Lens.^. weiNextPageToken

instance Lude.AWSRequest ListClosedWorkflowExecutions where
  type Rs ListClosedWorkflowExecutions = WorkflowExecutionInfos
  request = Req.postJSON swfService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders ListClosedWorkflowExecutions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SimpleWorkflowService.ListClosedWorkflowExecutions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListClosedWorkflowExecutions where
  toJSON ListClosedWorkflowExecutions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("executionFilter" Lude..=) Lude.<$> executionFilter,
            ("closeStatusFilter" Lude..=) Lude.<$> closeStatusFilter,
            ("typeFilter" Lude..=) Lude.<$> typeFilter,
            Lude.Just ("domain" Lude..= domain),
            ("closeTimeFilter" Lude..=) Lude.<$> closeTimeFilter,
            ("reverseOrder" Lude..=) Lude.<$> reverseOrder,
            ("tagFilter" Lude..=) Lude.<$> tagFilter,
            ("startTimeFilter" Lude..=) Lude.<$> startTimeFilter,
            ("maximumPageSize" Lude..=) Lude.<$> maximumPageSize
          ]
      )

instance Lude.ToPath ListClosedWorkflowExecutions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListClosedWorkflowExecutions where
  toQuery = Lude.const Lude.mempty
