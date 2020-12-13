{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.ListOpenWorkflowExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of open workflow executions in the specified domain that meet the filtering criteria. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.
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
module Network.AWS.SWF.ListOpenWorkflowExecutions
  ( -- * Creating a request
    ListOpenWorkflowExecutions (..),
    mkListOpenWorkflowExecutions,

    -- ** Request lenses
    loweNextPageToken,
    loweExecutionFilter,
    loweTypeFilter,
    loweDomain,
    loweReverseOrder,
    loweTagFilter,
    loweStartTimeFilter,
    loweMaximumPageSize,

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

-- | /See:/ 'mkListOpenWorkflowExecutions' smart constructor.
data ListOpenWorkflowExecutions = ListOpenWorkflowExecutions'
  { -- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
    --
    -- The configured @maximumPageSize@ determines how many results can be returned in a single call.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | If specified, only workflow executions matching the workflow ID specified in the filter are returned.
    executionFilter :: Lude.Maybe WorkflowExecutionFilter,
    -- | If specified, only executions of the type specified in the filter are returned.
    typeFilter :: Lude.Maybe WorkflowTypeFilter,
    -- | The name of the domain that contains the workflow executions to list.
    domain :: Lude.Text,
    -- | When set to @true@ , returns the results in reverse order. By default the results are returned in descending order of the start time of the executions.
    reverseOrder :: Lude.Maybe Lude.Bool,
    -- | If specified, only executions that have the matching tag are listed.
    tagFilter :: Lude.Maybe TagFilter,
    -- | Workflow executions are included in the returned results based on whether their start times are within the range specified by this filter.
    startTimeFilter :: ExecutionTimeFilter,
    -- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
    maximumPageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOpenWorkflowExecutions' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
-- * 'executionFilter' - If specified, only workflow executions matching the workflow ID specified in the filter are returned.
-- * 'typeFilter' - If specified, only executions of the type specified in the filter are returned.
-- * 'domain' - The name of the domain that contains the workflow executions to list.
-- * 'reverseOrder' - When set to @true@ , returns the results in reverse order. By default the results are returned in descending order of the start time of the executions.
-- * 'tagFilter' - If specified, only executions that have the matching tag are listed.
-- * 'startTimeFilter' - Workflow executions are included in the returned results based on whether their start times are within the range specified by this filter.
-- * 'maximumPageSize' - The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
mkListOpenWorkflowExecutions ::
  -- | 'domain'
  Lude.Text ->
  -- | 'startTimeFilter'
  ExecutionTimeFilter ->
  ListOpenWorkflowExecutions
mkListOpenWorkflowExecutions pDomain_ pStartTimeFilter_ =
  ListOpenWorkflowExecutions'
    { nextPageToken = Lude.Nothing,
      executionFilter = Lude.Nothing,
      typeFilter = Lude.Nothing,
      domain = pDomain_,
      reverseOrder = Lude.Nothing,
      tagFilter = Lude.Nothing,
      startTimeFilter = pStartTimeFilter_,
      maximumPageSize = Lude.Nothing
    }

-- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweNextPageToken :: Lens.Lens' ListOpenWorkflowExecutions (Lude.Maybe Lude.Text)
loweNextPageToken = Lens.lens (nextPageToken :: ListOpenWorkflowExecutions -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListOpenWorkflowExecutions)
{-# DEPRECATED loweNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | If specified, only workflow executions matching the workflow ID specified in the filter are returned.
--
-- /Note:/ Consider using 'executionFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweExecutionFilter :: Lens.Lens' ListOpenWorkflowExecutions (Lude.Maybe WorkflowExecutionFilter)
loweExecutionFilter = Lens.lens (executionFilter :: ListOpenWorkflowExecutions -> Lude.Maybe WorkflowExecutionFilter) (\s a -> s {executionFilter = a} :: ListOpenWorkflowExecutions)
{-# DEPRECATED loweExecutionFilter "Use generic-lens or generic-optics with 'executionFilter' instead." #-}

-- | If specified, only executions of the type specified in the filter are returned.
--
-- /Note:/ Consider using 'typeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweTypeFilter :: Lens.Lens' ListOpenWorkflowExecutions (Lude.Maybe WorkflowTypeFilter)
loweTypeFilter = Lens.lens (typeFilter :: ListOpenWorkflowExecutions -> Lude.Maybe WorkflowTypeFilter) (\s a -> s {typeFilter = a} :: ListOpenWorkflowExecutions)
{-# DEPRECATED loweTypeFilter "Use generic-lens or generic-optics with 'typeFilter' instead." #-}

-- | The name of the domain that contains the workflow executions to list.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweDomain :: Lens.Lens' ListOpenWorkflowExecutions Lude.Text
loweDomain = Lens.lens (domain :: ListOpenWorkflowExecutions -> Lude.Text) (\s a -> s {domain = a} :: ListOpenWorkflowExecutions)
{-# DEPRECATED loweDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | When set to @true@ , returns the results in reverse order. By default the results are returned in descending order of the start time of the executions.
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweReverseOrder :: Lens.Lens' ListOpenWorkflowExecutions (Lude.Maybe Lude.Bool)
loweReverseOrder = Lens.lens (reverseOrder :: ListOpenWorkflowExecutions -> Lude.Maybe Lude.Bool) (\s a -> s {reverseOrder = a} :: ListOpenWorkflowExecutions)
{-# DEPRECATED loweReverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead." #-}

-- | If specified, only executions that have the matching tag are listed.
--
-- /Note:/ Consider using 'tagFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweTagFilter :: Lens.Lens' ListOpenWorkflowExecutions (Lude.Maybe TagFilter)
loweTagFilter = Lens.lens (tagFilter :: ListOpenWorkflowExecutions -> Lude.Maybe TagFilter) (\s a -> s {tagFilter = a} :: ListOpenWorkflowExecutions)
{-# DEPRECATED loweTagFilter "Use generic-lens or generic-optics with 'tagFilter' instead." #-}

-- | Workflow executions are included in the returned results based on whether their start times are within the range specified by this filter.
--
-- /Note:/ Consider using 'startTimeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweStartTimeFilter :: Lens.Lens' ListOpenWorkflowExecutions ExecutionTimeFilter
loweStartTimeFilter = Lens.lens (startTimeFilter :: ListOpenWorkflowExecutions -> ExecutionTimeFilter) (\s a -> s {startTimeFilter = a} :: ListOpenWorkflowExecutions)
{-# DEPRECATED loweStartTimeFilter "Use generic-lens or generic-optics with 'startTimeFilter' instead." #-}

-- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
--
-- /Note:/ Consider using 'maximumPageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweMaximumPageSize :: Lens.Lens' ListOpenWorkflowExecutions (Lude.Maybe Lude.Natural)
loweMaximumPageSize = Lens.lens (maximumPageSize :: ListOpenWorkflowExecutions -> Lude.Maybe Lude.Natural) (\s a -> s {maximumPageSize = a} :: ListOpenWorkflowExecutions)
{-# DEPRECATED loweMaximumPageSize "Use generic-lens or generic-optics with 'maximumPageSize' instead." #-}

instance Page.AWSPager ListOpenWorkflowExecutions where
  page rq rs
    | Page.stop (rs Lens.^. weiNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. weiExecutionInfos) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& loweNextPageToken Lens..~ rs Lens.^. weiNextPageToken

instance Lude.AWSRequest ListOpenWorkflowExecutions where
  type Rs ListOpenWorkflowExecutions = WorkflowExecutionInfos
  request = Req.postJSON swfService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders ListOpenWorkflowExecutions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SimpleWorkflowService.ListOpenWorkflowExecutions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListOpenWorkflowExecutions where
  toJSON ListOpenWorkflowExecutions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("executionFilter" Lude..=) Lude.<$> executionFilter,
            ("typeFilter" Lude..=) Lude.<$> typeFilter,
            Lude.Just ("domain" Lude..= domain),
            ("reverseOrder" Lude..=) Lude.<$> reverseOrder,
            ("tagFilter" Lude..=) Lude.<$> tagFilter,
            Lude.Just ("startTimeFilter" Lude..= startTimeFilter),
            ("maximumPageSize" Lude..=) Lude.<$> maximumPageSize
          ]
      )

instance Lude.ToPath ListOpenWorkflowExecutions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListOpenWorkflowExecutions where
  toQuery = Lude.const Lude.mempty
