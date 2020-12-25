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
    loweDomain,
    loweStartTimeFilter,
    loweExecutionFilter,
    loweMaximumPageSize,
    loweNextPageToken,
    loweReverseOrder,
    loweTagFilter,
    loweTypeFilter,

    -- * Destructuring the response
    Types.WorkflowExecutionInfos (..),
    Types.mkWorkflowExecutionInfos,

    -- ** Response lenses
    Types.weiExecutionInfos,
    Types.weiNextPageToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkListOpenWorkflowExecutions' smart constructor.
data ListOpenWorkflowExecutions = ListOpenWorkflowExecutions'
  { -- | The name of the domain that contains the workflow executions to list.
    domain :: Types.DomainName,
    -- | Workflow executions are included in the returned results based on whether their start times are within the range specified by this filter.
    startTimeFilter :: Types.ExecutionTimeFilter,
    -- | If specified, only workflow executions matching the workflow ID specified in the filter are returned.
    executionFilter :: Core.Maybe Types.WorkflowExecutionFilter,
    -- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
    maximumPageSize :: Core.Maybe Core.Natural,
    -- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
    --
    -- The configured @maximumPageSize@ determines how many results can be returned in a single call.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | When set to @true@ , returns the results in reverse order. By default the results are returned in descending order of the start time of the executions.
    reverseOrder :: Core.Maybe Core.Bool,
    -- | If specified, only executions that have the matching tag are listed.
    tagFilter :: Core.Maybe Types.TagFilter,
    -- | If specified, only executions of the type specified in the filter are returned.
    typeFilter :: Core.Maybe Types.WorkflowTypeFilter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListOpenWorkflowExecutions' value with any optional fields omitted.
mkListOpenWorkflowExecutions ::
  -- | 'domain'
  Types.DomainName ->
  -- | 'startTimeFilter'
  Types.ExecutionTimeFilter ->
  ListOpenWorkflowExecutions
mkListOpenWorkflowExecutions domain startTimeFilter =
  ListOpenWorkflowExecutions'
    { domain,
      startTimeFilter,
      executionFilter = Core.Nothing,
      maximumPageSize = Core.Nothing,
      nextPageToken = Core.Nothing,
      reverseOrder = Core.Nothing,
      tagFilter = Core.Nothing,
      typeFilter = Core.Nothing
    }

-- | The name of the domain that contains the workflow executions to list.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweDomain :: Lens.Lens' ListOpenWorkflowExecutions Types.DomainName
loweDomain = Lens.field @"domain"
{-# DEPRECATED loweDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Workflow executions are included in the returned results based on whether their start times are within the range specified by this filter.
--
-- /Note:/ Consider using 'startTimeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweStartTimeFilter :: Lens.Lens' ListOpenWorkflowExecutions Types.ExecutionTimeFilter
loweStartTimeFilter = Lens.field @"startTimeFilter"
{-# DEPRECATED loweStartTimeFilter "Use generic-lens or generic-optics with 'startTimeFilter' instead." #-}

-- | If specified, only workflow executions matching the workflow ID specified in the filter are returned.
--
-- /Note:/ Consider using 'executionFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweExecutionFilter :: Lens.Lens' ListOpenWorkflowExecutions (Core.Maybe Types.WorkflowExecutionFilter)
loweExecutionFilter = Lens.field @"executionFilter"
{-# DEPRECATED loweExecutionFilter "Use generic-lens or generic-optics with 'executionFilter' instead." #-}

-- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
--
-- /Note:/ Consider using 'maximumPageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweMaximumPageSize :: Lens.Lens' ListOpenWorkflowExecutions (Core.Maybe Core.Natural)
loweMaximumPageSize = Lens.field @"maximumPageSize"
{-# DEPRECATED loweMaximumPageSize "Use generic-lens or generic-optics with 'maximumPageSize' instead." #-}

-- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweNextPageToken :: Lens.Lens' ListOpenWorkflowExecutions (Core.Maybe Types.NextPageToken)
loweNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED loweNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | When set to @true@ , returns the results in reverse order. By default the results are returned in descending order of the start time of the executions.
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweReverseOrder :: Lens.Lens' ListOpenWorkflowExecutions (Core.Maybe Core.Bool)
loweReverseOrder = Lens.field @"reverseOrder"
{-# DEPRECATED loweReverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead." #-}

-- | If specified, only executions that have the matching tag are listed.
--
-- /Note:/ Consider using 'tagFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweTagFilter :: Lens.Lens' ListOpenWorkflowExecutions (Core.Maybe Types.TagFilter)
loweTagFilter = Lens.field @"tagFilter"
{-# DEPRECATED loweTagFilter "Use generic-lens or generic-optics with 'tagFilter' instead." #-}

-- | If specified, only executions of the type specified in the filter are returned.
--
-- /Note:/ Consider using 'typeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loweTypeFilter :: Lens.Lens' ListOpenWorkflowExecutions (Core.Maybe Types.WorkflowTypeFilter)
loweTypeFilter = Lens.field @"typeFilter"
{-# DEPRECATED loweTypeFilter "Use generic-lens or generic-optics with 'typeFilter' instead." #-}

instance Core.FromJSON ListOpenWorkflowExecutions where
  toJSON ListOpenWorkflowExecutions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domain" Core..= domain),
            Core.Just ("startTimeFilter" Core..= startTimeFilter),
            ("executionFilter" Core..=) Core.<$> executionFilter,
            ("maximumPageSize" Core..=) Core.<$> maximumPageSize,
            ("nextPageToken" Core..=) Core.<$> nextPageToken,
            ("reverseOrder" Core..=) Core.<$> reverseOrder,
            ("tagFilter" Core..=) Core.<$> tagFilter,
            ("typeFilter" Core..=) Core.<$> typeFilter
          ]
      )

instance Core.AWSRequest ListOpenWorkflowExecutions where
  type Rs ListOpenWorkflowExecutions = Types.WorkflowExecutionInfos
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "SimpleWorkflowService.ListOpenWorkflowExecutions"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)

instance Pager.AWSPager ListOpenWorkflowExecutions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"executionInfos") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextPageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )
