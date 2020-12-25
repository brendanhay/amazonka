{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.ListExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the executions of a state machine that meet the filtering criteria. Results are sorted by time, with the most recent execution first.
--
-- If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
-- This API action is not supported by @EXPRESS@ state machines.
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.ListExecutions
  ( -- * Creating a request
    ListExecutions (..),
    mkListExecutions,

    -- ** Request lenses
    leStateMachineArn,
    leMaxResults,
    leNextToken,
    leStatusFilter,

    -- * Destructuring the response
    ListExecutionsResponse (..),
    mkListExecutionsResponse,

    -- ** Response lenses
    lerrsExecutions,
    lerrsNextToken,
    lerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkListExecutions' smart constructor.
data ListExecutions = ListExecutions'
  { -- | The Amazon Resource Name (ARN) of the state machine whose executions is listed.
    stateMachineArn :: Types.Arn,
    -- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
    --
    -- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
    maxResults :: Core.Maybe Core.Natural,
    -- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
    nextToken :: Core.Maybe Types.NextToken,
    -- | If specified, only list the executions whose current execution status matches the given filter.
    statusFilter :: Core.Maybe Types.ExecutionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListExecutions' value with any optional fields omitted.
mkListExecutions ::
  -- | 'stateMachineArn'
  Types.Arn ->
  ListExecutions
mkListExecutions stateMachineArn =
  ListExecutions'
    { stateMachineArn,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      statusFilter = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the state machine whose executions is listed.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leStateMachineArn :: Lens.Lens' ListExecutions Types.Arn
leStateMachineArn = Lens.field @"stateMachineArn"
{-# DEPRECATED leStateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead." #-}

-- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' ListExecutions (Core.Maybe Core.Natural)
leMaxResults = Lens.field @"maxResults"
{-# DEPRECATED leMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListExecutions (Core.Maybe Types.NextToken)
leNextToken = Lens.field @"nextToken"
{-# DEPRECATED leNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If specified, only list the executions whose current execution status matches the given filter.
--
-- /Note:/ Consider using 'statusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leStatusFilter :: Lens.Lens' ListExecutions (Core.Maybe Types.ExecutionStatus)
leStatusFilter = Lens.field @"statusFilter"
{-# DEPRECATED leStatusFilter "Use generic-lens or generic-optics with 'statusFilter' instead." #-}

instance Core.FromJSON ListExecutions where
  toJSON ListExecutions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("stateMachineArn" Core..= stateMachineArn),
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("statusFilter" Core..=) Core.<$> statusFilter
          ]
      )

instance Core.AWSRequest ListExecutions where
  type Rs ListExecutions = ListExecutionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSStepFunctions.ListExecutions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExecutionsResponse'
            Core.<$> (x Core..:? "executions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListExecutions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"executions") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListExecutionsResponse' smart constructor.
data ListExecutionsResponse = ListExecutionsResponse'
  { -- | The list of matching executions.
    executions :: [Types.ExecutionListItem],
    -- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
    nextToken :: Core.Maybe Types.ListExecutionsPageToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListExecutionsResponse' value with any optional fields omitted.
mkListExecutionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListExecutionsResponse
mkListExecutionsResponse responseStatus =
  ListExecutionsResponse'
    { executions = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of matching executions.
--
-- /Note:/ Consider using 'executions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsExecutions :: Lens.Lens' ListExecutionsResponse [Types.ExecutionListItem]
lerrsExecutions = Lens.field @"executions"
{-# DEPRECATED lerrsExecutions "Use generic-lens or generic-optics with 'executions' instead." #-}

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsNextToken :: Lens.Lens' ListExecutionsResponse (Core.Maybe Types.ListExecutionsPageToken)
lerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsResponseStatus :: Lens.Lens' ListExecutionsResponse Core.Int
lerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
