{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribePendingAggregationRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all pending aggregation requests.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribePendingAggregationRequests
  ( -- * Creating a request
    DescribePendingAggregationRequests (..),
    mkDescribePendingAggregationRequests,

    -- ** Request lenses
    dparLimit,
    dparNextToken,

    -- * Destructuring the response
    DescribePendingAggregationRequestsResponse (..),
    mkDescribePendingAggregationRequestsResponse,

    -- ** Response lenses
    dparrrsNextToken,
    dparrrsPendingAggregationRequests,
    dparrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePendingAggregationRequests' smart constructor.
data DescribePendingAggregationRequests = DescribePendingAggregationRequests'
  { -- | The maximum number of evaluation results returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePendingAggregationRequests' value with any optional fields omitted.
mkDescribePendingAggregationRequests ::
  DescribePendingAggregationRequests
mkDescribePendingAggregationRequests =
  DescribePendingAggregationRequests'
    { limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of evaluation results returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparLimit :: Lens.Lens' DescribePendingAggregationRequests (Core.Maybe Core.Natural)
dparLimit = Lens.field @"limit"
{-# DEPRECATED dparLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparNextToken :: Lens.Lens' DescribePendingAggregationRequests (Core.Maybe Types.String)
dparNextToken = Lens.field @"nextToken"
{-# DEPRECATED dparNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribePendingAggregationRequests where
  toJSON DescribePendingAggregationRequests {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribePendingAggregationRequests where
  type
    Rs DescribePendingAggregationRequests =
      DescribePendingAggregationRequestsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DescribePendingAggregationRequests"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePendingAggregationRequestsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "PendingAggregationRequests")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribePendingAggregationRequests where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"pendingAggregationRequests" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribePendingAggregationRequestsResponse' smart constructor.
data DescribePendingAggregationRequestsResponse = DescribePendingAggregationRequestsResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Returns a PendingAggregationRequests object.
    pendingAggregationRequests :: Core.Maybe [Types.PendingAggregationRequest],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePendingAggregationRequestsResponse' value with any optional fields omitted.
mkDescribePendingAggregationRequestsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePendingAggregationRequestsResponse
mkDescribePendingAggregationRequestsResponse responseStatus =
  DescribePendingAggregationRequestsResponse'
    { nextToken =
        Core.Nothing,
      pendingAggregationRequests = Core.Nothing,
      responseStatus
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparrrsNextToken :: Lens.Lens' DescribePendingAggregationRequestsResponse (Core.Maybe Types.NextToken)
dparrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dparrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns a PendingAggregationRequests object.
--
-- /Note:/ Consider using 'pendingAggregationRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparrrsPendingAggregationRequests :: Lens.Lens' DescribePendingAggregationRequestsResponse (Core.Maybe [Types.PendingAggregationRequest])
dparrrsPendingAggregationRequests = Lens.field @"pendingAggregationRequests"
{-# DEPRECATED dparrrsPendingAggregationRequests "Use generic-lens or generic-optics with 'pendingAggregationRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparrrsResponseStatus :: Lens.Lens' DescribePendingAggregationRequestsResponse Core.Int
dparrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dparrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
