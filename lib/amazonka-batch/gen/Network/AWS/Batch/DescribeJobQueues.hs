{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.DescribeJobQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your job queues.
--
-- This operation returns paginated results.
module Network.AWS.Batch.DescribeJobQueues
  ( -- * Creating a request
    DescribeJobQueues (..),
    mkDescribeJobQueues,

    -- ** Request lenses
    djqJobQueues,
    djqMaxResults,
    djqNextToken,

    -- * Destructuring the response
    DescribeJobQueuesResponse (..),
    mkDescribeJobQueuesResponse,

    -- ** Response lenses
    djqrfrsJobQueues,
    djqrfrsNextToken,
    djqrfrsResponseStatus,
  )
where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeJobQueues' smart constructor.
data DescribeJobQueues = DescribeJobQueues'
  { -- | A list of up to 100 queue names or full queue Amazon Resource Name (ARN) entries.
    jobQueues :: Core.Maybe [Types.String],
    -- | The maximum number of results returned by @DescribeJobQueues@ in paginated output. When this parameter is used, @DescribeJobQueues@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeJobQueues@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeJobQueues@ returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Core.Maybe Core.Int,
    -- | The @nextToken@ value returned from a previous paginated @DescribeJobQueues@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeJobQueues' value with any optional fields omitted.
mkDescribeJobQueues ::
  DescribeJobQueues
mkDescribeJobQueues =
  DescribeJobQueues'
    { jobQueues = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | A list of up to 100 queue names or full queue Amazon Resource Name (ARN) entries.
--
-- /Note:/ Consider using 'jobQueues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqJobQueues :: Lens.Lens' DescribeJobQueues (Core.Maybe [Types.String])
djqJobQueues = Lens.field @"jobQueues"
{-# DEPRECATED djqJobQueues "Use generic-lens or generic-optics with 'jobQueues' instead." #-}

-- | The maximum number of results returned by @DescribeJobQueues@ in paginated output. When this parameter is used, @DescribeJobQueues@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeJobQueues@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeJobQueues@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqMaxResults :: Lens.Lens' DescribeJobQueues (Core.Maybe Core.Int)
djqMaxResults = Lens.field @"maxResults"
{-# DEPRECATED djqMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeJobQueues@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqNextToken :: Lens.Lens' DescribeJobQueues (Core.Maybe Types.String)
djqNextToken = Lens.field @"nextToken"
{-# DEPRECATED djqNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeJobQueues where
  toJSON DescribeJobQueues {..} =
    Core.object
      ( Core.catMaybes
          [ ("jobQueues" Core..=) Core.<$> jobQueues,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeJobQueues where
  type Rs DescribeJobQueues = DescribeJobQueuesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/v1/describejobqueues",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobQueuesResponse'
            Core.<$> (x Core..:? "jobQueues")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeJobQueues where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"jobQueues" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeJobQueuesResponse' smart constructor.
data DescribeJobQueuesResponse = DescribeJobQueuesResponse'
  { -- | The list of job queues.
    jobQueues :: Core.Maybe [Types.JobQueueDetail],
    -- | The @nextToken@ value to include in a future @DescribeJobQueues@ request. When the results of a @DescribeJobQueues@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeJobQueuesResponse' value with any optional fields omitted.
mkDescribeJobQueuesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeJobQueuesResponse
mkDescribeJobQueuesResponse responseStatus =
  DescribeJobQueuesResponse'
    { jobQueues = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of job queues.
--
-- /Note:/ Consider using 'jobQueues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqrfrsJobQueues :: Lens.Lens' DescribeJobQueuesResponse (Core.Maybe [Types.JobQueueDetail])
djqrfrsJobQueues = Lens.field @"jobQueues"
{-# DEPRECATED djqrfrsJobQueues "Use generic-lens or generic-optics with 'jobQueues' instead." #-}

-- | The @nextToken@ value to include in a future @DescribeJobQueues@ request. When the results of a @DescribeJobQueues@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqrfrsNextToken :: Lens.Lens' DescribeJobQueuesResponse (Core.Maybe Types.String)
djqrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED djqrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqrfrsResponseStatus :: Lens.Lens' DescribeJobQueuesResponse Core.Int
djqrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED djqrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
