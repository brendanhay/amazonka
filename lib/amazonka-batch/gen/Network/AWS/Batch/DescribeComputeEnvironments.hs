{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.DescribeComputeEnvironments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your compute environments.
--
-- If you are using an unmanaged compute environment, you can use the @DescribeComputeEnvironment@ operation to determine the @ecsClusterArn@ that you should launch your Amazon ECS container instances into.
--
-- This operation returns paginated results.
module Network.AWS.Batch.DescribeComputeEnvironments
  ( -- * Creating a request
    DescribeComputeEnvironments (..),
    mkDescribeComputeEnvironments,

    -- ** Request lenses
    dceComputeEnvironments,
    dceMaxResults,
    dceNextToken,

    -- * Destructuring the response
    DescribeComputeEnvironmentsResponse (..),
    mkDescribeComputeEnvironmentsResponse,

    -- ** Response lenses
    drsComputeEnvironments,
    drsNextToken,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeComputeEnvironments' smart constructor.
data DescribeComputeEnvironments = DescribeComputeEnvironments'
  { -- | A list of up to 100 compute environment names or full Amazon Resource Name (ARN) entries.
    computeEnvironments :: Core.Maybe [Types.String],
    -- | The maximum number of cluster results returned by @DescribeComputeEnvironments@ in paginated output. When this parameter is used, @DescribeComputeEnvironments@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeComputeEnvironments@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeComputeEnvironments@ returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Core.Maybe Core.Int,
    -- | The @nextToken@ value returned from a previous paginated @DescribeComputeEnvironments@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeComputeEnvironments' value with any optional fields omitted.
mkDescribeComputeEnvironments ::
  DescribeComputeEnvironments
mkDescribeComputeEnvironments =
  DescribeComputeEnvironments'
    { computeEnvironments = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | A list of up to 100 compute environment names or full Amazon Resource Name (ARN) entries.
--
-- /Note:/ Consider using 'computeEnvironments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceComputeEnvironments :: Lens.Lens' DescribeComputeEnvironments (Core.Maybe [Types.String])
dceComputeEnvironments = Lens.field @"computeEnvironments"
{-# DEPRECATED dceComputeEnvironments "Use generic-lens or generic-optics with 'computeEnvironments' instead." #-}

-- | The maximum number of cluster results returned by @DescribeComputeEnvironments@ in paginated output. When this parameter is used, @DescribeComputeEnvironments@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeComputeEnvironments@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeComputeEnvironments@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceMaxResults :: Lens.Lens' DescribeComputeEnvironments (Core.Maybe Core.Int)
dceMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dceMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeComputeEnvironments@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceNextToken :: Lens.Lens' DescribeComputeEnvironments (Core.Maybe Types.String)
dceNextToken = Lens.field @"nextToken"
{-# DEPRECATED dceNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeComputeEnvironments where
  toJSON DescribeComputeEnvironments {..} =
    Core.object
      ( Core.catMaybes
          [ ("computeEnvironments" Core..=) Core.<$> computeEnvironments,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeComputeEnvironments where
  type
    Rs DescribeComputeEnvironments =
      DescribeComputeEnvironmentsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/v1/describecomputeenvironments",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeComputeEnvironmentsResponse'
            Core.<$> (x Core..:? "computeEnvironments")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeComputeEnvironments where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"computeEnvironments" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeComputeEnvironmentsResponse' smart constructor.
data DescribeComputeEnvironmentsResponse = DescribeComputeEnvironmentsResponse'
  { -- | The list of compute environments.
    computeEnvironments :: Core.Maybe [Types.ComputeEnvironmentDetail],
    -- | The @nextToken@ value to include in a future @DescribeComputeEnvironments@ request. When the results of a @DescribeJobDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeComputeEnvironmentsResponse' value with any optional fields omitted.
mkDescribeComputeEnvironmentsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeComputeEnvironmentsResponse
mkDescribeComputeEnvironmentsResponse responseStatus =
  DescribeComputeEnvironmentsResponse'
    { computeEnvironments =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of compute environments.
--
-- /Note:/ Consider using 'computeEnvironments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsComputeEnvironments :: Lens.Lens' DescribeComputeEnvironmentsResponse (Core.Maybe [Types.ComputeEnvironmentDetail])
drsComputeEnvironments = Lens.field @"computeEnvironments"
{-# DEPRECATED drsComputeEnvironments "Use generic-lens or generic-optics with 'computeEnvironments' instead." #-}

-- | The @nextToken@ value to include in a future @DescribeComputeEnvironments@ request. When the results of a @DescribeJobDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeComputeEnvironmentsResponse (Core.Maybe Types.String)
drsNextToken = Lens.field @"nextToken"
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeComputeEnvironmentsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
