{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeEcsClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon ECS clusters that are registered with a stack. If you specify only a stack ID, you can use the @MaxResults@ and @NextToken@ parameters to paginate the response. However, AWS OpsWorks Stacks currently supports only one cluster per layer, so the result set has a maximum of one element.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack or an attached policy that explicitly grants permission. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
-- This call accepts only one resource-identifying parameter.
--
-- This operation returns paginated results.
module Network.AWS.OpsWorks.DescribeEcsClusters
  ( -- * Creating a request
    DescribeEcsClusters (..),
    mkDescribeEcsClusters,

    -- ** Request lenses
    decEcsClusterArns,
    decMaxResults,
    decNextToken,
    decStackId,

    -- * Destructuring the response
    DescribeEcsClustersResponse (..),
    mkDescribeEcsClustersResponse,

    -- ** Response lenses
    decrrsEcsClusters,
    decrrsNextToken,
    decrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEcsClusters' smart constructor.
data DescribeEcsClusters = DescribeEcsClusters'
  { -- | A list of ARNs, one for each cluster to be described.
    ecsClusterArns :: Core.Maybe [Types.String],
    -- | To receive a paginated response, use this parameter to specify the maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
    maxResults :: Core.Maybe Core.Int,
    -- | If the previous paginated request did not return all of the remaining results, the response object's@NextToken@ parameter value is set to a token. To retrieve the next set of results, call @DescribeEcsClusters@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
    nextToken :: Core.Maybe Types.String,
    -- | A stack ID. @DescribeEcsClusters@ returns a description of the cluster that is registered with the stack.
    stackId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEcsClusters' value with any optional fields omitted.
mkDescribeEcsClusters ::
  DescribeEcsClusters
mkDescribeEcsClusters =
  DescribeEcsClusters'
    { ecsClusterArns = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      stackId = Core.Nothing
    }

-- | A list of ARNs, one for each cluster to be described.
--
-- /Note:/ Consider using 'ecsClusterArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decEcsClusterArns :: Lens.Lens' DescribeEcsClusters (Core.Maybe [Types.String])
decEcsClusterArns = Lens.field @"ecsClusterArns"
{-# DEPRECATED decEcsClusterArns "Use generic-lens or generic-optics with 'ecsClusterArns' instead." #-}

-- | To receive a paginated response, use this parameter to specify the maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decMaxResults :: Lens.Lens' DescribeEcsClusters (Core.Maybe Core.Int)
decMaxResults = Lens.field @"maxResults"
{-# DEPRECATED decMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous paginated request did not return all of the remaining results, the response object's@NextToken@ parameter value is set to a token. To retrieve the next set of results, call @DescribeEcsClusters@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decNextToken :: Lens.Lens' DescribeEcsClusters (Core.Maybe Types.String)
decNextToken = Lens.field @"nextToken"
{-# DEPRECATED decNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A stack ID. @DescribeEcsClusters@ returns a description of the cluster that is registered with the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decStackId :: Lens.Lens' DescribeEcsClusters (Core.Maybe Types.String)
decStackId = Lens.field @"stackId"
{-# DEPRECATED decStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Core.FromJSON DescribeEcsClusters where
  toJSON DescribeEcsClusters {..} =
    Core.object
      ( Core.catMaybes
          [ ("EcsClusterArns" Core..=) Core.<$> ecsClusterArns,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("StackId" Core..=) Core.<$> stackId
          ]
      )

instance Core.AWSRequest DescribeEcsClusters where
  type Rs DescribeEcsClusters = DescribeEcsClustersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.DescribeEcsClusters")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEcsClustersResponse'
            Core.<$> (x Core..:? "EcsClusters")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeEcsClusters where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"ecsClusters" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Contains the response to a @DescribeEcsClusters@ request.
--
-- /See:/ 'mkDescribeEcsClustersResponse' smart constructor.
data DescribeEcsClustersResponse = DescribeEcsClustersResponse'
  { -- | A list of @EcsCluster@ objects containing the cluster descriptions.
    ecsClusters :: Core.Maybe [Types.EcsCluster],
    -- | If a paginated request does not return all of the remaining results, this parameter is set to a token that you can assign to the request object's @NextToken@ parameter to retrieve the next set of results. If the previous paginated request returned all of the remaining results, this parameter is set to @null@ .
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEcsClustersResponse' value with any optional fields omitted.
mkDescribeEcsClustersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEcsClustersResponse
mkDescribeEcsClustersResponse responseStatus =
  DescribeEcsClustersResponse'
    { ecsClusters = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of @EcsCluster@ objects containing the cluster descriptions.
--
-- /Note:/ Consider using 'ecsClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsEcsClusters :: Lens.Lens' DescribeEcsClustersResponse (Core.Maybe [Types.EcsCluster])
decrrsEcsClusters = Lens.field @"ecsClusters"
{-# DEPRECATED decrrsEcsClusters "Use generic-lens or generic-optics with 'ecsClusters' instead." #-}

-- | If a paginated request does not return all of the remaining results, this parameter is set to a token that you can assign to the request object's @NextToken@ parameter to retrieve the next set of results. If the previous paginated request returned all of the remaining results, this parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsNextToken :: Lens.Lens' DescribeEcsClustersResponse (Core.Maybe Types.String)
decrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED decrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsResponseStatus :: Lens.Lens' DescribeEcsClustersResponse Core.Int
decrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED decrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
