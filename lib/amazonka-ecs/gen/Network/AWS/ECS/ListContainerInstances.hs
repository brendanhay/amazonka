{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListContainerInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of container instances in a specified cluster. You can filter the results of a @ListContainerInstances@ operation with cluster query language statements inside the @filter@ parameter. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListContainerInstances
    (
    -- * Creating a request
      ListContainerInstances (..)
    , mkListContainerInstances
    -- ** Request lenses
    , lciCluster
    , lciFilter
    , lciMaxResults
    , lciNextToken
    , lciStatus

    -- * Destructuring the response
    , ListContainerInstancesResponse (..)
    , mkListContainerInstancesResponse
    -- ** Response lenses
    , lcirrsContainerInstanceArns
    , lcirrsNextToken
    , lcirrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListContainerInstances' smart constructor.
data ListContainerInstances = ListContainerInstances'
  { cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instances to list. If you do not specify a cluster, the default cluster is assumed.
  , filter :: Core.Maybe Core.Text
    -- ^ You can filter the results of a @ListContainerInstances@ operation with cluster query language statements. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of container instance results returned by @ListContainerInstances@ in paginated output. When this parameter is used, @ListContainerInstances@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListContainerInstances@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListContainerInstances@ returns up to 100 results and a @nextToken@ value if applicable.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ value returned from a @ListContainerInstances@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
  , status :: Core.Maybe Types.ContainerInstanceStatus
    -- ^ Filters the container instances by status. For example, if you specify the @DRAINING@ status, the results include only container instances that have been set to @DRAINING@ using 'UpdateContainerInstancesState' . If you do not specify this parameter, the default is to include container instances set to all states other than @INACTIVE@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListContainerInstances' value with any optional fields omitted.
mkListContainerInstances
    :: ListContainerInstances
mkListContainerInstances
  = ListContainerInstances'{cluster = Core.Nothing,
                            filter = Core.Nothing, maxResults = Core.Nothing,
                            nextToken = Core.Nothing, status = Core.Nothing}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instances to list. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciCluster :: Lens.Lens' ListContainerInstances (Core.Maybe Core.Text)
lciCluster = Lens.field @"cluster"
{-# INLINEABLE lciCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | You can filter the results of a @ListContainerInstances@ operation with cluster query language statements. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciFilter :: Lens.Lens' ListContainerInstances (Core.Maybe Core.Text)
lciFilter = Lens.field @"filter"
{-# INLINEABLE lciFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The maximum number of container instance results returned by @ListContainerInstances@ in paginated output. When this parameter is used, @ListContainerInstances@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListContainerInstances@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListContainerInstances@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciMaxResults :: Lens.Lens' ListContainerInstances (Core.Maybe Core.Int)
lciMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lciMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The @nextToken@ value returned from a @ListContainerInstances@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciNextToken :: Lens.Lens' ListContainerInstances (Core.Maybe Core.Text)
lciNextToken = Lens.field @"nextToken"
{-# INLINEABLE lciNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Filters the container instances by status. For example, if you specify the @DRAINING@ status, the results include only container instances that have been set to @DRAINING@ using 'UpdateContainerInstancesState' . If you do not specify this parameter, the default is to include container instances set to all states other than @INACTIVE@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciStatus :: Lens.Lens' ListContainerInstances (Core.Maybe Types.ContainerInstanceStatus)
lciStatus = Lens.field @"status"
{-# INLINEABLE lciStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToQuery ListContainerInstances where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListContainerInstances where
        toHeaders ListContainerInstances{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.ListContainerInstances")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListContainerInstances where
        toJSON ListContainerInstances{..}
          = Core.object
              (Core.catMaybes
                 [("cluster" Core..=) Core.<$> cluster,
                  ("filter" Core..=) Core.<$> filter,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("status" Core..=) Core.<$> status])

instance Core.AWSRequest ListContainerInstances where
        type Rs ListContainerInstances = ListContainerInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListContainerInstancesResponse' Core.<$>
                   (x Core..:? "containerInstanceArns") Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListContainerInstances where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"containerInstanceArns" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListContainerInstancesResponse' smart constructor.
data ListContainerInstancesResponse = ListContainerInstancesResponse'
  { containerInstanceArns :: Core.Maybe [Core.Text]
    -- ^ The list of container instances with full ARN entries for each container instance associated with the specified cluster.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ value to include in a future @ListContainerInstances@ request. When the results of a @ListContainerInstances@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListContainerInstancesResponse' value with any optional fields omitted.
mkListContainerInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListContainerInstancesResponse
mkListContainerInstancesResponse responseStatus
  = ListContainerInstancesResponse'{containerInstanceArns =
                                      Core.Nothing,
                                    nextToken = Core.Nothing, responseStatus}

-- | The list of container instances with full ARN entries for each container instance associated with the specified cluster.
--
-- /Note:/ Consider using 'containerInstanceArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsContainerInstanceArns :: Lens.Lens' ListContainerInstancesResponse (Core.Maybe [Core.Text])
lcirrsContainerInstanceArns = Lens.field @"containerInstanceArns"
{-# INLINEABLE lcirrsContainerInstanceArns #-}
{-# DEPRECATED containerInstanceArns "Use generic-lens or generic-optics with 'containerInstanceArns' instead"  #-}

-- | The @nextToken@ value to include in a future @ListContainerInstances@ request. When the results of a @ListContainerInstances@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsNextToken :: Lens.Lens' ListContainerInstancesResponse (Core.Maybe Core.Text)
lcirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsResponseStatus :: Lens.Lens' ListContainerInstancesResponse Core.Int
lcirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
