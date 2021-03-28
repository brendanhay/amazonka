{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing clusters.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListClusters
    (
    -- * Creating a request
      ListClusters (..)
    , mkListClusters
    -- ** Request lenses
    , lcMaxResults
    , lcNextToken

    -- * Destructuring the response
    , ListClustersResponse (..)
    , mkListClustersResponse
    -- ** Response lenses
    , lcrrsClusterArns
    , lcrrsNextToken
    , lcrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListClusters' smart constructor.
data ListClusters = ListClusters'
  { maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of cluster results returned by @ListClusters@ in paginated output. When this parameter is used, @ListClusters@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListClusters@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListClusters@ returns up to 100 results and a @nextToken@ value if applicable.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ value returned from a @ListClusters@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListClusters' value with any optional fields omitted.
mkListClusters
    :: ListClusters
mkListClusters
  = ListClusters'{maxResults = Core.Nothing,
                  nextToken = Core.Nothing}

-- | The maximum number of cluster results returned by @ListClusters@ in paginated output. When this parameter is used, @ListClusters@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListClusters@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListClusters@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListClusters (Core.Maybe Core.Int)
lcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The @nextToken@ value returned from a @ListClusters@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListClusters (Core.Maybe Core.Text)
lcNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListClusters where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListClusters where
        toHeaders ListClusters{..}
          = Core.pure
              ("X-Amz-Target", "AmazonEC2ContainerServiceV20141113.ListClusters")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListClusters where
        toJSON ListClusters{..}
          = Core.object
              (Core.catMaybes
                 [("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListClusters where
        type Rs ListClusters = ListClustersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListClustersResponse' Core.<$>
                   (x Core..:? "clusterArns") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListClusters where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"clusterArns" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { clusterArns :: Core.Maybe [Core.Text]
    -- ^ The list of full Amazon Resource Name (ARN) entries for each cluster associated with your account.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ value to include in a future @ListClusters@ request. When the results of a @ListClusters@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListClustersResponse' value with any optional fields omitted.
mkListClustersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListClustersResponse
mkListClustersResponse responseStatus
  = ListClustersResponse'{clusterArns = Core.Nothing,
                          nextToken = Core.Nothing, responseStatus}

-- | The list of full Amazon Resource Name (ARN) entries for each cluster associated with your account.
--
-- /Note:/ Consider using 'clusterArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsClusterArns :: Lens.Lens' ListClustersResponse (Core.Maybe [Core.Text])
lcrrsClusterArns = Lens.field @"clusterArns"
{-# INLINEABLE lcrrsClusterArns #-}
{-# DEPRECATED clusterArns "Use generic-lens or generic-optics with 'clusterArns' instead"  #-}

-- | The @nextToken@ value to include in a future @ListClusters@ request. When the results of a @ListClusters@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsNextToken :: Lens.Lens' ListClustersResponse (Core.Maybe Core.Text)
lcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListClustersResponse Core.Int
lcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
