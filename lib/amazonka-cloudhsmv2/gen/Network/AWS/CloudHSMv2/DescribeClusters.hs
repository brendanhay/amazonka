{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.DescribeClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about AWS CloudHSM clusters.
--
-- This is a paginated operation, which means that each response might contain only a subset of all the clusters. When the response contains only a subset of clusters, it includes a @NextToken@ value. Use this value in a subsequent @DescribeClusters@ request to get more clusters. When you receive a response with no @NextToken@ (or an empty or null value), that means there are no more clusters to get.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSMv2.DescribeClusters
    (
    -- * Creating a request
      DescribeClusters (..)
    , mkDescribeClusters
    -- ** Request lenses
    , dcFilters
    , dcMaxResults
    , dcNextToken

    -- * Destructuring the response
    , DescribeClustersResponse (..)
    , mkDescribeClustersResponse
    -- ** Response lenses
    , dcrrsClusters
    , dcrrsNextToken
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSMv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { filters :: Core.Maybe (Core.HashMap Types.Field [Core.Text])
    -- ^ One or more filters to limit the items returned in the response.
--
-- Use the @clusterIds@ filter to return only the specified clusters. Specify clusters by their cluster identifier (ID).
-- Use the @vpcIds@ filter to return only the clusters in the specified virtual private clouds (VPCs). Specify VPCs by their VPC identifier (ID).
-- Use the @states@ filter to return only clusters that match the specified state.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of clusters to return in the response. When there are more clusters than the number you specify, the response contains a @NextToken@ value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @NextToken@ value that you received in the previous response. Use this value to get more clusters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusters' value with any optional fields omitted.
mkDescribeClusters
    :: DescribeClusters
mkDescribeClusters
  = DescribeClusters'{filters = Core.Nothing,
                      maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | One or more filters to limit the items returned in the response.
--
-- Use the @clusterIds@ filter to return only the specified clusters. Specify clusters by their cluster identifier (ID).
-- Use the @vpcIds@ filter to return only the clusters in the specified virtual private clouds (VPCs). Specify VPCs by their VPC identifier (ID).
-- Use the @states@ filter to return only clusters that match the specified state.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcFilters :: Lens.Lens' DescribeClusters (Core.Maybe (Core.HashMap Types.Field [Core.Text]))
dcFilters = Lens.field @"filters"
{-# INLINEABLE dcFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of clusters to return in the response. When there are more clusters than the number you specify, the response contains a @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMaxResults :: Lens.Lens' DescribeClusters (Core.Maybe Core.Natural)
dcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The @NextToken@ value that you received in the previous response. Use this value to get more clusters.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcNextToken :: Lens.Lens' DescribeClusters (Core.Maybe Types.NextToken)
dcNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeClusters where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeClusters where
        toHeaders DescribeClusters{..}
          = Core.pure ("X-Amz-Target", "BaldrApiService.DescribeClusters")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeClusters where
        toJSON DescribeClusters{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeClusters where
        type Rs DescribeClusters = DescribeClustersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeClustersResponse' Core.<$>
                   (x Core..:? "Clusters") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeClusters where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"clusters" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { clusters :: Core.Maybe [Types.Cluster]
    -- ^ A list of clusters.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An opaque string that indicates that the response contains only a subset of clusters. Use this value in a subsequent @DescribeClusters@ request to get more clusters.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeClustersResponse' value with any optional fields omitted.
mkDescribeClustersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeClustersResponse
mkDescribeClustersResponse responseStatus
  = DescribeClustersResponse'{clusters = Core.Nothing,
                              nextToken = Core.Nothing, responseStatus}

-- | A list of clusters.
--
-- /Note:/ Consider using 'clusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsClusters :: Lens.Lens' DescribeClustersResponse (Core.Maybe [Types.Cluster])
dcrrsClusters = Lens.field @"clusters"
{-# INLINEABLE dcrrsClusters #-}
{-# DEPRECATED clusters "Use generic-lens or generic-optics with 'clusters' instead"  #-}

-- | An opaque string that indicates that the response contains only a subset of clusters. Use this value in a subsequent @DescribeClusters@ request to get more clusters.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsNextToken :: Lens.Lens' DescribeClustersResponse (Core.Maybe Types.NextToken)
dcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeClustersResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
