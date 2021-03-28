{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the status of all clusters visible to this AWS account. Allows you to filter the list of clusters based on certain criteria; for example, filtering by cluster creation date and time or by status. This call returns a maximum of 50 clusters per call, but returns a marker to track the paging of the cluster list across multiple ListClusters calls.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListClusters
    (
    -- * Creating a request
      ListClusters (..)
    , mkListClusters
    -- ** Request lenses
    , lcClusterStates
    , lcCreatedAfter
    , lcCreatedBefore
    , lcMarker

    -- * Destructuring the response
    , ListClustersResponse (..)
    , mkListClustersResponse
    -- ** Response lenses
    , lcrrsClusters
    , lcrrsMarker
    , lcrrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines how the ListClusters action filters the list of clusters that it returns.
--
-- /See:/ 'mkListClusters' smart constructor.
data ListClusters = ListClusters'
  { clusterStates :: Core.Maybe [Types.ClusterState]
    -- ^ The cluster state filters to apply when listing clusters.
  , createdAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ The creation date and time beginning value filter for listing clusters.
  , createdBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ The creation date and time end value filter for listing clusters.
  , marker :: Core.Maybe Types.Marker
    -- ^ The pagination token that indicates the next set of results to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListClusters' value with any optional fields omitted.
mkListClusters
    :: ListClusters
mkListClusters
  = ListClusters'{clusterStates = Core.Nothing,
                  createdAfter = Core.Nothing, createdBefore = Core.Nothing,
                  marker = Core.Nothing}

-- | The cluster state filters to apply when listing clusters.
--
-- /Note:/ Consider using 'clusterStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcClusterStates :: Lens.Lens' ListClusters (Core.Maybe [Types.ClusterState])
lcClusterStates = Lens.field @"clusterStates"
{-# INLINEABLE lcClusterStates #-}
{-# DEPRECATED clusterStates "Use generic-lens or generic-optics with 'clusterStates' instead"  #-}

-- | The creation date and time beginning value filter for listing clusters.
--
-- /Note:/ Consider using 'createdAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcCreatedAfter :: Lens.Lens' ListClusters (Core.Maybe Core.NominalDiffTime)
lcCreatedAfter = Lens.field @"createdAfter"
{-# INLINEABLE lcCreatedAfter #-}
{-# DEPRECATED createdAfter "Use generic-lens or generic-optics with 'createdAfter' instead"  #-}

-- | The creation date and time end value filter for listing clusters.
--
-- /Note:/ Consider using 'createdBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcCreatedBefore :: Lens.Lens' ListClusters (Core.Maybe Core.NominalDiffTime)
lcCreatedBefore = Lens.field @"createdBefore"
{-# INLINEABLE lcCreatedBefore #-}
{-# DEPRECATED createdBefore "Use generic-lens or generic-optics with 'createdBefore' instead"  #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMarker :: Lens.Lens' ListClusters (Core.Maybe Types.Marker)
lcMarker = Lens.field @"marker"
{-# INLINEABLE lcMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery ListClusters where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListClusters where
        toHeaders ListClusters{..}
          = Core.pure ("X-Amz-Target", "ElasticMapReduce.ListClusters")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListClusters where
        toJSON ListClusters{..}
          = Core.object
              (Core.catMaybes
                 [("ClusterStates" Core..=) Core.<$> clusterStates,
                  ("CreatedAfter" Core..=) Core.<$> createdAfter,
                  ("CreatedBefore" Core..=) Core.<$> createdBefore,
                  ("Marker" Core..=) Core.<$> marker])

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
                   (x Core..:? "Clusters") Core.<*> x Core..:? "Marker" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListClusters where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"clusters" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | This contains a ClusterSummaryList with the cluster details; for example, the cluster IDs, names, and status.
--
-- /See:/ 'mkListClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { clusters :: Core.Maybe [Types.ClusterSummary]
    -- ^ The list of clusters for the account based on the given filters.
  , marker :: Core.Maybe Types.Marker
    -- ^ The pagination token that indicates the next set of results to retrieve.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListClustersResponse' value with any optional fields omitted.
mkListClustersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListClustersResponse
mkListClustersResponse responseStatus
  = ListClustersResponse'{clusters = Core.Nothing,
                          marker = Core.Nothing, responseStatus}

-- | The list of clusters for the account based on the given filters.
--
-- /Note:/ Consider using 'clusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsClusters :: Lens.Lens' ListClustersResponse (Core.Maybe [Types.ClusterSummary])
lcrrsClusters = Lens.field @"clusters"
{-# INLINEABLE lcrrsClusters #-}
{-# DEPRECATED clusters "Use generic-lens or generic-optics with 'clusters' instead"  #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsMarker :: Lens.Lens' ListClustersResponse (Core.Maybe Types.Marker)
lcrrsMarker = Lens.field @"marker"
{-# INLINEABLE lcrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListClustersResponse Core.Int
lcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
