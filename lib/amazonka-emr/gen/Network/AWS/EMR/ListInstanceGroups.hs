{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListInstanceGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides all available details about the instance groups in a cluster.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListInstanceGroups
    (
    -- * Creating a request
      ListInstanceGroups (..)
    , mkListInstanceGroups
    -- ** Request lenses
    , ligClusterId
    , ligMarker

    -- * Destructuring the response
    , ListInstanceGroupsResponse (..)
    , mkListInstanceGroupsResponse
    -- ** Response lenses
    , ligrrsInstanceGroups
    , ligrrsMarker
    , ligrrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines which instance groups to retrieve.
--
-- /See:/ 'mkListInstanceGroups' smart constructor.
data ListInstanceGroups = ListInstanceGroups'
  { clusterId :: Types.ClusterId
    -- ^ The identifier of the cluster for which to list the instance groups.
  , marker :: Core.Maybe Types.Marker
    -- ^ The pagination token that indicates the next set of results to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstanceGroups' value with any optional fields omitted.
mkListInstanceGroups
    :: Types.ClusterId -- ^ 'clusterId'
    -> ListInstanceGroups
mkListInstanceGroups clusterId
  = ListInstanceGroups'{clusterId, marker = Core.Nothing}

-- | The identifier of the cluster for which to list the instance groups.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ligClusterId :: Lens.Lens' ListInstanceGroups Types.ClusterId
ligClusterId = Lens.field @"clusterId"
{-# INLINEABLE ligClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ligMarker :: Lens.Lens' ListInstanceGroups (Core.Maybe Types.Marker)
ligMarker = Lens.field @"marker"
{-# INLINEABLE ligMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery ListInstanceGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListInstanceGroups where
        toHeaders ListInstanceGroups{..}
          = Core.pure ("X-Amz-Target", "ElasticMapReduce.ListInstanceGroups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListInstanceGroups where
        toJSON ListInstanceGroups{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterId" Core..= clusterId),
                  ("Marker" Core..=) Core.<$> marker])

instance Core.AWSRequest ListInstanceGroups where
        type Rs ListInstanceGroups = ListInstanceGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListInstanceGroupsResponse' Core.<$>
                   (x Core..:? "InstanceGroups") Core.<*> x Core..:? "Marker" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListInstanceGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"instanceGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | This input determines which instance groups to retrieve.
--
-- /See:/ 'mkListInstanceGroupsResponse' smart constructor.
data ListInstanceGroupsResponse = ListInstanceGroupsResponse'
  { instanceGroups :: Core.Maybe [Types.InstanceGroup]
    -- ^ The list of instance groups for the cluster and given filters.
  , marker :: Core.Maybe Types.Marker
    -- ^ The pagination token that indicates the next set of results to retrieve.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListInstanceGroupsResponse' value with any optional fields omitted.
mkListInstanceGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListInstanceGroupsResponse
mkListInstanceGroupsResponse responseStatus
  = ListInstanceGroupsResponse'{instanceGroups = Core.Nothing,
                                marker = Core.Nothing, responseStatus}

-- | The list of instance groups for the cluster and given filters.
--
-- /Note:/ Consider using 'instanceGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ligrrsInstanceGroups :: Lens.Lens' ListInstanceGroupsResponse (Core.Maybe [Types.InstanceGroup])
ligrrsInstanceGroups = Lens.field @"instanceGroups"
{-# INLINEABLE ligrrsInstanceGroups #-}
{-# DEPRECATED instanceGroups "Use generic-lens or generic-optics with 'instanceGroups' instead"  #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ligrrsMarker :: Lens.Lens' ListInstanceGroupsResponse (Core.Maybe Types.Marker)
ligrrsMarker = Lens.field @"marker"
{-# INLINEABLE ligrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ligrrsResponseStatus :: Lens.Lens' ListInstanceGroupsResponse Core.Int
ligrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ligrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
