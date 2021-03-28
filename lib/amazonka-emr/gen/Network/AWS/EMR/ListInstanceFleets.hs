{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListInstanceFleets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available details about the instance fleets in a cluster.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListInstanceFleets
    (
    -- * Creating a request
      ListInstanceFleets (..)
    , mkListInstanceFleets
    -- ** Request lenses
    , lifClusterId
    , lifMarker

    -- * Destructuring the response
    , ListInstanceFleetsResponse (..)
    , mkListInstanceFleetsResponse
    -- ** Response lenses
    , lifrrsInstanceFleets
    , lifrrsMarker
    , lifrrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListInstanceFleets' smart constructor.
data ListInstanceFleets = ListInstanceFleets'
  { clusterId :: Types.ClusterId
    -- ^ The unique identifier of the cluster.
  , marker :: Core.Maybe Types.Marker
    -- ^ The pagination token that indicates the next set of results to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstanceFleets' value with any optional fields omitted.
mkListInstanceFleets
    :: Types.ClusterId -- ^ 'clusterId'
    -> ListInstanceFleets
mkListInstanceFleets clusterId
  = ListInstanceFleets'{clusterId, marker = Core.Nothing}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifClusterId :: Lens.Lens' ListInstanceFleets Types.ClusterId
lifClusterId = Lens.field @"clusterId"
{-# INLINEABLE lifClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifMarker :: Lens.Lens' ListInstanceFleets (Core.Maybe Types.Marker)
lifMarker = Lens.field @"marker"
{-# INLINEABLE lifMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery ListInstanceFleets where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListInstanceFleets where
        toHeaders ListInstanceFleets{..}
          = Core.pure ("X-Amz-Target", "ElasticMapReduce.ListInstanceFleets")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListInstanceFleets where
        toJSON ListInstanceFleets{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterId" Core..= clusterId),
                  ("Marker" Core..=) Core.<$> marker])

instance Core.AWSRequest ListInstanceFleets where
        type Rs ListInstanceFleets = ListInstanceFleetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListInstanceFleetsResponse' Core.<$>
                   (x Core..:? "InstanceFleets") Core.<*> x Core..:? "Marker" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListInstanceFleets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"instanceFleets" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkListInstanceFleetsResponse' smart constructor.
data ListInstanceFleetsResponse = ListInstanceFleetsResponse'
  { instanceFleets :: Core.Maybe [Types.InstanceFleet]
    -- ^ The list of instance fleets for the cluster and given filters.
  , marker :: Core.Maybe Types.Marker
    -- ^ The pagination token that indicates the next set of results to retrieve.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListInstanceFleetsResponse' value with any optional fields omitted.
mkListInstanceFleetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListInstanceFleetsResponse
mkListInstanceFleetsResponse responseStatus
  = ListInstanceFleetsResponse'{instanceFleets = Core.Nothing,
                                marker = Core.Nothing, responseStatus}

-- | The list of instance fleets for the cluster and given filters.
--
-- /Note:/ Consider using 'instanceFleets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifrrsInstanceFleets :: Lens.Lens' ListInstanceFleetsResponse (Core.Maybe [Types.InstanceFleet])
lifrrsInstanceFleets = Lens.field @"instanceFleets"
{-# INLINEABLE lifrrsInstanceFleets #-}
{-# DEPRECATED instanceFleets "Use generic-lens or generic-optics with 'instanceFleets' instead"  #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifrrsMarker :: Lens.Lens' ListInstanceFleetsResponse (Core.Maybe Types.Marker)
lifrrsMarker = Lens.field @"marker"
{-# INLINEABLE lifrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifrrsResponseStatus :: Lens.Lens' ListInstanceFleetsResponse Core.Int
lifrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lifrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
