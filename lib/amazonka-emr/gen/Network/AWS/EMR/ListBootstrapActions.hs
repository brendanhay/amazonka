{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListBootstrapActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the bootstrap actions associated with a cluster.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListBootstrapActions
    (
    -- * Creating a request
      ListBootstrapActions (..)
    , mkListBootstrapActions
    -- ** Request lenses
    , lbaClusterId
    , lbaMarker

    -- * Destructuring the response
    , ListBootstrapActionsResponse (..)
    , mkListBootstrapActionsResponse
    -- ** Response lenses
    , lbarrsBootstrapActions
    , lbarrsMarker
    , lbarrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines which bootstrap actions to retrieve.
--
-- /See:/ 'mkListBootstrapActions' smart constructor.
data ListBootstrapActions = ListBootstrapActions'
  { clusterId :: Types.ClusterId
    -- ^ The cluster identifier for the bootstrap actions to list.
  , marker :: Core.Maybe Types.Marker
    -- ^ The pagination token that indicates the next set of results to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBootstrapActions' value with any optional fields omitted.
mkListBootstrapActions
    :: Types.ClusterId -- ^ 'clusterId'
    -> ListBootstrapActions
mkListBootstrapActions clusterId
  = ListBootstrapActions'{clusterId, marker = Core.Nothing}

-- | The cluster identifier for the bootstrap actions to list.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaClusterId :: Lens.Lens' ListBootstrapActions Types.ClusterId
lbaClusterId = Lens.field @"clusterId"
{-# INLINEABLE lbaClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaMarker :: Lens.Lens' ListBootstrapActions (Core.Maybe Types.Marker)
lbaMarker = Lens.field @"marker"
{-# INLINEABLE lbaMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery ListBootstrapActions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListBootstrapActions where
        toHeaders ListBootstrapActions{..}
          = Core.pure
              ("X-Amz-Target", "ElasticMapReduce.ListBootstrapActions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListBootstrapActions where
        toJSON ListBootstrapActions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterId" Core..= clusterId),
                  ("Marker" Core..=) Core.<$> marker])

instance Core.AWSRequest ListBootstrapActions where
        type Rs ListBootstrapActions = ListBootstrapActionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListBootstrapActionsResponse' Core.<$>
                   (x Core..:? "BootstrapActions") Core.<*> x Core..:? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListBootstrapActions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"bootstrapActions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | This output contains the bootstrap actions detail.
--
-- /See:/ 'mkListBootstrapActionsResponse' smart constructor.
data ListBootstrapActionsResponse = ListBootstrapActionsResponse'
  { bootstrapActions :: Core.Maybe [Types.Command]
    -- ^ The bootstrap actions associated with the cluster.
  , marker :: Core.Maybe Types.Marker
    -- ^ The pagination token that indicates the next set of results to retrieve.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBootstrapActionsResponse' value with any optional fields omitted.
mkListBootstrapActionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListBootstrapActionsResponse
mkListBootstrapActionsResponse responseStatus
  = ListBootstrapActionsResponse'{bootstrapActions = Core.Nothing,
                                  marker = Core.Nothing, responseStatus}

-- | The bootstrap actions associated with the cluster.
--
-- /Note:/ Consider using 'bootstrapActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbarrsBootstrapActions :: Lens.Lens' ListBootstrapActionsResponse (Core.Maybe [Types.Command])
lbarrsBootstrapActions = Lens.field @"bootstrapActions"
{-# INLINEABLE lbarrsBootstrapActions #-}
{-# DEPRECATED bootstrapActions "Use generic-lens or generic-optics with 'bootstrapActions' instead"  #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbarrsMarker :: Lens.Lens' ListBootstrapActionsResponse (Core.Maybe Types.Marker)
lbarrsMarker = Lens.field @"marker"
{-# INLINEABLE lbarrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbarrsResponseStatus :: Lens.Lens' ListBootstrapActionsResponse Core.Int
lbarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lbarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
