{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that have been added to the specified resource. This operation is supported in storage gateways of all types.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListTagsForResource
    (
    -- * Creating a request
      ListTagsForResource (..)
    , mkListTagsForResource
    -- ** Request lenses
    , ltfrResourceARN
    , ltfrLimit
    , ltfrMarker

    -- * Destructuring the response
    , ListTagsForResourceResponse (..)
    , mkListTagsForResourceResponse
    -- ** Response lenses
    , ltfrrrsMarker
    , ltfrrrsResourceARN
    , ltfrrrsTags
    , ltfrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | ListTagsForResourceInput
--
-- /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { resourceARN :: Types.ResourceARN
    -- ^ The Amazon Resource Name (ARN) of the resource for which you want to list tags.
  , limit :: Core.Maybe Core.Natural
    -- ^ Specifies that the list of tags returned be limited to the specified number of items.
  , marker :: Core.Maybe Types.Marker
    -- ^ An opaque string that indicates the position at which to begin returning the list of tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResource' value with any optional fields omitted.
mkListTagsForResource
    :: Types.ResourceARN -- ^ 'resourceARN'
    -> ListTagsForResource
mkListTagsForResource resourceARN
  = ListTagsForResource'{resourceARN, limit = Core.Nothing,
                         marker = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the resource for which you want to list tags.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceARN :: Lens.Lens' ListTagsForResource Types.ResourceARN
ltfrResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE ltfrResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | Specifies that the list of tags returned be limited to the specified number of items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrLimit :: Lens.Lens' ListTagsForResource (Core.Maybe Core.Natural)
ltfrLimit = Lens.field @"limit"
{-# INLINEABLE ltfrLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | An opaque string that indicates the position at which to begin returning the list of tags.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrMarker :: Lens.Lens' ListTagsForResource (Core.Maybe Types.Marker)
ltfrMarker = Lens.field @"marker"
{-# INLINEABLE ltfrMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery ListTagsForResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTagsForResource where
        toHeaders ListTagsForResource{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.ListTagsForResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTagsForResource where
        toJSON ListTagsForResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceARN" Core..= resourceARN),
                  ("Limit" Core..=) Core.<$> limit,
                  ("Marker" Core..=) Core.<$> marker])

instance Core.AWSRequest ListTagsForResource where
        type Rs ListTagsForResource = ListTagsForResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTagsForResourceResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "ResourceARN" Core.<*>
                     x Core..:? "Tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTagsForResource where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"tags" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | ListTagsForResourceOutput
--
-- /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { marker :: Core.Maybe Types.Marker
    -- ^ An opaque string that indicates the position at which to stop returning the list of tags.
  , resourceARN :: Core.Maybe Types.ResourceARN
    -- ^ The Amazon Resource Name (ARN) of the resource for which you want to list tags.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ An array that contains the tags for the specified resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResourceResponse' value with any optional fields omitted.
mkListTagsForResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagsForResourceResponse
mkListTagsForResourceResponse responseStatus
  = ListTagsForResourceResponse'{marker = Core.Nothing,
                                 resourceARN = Core.Nothing, tags = Core.Nothing, responseStatus}

-- | An opaque string that indicates the position at which to stop returning the list of tags.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsMarker :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Types.Marker)
ltfrrrsMarker = Lens.field @"marker"
{-# INLINEABLE ltfrrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The Amazon Resource Name (ARN) of the resource for which you want to list tags.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsResourceARN :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Types.ResourceARN)
ltfrrrsResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE ltfrrrsResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | An array that contains the tags for the specified resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsTags :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe [Types.Tag])
ltfrrrsTags = Lens.field @"tags"
{-# INLINEABLE ltfrrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
ltfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
