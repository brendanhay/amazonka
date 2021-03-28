{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the tags associated with the specified AWS resource. Tags are key:value pairs that you can use to categorize and manage your resources, for purposes like billing. For example, you might set the tag key to "customer" and the value to the customer name or ID. You can specify one or more tags to add to each AWS resource, up to 50 tags for a resource.
--
-- Tagging is only available through the API, SDKs, and CLI. You can't manage or view tags through the AWS WAF Classic console. You can tag the AWS resources that you manage through AWS WAF Classic: web ACLs, rule groups, and rules. 
module Network.AWS.WAFRegional.ListTagsForResource
    (
    -- * Creating a request
      ListTagsForResource (..)
    , mkListTagsForResource
    -- ** Request lenses
    , ltfrResourceARN
    , ltfrLimit
    , ltfrNextMarker

    -- * Destructuring the response
    , ListTagsForResourceResponse (..)
    , mkListTagsForResourceResponse
    -- ** Response lenses
    , ltfrrrsNextMarker
    , ltfrrrsTagInfoForResource
    , ltfrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { resourceARN :: Types.ResourceArn
    -- ^ 
  , limit :: Core.Maybe Core.Natural
    -- ^ 
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResource' value with any optional fields omitted.
mkListTagsForResource
    :: Types.ResourceArn -- ^ 'resourceARN'
    -> ListTagsForResource
mkListTagsForResource resourceARN
  = ListTagsForResource'{resourceARN, limit = Core.Nothing,
                         nextMarker = Core.Nothing}

-- | 
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceARN :: Lens.Lens' ListTagsForResource Types.ResourceArn
ltfrResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE ltfrResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrLimit :: Lens.Lens' ListTagsForResource (Core.Maybe Core.Natural)
ltfrLimit = Lens.field @"limit"
{-# INLINEABLE ltfrLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrNextMarker :: Lens.Lens' ListTagsForResource (Core.Maybe Types.NextMarker)
ltfrNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE ltfrNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.ToQuery ListTagsForResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTagsForResource where
        toHeaders ListTagsForResource{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.ListTagsForResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTagsForResource where
        toJSON ListTagsForResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceARN" Core..= resourceARN),
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextMarker" Core..=) Core.<$> nextMarker])

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
                   (x Core..:? "NextMarker") Core.<*> x Core..:? "TagInfoForResource"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { nextMarker :: Core.Maybe Types.NextMarker
    -- ^ 
  , tagInfoForResource :: Core.Maybe Types.TagInfoForResource
    -- ^ 
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
  = ListTagsForResourceResponse'{nextMarker = Core.Nothing,
                                 tagInfoForResource = Core.Nothing, responseStatus}

-- | 
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsNextMarker :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Types.NextMarker)
ltfrrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE ltfrrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'tagInfoForResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsTagInfoForResource :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Types.TagInfoForResource)
ltfrrrsTagInfoForResource = Lens.field @"tagInfoForResource"
{-# INLINEABLE ltfrrrsTagInfoForResource #-}
{-# DEPRECATED tagInfoForResource "Use generic-lens or generic-optics with 'tagInfoForResource' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
ltfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
