{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all customer master keys (CMKs) in the caller's AWS account and Region.
--
-- This operation returns paginated results.
module Network.AWS.KMS.ListKeys
    (
    -- * Creating a request
      ListKeys (..)
    , mkListKeys
    -- ** Request lenses
    , lkLimit
    , lkMarker

    -- * Destructuring the response
    , ListKeysResponse (..)
    , mkListKeysResponse
    -- ** Response lenses
    , lkrrsKeys
    , lkrrsNextMarker
    , lkrrsTruncated
    , lkrrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListKeys' smart constructor.
data ListKeys = ListKeys'
  { limit :: Core.Maybe Core.Natural
    -- ^ Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 1000, inclusive. If you do not include a value, it defaults to 100.
  , marker :: Core.Maybe Types.Marker
    -- ^ Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListKeys' value with any optional fields omitted.
mkListKeys
    :: ListKeys
mkListKeys = ListKeys'{limit = Core.Nothing, marker = Core.Nothing}

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 1000, inclusive. If you do not include a value, it defaults to 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkLimit :: Lens.Lens' ListKeys (Core.Maybe Core.Natural)
lkLimit = Lens.field @"limit"
{-# INLINEABLE lkLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkMarker :: Lens.Lens' ListKeys (Core.Maybe Types.Marker)
lkMarker = Lens.field @"marker"
{-# INLINEABLE lkMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery ListKeys where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListKeys where
        toHeaders ListKeys{..}
          = Core.pure ("X-Amz-Target", "TrentService.ListKeys") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListKeys where
        toJSON ListKeys{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit,
                  ("Marker" Core..=) Core.<$> marker])

instance Core.AWSRequest ListKeys where
        type Rs ListKeys = ListKeysResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListKeysResponse' Core.<$>
                   (x Core..:? "Keys") Core.<*> x Core..:? "NextMarker" Core.<*>
                     x Core..:? "Truncated"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListKeys where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"truncated") = Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"nextMarker") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkListKeysResponse' smart constructor.
data ListKeysResponse = ListKeysResponse'
  { keys :: Core.Maybe [Types.KeyListEntry]
    -- ^ A list of customer master keys (CMKs).
  , nextMarker :: Core.Maybe Types.MarkerType
    -- ^ When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
  , truncated :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListKeysResponse' value with any optional fields omitted.
mkListKeysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListKeysResponse
mkListKeysResponse responseStatus
  = ListKeysResponse'{keys = Core.Nothing, nextMarker = Core.Nothing,
                      truncated = Core.Nothing, responseStatus}

-- | A list of customer master keys (CMKs).
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkrrsKeys :: Lens.Lens' ListKeysResponse (Core.Maybe [Types.KeyListEntry])
lkrrsKeys = Lens.field @"keys"
{-# INLINEABLE lkrrsKeys #-}
{-# DEPRECATED keys "Use generic-lens or generic-optics with 'keys' instead"  #-}

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkrrsNextMarker :: Lens.Lens' ListKeysResponse (Core.Maybe Types.MarkerType)
lkrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lkrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkrrsTruncated :: Lens.Lens' ListKeysResponse (Core.Maybe Core.Bool)
lkrrsTruncated = Lens.field @"truncated"
{-# INLINEABLE lkrrsTruncated #-}
{-# DEPRECATED truncated "Use generic-lens or generic-optics with 'truncated' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkrrsResponseStatus :: Lens.Lens' ListKeysResponse Core.Int
lkrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lkrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
