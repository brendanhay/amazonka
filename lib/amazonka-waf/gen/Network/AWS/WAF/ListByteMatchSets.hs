{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.ListByteMatchSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'ByteMatchSetSummary' objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListByteMatchSets
    (
    -- * Creating a request
      ListByteMatchSets (..)
    , mkListByteMatchSets
    -- ** Request lenses
    , lbmsLimit
    , lbmsNextMarker

    -- * Destructuring the response
    , ListByteMatchSetsResponse (..)
    , mkListByteMatchSetsResponse
    -- ** Response lenses
    , lbmsrrsByteMatchSets
    , lbmsrrsNextMarker
    , lbmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkListByteMatchSets' smart constructor.
data ListByteMatchSets = ListByteMatchSets'
  { limit :: Core.Maybe Core.Natural
    -- ^ Specifies the number of @ByteMatchSet@ objects that you want AWS WAF to return for this request. If you have more @ByteMatchSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @ByteMatchSet@ objects.
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you specify a value for @Limit@ and you have more @ByteMatchSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ByteMatchSets@ . For the second and subsequent @ListByteMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ByteMatchSets@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListByteMatchSets' value with any optional fields omitted.
mkListByteMatchSets
    :: ListByteMatchSets
mkListByteMatchSets
  = ListByteMatchSets'{limit = Core.Nothing,
                       nextMarker = Core.Nothing}

-- | Specifies the number of @ByteMatchSet@ objects that you want AWS WAF to return for this request. If you have more @ByteMatchSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @ByteMatchSet@ objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmsLimit :: Lens.Lens' ListByteMatchSets (Core.Maybe Core.Natural)
lbmsLimit = Lens.field @"limit"
{-# INLINEABLE lbmsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | If you specify a value for @Limit@ and you have more @ByteMatchSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ByteMatchSets@ . For the second and subsequent @ListByteMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ByteMatchSets@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmsNextMarker :: Lens.Lens' ListByteMatchSets (Core.Maybe Types.NextMarker)
lbmsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lbmsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.ToQuery ListByteMatchSets where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListByteMatchSets where
        toHeaders ListByteMatchSets{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.ListByteMatchSets")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListByteMatchSets where
        toJSON ListByteMatchSets{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit,
                  ("NextMarker" Core..=) Core.<$> nextMarker])

instance Core.AWSRequest ListByteMatchSets where
        type Rs ListByteMatchSets = ListByteMatchSetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListByteMatchSetsResponse' Core.<$>
                   (x Core..:? "ByteMatchSets") Core.<*> x Core..:? "NextMarker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListByteMatchSets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"byteMatchSets" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextMarker" Lens..~
                   rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkListByteMatchSetsResponse' smart constructor.
data ListByteMatchSetsResponse = ListByteMatchSetsResponse'
  { byteMatchSets :: Core.Maybe [Types.ByteMatchSetSummary]
    -- ^ An array of 'ByteMatchSetSummary' objects.
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you have more @ByteMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @ByteMatchSet@ objects, submit another @ListByteMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListByteMatchSetsResponse' value with any optional fields omitted.
mkListByteMatchSetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListByteMatchSetsResponse
mkListByteMatchSetsResponse responseStatus
  = ListByteMatchSetsResponse'{byteMatchSets = Core.Nothing,
                               nextMarker = Core.Nothing, responseStatus}

-- | An array of 'ByteMatchSetSummary' objects.
--
-- /Note:/ Consider using 'byteMatchSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmsrrsByteMatchSets :: Lens.Lens' ListByteMatchSetsResponse (Core.Maybe [Types.ByteMatchSetSummary])
lbmsrrsByteMatchSets = Lens.field @"byteMatchSets"
{-# INLINEABLE lbmsrrsByteMatchSets #-}
{-# DEPRECATED byteMatchSets "Use generic-lens or generic-optics with 'byteMatchSets' instead"  #-}

-- | If you have more @ByteMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @ByteMatchSet@ objects, submit another @ListByteMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmsrrsNextMarker :: Lens.Lens' ListByteMatchSetsResponse (Core.Maybe Types.NextMarker)
lbmsrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lbmsrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbmsrrsResponseStatus :: Lens.Lens' ListByteMatchSetsResponse Core.Int
lbmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lbmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
