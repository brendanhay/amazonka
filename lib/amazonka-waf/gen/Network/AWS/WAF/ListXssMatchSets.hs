{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.ListXssMatchSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'XssMatchSet' objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListXssMatchSets
    (
    -- * Creating a request
      ListXssMatchSets (..)
    , mkListXssMatchSets
    -- ** Request lenses
    , lxmsLimit
    , lxmsNextMarker

    -- * Destructuring the response
    , ListXssMatchSetsResponse (..)
    , mkListXssMatchSetsResponse
    -- ** Response lenses
    , lxmsrrsNextMarker
    , lxmsrrsXssMatchSets
    , lxmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | A request to list the 'XssMatchSet' objects created by the current AWS account.
--
-- /See:/ 'mkListXssMatchSets' smart constructor.
data ListXssMatchSets = ListXssMatchSets'
  { limit :: Core.Maybe Core.Natural
    -- ^ Specifies the number of 'XssMatchSet' objects that you want AWS WAF to return for this request. If you have more @XssMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you specify a value for @Limit@ and you have more 'XssMatchSet' objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @XssMatchSets@ . For the second and subsequent @ListXssMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @XssMatchSets@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListXssMatchSets' value with any optional fields omitted.
mkListXssMatchSets
    :: ListXssMatchSets
mkListXssMatchSets
  = ListXssMatchSets'{limit = Core.Nothing,
                      nextMarker = Core.Nothing}

-- | Specifies the number of 'XssMatchSet' objects that you want AWS WAF to return for this request. If you have more @XssMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lxmsLimit :: Lens.Lens' ListXssMatchSets (Core.Maybe Core.Natural)
lxmsLimit = Lens.field @"limit"
{-# INLINEABLE lxmsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | If you specify a value for @Limit@ and you have more 'XssMatchSet' objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @XssMatchSets@ . For the second and subsequent @ListXssMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @XssMatchSets@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lxmsNextMarker :: Lens.Lens' ListXssMatchSets (Core.Maybe Types.NextMarker)
lxmsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lxmsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.ToQuery ListXssMatchSets where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListXssMatchSets where
        toHeaders ListXssMatchSets{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.ListXssMatchSets")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListXssMatchSets where
        toJSON ListXssMatchSets{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit,
                  ("NextMarker" Core..=) Core.<$> nextMarker])

instance Core.AWSRequest ListXssMatchSets where
        type Rs ListXssMatchSets = ListXssMatchSetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListXssMatchSetsResponse' Core.<$>
                   (x Core..:? "NextMarker") Core.<*> x Core..:? "XssMatchSets"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListXssMatchSets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"xssMatchSets" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextMarker" Lens..~
                   rs Lens.^. Lens.field @"nextMarker")

-- | The response to a 'ListXssMatchSets' request.
--
-- /See:/ 'mkListXssMatchSetsResponse' smart constructor.
data ListXssMatchSetsResponse = ListXssMatchSetsResponse'
  { nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you have more 'XssMatchSet' objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @XssMatchSet@ objects, submit another @ListXssMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
  , xssMatchSets :: Core.Maybe [Types.XssMatchSetSummary]
    -- ^ An array of 'XssMatchSetSummary' objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListXssMatchSetsResponse' value with any optional fields omitted.
mkListXssMatchSetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListXssMatchSetsResponse
mkListXssMatchSetsResponse responseStatus
  = ListXssMatchSetsResponse'{nextMarker = Core.Nothing,
                              xssMatchSets = Core.Nothing, responseStatus}

-- | If you have more 'XssMatchSet' objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @XssMatchSet@ objects, submit another @ListXssMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lxmsrrsNextMarker :: Lens.Lens' ListXssMatchSetsResponse (Core.Maybe Types.NextMarker)
lxmsrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lxmsrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | An array of 'XssMatchSetSummary' objects.
--
-- /Note:/ Consider using 'xssMatchSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lxmsrrsXssMatchSets :: Lens.Lens' ListXssMatchSetsResponse (Core.Maybe [Types.XssMatchSetSummary])
lxmsrrsXssMatchSets = Lens.field @"xssMatchSets"
{-# INLINEABLE lxmsrrsXssMatchSets #-}
{-# DEPRECATED xssMatchSets "Use generic-lens or generic-optics with 'xssMatchSets' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lxmsrrsResponseStatus :: Lens.Lens' ListXssMatchSetsResponse Core.Int
lxmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lxmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
