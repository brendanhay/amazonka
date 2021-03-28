{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.ListRegexPatternSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'RegexPatternSetSummary' objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListRegexPatternSets
    (
    -- * Creating a request
      ListRegexPatternSets (..)
    , mkListRegexPatternSets
    -- ** Request lenses
    , lrpsLimit
    , lrpsNextMarker

    -- * Destructuring the response
    , ListRegexPatternSetsResponse (..)
    , mkListRegexPatternSetsResponse
    -- ** Response lenses
    , lrpsrrsNextMarker
    , lrpsrrsRegexPatternSets
    , lrpsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkListRegexPatternSets' smart constructor.
data ListRegexPatternSets = ListRegexPatternSets'
  { limit :: Core.Maybe Core.Natural
    -- ^ Specifies the number of @RegexPatternSet@ objects that you want AWS WAF to return for this request. If you have more @RegexPatternSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RegexPatternSet@ objects.
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you specify a value for @Limit@ and you have more @RegexPatternSet@ objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @RegexPatternSet@ objects. For the second and subsequent @ListRegexPatternSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RegexPatternSet@ objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRegexPatternSets' value with any optional fields omitted.
mkListRegexPatternSets
    :: ListRegexPatternSets
mkListRegexPatternSets
  = ListRegexPatternSets'{limit = Core.Nothing,
                          nextMarker = Core.Nothing}

-- | Specifies the number of @RegexPatternSet@ objects that you want AWS WAF to return for this request. If you have more @RegexPatternSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RegexPatternSet@ objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpsLimit :: Lens.Lens' ListRegexPatternSets (Core.Maybe Core.Natural)
lrpsLimit = Lens.field @"limit"
{-# INLINEABLE lrpsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | If you specify a value for @Limit@ and you have more @RegexPatternSet@ objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @RegexPatternSet@ objects. For the second and subsequent @ListRegexPatternSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RegexPatternSet@ objects.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpsNextMarker :: Lens.Lens' ListRegexPatternSets (Core.Maybe Types.NextMarker)
lrpsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lrpsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.ToQuery ListRegexPatternSets where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListRegexPatternSets where
        toHeaders ListRegexPatternSets{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_20150824.ListRegexPatternSets")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListRegexPatternSets where
        toJSON ListRegexPatternSets{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit,
                  ("NextMarker" Core..=) Core.<$> nextMarker])

instance Core.AWSRequest ListRegexPatternSets where
        type Rs ListRegexPatternSets = ListRegexPatternSetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListRegexPatternSetsResponse' Core.<$>
                   (x Core..:? "NextMarker") Core.<*> x Core..:? "RegexPatternSets"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListRegexPatternSets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"regexPatternSets" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextMarker" Lens..~
                   rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkListRegexPatternSetsResponse' smart constructor.
data ListRegexPatternSetsResponse = ListRegexPatternSetsResponse'
  { nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you have more @RegexPatternSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RegexPatternSet@ objects, submit another @ListRegexPatternSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
  , regexPatternSets :: Core.Maybe [Types.RegexPatternSetSummary]
    -- ^ An array of 'RegexPatternSetSummary' objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRegexPatternSetsResponse' value with any optional fields omitted.
mkListRegexPatternSetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListRegexPatternSetsResponse
mkListRegexPatternSetsResponse responseStatus
  = ListRegexPatternSetsResponse'{nextMarker = Core.Nothing,
                                  regexPatternSets = Core.Nothing, responseStatus}

-- | If you have more @RegexPatternSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RegexPatternSet@ objects, submit another @ListRegexPatternSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpsrrsNextMarker :: Lens.Lens' ListRegexPatternSetsResponse (Core.Maybe Types.NextMarker)
lrpsrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lrpsrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | An array of 'RegexPatternSetSummary' objects.
--
-- /Note:/ Consider using 'regexPatternSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpsrrsRegexPatternSets :: Lens.Lens' ListRegexPatternSetsResponse (Core.Maybe [Types.RegexPatternSetSummary])
lrpsrrsRegexPatternSets = Lens.field @"regexPatternSets"
{-# INLINEABLE lrpsrrsRegexPatternSets #-}
{-# DEPRECATED regexPatternSets "Use generic-lens or generic-optics with 'regexPatternSets' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpsrrsResponseStatus :: Lens.Lens' ListRegexPatternSetsResponse Core.Int
lrpsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrpsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
