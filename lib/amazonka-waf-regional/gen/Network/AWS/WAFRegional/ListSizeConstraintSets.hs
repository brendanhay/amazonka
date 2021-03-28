{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListSizeConstraintSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'SizeConstraintSetSummary' objects.
module Network.AWS.WAFRegional.ListSizeConstraintSets
    (
    -- * Creating a request
      ListSizeConstraintSets (..)
    , mkListSizeConstraintSets
    -- ** Request lenses
    , lscsLimit
    , lscsNextMarker

    -- * Destructuring the response
    , ListSizeConstraintSetsResponse (..)
    , mkListSizeConstraintSetsResponse
    -- ** Response lenses
    , lscsrrsNextMarker
    , lscsrrsSizeConstraintSets
    , lscsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkListSizeConstraintSets' smart constructor.
data ListSizeConstraintSets = ListSizeConstraintSets'
  { limit :: Core.Maybe Core.Natural
    -- ^ Specifies the number of @SizeConstraintSet@ objects that you want AWS WAF to return for this request. If you have more @SizeConstraintSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @SizeConstraintSet@ objects.
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you specify a value for @Limit@ and you have more @SizeConstraintSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @SizeConstraintSets@ . For the second and subsequent @ListSizeConstraintSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @SizeConstraintSets@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSizeConstraintSets' value with any optional fields omitted.
mkListSizeConstraintSets
    :: ListSizeConstraintSets
mkListSizeConstraintSets
  = ListSizeConstraintSets'{limit = Core.Nothing,
                            nextMarker = Core.Nothing}

-- | Specifies the number of @SizeConstraintSet@ objects that you want AWS WAF to return for this request. If you have more @SizeConstraintSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @SizeConstraintSet@ objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsLimit :: Lens.Lens' ListSizeConstraintSets (Core.Maybe Core.Natural)
lscsLimit = Lens.field @"limit"
{-# INLINEABLE lscsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | If you specify a value for @Limit@ and you have more @SizeConstraintSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @SizeConstraintSets@ . For the second and subsequent @ListSizeConstraintSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @SizeConstraintSets@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsNextMarker :: Lens.Lens' ListSizeConstraintSets (Core.Maybe Types.NextMarker)
lscsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lscsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.ToQuery ListSizeConstraintSets where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListSizeConstraintSets where
        toHeaders ListSizeConstraintSets{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.ListSizeConstraintSets")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListSizeConstraintSets where
        toJSON ListSizeConstraintSets{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit,
                  ("NextMarker" Core..=) Core.<$> nextMarker])

instance Core.AWSRequest ListSizeConstraintSets where
        type Rs ListSizeConstraintSets = ListSizeConstraintSetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSizeConstraintSetsResponse' Core.<$>
                   (x Core..:? "NextMarker") Core.<*> x Core..:? "SizeConstraintSets"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListSizeConstraintSetsResponse' smart constructor.
data ListSizeConstraintSetsResponse = ListSizeConstraintSetsResponse'
  { nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you have more @SizeConstraintSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @SizeConstraintSet@ objects, submit another @ListSizeConstraintSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
  , sizeConstraintSets :: Core.Maybe [Types.SizeConstraintSetSummary]
    -- ^ An array of 'SizeConstraintSetSummary' objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSizeConstraintSetsResponse' value with any optional fields omitted.
mkListSizeConstraintSetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSizeConstraintSetsResponse
mkListSizeConstraintSetsResponse responseStatus
  = ListSizeConstraintSetsResponse'{nextMarker = Core.Nothing,
                                    sizeConstraintSets = Core.Nothing, responseStatus}

-- | If you have more @SizeConstraintSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @SizeConstraintSet@ objects, submit another @ListSizeConstraintSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsrrsNextMarker :: Lens.Lens' ListSizeConstraintSetsResponse (Core.Maybe Types.NextMarker)
lscsrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lscsrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | An array of 'SizeConstraintSetSummary' objects.
--
-- /Note:/ Consider using 'sizeConstraintSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsrrsSizeConstraintSets :: Lens.Lens' ListSizeConstraintSetsResponse (Core.Maybe [Types.SizeConstraintSetSummary])
lscsrrsSizeConstraintSets = Lens.field @"sizeConstraintSets"
{-# INLINEABLE lscsrrsSizeConstraintSets #-}
{-# DEPRECATED sizeConstraintSets "Use generic-lens or generic-optics with 'sizeConstraintSets' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsrrsResponseStatus :: Lens.Lens' ListSizeConstraintSetsResponse Core.Int
lscsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lscsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
