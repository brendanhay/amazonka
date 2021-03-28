{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListWebACLs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'WebACLSummary' objects in the response.
module Network.AWS.WAFRegional.ListWebACLs
    (
    -- * Creating a request
      ListWebACLs (..)
    , mkListWebACLs
    -- ** Request lenses
    , lwaclLimit
    , lwaclNextMarker

    -- * Destructuring the response
    , ListWebACLsResponse (..)
    , mkListWebACLsResponse
    -- ** Response lenses
    , lwaclrrsNextMarker
    , lwaclrrsWebACLs
    , lwaclrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkListWebACLs' smart constructor.
data ListWebACLs = ListWebACLs'
  { limit :: Core.Maybe Core.Natural
    -- ^ Specifies the number of @WebACL@ objects that you want AWS WAF to return for this request. If you have more @WebACL@ objects than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @WebACL@ objects.
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you specify a value for @Limit@ and you have more @WebACL@ objects than the number that you specify for @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @WebACL@ objects. For the second and subsequent @ListWebACLs@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @WebACL@ objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListWebACLs' value with any optional fields omitted.
mkListWebACLs
    :: ListWebACLs
mkListWebACLs
  = ListWebACLs'{limit = Core.Nothing, nextMarker = Core.Nothing}

-- | Specifies the number of @WebACL@ objects that you want AWS WAF to return for this request. If you have more @WebACL@ objects than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @WebACL@ objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwaclLimit :: Lens.Lens' ListWebACLs (Core.Maybe Core.Natural)
lwaclLimit = Lens.field @"limit"
{-# INLINEABLE lwaclLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | If you specify a value for @Limit@ and you have more @WebACL@ objects than the number that you specify for @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @WebACL@ objects. For the second and subsequent @ListWebACLs@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @WebACL@ objects.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwaclNextMarker :: Lens.Lens' ListWebACLs (Core.Maybe Types.NextMarker)
lwaclNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lwaclNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.ToQuery ListWebACLs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListWebACLs where
        toHeaders ListWebACLs{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.ListWebACLs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListWebACLs where
        toJSON ListWebACLs{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit,
                  ("NextMarker" Core..=) Core.<$> nextMarker])

instance Core.AWSRequest ListWebACLs where
        type Rs ListWebACLs = ListWebACLsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListWebACLsResponse' Core.<$>
                   (x Core..:? "NextMarker") Core.<*> x Core..:? "WebACLs" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListWebACLsResponse' smart constructor.
data ListWebACLsResponse = ListWebACLsResponse'
  { nextMarker :: Core.Maybe Types.NextMarker
    -- ^ If you have more @WebACL@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @WebACL@ objects, submit another @ListWebACLs@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
  , webACLs :: Core.Maybe [Types.WebACLSummary]
    -- ^ An array of 'WebACLSummary' objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListWebACLsResponse' value with any optional fields omitted.
mkListWebACLsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListWebACLsResponse
mkListWebACLsResponse responseStatus
  = ListWebACLsResponse'{nextMarker = Core.Nothing,
                         webACLs = Core.Nothing, responseStatus}

-- | If you have more @WebACL@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @WebACL@ objects, submit another @ListWebACLs@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwaclrrsNextMarker :: Lens.Lens' ListWebACLsResponse (Core.Maybe Types.NextMarker)
lwaclrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lwaclrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | An array of 'WebACLSummary' objects.
--
-- /Note:/ Consider using 'webACLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwaclrrsWebACLs :: Lens.Lens' ListWebACLsResponse (Core.Maybe [Types.WebACLSummary])
lwaclrrsWebACLs = Lens.field @"webACLs"
{-# INLINEABLE lwaclrrsWebACLs #-}
{-# DEPRECATED webACLs "Use generic-lens or generic-optics with 'webACLs' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwaclrrsResponseStatus :: Lens.Lens' ListWebACLsResponse Core.Int
lwaclrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lwaclrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
