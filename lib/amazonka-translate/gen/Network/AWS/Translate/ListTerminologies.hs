{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.ListTerminologies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of custom terminologies associated with your account.
--
-- This operation returns paginated results.
module Network.AWS.Translate.ListTerminologies
    (
    -- * Creating a request
      ListTerminologies (..)
    , mkListTerminologies
    -- ** Request lenses
    , ltMaxResults
    , ltNextToken

    -- * Destructuring the response
    , ListTerminologiesResponse (..)
    , mkListTerminologiesResponse
    -- ** Response lenses
    , ltrrsNextToken
    , ltrrsTerminologyPropertiesList
    , ltrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Translate.Types as Types

-- | /See:/ 'mkListTerminologies' smart constructor.
data ListTerminologies = ListTerminologies'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of custom terminologies returned per list request.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the result of the request to ListTerminologies was truncated, include the NextToken to fetch the next group of custom terminologies. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTerminologies' value with any optional fields omitted.
mkListTerminologies
    :: ListTerminologies
mkListTerminologies
  = ListTerminologies'{maxResults = Core.Nothing,
                       nextToken = Core.Nothing}

-- | The maximum number of custom terminologies returned per list request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTerminologies (Core.Maybe Core.Natural)
ltMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the result of the request to ListTerminologies was truncated, include the NextToken to fetch the next group of custom terminologies. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTerminologies (Core.Maybe Types.NextToken)
ltNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTerminologies where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTerminologies where
        toHeaders ListTerminologies{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSShineFrontendService_20170701.ListTerminologies")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTerminologies where
        toJSON ListTerminologies{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListTerminologies where
        type Rs ListTerminologies = ListTerminologiesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTerminologiesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "TerminologyPropertiesList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTerminologies where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"terminologyPropertiesList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListTerminologiesResponse' smart constructor.
data ListTerminologiesResponse = ListTerminologiesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If the response to the ListTerminologies was truncated, the NextToken fetches the next group of custom terminologies.
  , terminologyPropertiesList :: Core.Maybe [Types.TerminologyProperties]
    -- ^ The properties list of the custom terminologies returned on the list request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListTerminologiesResponse' value with any optional fields omitted.
mkListTerminologiesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTerminologiesResponse
mkListTerminologiesResponse responseStatus
  = ListTerminologiesResponse'{nextToken = Core.Nothing,
                               terminologyPropertiesList = Core.Nothing, responseStatus}

-- | If the response to the ListTerminologies was truncated, the NextToken fetches the next group of custom terminologies.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListTerminologiesResponse (Core.Maybe Types.NextToken)
ltrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The properties list of the custom terminologies returned on the list request.
--
-- /Note:/ Consider using 'terminologyPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTerminologyPropertiesList :: Lens.Lens' ListTerminologiesResponse (Core.Maybe [Types.TerminologyProperties])
ltrrsTerminologyPropertiesList = Lens.field @"terminologyPropertiesList"
{-# INLINEABLE ltrrsTerminologyPropertiesList #-}
{-# DEPRECATED terminologyPropertiesList "Use generic-lens or generic-optics with 'terminologyPropertiesList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTerminologiesResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
