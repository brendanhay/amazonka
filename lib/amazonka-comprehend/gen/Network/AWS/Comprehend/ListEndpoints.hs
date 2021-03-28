{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all existing endpoints that you've created.
module Network.AWS.Comprehend.ListEndpoints
    (
    -- * Creating a request
      ListEndpoints (..)
    , mkListEndpoints
    -- ** Request lenses
    , leFilter
    , leMaxResults
    , leNextToken

    -- * Destructuring the response
    , ListEndpointsResponse (..)
    , mkListEndpointsResponse
    -- ** Response lenses
    , lerrsEndpointPropertiesList
    , lerrsNextToken
    , lerrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListEndpoints' smart constructor.
data ListEndpoints = ListEndpoints'
  { filter :: Core.Maybe Types.EndpointFilter
    -- ^ Filters the endpoints that are returned. You can filter endpoints on their name, model, status, or the date and time that they were created. You can only set one filter at a time. 
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in each page. The default is 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Identifies the next page of results to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListEndpoints' value with any optional fields omitted.
mkListEndpoints
    :: ListEndpoints
mkListEndpoints
  = ListEndpoints'{filter = Core.Nothing, maxResults = Core.Nothing,
                   nextToken = Core.Nothing}

-- | Filters the endpoints that are returned. You can filter endpoints on their name, model, status, or the date and time that they were created. You can only set one filter at a time. 
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leFilter :: Lens.Lens' ListEndpoints (Core.Maybe Types.EndpointFilter)
leFilter = Lens.field @"filter"
{-# INLINEABLE leFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' ListEndpoints (Core.Maybe Core.Natural)
leMaxResults = Lens.field @"maxResults"
{-# INLINEABLE leMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListEndpoints (Core.Maybe Core.Text)
leNextToken = Lens.field @"nextToken"
{-# INLINEABLE leNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListEndpoints where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListEndpoints where
        toHeaders ListEndpoints{..}
          = Core.pure ("X-Amz-Target", "Comprehend_20171127.ListEndpoints")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListEndpoints where
        toJSON ListEndpoints{..}
          = Core.object
              (Core.catMaybes
                 [("Filter" Core..=) Core.<$> filter,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListEndpoints where
        type Rs ListEndpoints = ListEndpointsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListEndpointsResponse' Core.<$>
                   (x Core..:? "EndpointPropertiesList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListEndpointsResponse' smart constructor.
data ListEndpointsResponse = ListEndpointsResponse'
  { endpointPropertiesList :: Core.Maybe [Types.EndpointProperties]
    -- ^ Displays a list of endpoint properties being retrieved by the service in response to the request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Identifies the next page of results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListEndpointsResponse' value with any optional fields omitted.
mkListEndpointsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListEndpointsResponse
mkListEndpointsResponse responseStatus
  = ListEndpointsResponse'{endpointPropertiesList = Core.Nothing,
                           nextToken = Core.Nothing, responseStatus}

-- | Displays a list of endpoint properties being retrieved by the service in response to the request.
--
-- /Note:/ Consider using 'endpointPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsEndpointPropertiesList :: Lens.Lens' ListEndpointsResponse (Core.Maybe [Types.EndpointProperties])
lerrsEndpointPropertiesList = Lens.field @"endpointPropertiesList"
{-# INLINEABLE lerrsEndpointPropertiesList #-}
{-# DEPRECATED endpointPropertiesList "Use generic-lens or generic-optics with 'endpointPropertiesList' instead"  #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsNextToken :: Lens.Lens' ListEndpointsResponse (Core.Maybe Core.Text)
lerrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lerrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsResponseStatus :: Lens.Lens' ListEndpointsResponse Core.Int
lerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
