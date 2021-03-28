{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetOperationsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets operations for a specific resource (e.g., an instance or a static IP).
module Network.AWS.Lightsail.GetOperationsForResource
    (
    -- * Creating a request
      GetOperationsForResource (..)
    , mkGetOperationsForResource
    -- ** Request lenses
    , gofrResourceName
    , gofrPageToken

    -- * Destructuring the response
    , GetOperationsForResourceResponse (..)
    , mkGetOperationsForResourceResponse
    -- ** Response lenses
    , gofrrrsNextPageCount
    , gofrrrsNextPageToken
    , gofrrrsOperations
    , gofrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetOperationsForResource' smart constructor.
data GetOperationsForResource = GetOperationsForResource'
  { resourceName :: Types.ResourceName
    -- ^ The name of the resource for which you are requesting information.
  , pageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetOperationsForResource@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetOperationsForResource' value with any optional fields omitted.
mkGetOperationsForResource
    :: Types.ResourceName -- ^ 'resourceName'
    -> GetOperationsForResource
mkGetOperationsForResource resourceName
  = GetOperationsForResource'{resourceName, pageToken = Core.Nothing}

-- | The name of the resource for which you are requesting information.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gofrResourceName :: Lens.Lens' GetOperationsForResource Types.ResourceName
gofrResourceName = Lens.field @"resourceName"
{-# INLINEABLE gofrResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetOperationsForResource@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gofrPageToken :: Lens.Lens' GetOperationsForResource (Core.Maybe Core.Text)
gofrPageToken = Lens.field @"pageToken"
{-# INLINEABLE gofrPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery GetOperationsForResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetOperationsForResource where
        toHeaders GetOperationsForResource{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.GetOperationsForResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetOperationsForResource where
        toJSON GetOperationsForResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("resourceName" Core..= resourceName),
                  ("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetOperationsForResource where
        type Rs GetOperationsForResource = GetOperationsForResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetOperationsForResourceResponse' Core.<$>
                   (x Core..:? "nextPageCount") Core.<*> x Core..:? "nextPageToken"
                     Core.<*> x Core..:? "operations"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetOperationsForResourceResponse' smart constructor.
data GetOperationsForResourceResponse = GetOperationsForResourceResponse'
  { nextPageCount :: Core.Maybe Core.Text
    -- ^ (Deprecated) Returns the number of pages of results that remain.
  , nextPageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetOperationsForResource@ request and specify the next page token using the @pageToken@ parameter.
  , operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetOperationsForResourceResponse' value with any optional fields omitted.
mkGetOperationsForResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetOperationsForResourceResponse
mkGetOperationsForResourceResponse responseStatus
  = GetOperationsForResourceResponse'{nextPageCount = Core.Nothing,
                                      nextPageToken = Core.Nothing, operations = Core.Nothing,
                                      responseStatus}

-- | (Deprecated) Returns the number of pages of results that remain.
--
-- /Note:/ Consider using 'nextPageCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gofrrrsNextPageCount :: Lens.Lens' GetOperationsForResourceResponse (Core.Maybe Core.Text)
gofrrrsNextPageCount = Lens.field @"nextPageCount"
{-# INLINEABLE gofrrrsNextPageCount #-}
{-# DEPRECATED nextPageCount "Use generic-lens or generic-optics with 'nextPageCount' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetOperationsForResource@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gofrrrsNextPageToken :: Lens.Lens' GetOperationsForResourceResponse (Core.Maybe Core.Text)
gofrrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gofrrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gofrrrsOperations :: Lens.Lens' GetOperationsForResourceResponse (Core.Maybe [Types.Operation])
gofrrrsOperations = Lens.field @"operations"
{-# INLINEABLE gofrrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gofrrrsResponseStatus :: Lens.Lens' GetOperationsForResourceResponse Core.Int
gofrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gofrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
