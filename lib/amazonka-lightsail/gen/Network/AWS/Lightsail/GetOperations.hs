{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all operations.
--
-- Results are returned from oldest to newest, up to a maximum of 200. Results can be paged by making each subsequent call to @GetOperations@ use the maximum (last) @statusChangedAt@ value from the previous request.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetOperations
    (
    -- * Creating a request
      GetOperations (..)
    , mkGetOperations
    -- ** Request lenses
    , goPageToken

    -- * Destructuring the response
    , GetOperationsResponse (..)
    , mkGetOperationsResponse
    -- ** Response lenses
    , gorrsNextPageToken
    , gorrsOperations
    , gorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetOperations' smart constructor.
newtype GetOperations = GetOperations'
  { pageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetOperations@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetOperations' value with any optional fields omitted.
mkGetOperations
    :: GetOperations
mkGetOperations = GetOperations'{pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetOperations@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goPageToken :: Lens.Lens' GetOperations (Core.Maybe Core.Text)
goPageToken = Lens.field @"pageToken"
{-# INLINEABLE goPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery GetOperations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetOperations where
        toHeaders GetOperations{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetOperations")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetOperations where
        toJSON GetOperations{..}
          = Core.object
              (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetOperations where
        type Rs GetOperations = GetOperationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetOperationsResponse' Core.<$>
                   (x Core..:? "nextPageToken") Core.<*> x Core..:? "operations"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetOperations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"operations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkGetOperationsResponse' smart constructor.
data GetOperationsResponse = GetOperationsResponse'
  { nextPageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetOperations@ request and specify the next page token using the @pageToken@ parameter.
  , operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetOperationsResponse' value with any optional fields omitted.
mkGetOperationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetOperationsResponse
mkGetOperationsResponse responseStatus
  = GetOperationsResponse'{nextPageToken = Core.Nothing,
                           operations = Core.Nothing, responseStatus}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetOperations@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsNextPageToken :: Lens.Lens' GetOperationsResponse (Core.Maybe Core.Text)
gorrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gorrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsOperations :: Lens.Lens' GetOperationsResponse (Core.Maybe [Types.Operation])
gorrsOperations = Lens.field @"operations"
{-# INLINEABLE gorrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsResponseStatus :: Lens.Lens' GetOperationsResponse Core.Int
gorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
