{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetActiveNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the names of all active (not deleted) resources.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetActiveNames
    (
    -- * Creating a request
      GetActiveNames (..)
    , mkGetActiveNames
    -- ** Request lenses
    , ganPageToken

    -- * Destructuring the response
    , GetActiveNamesResponse (..)
    , mkGetActiveNamesResponse
    -- ** Response lenses
    , ganrrsActiveNames
    , ganrrsNextPageToken
    , ganrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetActiveNames' smart constructor.
newtype GetActiveNames = GetActiveNames'
  { pageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetActiveNames@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetActiveNames' value with any optional fields omitted.
mkGetActiveNames
    :: GetActiveNames
mkGetActiveNames = GetActiveNames'{pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetActiveNames@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ganPageToken :: Lens.Lens' GetActiveNames (Core.Maybe Core.Text)
ganPageToken = Lens.field @"pageToken"
{-# INLINEABLE ganPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery GetActiveNames where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetActiveNames where
        toHeaders GetActiveNames{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetActiveNames")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetActiveNames where
        toJSON GetActiveNames{..}
          = Core.object
              (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetActiveNames where
        type Rs GetActiveNames = GetActiveNamesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetActiveNamesResponse' Core.<$>
                   (x Core..:? "activeNames") Core.<*> x Core..:? "nextPageToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetActiveNames where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"activeNames" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkGetActiveNamesResponse' smart constructor.
data GetActiveNamesResponse = GetActiveNamesResponse'
  { activeNames :: Core.Maybe [Core.Text]
    -- ^ The list of active names returned by the get active names request.
  , nextPageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetActiveNames@ request and specify the next page token using the @pageToken@ parameter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetActiveNamesResponse' value with any optional fields omitted.
mkGetActiveNamesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetActiveNamesResponse
mkGetActiveNamesResponse responseStatus
  = GetActiveNamesResponse'{activeNames = Core.Nothing,
                            nextPageToken = Core.Nothing, responseStatus}

-- | The list of active names returned by the get active names request.
--
-- /Note:/ Consider using 'activeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ganrrsActiveNames :: Lens.Lens' GetActiveNamesResponse (Core.Maybe [Core.Text])
ganrrsActiveNames = Lens.field @"activeNames"
{-# INLINEABLE ganrrsActiveNames #-}
{-# DEPRECATED activeNames "Use generic-lens or generic-optics with 'activeNames' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetActiveNames@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ganrrsNextPageToken :: Lens.Lens' GetActiveNamesResponse (Core.Maybe Core.Text)
ganrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE ganrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ganrrsResponseStatus :: Lens.Lens' GetActiveNamesResponse Core.Int
ganrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ganrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
