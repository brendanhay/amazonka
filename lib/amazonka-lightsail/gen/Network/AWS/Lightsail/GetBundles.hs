{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetBundles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of bundles that are available for purchase. A bundle describes the specs for your virtual private server (or /instance/ ).
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetBundles
    (
    -- * Creating a request
      GetBundles (..)
    , mkGetBundles
    -- ** Request lenses
    , gbsIncludeInactive
    , gbsPageToken

    -- * Destructuring the response
    , GetBundlesResponse (..)
    , mkGetBundlesResponse
    -- ** Response lenses
    , gbrfrsBundles
    , gbrfrsNextPageToken
    , gbrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBundles' smart constructor.
data GetBundles = GetBundles'
  { includeInactive :: Core.Maybe Core.Bool
    -- ^ A Boolean value that indicates whether to include inactive bundle results in your request.
  , pageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBundles@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBundles' value with any optional fields omitted.
mkGetBundles
    :: GetBundles
mkGetBundles
  = GetBundles'{includeInactive = Core.Nothing,
                pageToken = Core.Nothing}

-- | A Boolean value that indicates whether to include inactive bundle results in your request.
--
-- /Note:/ Consider using 'includeInactive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbsIncludeInactive :: Lens.Lens' GetBundles (Core.Maybe Core.Bool)
gbsIncludeInactive = Lens.field @"includeInactive"
{-# INLINEABLE gbsIncludeInactive #-}
{-# DEPRECATED includeInactive "Use generic-lens or generic-optics with 'includeInactive' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBundles@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbsPageToken :: Lens.Lens' GetBundles (Core.Maybe Core.Text)
gbsPageToken = Lens.field @"pageToken"
{-# INLINEABLE gbsPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery GetBundles where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetBundles where
        toHeaders GetBundles{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetBundles")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetBundles where
        toJSON GetBundles{..}
          = Core.object
              (Core.catMaybes
                 [("includeInactive" Core..=) Core.<$> includeInactive,
                  ("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetBundles where
        type Rs GetBundles = GetBundlesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetBundlesResponse' Core.<$>
                   (x Core..:? "bundles") Core.<*> x Core..:? "nextPageToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetBundles where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"bundles" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkGetBundlesResponse' smart constructor.
data GetBundlesResponse = GetBundlesResponse'
  { bundles :: Core.Maybe [Types.Bundle]
    -- ^ An array of key-value pairs that contains information about the available bundles.
  , nextPageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetBundles@ request and specify the next page token using the @pageToken@ parameter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBundlesResponse' value with any optional fields omitted.
mkGetBundlesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBundlesResponse
mkGetBundlesResponse responseStatus
  = GetBundlesResponse'{bundles = Core.Nothing,
                        nextPageToken = Core.Nothing, responseStatus}

-- | An array of key-value pairs that contains information about the available bundles.
--
-- /Note:/ Consider using 'bundles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrfrsBundles :: Lens.Lens' GetBundlesResponse (Core.Maybe [Types.Bundle])
gbrfrsBundles = Lens.field @"bundles"
{-# INLINEABLE gbrfrsBundles #-}
{-# DEPRECATED bundles "Use generic-lens or generic-optics with 'bundles' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetBundles@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrfrsNextPageToken :: Lens.Lens' GetBundlesResponse (Core.Maybe Core.Text)
gbrfrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gbrfrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrfrsResponseStatus :: Lens.Lens' GetBundlesResponse Core.Int
gbrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
