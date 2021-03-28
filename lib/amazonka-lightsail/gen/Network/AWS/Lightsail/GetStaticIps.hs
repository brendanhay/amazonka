{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetStaticIps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all static IPs in the user's account.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetStaticIps
    (
    -- * Creating a request
      GetStaticIps (..)
    , mkGetStaticIps
    -- ** Request lenses
    , gsiPageToken

    -- * Destructuring the response
    , GetStaticIpsResponse (..)
    , mkGetStaticIpsResponse
    -- ** Response lenses
    , gsirrsNextPageToken
    , gsirrsStaticIps
    , gsirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetStaticIps' smart constructor.
newtype GetStaticIps = GetStaticIps'
  { pageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetStaticIps@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetStaticIps' value with any optional fields omitted.
mkGetStaticIps
    :: GetStaticIps
mkGetStaticIps = GetStaticIps'{pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetStaticIps@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiPageToken :: Lens.Lens' GetStaticIps (Core.Maybe Core.Text)
gsiPageToken = Lens.field @"pageToken"
{-# INLINEABLE gsiPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery GetStaticIps where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetStaticIps where
        toHeaders GetStaticIps{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetStaticIps")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetStaticIps where
        toJSON GetStaticIps{..}
          = Core.object
              (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetStaticIps where
        type Rs GetStaticIps = GetStaticIpsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetStaticIpsResponse' Core.<$>
                   (x Core..:? "nextPageToken") Core.<*> x Core..:? "staticIps"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetStaticIps where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"staticIps" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkGetStaticIpsResponse' smart constructor.
data GetStaticIpsResponse = GetStaticIpsResponse'
  { nextPageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetStaticIps@ request and specify the next page token using the @pageToken@ parameter.
  , staticIps :: Core.Maybe [Types.StaticIp]
    -- ^ An array of key-value pairs containing information about your get static IPs request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetStaticIpsResponse' value with any optional fields omitted.
mkGetStaticIpsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetStaticIpsResponse
mkGetStaticIpsResponse responseStatus
  = GetStaticIpsResponse'{nextPageToken = Core.Nothing,
                          staticIps = Core.Nothing, responseStatus}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetStaticIps@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsirrsNextPageToken :: Lens.Lens' GetStaticIpsResponse (Core.Maybe Core.Text)
gsirrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gsirrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | An array of key-value pairs containing information about your get static IPs request.
--
-- /Note:/ Consider using 'staticIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsirrsStaticIps :: Lens.Lens' GetStaticIpsResponse (Core.Maybe [Types.StaticIp])
gsirrsStaticIps = Lens.field @"staticIps"
{-# INLINEABLE gsirrsStaticIps #-}
{-# DEPRECATED staticIps "Use generic-lens or generic-optics with 'staticIps' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsirrsResponseStatus :: Lens.Lens' GetStaticIpsResponse Core.Int
gsirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
