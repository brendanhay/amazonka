{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetKeyPairs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all key pairs in the user's account.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetKeyPairs
    (
    -- * Creating a request
      GetKeyPairs (..)
    , mkGetKeyPairs
    -- ** Request lenses
    , gkpPageToken

    -- * Destructuring the response
    , GetKeyPairsResponse (..)
    , mkGetKeyPairsResponse
    -- ** Response lenses
    , gkprrsKeyPairs
    , gkprrsNextPageToken
    , gkprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetKeyPairs' smart constructor.
newtype GetKeyPairs = GetKeyPairs'
  { pageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetKeyPairs@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetKeyPairs' value with any optional fields omitted.
mkGetKeyPairs
    :: GetKeyPairs
mkGetKeyPairs = GetKeyPairs'{pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetKeyPairs@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkpPageToken :: Lens.Lens' GetKeyPairs (Core.Maybe Core.Text)
gkpPageToken = Lens.field @"pageToken"
{-# INLINEABLE gkpPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery GetKeyPairs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetKeyPairs where
        toHeaders GetKeyPairs{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetKeyPairs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetKeyPairs where
        toJSON GetKeyPairs{..}
          = Core.object
              (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetKeyPairs where
        type Rs GetKeyPairs = GetKeyPairsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetKeyPairsResponse' Core.<$>
                   (x Core..:? "keyPairs") Core.<*> x Core..:? "nextPageToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetKeyPairs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"keyPairs" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkGetKeyPairsResponse' smart constructor.
data GetKeyPairsResponse = GetKeyPairsResponse'
  { keyPairs :: Core.Maybe [Types.KeyPair]
    -- ^ An array of key-value pairs containing information about the key pairs.
  , nextPageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetKeyPairs@ request and specify the next page token using the @pageToken@ parameter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetKeyPairsResponse' value with any optional fields omitted.
mkGetKeyPairsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetKeyPairsResponse
mkGetKeyPairsResponse responseStatus
  = GetKeyPairsResponse'{keyPairs = Core.Nothing,
                         nextPageToken = Core.Nothing, responseStatus}

-- | An array of key-value pairs containing information about the key pairs.
--
-- /Note:/ Consider using 'keyPairs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkprrsKeyPairs :: Lens.Lens' GetKeyPairsResponse (Core.Maybe [Types.KeyPair])
gkprrsKeyPairs = Lens.field @"keyPairs"
{-# INLINEABLE gkprrsKeyPairs #-}
{-# DEPRECATED keyPairs "Use generic-lens or generic-optics with 'keyPairs' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetKeyPairs@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkprrsNextPageToken :: Lens.Lens' GetKeyPairsResponse (Core.Maybe Core.Text)
gkprrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gkprrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkprrsResponseStatus :: Lens.Lens' GetKeyPairsResponse Core.Int
gkprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gkprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
