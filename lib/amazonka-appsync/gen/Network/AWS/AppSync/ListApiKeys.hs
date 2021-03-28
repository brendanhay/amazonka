{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListApiKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the API keys for a given API.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListApiKeys
    (
    -- * Creating a request
      ListApiKeys (..)
    , mkListApiKeys
    -- ** Request lenses
    , lakApiId
    , lakMaxResults
    , lakNextToken

    -- * Destructuring the response
    , ListApiKeysResponse (..)
    , mkListApiKeysResponse
    -- ** Response lenses
    , lakrrsApiKeys
    , lakrrsNextToken
    , lakrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListApiKeys' smart constructor.
data ListApiKeys = ListApiKeys'
  { apiId :: Core.Text
    -- ^ The API ID.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results you want the request to return.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApiKeys' value with any optional fields omitted.
mkListApiKeys
    :: Core.Text -- ^ 'apiId'
    -> ListApiKeys
mkListApiKeys apiId
  = ListApiKeys'{apiId, maxResults = Core.Nothing,
                 nextToken = Core.Nothing}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakApiId :: Lens.Lens' ListApiKeys Core.Text
lakApiId = Lens.field @"apiId"
{-# INLINEABLE lakApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakMaxResults :: Lens.Lens' ListApiKeys (Core.Maybe Core.Natural)
lakMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lakMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakNextToken :: Lens.Lens' ListApiKeys (Core.Maybe Types.PaginationToken)
lakNextToken = Lens.field @"nextToken"
{-# INLINEABLE lakNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListApiKeys where
        toQuery ListApiKeys{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListApiKeys where
        toHeaders ListApiKeys{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListApiKeys where
        type Rs ListApiKeys = ListApiKeysResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/apikeys",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListApiKeysResponse' Core.<$>
                   (x Core..:? "apiKeys") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListApiKeys where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"apiKeys" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListApiKeysResponse' smart constructor.
data ListApiKeysResponse = ListApiKeysResponse'
  { apiKeys :: Core.Maybe [Types.ApiKey]
    -- ^ The @ApiKey@ objects.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier to be passed in the next request to this operation to return the next set of items in the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApiKeysResponse' value with any optional fields omitted.
mkListApiKeysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListApiKeysResponse
mkListApiKeysResponse responseStatus
  = ListApiKeysResponse'{apiKeys = Core.Nothing,
                         nextToken = Core.Nothing, responseStatus}

-- | The @ApiKey@ objects.
--
-- /Note:/ Consider using 'apiKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakrrsApiKeys :: Lens.Lens' ListApiKeysResponse (Core.Maybe [Types.ApiKey])
lakrrsApiKeys = Lens.field @"apiKeys"
{-# INLINEABLE lakrrsApiKeys #-}
{-# DEPRECATED apiKeys "Use generic-lens or generic-optics with 'apiKeys' instead"  #-}

-- | An identifier to be passed in the next request to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakrrsNextToken :: Lens.Lens' ListApiKeysResponse (Core.Maybe Types.PaginationToken)
lakrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lakrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakrrsResponseStatus :: Lens.Lens' ListApiKeysResponse Core.Int
lakrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lakrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
