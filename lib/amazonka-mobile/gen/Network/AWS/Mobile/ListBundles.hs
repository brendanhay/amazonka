{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.ListBundles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all available bundles. 
--
-- This operation returns paginated results.
module Network.AWS.Mobile.ListBundles
    (
    -- * Creating a request
      ListBundles (..)
    , mkListBundles
    -- ** Request lenses
    , lbMaxResults
    , lbNextToken

    -- * Destructuring the response
    , ListBundlesResponse (..)
    , mkListBundlesResponse
    -- ** Response lenses
    , lbrrsBundleList
    , lbrrsNextToken
    , lbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Mobile.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure to request all available bundles. 
--
-- /See:/ 'mkListBundles' smart constructor.
data ListBundles = ListBundles'
  { maxResults :: Core.Maybe Core.Int
    -- ^ Maximum number of records to list in a single response. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Pagination token. Set to null to start listing bundles from start. If non-null pagination token is returned in a result, then pass its value in here in another request to list more bundles. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBundles' value with any optional fields omitted.
mkListBundles
    :: ListBundles
mkListBundles
  = ListBundles'{maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Maximum number of records to list in a single response. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbMaxResults :: Lens.Lens' ListBundles (Core.Maybe Core.Int)
lbMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lbMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Pagination token. Set to null to start listing bundles from start. If non-null pagination token is returned in a result, then pass its value in here in another request to list more bundles. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbNextToken :: Lens.Lens' ListBundles (Core.Maybe Types.NextToken)
lbNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListBundles where
        toQuery ListBundles{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListBundles where
        toHeaders ListBundles{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListBundles where
        type Rs ListBundles = ListBundlesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/bundles",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListBundlesResponse' Core.<$>
                   (x Core..:? "bundleList") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListBundles where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"bundleList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Result structure contains a list of all available bundles with details. 
--
-- /See:/ 'mkListBundlesResponse' smart constructor.
data ListBundlesResponse = ListBundlesResponse'
  { bundleList :: Core.Maybe [Types.BundleDetails]
    -- ^ A list of bundles. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Pagination token. If non-null pagination token is returned in a result, then pass its value in another request to fetch more entries. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBundlesResponse' value with any optional fields omitted.
mkListBundlesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListBundlesResponse
mkListBundlesResponse responseStatus
  = ListBundlesResponse'{bundleList = Core.Nothing,
                         nextToken = Core.Nothing, responseStatus}

-- | A list of bundles. 
--
-- /Note:/ Consider using 'bundleList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsBundleList :: Lens.Lens' ListBundlesResponse (Core.Maybe [Types.BundleDetails])
lbrrsBundleList = Lens.field @"bundleList"
{-# INLINEABLE lbrrsBundleList #-}
{-# DEPRECATED bundleList "Use generic-lens or generic-optics with 'bundleList' instead"  #-}

-- | Pagination token. If non-null pagination token is returned in a result, then pass its value in another request to fetch more entries. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsNextToken :: Lens.Lens' ListBundlesResponse (Core.Maybe Types.NextToken)
lbrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsResponseStatus :: Lens.Lens' ListBundlesResponse Core.Int
lbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
