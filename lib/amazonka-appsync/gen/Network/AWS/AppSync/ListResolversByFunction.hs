{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListResolversByFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the resolvers that are associated with a specific function.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListResolversByFunction
    (
    -- * Creating a request
      ListResolversByFunction (..)
    , mkListResolversByFunction
    -- ** Request lenses
    , lrbfApiId
    , lrbfFunctionId
    , lrbfMaxResults
    , lrbfNextToken

    -- * Destructuring the response
    , ListResolversByFunctionResponse (..)
    , mkListResolversByFunctionResponse
    -- ** Response lenses
    , lrbfrrsNextToken
    , lrbfrrsResolvers
    , lrbfrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListResolversByFunction' smart constructor.
data ListResolversByFunction = ListResolversByFunction'
  { apiId :: Core.Text
    -- ^ The API ID.
  , functionId :: Core.Text
    -- ^ The Function ID.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results you want the request to return.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which you can use to return the next set of items in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResolversByFunction' value with any optional fields omitted.
mkListResolversByFunction
    :: Core.Text -- ^ 'apiId'
    -> Core.Text -- ^ 'functionId'
    -> ListResolversByFunction
mkListResolversByFunction apiId functionId
  = ListResolversByFunction'{apiId, functionId,
                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfApiId :: Lens.Lens' ListResolversByFunction Core.Text
lrbfApiId = Lens.field @"apiId"
{-# INLINEABLE lrbfApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The Function ID.
--
-- /Note:/ Consider using 'functionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfFunctionId :: Lens.Lens' ListResolversByFunction Core.Text
lrbfFunctionId = Lens.field @"functionId"
{-# INLINEABLE lrbfFunctionId #-}
{-# DEPRECATED functionId "Use generic-lens or generic-optics with 'functionId' instead"  #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfMaxResults :: Lens.Lens' ListResolversByFunction (Core.Maybe Core.Natural)
lrbfMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lrbfMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which you can use to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfNextToken :: Lens.Lens' ListResolversByFunction (Core.Maybe Types.PaginationToken)
lrbfNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrbfNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListResolversByFunction where
        toQuery ListResolversByFunction{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListResolversByFunction where
        toHeaders ListResolversByFunction{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListResolversByFunction where
        type Rs ListResolversByFunction = ListResolversByFunctionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/functions/" Core.<>
                             Core.toText functionId
                             Core.<> "/resolvers",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListResolversByFunctionResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "resolvers" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListResolversByFunction where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"resolvers" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListResolversByFunctionResponse' smart constructor.
data ListResolversByFunctionResponse = ListResolversByFunctionResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that can be used to return the next set of items in the list.
  , resolvers :: Core.Maybe [Types.Resolver]
    -- ^ The list of resolvers.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResolversByFunctionResponse' value with any optional fields omitted.
mkListResolversByFunctionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListResolversByFunctionResponse
mkListResolversByFunctionResponse responseStatus
  = ListResolversByFunctionResponse'{nextToken = Core.Nothing,
                                     resolvers = Core.Nothing, responseStatus}

-- | An identifier that can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfrrsNextToken :: Lens.Lens' ListResolversByFunctionResponse (Core.Maybe Types.PaginationToken)
lrbfrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrbfrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of resolvers.
--
-- /Note:/ Consider using 'resolvers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfrrsResolvers :: Lens.Lens' ListResolversByFunctionResponse (Core.Maybe [Types.Resolver])
lrbfrrsResolvers = Lens.field @"resolvers"
{-# INLINEABLE lrbfrrsResolvers #-}
{-# DEPRECATED resolvers "Use generic-lens or generic-optics with 'resolvers' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfrrsResponseStatus :: Lens.Lens' ListResolversByFunctionResponse Core.Int
lrbfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrbfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
