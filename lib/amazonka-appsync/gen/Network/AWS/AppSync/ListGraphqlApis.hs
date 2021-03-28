{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListGraphqlApis
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your GraphQL APIs.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListGraphqlApis
    (
    -- * Creating a request
      ListGraphqlApis (..)
    , mkListGraphqlApis
    -- ** Request lenses
    , lgaMaxResults
    , lgaNextToken

    -- * Destructuring the response
    , ListGraphqlApisResponse (..)
    , mkListGraphqlApisResponse
    -- ** Response lenses
    , lgarrsGraphqlApis
    , lgarrsNextToken
    , lgarrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListGraphqlApis' smart constructor.
data ListGraphqlApis = ListGraphqlApis'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results you want the request to return.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGraphqlApis' value with any optional fields omitted.
mkListGraphqlApis
    :: ListGraphqlApis
mkListGraphqlApis
  = ListGraphqlApis'{maxResults = Core.Nothing,
                     nextToken = Core.Nothing}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgaMaxResults :: Lens.Lens' ListGraphqlApis (Core.Maybe Core.Natural)
lgaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lgaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgaNextToken :: Lens.Lens' ListGraphqlApis (Core.Maybe Types.PaginationToken)
lgaNextToken = Lens.field @"nextToken"
{-# INLINEABLE lgaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListGraphqlApis where
        toQuery ListGraphqlApis{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListGraphqlApis where
        toHeaders ListGraphqlApis{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListGraphqlApis where
        type Rs ListGraphqlApis = ListGraphqlApisResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/v1/apis",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListGraphqlApisResponse' Core.<$>
                   (x Core..:? "graphqlApis") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListGraphqlApis where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"graphqlApis" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListGraphqlApisResponse' smart constructor.
data ListGraphqlApisResponse = ListGraphqlApisResponse'
  { graphqlApis :: Core.Maybe [Types.GraphqlApi]
    -- ^ The @GraphqlApi@ objects.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier to be passed in the next request to this operation to return the next set of items in the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGraphqlApisResponse' value with any optional fields omitted.
mkListGraphqlApisResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListGraphqlApisResponse
mkListGraphqlApisResponse responseStatus
  = ListGraphqlApisResponse'{graphqlApis = Core.Nothing,
                             nextToken = Core.Nothing, responseStatus}

-- | The @GraphqlApi@ objects.
--
-- /Note:/ Consider using 'graphqlApis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgarrsGraphqlApis :: Lens.Lens' ListGraphqlApisResponse (Core.Maybe [Types.GraphqlApi])
lgarrsGraphqlApis = Lens.field @"graphqlApis"
{-# INLINEABLE lgarrsGraphqlApis #-}
{-# DEPRECATED graphqlApis "Use generic-lens or generic-optics with 'graphqlApis' instead"  #-}

-- | An identifier to be passed in the next request to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgarrsNextToken :: Lens.Lens' ListGraphqlApisResponse (Core.Maybe Types.PaginationToken)
lgarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lgarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgarrsResponseStatus :: Lens.Lens' ListGraphqlApisResponse Core.Int
lgarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lgarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
