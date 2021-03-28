{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListFunctions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List multiple functions.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListFunctions
    (
    -- * Creating a request
      ListFunctions (..)
    , mkListFunctions
    -- ** Request lenses
    , lfApiId
    , lfMaxResults
    , lfNextToken

    -- * Destructuring the response
    , ListFunctionsResponse (..)
    , mkListFunctionsResponse
    -- ** Response lenses
    , lfrrsFunctions
    , lfrrsNextToken
    , lfrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFunctions' smart constructor.
data ListFunctions = ListFunctions'
  { apiId :: Core.Text
    -- ^ The GraphQL API ID.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results you want the request to return.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctions' value with any optional fields omitted.
mkListFunctions
    :: Core.Text -- ^ 'apiId'
    -> ListFunctions
mkListFunctions apiId
  = ListFunctions'{apiId, maxResults = Core.Nothing,
                   nextToken = Core.Nothing}

-- | The GraphQL API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfApiId :: Lens.Lens' ListFunctions Core.Text
lfApiId = Lens.field @"apiId"
{-# INLINEABLE lfApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMaxResults :: Lens.Lens' ListFunctions (Core.Maybe Core.Natural)
lfMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lfMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfNextToken :: Lens.Lens' ListFunctions (Core.Maybe Types.PaginationToken)
lfNextToken = Lens.field @"nextToken"
{-# INLINEABLE lfNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListFunctions where
        toQuery ListFunctions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListFunctions where
        toHeaders ListFunctions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListFunctions where
        type Rs ListFunctions = ListFunctionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/functions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListFunctionsResponse' Core.<$>
                   (x Core..:? "functions") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListFunctions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"functions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListFunctionsResponse' smart constructor.
data ListFunctionsResponse = ListFunctionsResponse'
  { functions :: Core.Maybe [Types.FunctionConfiguration]
    -- ^ A list of @Function@ objects.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctionsResponse' value with any optional fields omitted.
mkListFunctionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListFunctionsResponse
mkListFunctionsResponse responseStatus
  = ListFunctionsResponse'{functions = Core.Nothing,
                           nextToken = Core.Nothing, responseStatus}

-- | A list of @Function@ objects.
--
-- /Note:/ Consider using 'functions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsFunctions :: Lens.Lens' ListFunctionsResponse (Core.Maybe [Types.FunctionConfiguration])
lfrrsFunctions = Lens.field @"functions"
{-# INLINEABLE lfrrsFunctions #-}
{-# DEPRECATED functions "Use generic-lens or generic-optics with 'functions' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsNextToken :: Lens.Lens' ListFunctionsResponse (Core.Maybe Types.PaginationToken)
lfrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lfrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsResponseStatus :: Lens.Lens' ListFunctionsResponse Core.Int
lfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
