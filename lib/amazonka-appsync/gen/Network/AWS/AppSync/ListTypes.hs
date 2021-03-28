{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the types for a given API.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListTypes
    (
    -- * Creating a request
      ListTypes (..)
    , mkListTypes
    -- ** Request lenses
    , ltApiId
    , ltFormat
    , ltMaxResults
    , ltNextToken

    -- * Destructuring the response
    , ListTypesResponse (..)
    , mkListTypesResponse
    -- ** Response lenses
    , ltrrsNextToken
    , ltrrsTypes
    , ltrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTypes' smart constructor.
data ListTypes = ListTypes'
  { apiId :: Core.Text
    -- ^ The API ID.
  , format :: Types.TypeDefinitionFormat
    -- ^ The type format: SDL or JSON.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results you want the request to return.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTypes' value with any optional fields omitted.
mkListTypes
    :: Core.Text -- ^ 'apiId'
    -> Types.TypeDefinitionFormat -- ^ 'format'
    -> ListTypes
mkListTypes apiId format
  = ListTypes'{apiId, format, maxResults = Core.Nothing,
               nextToken = Core.Nothing}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltApiId :: Lens.Lens' ListTypes Core.Text
ltApiId = Lens.field @"apiId"
{-# INLINEABLE ltApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The type format: SDL or JSON.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltFormat :: Lens.Lens' ListTypes Types.TypeDefinitionFormat
ltFormat = Lens.field @"format"
{-# INLINEABLE ltFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTypes (Core.Maybe Core.Natural)
ltMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTypes (Core.Maybe Types.PaginationToken)
ltNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTypes where
        toQuery ListTypes{..}
          = Core.toQueryPair "format" format Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListTypes where
        toHeaders ListTypes{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListTypes where
        type Rs ListTypes = ListTypesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/types",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTypesResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "types" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTypes where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"types" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListTypesResponse' smart constructor.
data ListTypesResponse = ListTypesResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ An identifier to be passed in the next request to this operation to return the next set of items in the list.
  , types :: Core.Maybe [Types.Type]
    -- ^ The @Type@ objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTypesResponse' value with any optional fields omitted.
mkListTypesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTypesResponse
mkListTypesResponse responseStatus
  = ListTypesResponse'{nextToken = Core.Nothing,
                       types = Core.Nothing, responseStatus}

-- | An identifier to be passed in the next request to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListTypesResponse (Core.Maybe Types.PaginationToken)
ltrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The @Type@ objects.
--
-- /Note:/ Consider using 'types' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTypes :: Lens.Lens' ListTypesResponse (Core.Maybe [Types.Type])
ltrrsTypes = Lens.field @"types"
{-# INLINEABLE ltrrsTypes #-}
{-# DEPRECATED types "Use generic-lens or generic-optics with 'types' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTypesResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
