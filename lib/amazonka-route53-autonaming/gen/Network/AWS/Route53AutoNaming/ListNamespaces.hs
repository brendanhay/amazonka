{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.ListNamespaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists summary information about the namespaces that were created by the current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListNamespaces
    (
    -- * Creating a request
      ListNamespaces (..)
    , mkListNamespaces
    -- ** Request lenses
    , lnFilters
    , lnMaxResults
    , lnNextToken

    -- * Destructuring the response
    , ListNamespacesResponse (..)
    , mkListNamespacesResponse
    -- ** Response lenses
    , lnrrsNamespaces
    , lnrrsNextToken
    , lnrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkListNamespaces' smart constructor.
data ListNamespaces = ListNamespaces'
  { filters :: Core.Maybe [Types.NamespaceFilter]
    -- ^ A complex type that contains specifications for the namespaces that you want to list.
--
-- If you specify more than one filter, a namespace must match all filters to be returned by @ListNamespaces@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of namespaces that you want AWS Cloud Map to return in the response to a @ListNamespaces@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 namespaces.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ For the first @ListNamespaces@ request, omit this value.
--
-- If the response contains @NextToken@ , submit another @ListNamespaces@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListNamespaces' value with any optional fields omitted.
mkListNamespaces
    :: ListNamespaces
mkListNamespaces
  = ListNamespaces'{filters = Core.Nothing,
                    maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | A complex type that contains specifications for the namespaces that you want to list.
--
-- If you specify more than one filter, a namespace must match all filters to be returned by @ListNamespaces@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnFilters :: Lens.Lens' ListNamespaces (Core.Maybe [Types.NamespaceFilter])
lnFilters = Lens.field @"filters"
{-# INLINEABLE lnFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of namespaces that you want AWS Cloud Map to return in the response to a @ListNamespaces@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 namespaces.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnMaxResults :: Lens.Lens' ListNamespaces (Core.Maybe Core.Natural)
lnMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lnMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | For the first @ListNamespaces@ request, omit this value.
--
-- If the response contains @NextToken@ , submit another @ListNamespaces@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnNextToken :: Lens.Lens' ListNamespaces (Core.Maybe Types.NextToken)
lnNextToken = Lens.field @"nextToken"
{-# INLINEABLE lnNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListNamespaces where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListNamespaces where
        toHeaders ListNamespaces{..}
          = Core.pure
              ("X-Amz-Target", "Route53AutoNaming_v20170314.ListNamespaces")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListNamespaces where
        toJSON ListNamespaces{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListNamespaces where
        type Rs ListNamespaces = ListNamespacesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListNamespacesResponse' Core.<$>
                   (x Core..:? "Namespaces") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListNamespaces where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"namespaces" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListNamespacesResponse' smart constructor.
data ListNamespacesResponse = ListNamespacesResponse'
  { namespaces :: Core.Maybe [Types.NamespaceSummary]
    -- ^ An array that contains one @NamespaceSummary@ object for each namespace that matches the specified filter criteria.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the response contains @NextToken@ , submit another @ListNamespaces@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListNamespacesResponse' value with any optional fields omitted.
mkListNamespacesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListNamespacesResponse
mkListNamespacesResponse responseStatus
  = ListNamespacesResponse'{namespaces = Core.Nothing,
                            nextToken = Core.Nothing, responseStatus}

-- | An array that contains one @NamespaceSummary@ object for each namespace that matches the specified filter criteria.
--
-- /Note:/ Consider using 'namespaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnrrsNamespaces :: Lens.Lens' ListNamespacesResponse (Core.Maybe [Types.NamespaceSummary])
lnrrsNamespaces = Lens.field @"namespaces"
{-# INLINEABLE lnrrsNamespaces #-}
{-# DEPRECATED namespaces "Use generic-lens or generic-optics with 'namespaces' instead"  #-}

-- | If the response contains @NextToken@ , submit another @ListNamespaces@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnrrsNextToken :: Lens.Lens' ListNamespacesResponse (Core.Maybe Types.NextToken)
lnrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lnrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnrrsResponseStatus :: Lens.Lens' ListNamespacesResponse Core.Int
lnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
