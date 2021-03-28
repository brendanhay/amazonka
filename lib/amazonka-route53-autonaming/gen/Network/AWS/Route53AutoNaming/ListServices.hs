{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.ListServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists summary information for all the services that are associated with one or more specified namespaces.
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListServices
    (
    -- * Creating a request
      ListServices (..)
    , mkListServices
    -- ** Request lenses
    , lsFilters
    , lsMaxResults
    , lsNextToken

    -- * Destructuring the response
    , ListServicesResponse (..)
    , mkListServicesResponse
    -- ** Response lenses
    , lsrrsNextToken
    , lsrrsServices
    , lsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkListServices' smart constructor.
data ListServices = ListServices'
  { filters :: Core.Maybe [Types.ServiceFilter]
    -- ^ A complex type that contains specifications for the namespaces that you want to list services for. 
--
-- If you specify more than one filter, an operation must match all filters to be returned by @ListServices@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of services that you want AWS Cloud Map to return in the response to a @ListServices@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 services.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ For the first @ListServices@ request, omit this value.
--
-- If the response contains @NextToken@ , submit another @ListServices@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListServices' value with any optional fields omitted.
mkListServices
    :: ListServices
mkListServices
  = ListServices'{filters = Core.Nothing, maxResults = Core.Nothing,
                  nextToken = Core.Nothing}

-- | A complex type that contains specifications for the namespaces that you want to list services for. 
--
-- If you specify more than one filter, an operation must match all filters to be returned by @ListServices@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsFilters :: Lens.Lens' ListServices (Core.Maybe [Types.ServiceFilter])
lsFilters = Lens.field @"filters"
{-# INLINEABLE lsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of services that you want AWS Cloud Map to return in the response to a @ListServices@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 services.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMaxResults :: Lens.Lens' ListServices (Core.Maybe Core.Natural)
lsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | For the first @ListServices@ request, omit this value.
--
-- If the response contains @NextToken@ , submit another @ListServices@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListServices (Core.Maybe Types.NextToken)
lsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListServices where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListServices where
        toHeaders ListServices{..}
          = Core.pure
              ("X-Amz-Target", "Route53AutoNaming_v20170314.ListServices")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListServices where
        toJSON ListServices{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListServices where
        type Rs ListServices = ListServicesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListServicesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Services" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListServices where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"services" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListServicesResponse' smart constructor.
data ListServicesResponse = ListServicesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If the response contains @NextToken@ , submit another @ListServices@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
  , services :: Core.Maybe [Types.ServiceSummary]
    -- ^ An array that contains one @ServiceSummary@ object for each service that matches the specified filter criteria.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListServicesResponse' value with any optional fields omitted.
mkListServicesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListServicesResponse
mkListServicesResponse responseStatus
  = ListServicesResponse'{nextToken = Core.Nothing,
                          services = Core.Nothing, responseStatus}

-- | If the response contains @NextToken@ , submit another @ListServices@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsNextToken :: Lens.Lens' ListServicesResponse (Core.Maybe Types.NextToken)
lsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array that contains one @ServiceSummary@ object for each service that matches the specified filter criteria.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsServices :: Lens.Lens' ListServicesResponse (Core.Maybe [Types.ServiceSummary])
lsrrsServices = Lens.field @"services"
{-# INLINEABLE lsrrsServices #-}
{-# DEPRECATED services "Use generic-lens or generic-optics with 'services' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsResponseStatus :: Lens.Lens' ListServicesResponse Core.Int
lsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
