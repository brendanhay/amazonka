{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the domains.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListDomains
    (
    -- * Creating a request
      ListDomains (..)
    , mkListDomains
    -- ** Request lenses
    , ldMaxResults
    , ldNextToken

    -- * Destructuring the response
    , ListDomainsResponse (..)
    , mkListDomainsResponse
    -- ** Response lenses
    , ldrrsDomains
    , ldrrsNextToken
    , ldrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListDomains' smart constructor.
data ListDomains = ListDomains'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ Returns a list up to a specified limit.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDomains' value with any optional fields omitted.
mkListDomains
    :: ListDomains
mkListDomains
  = ListDomains'{maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Returns a list up to a specified limit.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxResults :: Lens.Lens' ListDomains (Core.Maybe Core.Natural)
ldMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ldMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDomains (Core.Maybe Types.NextToken)
ldNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListDomains where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListDomains where
        toHeaders ListDomains{..}
          = Core.pure ("X-Amz-Target", "SageMaker.ListDomains") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListDomains where
        toJSON ListDomains{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListDomains where
        type Rs ListDomains = ListDomainsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDomainsResponse' Core.<$>
                   (x Core..:? "Domains") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListDomains where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"domains" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { domains :: Core.Maybe [Types.DomainDetails]
    -- ^ The list of domains.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDomainsResponse' value with any optional fields omitted.
mkListDomainsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDomainsResponse
mkListDomainsResponse responseStatus
  = ListDomainsResponse'{domains = Core.Nothing,
                         nextToken = Core.Nothing, responseStatus}

-- | The list of domains.
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDomains :: Lens.Lens' ListDomainsResponse (Core.Maybe [Types.DomainDetails])
ldrrsDomains = Lens.field @"domains"
{-# INLINEABLE ldrrsDomains #-}
{-# DEPRECATED domains "Use generic-lens or generic-optics with 'domains' instead"  #-}

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsNextToken :: Lens.Lens' ListDomainsResponse (Core.Maybe Types.NextToken)
ldrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDomainsResponse Core.Int
ldrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
