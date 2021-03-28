{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListPrincipalThings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the things associated with the specified principal. A principal can be X.509 certificates, IAM users, groups, and roles, Amazon Cognito identities or federated identities. 
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListPrincipalThings
    (
    -- * Creating a request
      ListPrincipalThings (..)
    , mkListPrincipalThings
    -- ** Request lenses
    , lptPrincipal
    , lptMaxResults
    , lptNextToken

    -- * Destructuring the response
    , ListPrincipalThingsResponse (..)
    , mkListPrincipalThingsResponse
    -- ** Response lenses
    , lptrrsNextToken
    , lptrrsThings
    , lptrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListPrincipalThings operation.
--
-- /See:/ 'mkListPrincipalThings' smart constructor.
data ListPrincipalThings = ListPrincipalThings'
  { principal :: Types.Principal
    -- ^ The principal.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in this operation.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPrincipalThings' value with any optional fields omitted.
mkListPrincipalThings
    :: Types.Principal -- ^ 'principal'
    -> ListPrincipalThings
mkListPrincipalThings principal
  = ListPrincipalThings'{principal, maxResults = Core.Nothing,
                         nextToken = Core.Nothing}

-- | The principal.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptPrincipal :: Lens.Lens' ListPrincipalThings Types.Principal
lptPrincipal = Lens.field @"principal"
{-# INLINEABLE lptPrincipal #-}
{-# DEPRECATED principal "Use generic-lens or generic-optics with 'principal' instead"  #-}

-- | The maximum number of results to return in this operation.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptMaxResults :: Lens.Lens' ListPrincipalThings (Core.Maybe Core.Natural)
lptMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lptMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptNextToken :: Lens.Lens' ListPrincipalThings (Core.Maybe Types.NextToken)
lptNextToken = Lens.field @"nextToken"
{-# INLINEABLE lptNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListPrincipalThings where
        toQuery ListPrincipalThings{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListPrincipalThings where
        toHeaders ListPrincipalThings{..}
          = Core.toHeaders "x-amzn-principal" principal

instance Core.AWSRequest ListPrincipalThings where
        type Rs ListPrincipalThings = ListPrincipalThingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/principals/things",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPrincipalThingsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "things" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPrincipalThings where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"things" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | The output from the ListPrincipalThings operation.
--
-- /See:/ 'mkListPrincipalThingsResponse' smart constructor.
data ListPrincipalThingsResponse = ListPrincipalThingsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to get the next set of results, or __null__ if there are no additional results.
  , things :: Core.Maybe [Types.ThingName]
    -- ^ The things.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPrincipalThingsResponse' value with any optional fields omitted.
mkListPrincipalThingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPrincipalThingsResponse
mkListPrincipalThingsResponse responseStatus
  = ListPrincipalThingsResponse'{nextToken = Core.Nothing,
                                 things = Core.Nothing, responseStatus}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptrrsNextToken :: Lens.Lens' ListPrincipalThingsResponse (Core.Maybe Types.NextToken)
lptrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lptrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The things.
--
-- /Note:/ Consider using 'things' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptrrsThings :: Lens.Lens' ListPrincipalThingsResponse (Core.Maybe [Types.ThingName])
lptrrsThings = Lens.field @"things"
{-# INLINEABLE lptrrsThings #-}
{-# DEPRECATED things "Use generic-lens or generic-optics with 'things' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptrrsResponseStatus :: Lens.Lens' ListPrincipalThingsResponse Core.Int
lptrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lptrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
