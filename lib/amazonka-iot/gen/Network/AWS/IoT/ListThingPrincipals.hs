{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingPrincipals
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the principals associated with the specified thing. A principal can be X.509 certificates, IAM users, groups, and roles, Amazon Cognito identities or federated identities.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingPrincipals
    (
    -- * Creating a request
      ListThingPrincipals (..)
    , mkListThingPrincipals
    -- ** Request lenses
    , ltpThingName
    , ltpMaxResults
    , ltpNextToken

    -- * Destructuring the response
    , ListThingPrincipalsResponse (..)
    , mkListThingPrincipalsResponse
    -- ** Response lenses
    , ltprrsNextToken
    , ltprrsPrincipals
    , ltprrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListThingPrincipal operation.
--
-- /See:/ 'mkListThingPrincipals' smart constructor.
data ListThingPrincipals = ListThingPrincipals'
  { thingName :: Types.ThingName
    -- ^ The name of the thing.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in this operation.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingPrincipals' value with any optional fields omitted.
mkListThingPrincipals
    :: Types.ThingName -- ^ 'thingName'
    -> ListThingPrincipals
mkListThingPrincipals thingName
  = ListThingPrincipals'{thingName, maxResults = Core.Nothing,
                         nextToken = Core.Nothing}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpThingName :: Lens.Lens' ListThingPrincipals Types.ThingName
ltpThingName = Lens.field @"thingName"
{-# INLINEABLE ltpThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | The maximum number of results to return in this operation.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpMaxResults :: Lens.Lens' ListThingPrincipals (Core.Maybe Core.Natural)
ltpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpNextToken :: Lens.Lens' ListThingPrincipals (Core.Maybe Types.NextToken)
ltpNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListThingPrincipals where
        toQuery ListThingPrincipals{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListThingPrincipals where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListThingPrincipals where
        type Rs ListThingPrincipals = ListThingPrincipalsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/things/" Core.<> Core.toText thingName Core.<> "/principals",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListThingPrincipalsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "principals" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListThingPrincipals where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"principals" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | The output from the ListThingPrincipals operation.
--
-- /See:/ 'mkListThingPrincipalsResponse' smart constructor.
data ListThingPrincipalsResponse = ListThingPrincipalsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to get the next set of results, or __null__ if there are no additional results.
  , principals :: Core.Maybe [Types.PrincipalArn]
    -- ^ The principals associated with the thing.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingPrincipalsResponse' value with any optional fields omitted.
mkListThingPrincipalsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListThingPrincipalsResponse
mkListThingPrincipalsResponse responseStatus
  = ListThingPrincipalsResponse'{nextToken = Core.Nothing,
                                 principals = Core.Nothing, responseStatus}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprrsNextToken :: Lens.Lens' ListThingPrincipalsResponse (Core.Maybe Types.NextToken)
ltprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The principals associated with the thing.
--
-- /Note:/ Consider using 'principals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprrsPrincipals :: Lens.Lens' ListThingPrincipalsResponse (Core.Maybe [Types.PrincipalArn])
ltprrsPrincipals = Lens.field @"principals"
{-# INLINEABLE ltprrsPrincipals #-}
{-# DEPRECATED principals "Use generic-lens or generic-optics with 'principals' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprrsResponseStatus :: Lens.Lens' ListThingPrincipalsResponse Core.Int
ltprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
