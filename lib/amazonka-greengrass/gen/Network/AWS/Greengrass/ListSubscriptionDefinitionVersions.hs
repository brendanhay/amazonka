{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListSubscriptionDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a subscription definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListSubscriptionDefinitionVersions
    (
    -- * Creating a request
      ListSubscriptionDefinitionVersions (..)
    , mkListSubscriptionDefinitionVersions
    -- ** Request lenses
    , lsdvSubscriptionDefinitionId
    , lsdvMaxResults
    , lsdvNextToken

    -- * Destructuring the response
    , ListSubscriptionDefinitionVersionsResponse (..)
    , mkListSubscriptionDefinitionVersionsResponse
    -- ** Response lenses
    , lsdvrrsNextToken
    , lsdvrrsVersions
    , lsdvrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSubscriptionDefinitionVersions' smart constructor.
data ListSubscriptionDefinitionVersions = ListSubscriptionDefinitionVersions'
  { subscriptionDefinitionId :: Core.Text
    -- ^ The ID of the subscription definition.
  , maxResults :: Core.Maybe Core.Text
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSubscriptionDefinitionVersions' value with any optional fields omitted.
mkListSubscriptionDefinitionVersions
    :: Core.Text -- ^ 'subscriptionDefinitionId'
    -> ListSubscriptionDefinitionVersions
mkListSubscriptionDefinitionVersions subscriptionDefinitionId
  = ListSubscriptionDefinitionVersions'{subscriptionDefinitionId,
                                        maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvSubscriptionDefinitionId :: Lens.Lens' ListSubscriptionDefinitionVersions Core.Text
lsdvSubscriptionDefinitionId = Lens.field @"subscriptionDefinitionId"
{-# INLINEABLE lsdvSubscriptionDefinitionId #-}
{-# DEPRECATED subscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead"  #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvMaxResults :: Lens.Lens' ListSubscriptionDefinitionVersions (Core.Maybe Core.Text)
lsdvMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lsdvMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvNextToken :: Lens.Lens' ListSubscriptionDefinitionVersions (Core.Maybe Core.Text)
lsdvNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsdvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListSubscriptionDefinitionVersions where
        toQuery ListSubscriptionDefinitionVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListSubscriptionDefinitionVersions where
        toHeaders ListSubscriptionDefinitionVersions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListSubscriptionDefinitionVersions where
        type Rs ListSubscriptionDefinitionVersions =
             ListSubscriptionDefinitionVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/definition/subscriptions/" Core.<>
                             Core.toText subscriptionDefinitionId
                             Core.<> "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSubscriptionDefinitionVersionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Versions" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSubscriptionDefinitionVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListSubscriptionDefinitionVersionsResponse' smart constructor.
data ListSubscriptionDefinitionVersionsResponse = ListSubscriptionDefinitionVersionsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , versions :: Core.Maybe [Types.VersionInformation]
    -- ^ Information about a version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSubscriptionDefinitionVersionsResponse' value with any optional fields omitted.
mkListSubscriptionDefinitionVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSubscriptionDefinitionVersionsResponse
mkListSubscriptionDefinitionVersionsResponse responseStatus
  = ListSubscriptionDefinitionVersionsResponse'{nextToken =
                                                  Core.Nothing,
                                                versions = Core.Nothing, responseStatus}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvrrsNextToken :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse (Core.Maybe Core.Text)
lsdvrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsdvrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvrrsVersions :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse (Core.Maybe [Types.VersionInformation])
lsdvrrsVersions = Lens.field @"versions"
{-# INLINEABLE lsdvrrsVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvrrsResponseStatus :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse Core.Int
lsdvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsdvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
