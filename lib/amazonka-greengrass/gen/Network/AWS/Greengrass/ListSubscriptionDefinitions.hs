{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListSubscriptionDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of subscription definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListSubscriptionDefinitions
    (
    -- * Creating a request
      ListSubscriptionDefinitions (..)
    , mkListSubscriptionDefinitions
    -- ** Request lenses
    , lsdMaxResults
    , lsdNextToken

    -- * Destructuring the response
    , ListSubscriptionDefinitionsResponse (..)
    , mkListSubscriptionDefinitionsResponse
    -- ** Response lenses
    , lsdrrsDefinitions
    , lsdrrsNextToken
    , lsdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSubscriptionDefinitions' smart constructor.
data ListSubscriptionDefinitions = ListSubscriptionDefinitions'
  { maxResults :: Core.Maybe Core.Text
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSubscriptionDefinitions' value with any optional fields omitted.
mkListSubscriptionDefinitions
    :: ListSubscriptionDefinitions
mkListSubscriptionDefinitions
  = ListSubscriptionDefinitions'{maxResults = Core.Nothing,
                                 nextToken = Core.Nothing}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdMaxResults :: Lens.Lens' ListSubscriptionDefinitions (Core.Maybe Core.Text)
lsdMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lsdMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdNextToken :: Lens.Lens' ListSubscriptionDefinitions (Core.Maybe Core.Text)
lsdNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListSubscriptionDefinitions where
        toQuery ListSubscriptionDefinitions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListSubscriptionDefinitions where
        toHeaders ListSubscriptionDefinitions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListSubscriptionDefinitions where
        type Rs ListSubscriptionDefinitions =
             ListSubscriptionDefinitionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/greengrass/definition/subscriptions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSubscriptionDefinitionsResponse' Core.<$>
                   (x Core..:? "Definitions") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSubscriptionDefinitions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"definitions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListSubscriptionDefinitionsResponse' smart constructor.
data ListSubscriptionDefinitionsResponse = ListSubscriptionDefinitionsResponse'
  { definitions :: Core.Maybe [Types.DefinitionInformation]
    -- ^ Information about a definition.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSubscriptionDefinitionsResponse' value with any optional fields omitted.
mkListSubscriptionDefinitionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSubscriptionDefinitionsResponse
mkListSubscriptionDefinitionsResponse responseStatus
  = ListSubscriptionDefinitionsResponse'{definitions = Core.Nothing,
                                         nextToken = Core.Nothing, responseStatus}

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdrrsDefinitions :: Lens.Lens' ListSubscriptionDefinitionsResponse (Core.Maybe [Types.DefinitionInformation])
lsdrrsDefinitions = Lens.field @"definitions"
{-# INLINEABLE lsdrrsDefinitions #-}
{-# DEPRECATED definitions "Use generic-lens or generic-optics with 'definitions' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdrrsNextToken :: Lens.Lens' ListSubscriptionDefinitionsResponse (Core.Maybe Core.Text)
lsdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdrrsResponseStatus :: Lens.Lens' ListSubscriptionDefinitionsResponse Core.Int
lsdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
