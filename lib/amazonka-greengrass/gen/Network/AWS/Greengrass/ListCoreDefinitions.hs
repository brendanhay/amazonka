{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListCoreDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of core definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListCoreDefinitions
    (
    -- * Creating a request
      ListCoreDefinitions (..)
    , mkListCoreDefinitions
    -- ** Request lenses
    , lcdMaxResults
    , lcdNextToken

    -- * Destructuring the response
    , ListCoreDefinitionsResponse (..)
    , mkListCoreDefinitionsResponse
    -- ** Response lenses
    , lcdrrsDefinitions
    , lcdrrsNextToken
    , lcdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCoreDefinitions' smart constructor.
data ListCoreDefinitions = ListCoreDefinitions'
  { maxResults :: Core.Maybe Core.Text
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCoreDefinitions' value with any optional fields omitted.
mkListCoreDefinitions
    :: ListCoreDefinitions
mkListCoreDefinitions
  = ListCoreDefinitions'{maxResults = Core.Nothing,
                         nextToken = Core.Nothing}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdMaxResults :: Lens.Lens' ListCoreDefinitions (Core.Maybe Core.Text)
lcdMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcdMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdNextToken :: Lens.Lens' ListCoreDefinitions (Core.Maybe Core.Text)
lcdNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListCoreDefinitions where
        toQuery ListCoreDefinitions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListCoreDefinitions where
        toHeaders ListCoreDefinitions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListCoreDefinitions where
        type Rs ListCoreDefinitions = ListCoreDefinitionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/greengrass/definition/cores",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListCoreDefinitionsResponse' Core.<$>
                   (x Core..:? "Definitions") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListCoreDefinitions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"definitions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListCoreDefinitionsResponse' smart constructor.
data ListCoreDefinitionsResponse = ListCoreDefinitionsResponse'
  { definitions :: Core.Maybe [Types.DefinitionInformation]
    -- ^ Information about a definition.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCoreDefinitionsResponse' value with any optional fields omitted.
mkListCoreDefinitionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListCoreDefinitionsResponse
mkListCoreDefinitionsResponse responseStatus
  = ListCoreDefinitionsResponse'{definitions = Core.Nothing,
                                 nextToken = Core.Nothing, responseStatus}

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdrrsDefinitions :: Lens.Lens' ListCoreDefinitionsResponse (Core.Maybe [Types.DefinitionInformation])
lcdrrsDefinitions = Lens.field @"definitions"
{-# INLINEABLE lcdrrsDefinitions #-}
{-# DEPRECATED definitions "Use generic-lens or generic-optics with 'definitions' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdrrsNextToken :: Lens.Lens' ListCoreDefinitionsResponse (Core.Maybe Core.Text)
lcdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdrrsResponseStatus :: Lens.Lens' ListCoreDefinitionsResponse Core.Int
lcdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
