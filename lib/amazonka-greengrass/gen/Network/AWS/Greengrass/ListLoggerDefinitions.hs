{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListLoggerDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of logger definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListLoggerDefinitions
    (
    -- * Creating a request
      ListLoggerDefinitions (..)
    , mkListLoggerDefinitions
    -- ** Request lenses
    , lldMaxResults
    , lldNextToken

    -- * Destructuring the response
    , ListLoggerDefinitionsResponse (..)
    , mkListLoggerDefinitionsResponse
    -- ** Response lenses
    , lldrrsDefinitions
    , lldrrsNextToken
    , lldrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListLoggerDefinitions' smart constructor.
data ListLoggerDefinitions = ListLoggerDefinitions'
  { maxResults :: Core.Maybe Core.Text
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLoggerDefinitions' value with any optional fields omitted.
mkListLoggerDefinitions
    :: ListLoggerDefinitions
mkListLoggerDefinitions
  = ListLoggerDefinitions'{maxResults = Core.Nothing,
                           nextToken = Core.Nothing}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldMaxResults :: Lens.Lens' ListLoggerDefinitions (Core.Maybe Core.Text)
lldMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lldMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldNextToken :: Lens.Lens' ListLoggerDefinitions (Core.Maybe Core.Text)
lldNextToken = Lens.field @"nextToken"
{-# INLINEABLE lldNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListLoggerDefinitions where
        toQuery ListLoggerDefinitions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListLoggerDefinitions where
        toHeaders ListLoggerDefinitions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListLoggerDefinitions where
        type Rs ListLoggerDefinitions = ListLoggerDefinitionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/greengrass/definition/loggers",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListLoggerDefinitionsResponse' Core.<$>
                   (x Core..:? "Definitions") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListLoggerDefinitions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"definitions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListLoggerDefinitionsResponse' smart constructor.
data ListLoggerDefinitionsResponse = ListLoggerDefinitionsResponse'
  { definitions :: Core.Maybe [Types.DefinitionInformation]
    -- ^ Information about a definition.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLoggerDefinitionsResponse' value with any optional fields omitted.
mkListLoggerDefinitionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListLoggerDefinitionsResponse
mkListLoggerDefinitionsResponse responseStatus
  = ListLoggerDefinitionsResponse'{definitions = Core.Nothing,
                                   nextToken = Core.Nothing, responseStatus}

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldrrsDefinitions :: Lens.Lens' ListLoggerDefinitionsResponse (Core.Maybe [Types.DefinitionInformation])
lldrrsDefinitions = Lens.field @"definitions"
{-# INLINEABLE lldrrsDefinitions #-}
{-# DEPRECATED definitions "Use generic-lens or generic-optics with 'definitions' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldrrsNextToken :: Lens.Lens' ListLoggerDefinitionsResponse (Core.Maybe Core.Text)
lldrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lldrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldrrsResponseStatus :: Lens.Lens' ListLoggerDefinitionsResponse Core.Int
lldrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lldrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
