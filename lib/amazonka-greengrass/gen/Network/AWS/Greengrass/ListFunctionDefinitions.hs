{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListFunctionDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of Lambda function definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListFunctionDefinitions
    (
    -- * Creating a request
      ListFunctionDefinitions (..)
    , mkListFunctionDefinitions
    -- ** Request lenses
    , lfdMaxResults
    , lfdNextToken

    -- * Destructuring the response
    , ListFunctionDefinitionsResponse (..)
    , mkListFunctionDefinitionsResponse
    -- ** Response lenses
    , lfdrrsDefinitions
    , lfdrrsNextToken
    , lfdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFunctionDefinitions' smart constructor.
data ListFunctionDefinitions = ListFunctionDefinitions'
  { maxResults :: Core.Maybe Core.Text
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctionDefinitions' value with any optional fields omitted.
mkListFunctionDefinitions
    :: ListFunctionDefinitions
mkListFunctionDefinitions
  = ListFunctionDefinitions'{maxResults = Core.Nothing,
                             nextToken = Core.Nothing}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdMaxResults :: Lens.Lens' ListFunctionDefinitions (Core.Maybe Core.Text)
lfdMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lfdMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdNextToken :: Lens.Lens' ListFunctionDefinitions (Core.Maybe Core.Text)
lfdNextToken = Lens.field @"nextToken"
{-# INLINEABLE lfdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListFunctionDefinitions where
        toQuery ListFunctionDefinitions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListFunctionDefinitions where
        toHeaders ListFunctionDefinitions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListFunctionDefinitions where
        type Rs ListFunctionDefinitions = ListFunctionDefinitionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/greengrass/definition/functions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListFunctionDefinitionsResponse' Core.<$>
                   (x Core..:? "Definitions") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListFunctionDefinitions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"definitions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListFunctionDefinitionsResponse' smart constructor.
data ListFunctionDefinitionsResponse = ListFunctionDefinitionsResponse'
  { definitions :: Core.Maybe [Types.DefinitionInformation]
    -- ^ Information about a definition.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctionDefinitionsResponse' value with any optional fields omitted.
mkListFunctionDefinitionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListFunctionDefinitionsResponse
mkListFunctionDefinitionsResponse responseStatus
  = ListFunctionDefinitionsResponse'{definitions = Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdrrsDefinitions :: Lens.Lens' ListFunctionDefinitionsResponse (Core.Maybe [Types.DefinitionInformation])
lfdrrsDefinitions = Lens.field @"definitions"
{-# INLINEABLE lfdrrsDefinitions #-}
{-# DEPRECATED definitions "Use generic-lens or generic-optics with 'definitions' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdrrsNextToken :: Lens.Lens' ListFunctionDefinitionsResponse (Core.Maybe Core.Text)
lfdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lfdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdrrsResponseStatus :: Lens.Lens' ListFunctionDefinitionsResponse Core.Int
lfdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lfdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
