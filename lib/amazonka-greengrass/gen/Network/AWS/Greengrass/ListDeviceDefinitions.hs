{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListDeviceDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of device definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListDeviceDefinitions
    (
    -- * Creating a request
      ListDeviceDefinitions (..)
    , mkListDeviceDefinitions
    -- ** Request lenses
    , lddMaxResults
    , lddNextToken

    -- * Destructuring the response
    , ListDeviceDefinitionsResponse (..)
    , mkListDeviceDefinitionsResponse
    -- ** Response lenses
    , lddrrsDefinitions
    , lddrrsNextToken
    , lddrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDeviceDefinitions' smart constructor.
data ListDeviceDefinitions = ListDeviceDefinitions'
  { maxResults :: Core.Maybe Core.Text
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeviceDefinitions' value with any optional fields omitted.
mkListDeviceDefinitions
    :: ListDeviceDefinitions
mkListDeviceDefinitions
  = ListDeviceDefinitions'{maxResults = Core.Nothing,
                           nextToken = Core.Nothing}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddMaxResults :: Lens.Lens' ListDeviceDefinitions (Core.Maybe Core.Text)
lddMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lddMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddNextToken :: Lens.Lens' ListDeviceDefinitions (Core.Maybe Core.Text)
lddNextToken = Lens.field @"nextToken"
{-# INLINEABLE lddNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListDeviceDefinitions where
        toQuery ListDeviceDefinitions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListDeviceDefinitions where
        toHeaders ListDeviceDefinitions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListDeviceDefinitions where
        type Rs ListDeviceDefinitions = ListDeviceDefinitionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/greengrass/definition/devices",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDeviceDefinitionsResponse' Core.<$>
                   (x Core..:? "Definitions") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListDeviceDefinitions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"definitions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListDeviceDefinitionsResponse' smart constructor.
data ListDeviceDefinitionsResponse = ListDeviceDefinitionsResponse'
  { definitions :: Core.Maybe [Types.DefinitionInformation]
    -- ^ Information about a definition.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeviceDefinitionsResponse' value with any optional fields omitted.
mkListDeviceDefinitionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDeviceDefinitionsResponse
mkListDeviceDefinitionsResponse responseStatus
  = ListDeviceDefinitionsResponse'{definitions = Core.Nothing,
                                   nextToken = Core.Nothing, responseStatus}

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddrrsDefinitions :: Lens.Lens' ListDeviceDefinitionsResponse (Core.Maybe [Types.DefinitionInformation])
lddrrsDefinitions = Lens.field @"definitions"
{-# INLINEABLE lddrrsDefinitions #-}
{-# DEPRECATED definitions "Use generic-lens or generic-optics with 'definitions' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddrrsNextToken :: Lens.Lens' ListDeviceDefinitionsResponse (Core.Maybe Core.Text)
lddrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lddrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddrrsResponseStatus :: Lens.Lens' ListDeviceDefinitionsResponse Core.Int
lddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
