{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListConnectorDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a connector definition, which are containers for connectors. Connectors run on the Greengrass core and contain built-in integration with local infrastructure, device protocols, AWS, and other cloud services.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListConnectorDefinitionVersions
    (
    -- * Creating a request
      ListConnectorDefinitionVersions (..)
    , mkListConnectorDefinitionVersions
    -- ** Request lenses
    , lcdvConnectorDefinitionId
    , lcdvMaxResults
    , lcdvNextToken

    -- * Destructuring the response
    , ListConnectorDefinitionVersionsResponse (..)
    , mkListConnectorDefinitionVersionsResponse
    -- ** Response lenses
    , lcdvrfrsNextToken
    , lcdvrfrsVersions
    , lcdvrfrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListConnectorDefinitionVersions' smart constructor.
data ListConnectorDefinitionVersions = ListConnectorDefinitionVersions'
  { connectorDefinitionId :: Core.Text
    -- ^ The ID of the connector definition.
  , maxResults :: Core.Maybe Core.Text
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConnectorDefinitionVersions' value with any optional fields omitted.
mkListConnectorDefinitionVersions
    :: Core.Text -- ^ 'connectorDefinitionId'
    -> ListConnectorDefinitionVersions
mkListConnectorDefinitionVersions connectorDefinitionId
  = ListConnectorDefinitionVersions'{connectorDefinitionId,
                                     maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvConnectorDefinitionId :: Lens.Lens' ListConnectorDefinitionVersions Core.Text
lcdvConnectorDefinitionId = Lens.field @"connectorDefinitionId"
{-# INLINEABLE lcdvConnectorDefinitionId #-}
{-# DEPRECATED connectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead"  #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvMaxResults :: Lens.Lens' ListConnectorDefinitionVersions (Core.Maybe Core.Text)
lcdvMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcdvMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvNextToken :: Lens.Lens' ListConnectorDefinitionVersions (Core.Maybe Core.Text)
lcdvNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcdvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListConnectorDefinitionVersions where
        toQuery ListConnectorDefinitionVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListConnectorDefinitionVersions where
        toHeaders ListConnectorDefinitionVersions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListConnectorDefinitionVersions where
        type Rs ListConnectorDefinitionVersions =
             ListConnectorDefinitionVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/definition/connectors/" Core.<>
                             Core.toText connectorDefinitionId
                             Core.<> "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListConnectorDefinitionVersionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Versions" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListConnectorDefinitionVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListConnectorDefinitionVersionsResponse' smart constructor.
data ListConnectorDefinitionVersionsResponse = ListConnectorDefinitionVersionsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , versions :: Core.Maybe [Types.VersionInformation]
    -- ^ Information about a version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConnectorDefinitionVersionsResponse' value with any optional fields omitted.
mkListConnectorDefinitionVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListConnectorDefinitionVersionsResponse
mkListConnectorDefinitionVersionsResponse responseStatus
  = ListConnectorDefinitionVersionsResponse'{nextToken =
                                               Core.Nothing,
                                             versions = Core.Nothing, responseStatus}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvrfrsNextToken :: Lens.Lens' ListConnectorDefinitionVersionsResponse (Core.Maybe Core.Text)
lcdvrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcdvrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvrfrsVersions :: Lens.Lens' ListConnectorDefinitionVersionsResponse (Core.Maybe [Types.VersionInformation])
lcdvrfrsVersions = Lens.field @"versions"
{-# INLINEABLE lcdvrfrsVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvrfrsResponseStatus :: Lens.Lens' ListConnectorDefinitionVersionsResponse Core.Int
lcdvrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcdvrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
