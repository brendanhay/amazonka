{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListConnectorDefinitionVersions (..),
    mkListConnectorDefinitionVersions,

    -- ** Request lenses
    lcdvConnectorDefinitionId,
    lcdvMaxResults,
    lcdvNextToken,

    -- * Destructuring the response
    ListConnectorDefinitionVersionsResponse (..),
    mkListConnectorDefinitionVersionsResponse,

    -- ** Response lenses
    lcdvrfrsNextToken,
    lcdvrfrsVersions,
    lcdvrfrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListConnectorDefinitionVersions' smart constructor.
data ListConnectorDefinitionVersions = ListConnectorDefinitionVersions'
  { -- | The ID of the connector definition.
    connectorDefinitionId :: Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConnectorDefinitionVersions' value with any optional fields omitted.
mkListConnectorDefinitionVersions ::
  -- | 'connectorDefinitionId'
  Core.Text ->
  ListConnectorDefinitionVersions
mkListConnectorDefinitionVersions connectorDefinitionId =
  ListConnectorDefinitionVersions'
    { connectorDefinitionId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvConnectorDefinitionId :: Lens.Lens' ListConnectorDefinitionVersions Core.Text
lcdvConnectorDefinitionId = Lens.field @"connectorDefinitionId"
{-# DEPRECATED lcdvConnectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvMaxResults :: Lens.Lens' ListConnectorDefinitionVersions (Core.Maybe Core.Text)
lcdvMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcdvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvNextToken :: Lens.Lens' ListConnectorDefinitionVersions (Core.Maybe Core.Text)
lcdvNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcdvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListConnectorDefinitionVersions where
  type
    Rs ListConnectorDefinitionVersions =
      ListConnectorDefinitionVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/connectors/"
                Core.<> (Core.toText connectorDefinitionId)
                Core.<> ("/versions")
            ),
        Core._rqQuery =
          Core.toQueryValue "MaxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConnectorDefinitionVersionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Versions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListConnectorDefinitionVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListConnectorDefinitionVersionsResponse' smart constructor.
data ListConnectorDefinitionVersionsResponse = ListConnectorDefinitionVersionsResponse'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about a version.
    versions :: Core.Maybe [Types.VersionInformation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConnectorDefinitionVersionsResponse' value with any optional fields omitted.
mkListConnectorDefinitionVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListConnectorDefinitionVersionsResponse
mkListConnectorDefinitionVersionsResponse responseStatus =
  ListConnectorDefinitionVersionsResponse'
    { nextToken =
        Core.Nothing,
      versions = Core.Nothing,
      responseStatus
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvrfrsNextToken :: Lens.Lens' ListConnectorDefinitionVersionsResponse (Core.Maybe Core.Text)
lcdvrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcdvrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvrfrsVersions :: Lens.Lens' ListConnectorDefinitionVersionsResponse (Core.Maybe [Types.VersionInformation])
lcdvrfrsVersions = Lens.field @"versions"
{-# DEPRECATED lcdvrfrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvrfrsResponseStatus :: Lens.Lens' ListConnectorDefinitionVersionsResponse Core.Int
lcdvrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcdvrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
