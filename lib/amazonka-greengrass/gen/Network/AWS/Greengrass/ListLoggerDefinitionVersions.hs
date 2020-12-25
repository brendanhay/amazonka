{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListLoggerDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a logger definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListLoggerDefinitionVersions
  ( -- * Creating a request
    ListLoggerDefinitionVersions (..),
    mkListLoggerDefinitionVersions,

    -- ** Request lenses
    lldvLoggerDefinitionId,
    lldvMaxResults,
    lldvNextToken,

    -- * Destructuring the response
    ListLoggerDefinitionVersionsResponse (..),
    mkListLoggerDefinitionVersionsResponse,

    -- ** Response lenses
    lldvrrsNextToken,
    lldvrrsVersions,
    lldvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListLoggerDefinitionVersions' smart constructor.
data ListLoggerDefinitionVersions = ListLoggerDefinitionVersions'
  { -- | The ID of the logger definition.
    loggerDefinitionId :: Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLoggerDefinitionVersions' value with any optional fields omitted.
mkListLoggerDefinitionVersions ::
  -- | 'loggerDefinitionId'
  Core.Text ->
  ListLoggerDefinitionVersions
mkListLoggerDefinitionVersions loggerDefinitionId =
  ListLoggerDefinitionVersions'
    { loggerDefinitionId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the logger definition.
--
-- /Note:/ Consider using 'loggerDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvLoggerDefinitionId :: Lens.Lens' ListLoggerDefinitionVersions Core.Text
lldvLoggerDefinitionId = Lens.field @"loggerDefinitionId"
{-# DEPRECATED lldvLoggerDefinitionId "Use generic-lens or generic-optics with 'loggerDefinitionId' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvMaxResults :: Lens.Lens' ListLoggerDefinitionVersions (Core.Maybe Core.Text)
lldvMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lldvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvNextToken :: Lens.Lens' ListLoggerDefinitionVersions (Core.Maybe Core.Text)
lldvNextToken = Lens.field @"nextToken"
{-# DEPRECATED lldvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListLoggerDefinitionVersions where
  type
    Rs ListLoggerDefinitionVersions =
      ListLoggerDefinitionVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/loggers/"
                Core.<> (Core.toText loggerDefinitionId)
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
          ListLoggerDefinitionVersionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Versions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListLoggerDefinitionVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListLoggerDefinitionVersionsResponse' smart constructor.
data ListLoggerDefinitionVersionsResponse = ListLoggerDefinitionVersionsResponse'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about a version.
    versions :: Core.Maybe [Types.VersionInformation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLoggerDefinitionVersionsResponse' value with any optional fields omitted.
mkListLoggerDefinitionVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListLoggerDefinitionVersionsResponse
mkListLoggerDefinitionVersionsResponse responseStatus =
  ListLoggerDefinitionVersionsResponse'
    { nextToken = Core.Nothing,
      versions = Core.Nothing,
      responseStatus
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvrrsNextToken :: Lens.Lens' ListLoggerDefinitionVersionsResponse (Core.Maybe Core.Text)
lldvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lldvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvrrsVersions :: Lens.Lens' ListLoggerDefinitionVersionsResponse (Core.Maybe [Types.VersionInformation])
lldvrrsVersions = Lens.field @"versions"
{-# DEPRECATED lldvrrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvrrsResponseStatus :: Lens.Lens' ListLoggerDefinitionVersionsResponse Core.Int
lldvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lldvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
