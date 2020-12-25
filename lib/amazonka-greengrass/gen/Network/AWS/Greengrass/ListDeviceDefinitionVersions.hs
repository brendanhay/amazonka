{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListDeviceDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a device definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListDeviceDefinitionVersions
  ( -- * Creating a request
    ListDeviceDefinitionVersions (..),
    mkListDeviceDefinitionVersions,

    -- ** Request lenses
    lddvDeviceDefinitionId,
    lddvMaxResults,
    lddvNextToken,

    -- * Destructuring the response
    ListDeviceDefinitionVersionsResponse (..),
    mkListDeviceDefinitionVersionsResponse,

    -- ** Response lenses
    lddvrrsNextToken,
    lddvrrsVersions,
    lddvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDeviceDefinitionVersions' smart constructor.
data ListDeviceDefinitionVersions = ListDeviceDefinitionVersions'
  { -- | The ID of the device definition.
    deviceDefinitionId :: Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeviceDefinitionVersions' value with any optional fields omitted.
mkListDeviceDefinitionVersions ::
  -- | 'deviceDefinitionId'
  Core.Text ->
  ListDeviceDefinitionVersions
mkListDeviceDefinitionVersions deviceDefinitionId =
  ListDeviceDefinitionVersions'
    { deviceDefinitionId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvDeviceDefinitionId :: Lens.Lens' ListDeviceDefinitionVersions Core.Text
lddvDeviceDefinitionId = Lens.field @"deviceDefinitionId"
{-# DEPRECATED lddvDeviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvMaxResults :: Lens.Lens' ListDeviceDefinitionVersions (Core.Maybe Core.Text)
lddvMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lddvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvNextToken :: Lens.Lens' ListDeviceDefinitionVersions (Core.Maybe Core.Text)
lddvNextToken = Lens.field @"nextToken"
{-# DEPRECATED lddvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListDeviceDefinitionVersions where
  type
    Rs ListDeviceDefinitionVersions =
      ListDeviceDefinitionVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/devices/"
                Core.<> (Core.toText deviceDefinitionId)
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
          ListDeviceDefinitionVersionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Versions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDeviceDefinitionVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListDeviceDefinitionVersionsResponse' smart constructor.
data ListDeviceDefinitionVersionsResponse = ListDeviceDefinitionVersionsResponse'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about a version.
    versions :: Core.Maybe [Types.VersionInformation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeviceDefinitionVersionsResponse' value with any optional fields omitted.
mkListDeviceDefinitionVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDeviceDefinitionVersionsResponse
mkListDeviceDefinitionVersionsResponse responseStatus =
  ListDeviceDefinitionVersionsResponse'
    { nextToken = Core.Nothing,
      versions = Core.Nothing,
      responseStatus
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvrrsNextToken :: Lens.Lens' ListDeviceDefinitionVersionsResponse (Core.Maybe Core.Text)
lddvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lddvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvrrsVersions :: Lens.Lens' ListDeviceDefinitionVersionsResponse (Core.Maybe [Types.VersionInformation])
lddvrrsVersions = Lens.field @"versions"
{-# DEPRECATED lddvrrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvrrsResponseStatus :: Lens.Lens' ListDeviceDefinitionVersionsResponse Core.Int
lddvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lddvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
