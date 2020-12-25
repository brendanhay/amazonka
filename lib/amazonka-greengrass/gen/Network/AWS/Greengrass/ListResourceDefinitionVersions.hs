{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListResourceDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a resource definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListResourceDefinitionVersions
  ( -- * Creating a request
    ListResourceDefinitionVersions (..),
    mkListResourceDefinitionVersions,

    -- ** Request lenses
    lrdvResourceDefinitionId,
    lrdvMaxResults,
    lrdvNextToken,

    -- * Destructuring the response
    ListResourceDefinitionVersionsResponse (..),
    mkListResourceDefinitionVersionsResponse,

    -- ** Response lenses
    lrdvrrsNextToken,
    lrdvrrsVersions,
    lrdvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListResourceDefinitionVersions' smart constructor.
data ListResourceDefinitionVersions = ListResourceDefinitionVersions'
  { -- | The ID of the resource definition.
    resourceDefinitionId :: Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceDefinitionVersions' value with any optional fields omitted.
mkListResourceDefinitionVersions ::
  -- | 'resourceDefinitionId'
  Core.Text ->
  ListResourceDefinitionVersions
mkListResourceDefinitionVersions resourceDefinitionId =
  ListResourceDefinitionVersions'
    { resourceDefinitionId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the resource definition.
--
-- /Note:/ Consider using 'resourceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdvResourceDefinitionId :: Lens.Lens' ListResourceDefinitionVersions Core.Text
lrdvResourceDefinitionId = Lens.field @"resourceDefinitionId"
{-# DEPRECATED lrdvResourceDefinitionId "Use generic-lens or generic-optics with 'resourceDefinitionId' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdvMaxResults :: Lens.Lens' ListResourceDefinitionVersions (Core.Maybe Core.Text)
lrdvMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lrdvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdvNextToken :: Lens.Lens' ListResourceDefinitionVersions (Core.Maybe Core.Text)
lrdvNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrdvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListResourceDefinitionVersions where
  type
    Rs ListResourceDefinitionVersions =
      ListResourceDefinitionVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/resources/"
                Core.<> (Core.toText resourceDefinitionId)
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
          ListResourceDefinitionVersionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Versions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListResourceDefinitionVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListResourceDefinitionVersionsResponse' smart constructor.
data ListResourceDefinitionVersionsResponse = ListResourceDefinitionVersionsResponse'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about a version.
    versions :: Core.Maybe [Types.VersionInformation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceDefinitionVersionsResponse' value with any optional fields omitted.
mkListResourceDefinitionVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListResourceDefinitionVersionsResponse
mkListResourceDefinitionVersionsResponse responseStatus =
  ListResourceDefinitionVersionsResponse'
    { nextToken = Core.Nothing,
      versions = Core.Nothing,
      responseStatus
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdvrrsNextToken :: Lens.Lens' ListResourceDefinitionVersionsResponse (Core.Maybe Core.Text)
lrdvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrdvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdvrrsVersions :: Lens.Lens' ListResourceDefinitionVersionsResponse (Core.Maybe [Types.VersionInformation])
lrdvrrsVersions = Lens.field @"versions"
{-# DEPRECATED lrdvrrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdvrrsResponseStatus :: Lens.Lens' ListResourceDefinitionVersionsResponse Core.Int
lrdvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrdvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
