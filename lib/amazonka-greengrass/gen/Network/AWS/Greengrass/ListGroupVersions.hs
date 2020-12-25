{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListGroupVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a group.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListGroupVersions
  ( -- * Creating a request
    ListGroupVersions (..),
    mkListGroupVersions,

    -- ** Request lenses
    lgvGroupId,
    lgvMaxResults,
    lgvNextToken,

    -- * Destructuring the response
    ListGroupVersionsResponse (..),
    mkListGroupVersionsResponse,

    -- ** Response lenses
    lgvrrsNextToken,
    lgvrrsVersions,
    lgvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListGroupVersions' smart constructor.
data ListGroupVersions = ListGroupVersions'
  { -- | The ID of the Greengrass group.
    groupId :: Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGroupVersions' value with any optional fields omitted.
mkListGroupVersions ::
  -- | 'groupId'
  Core.Text ->
  ListGroupVersions
mkListGroupVersions groupId =
  ListGroupVersions'
    { groupId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvGroupId :: Lens.Lens' ListGroupVersions Core.Text
lgvGroupId = Lens.field @"groupId"
{-# DEPRECATED lgvGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvMaxResults :: Lens.Lens' ListGroupVersions (Core.Maybe Core.Text)
lgvMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lgvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvNextToken :: Lens.Lens' ListGroupVersions (Core.Maybe Core.Text)
lgvNextToken = Lens.field @"nextToken"
{-# DEPRECATED lgvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListGroupVersions where
  type Rs ListGroupVersions = ListGroupVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/groups/" Core.<> (Core.toText groupId)
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
          ListGroupVersionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Versions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListGroupVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListGroupVersionsResponse' smart constructor.
data ListGroupVersionsResponse = ListGroupVersionsResponse'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about a version.
    versions :: Core.Maybe [Types.VersionInformation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGroupVersionsResponse' value with any optional fields omitted.
mkListGroupVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListGroupVersionsResponse
mkListGroupVersionsResponse responseStatus =
  ListGroupVersionsResponse'
    { nextToken = Core.Nothing,
      versions = Core.Nothing,
      responseStatus
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvrrsNextToken :: Lens.Lens' ListGroupVersionsResponse (Core.Maybe Core.Text)
lgvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lgvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvrrsVersions :: Lens.Lens' ListGroupVersionsResponse (Core.Maybe [Types.VersionInformation])
lgvrrsVersions = Lens.field @"versions"
{-# DEPRECATED lgvrrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvrrsResponseStatus :: Lens.Lens' ListGroupVersionsResponse Core.Int
lgvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lgvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
