{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the groups specified by the query. Groups are defined by the underlying Active Directory.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeGroups
  ( -- * Creating a request
    DescribeGroups (..),
    mkDescribeGroups,

    -- ** Request lenses
    dgSearchQuery,
    dgAuthenticationToken,
    dgLimit,
    dgMarker,
    dgOrganizationId,

    -- * Destructuring the response
    DescribeGroupsResponse (..),
    mkDescribeGroupsResponse,

    -- ** Response lenses
    dgrrsGroups,
    dgrrsMarker,
    dgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDescribeGroups' smart constructor.
data DescribeGroups = DescribeGroups'
  { -- | A query to describe groups by group name.
    searchQuery :: Types.SearchQueryType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | The maximum number of items to return with this call.
    limit :: Core.Maybe Core.Natural,
    -- | The marker for the next set of results. (You received this marker from a previous call.)
    marker :: Core.Maybe Types.MarkerType,
    -- | The ID of the organization.
    organizationId :: Core.Maybe Types.IdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGroups' value with any optional fields omitted.
mkDescribeGroups ::
  -- | 'searchQuery'
  Types.SearchQueryType ->
  DescribeGroups
mkDescribeGroups searchQuery =
  DescribeGroups'
    { searchQuery,
      authenticationToken = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing,
      organizationId = Core.Nothing
    }

-- | A query to describe groups by group name.
--
-- /Note:/ Consider using 'searchQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgSearchQuery :: Lens.Lens' DescribeGroups Types.SearchQueryType
dgSearchQuery = Lens.field @"searchQuery"
{-# DEPRECATED dgSearchQuery "Use generic-lens or generic-optics with 'searchQuery' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgAuthenticationToken :: Lens.Lens' DescribeGroups (Core.Maybe Types.AuthenticationHeaderType)
dgAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED dgAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgLimit :: Lens.Lens' DescribeGroups (Core.Maybe Core.Natural)
dgLimit = Lens.field @"limit"
{-# DEPRECATED dgLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgMarker :: Lens.Lens' DescribeGroups (Core.Maybe Types.MarkerType)
dgMarker = Lens.field @"marker"
{-# DEPRECATED dgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgOrganizationId :: Lens.Lens' DescribeGroups (Core.Maybe Types.IdType)
dgOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED dgOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Core.AWSRequest DescribeGroups where
  type Rs DescribeGroups = DescribeGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/api/v1/groups",
        Core._rqQuery =
          Core.toQueryValue "searchQuery" searchQuery
            Core.<> (Core.toQueryValue "limit" Core.<$> limit)
            Core.<> (Core.toQueryValue "marker" Core.<$> marker)
            Core.<> (Core.toQueryValue "organizationId" Core.<$> organizationId),
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGroupsResponse'
            Core.<$> (x Core..:? "Groups")
            Core.<*> (x Core..:? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"groups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeGroupsResponse' smart constructor.
data DescribeGroupsResponse = DescribeGroupsResponse'
  { -- | The list of groups.
    groups :: Core.Maybe [Types.GroupMetadata],
    -- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
    marker :: Core.Maybe Types.MarkerType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGroupsResponse' value with any optional fields omitted.
mkDescribeGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeGroupsResponse
mkDescribeGroupsResponse responseStatus =
  DescribeGroupsResponse'
    { groups = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | The list of groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrrsGroups :: Lens.Lens' DescribeGroupsResponse (Core.Maybe [Types.GroupMetadata])
dgrrsGroups = Lens.field @"groups"
{-# DEPRECATED dgrrsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrrsMarker :: Lens.Lens' DescribeGroupsResponse (Core.Maybe Types.MarkerType)
dgrrsMarker = Lens.field @"marker"
{-# DEPRECATED dgrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrrsResponseStatus :: Lens.Lens' DescribeGroupsResponse Core.Int
dgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
