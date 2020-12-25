{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified users. You can describe all users or filter the results (for example, by status or organization).
--
-- By default, Amazon WorkDocs returns the first 24 active or pending users. If there are more results, the response includes a marker that you can use to request the next set of results.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeUsers
  ( -- * Creating a request
    DescribeUsers (..),
    mkDescribeUsers,

    -- ** Request lenses
    duAuthenticationToken,
    duFields,
    duInclude,
    duLimit,
    duMarker,
    duOrder,
    duOrganizationId,
    duQuery,
    duSort,
    duUserIds,

    -- * Destructuring the response
    DescribeUsersResponse (..),
    mkDescribeUsersResponse,

    -- ** Response lenses
    durrsMarker,
    durrsTotalNumberOfUsers,
    durrsUsers,
    durrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDescribeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | A comma-separated list of values. Specify "STORAGE_METADATA" to include the user storage quota and utilization information.
    fields :: Core.Maybe Types.Fields,
    -- | The state of the users. Specify "ALL" to include inactive users.
    include :: Core.Maybe Types.UserFilterType,
    -- | The maximum number of items to return.
    limit :: Core.Maybe Core.Natural,
    -- | The marker for the next set of results. (You received this marker from a previous call.)
    marker :: Core.Maybe Types.PageMarkerType,
    -- | The order for the results.
    order :: Core.Maybe Types.OrderType,
    -- | The ID of the organization.
    organizationId :: Core.Maybe Types.IdType,
    -- | A query to filter users by user name.
    query :: Core.Maybe Types.SearchQueryType,
    -- | The sorting criteria.
    sort :: Core.Maybe Types.UserSortType,
    -- | The IDs of the users.
    userIds :: Core.Maybe Types.UserIdsType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUsers' value with any optional fields omitted.
mkDescribeUsers ::
  DescribeUsers
mkDescribeUsers =
  DescribeUsers'
    { authenticationToken = Core.Nothing,
      fields = Core.Nothing,
      include = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing,
      order = Core.Nothing,
      organizationId = Core.Nothing,
      query = Core.Nothing,
      sort = Core.Nothing,
      userIds = Core.Nothing
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duAuthenticationToken :: Lens.Lens' DescribeUsers (Core.Maybe Types.AuthenticationHeaderType)
duAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED duAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | A comma-separated list of values. Specify "STORAGE_METADATA" to include the user storage quota and utilization information.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duFields :: Lens.Lens' DescribeUsers (Core.Maybe Types.Fields)
duFields = Lens.field @"fields"
{-# DEPRECATED duFields "Use generic-lens or generic-optics with 'fields' instead." #-}

-- | The state of the users. Specify "ALL" to include inactive users.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duInclude :: Lens.Lens' DescribeUsers (Core.Maybe Types.UserFilterType)
duInclude = Lens.field @"include"
{-# DEPRECATED duInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duLimit :: Lens.Lens' DescribeUsers (Core.Maybe Core.Natural)
duLimit = Lens.field @"limit"
{-# DEPRECATED duLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duMarker :: Lens.Lens' DescribeUsers (Core.Maybe Types.PageMarkerType)
duMarker = Lens.field @"marker"
{-# DEPRECATED duMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The order for the results.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duOrder :: Lens.Lens' DescribeUsers (Core.Maybe Types.OrderType)
duOrder = Lens.field @"order"
{-# DEPRECATED duOrder "Use generic-lens or generic-optics with 'order' instead." #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duOrganizationId :: Lens.Lens' DescribeUsers (Core.Maybe Types.IdType)
duOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED duOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | A query to filter users by user name.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duQuery :: Lens.Lens' DescribeUsers (Core.Maybe Types.SearchQueryType)
duQuery = Lens.field @"query"
{-# DEPRECATED duQuery "Use generic-lens or generic-optics with 'query' instead." #-}

-- | The sorting criteria.
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duSort :: Lens.Lens' DescribeUsers (Core.Maybe Types.UserSortType)
duSort = Lens.field @"sort"
{-# DEPRECATED duSort "Use generic-lens or generic-optics with 'sort' instead." #-}

-- | The IDs of the users.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserIds :: Lens.Lens' DescribeUsers (Core.Maybe Types.UserIdsType)
duUserIds = Lens.field @"userIds"
{-# DEPRECATED duUserIds "Use generic-lens or generic-optics with 'userIds' instead." #-}

instance Core.AWSRequest DescribeUsers where
  type Rs DescribeUsers = DescribeUsersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/api/v1/users",
        Core._rqQuery =
          Core.toQueryValue "fields" Core.<$> fields
            Core.<> (Core.toQueryValue "include" Core.<$> include)
            Core.<> (Core.toQueryValue "limit" Core.<$> limit)
            Core.<> (Core.toQueryValue "marker" Core.<$> marker)
            Core.<> (Core.toQueryValue "order" Core.<$> order)
            Core.<> (Core.toQueryValue "organizationId" Core.<$> organizationId)
            Core.<> (Core.toQueryValue "query" Core.<$> query)
            Core.<> (Core.toQueryValue "sort" Core.<$> sort)
            Core.<> (Core.toQueryValue "userIds" Core.<$> userIds),
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUsersResponse'
            Core.<$> (x Core..:? "Marker")
            Core.<*> (x Core..:? "TotalNumberOfUsers")
            Core.<*> (x Core..:? "Users")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeUsers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"users" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { -- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
    marker :: Core.Maybe Types.Marker,
    -- | The total number of users included in the results.
    totalNumberOfUsers :: Core.Maybe Core.Integer,
    -- | The users.
    users :: Core.Maybe [Types.User],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeUsersResponse' value with any optional fields omitted.
mkDescribeUsersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeUsersResponse
mkDescribeUsersResponse responseStatus =
  DescribeUsersResponse'
    { marker = Core.Nothing,
      totalNumberOfUsers = Core.Nothing,
      users = Core.Nothing,
      responseStatus
    }

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsMarker :: Lens.Lens' DescribeUsersResponse (Core.Maybe Types.Marker)
durrsMarker = Lens.field @"marker"
{-# DEPRECATED durrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The total number of users included in the results.
--
-- /Note:/ Consider using 'totalNumberOfUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsTotalNumberOfUsers :: Lens.Lens' DescribeUsersResponse (Core.Maybe Core.Integer)
durrsTotalNumberOfUsers = Lens.field @"totalNumberOfUsers"
{-# DEPRECATED durrsTotalNumberOfUsers "Use generic-lens or generic-optics with 'totalNumberOfUsers' instead." #-}

-- | The users.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsUsers :: Lens.Lens' DescribeUsersResponse (Core.Maybe [Types.User])
durrsUsers = Lens.field @"users"
{-# DEPRECATED durrsUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsResponseStatus :: Lens.Lens' DescribeUsersResponse Core.Int
durrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED durrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
