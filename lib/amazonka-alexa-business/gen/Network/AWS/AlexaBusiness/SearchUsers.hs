{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SearchUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches users and lists the ones that meet a set of filter and sort criteria.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchUsers
  ( -- * Creating a request
    SearchUsers (..),
    mkSearchUsers,

    -- ** Request lenses
    suFilters,
    suMaxResults,
    suNextToken,
    suSortCriteria,

    -- * Destructuring the response
    SearchUsersResponse (..),
    mkSearchUsersResponse,

    -- ** Response lenses
    surrsNextToken,
    surrsTotalCount,
    surrsUsers,
    surrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchUsers' smart constructor.
data SearchUsers = SearchUsers'
  { -- | The filters to use for listing a specific set of users. Required. Supported filter keys are UserId, FirstName, LastName, Email, and EnrollmentStatus.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. Required.
    maxResults :: Core.Maybe Core.Natural,
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ . Required.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The sort order to use in listing the filtered set of users. Required. Supported sort keys are UserId, FirstName, LastName, Email, and EnrollmentStatus.
    sortCriteria :: Core.Maybe [Types.Sort]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchUsers' value with any optional fields omitted.
mkSearchUsers ::
  SearchUsers
mkSearchUsers =
  SearchUsers'
    { filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortCriteria = Core.Nothing
    }

-- | The filters to use for listing a specific set of users. Required. Supported filter keys are UserId, FirstName, LastName, Email, and EnrollmentStatus.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suFilters :: Lens.Lens' SearchUsers (Core.Maybe [Types.Filter])
suFilters = Lens.field @"filters"
{-# DEPRECATED suFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. Required.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suMaxResults :: Lens.Lens' SearchUsers (Core.Maybe Core.Natural)
suMaxResults = Lens.field @"maxResults"
{-# DEPRECATED suMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ . Required.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suNextToken :: Lens.Lens' SearchUsers (Core.Maybe Types.NextToken)
suNextToken = Lens.field @"nextToken"
{-# DEPRECATED suNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order to use in listing the filtered set of users. Required. Supported sort keys are UserId, FirstName, LastName, Email, and EnrollmentStatus.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suSortCriteria :: Lens.Lens' SearchUsers (Core.Maybe [Types.Sort])
suSortCriteria = Lens.field @"sortCriteria"
{-# DEPRECATED suSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

instance Core.FromJSON SearchUsers where
  toJSON SearchUsers {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortCriteria" Core..=) Core.<$> sortCriteria
          ]
      )

instance Core.AWSRequest SearchUsers where
  type Rs SearchUsers = SearchUsersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.SearchUsers")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchUsersResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "TotalCount")
            Core.<*> (x Core..:? "Users")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager SearchUsers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"users" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkSearchUsersResponse' smart constructor.
data SearchUsersResponse = SearchUsersResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The total number of users returned.
    totalCount :: Core.Maybe Core.Int,
    -- | The users that meet the specified set of filter criteria, in sort order.
    users :: Core.Maybe [Types.UserData],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchUsersResponse' value with any optional fields omitted.
mkSearchUsersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SearchUsersResponse
mkSearchUsersResponse responseStatus =
  SearchUsersResponse'
    { nextToken = Core.Nothing,
      totalCount = Core.Nothing,
      users = Core.Nothing,
      responseStatus
    }

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
surrsNextToken :: Lens.Lens' SearchUsersResponse (Core.Maybe Types.NextToken)
surrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED surrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of users returned.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
surrsTotalCount :: Lens.Lens' SearchUsersResponse (Core.Maybe Core.Int)
surrsTotalCount = Lens.field @"totalCount"
{-# DEPRECATED surrsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

-- | The users that meet the specified set of filter criteria, in sort order.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
surrsUsers :: Lens.Lens' SearchUsersResponse (Core.Maybe [Types.UserData])
surrsUsers = Lens.field @"users"
{-# DEPRECATED surrsUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
surrsResponseStatus :: Lens.Lens' SearchUsersResponse Core.Int
surrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED surrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
