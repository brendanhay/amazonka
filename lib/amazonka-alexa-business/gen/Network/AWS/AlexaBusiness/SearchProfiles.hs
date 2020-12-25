{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SearchProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches room profiles and lists the ones that meet a set of filter criteria.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchProfiles
  ( -- * Creating a request
    SearchProfiles (..),
    mkSearchProfiles,

    -- ** Request lenses
    spFilters,
    spMaxResults,
    spNextToken,
    spSortCriteria,

    -- * Destructuring the response
    SearchProfilesResponse (..),
    mkSearchProfilesResponse,

    -- ** Response lenses
    sprrsNextToken,
    sprrsProfiles,
    sprrsTotalCount,
    sprrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchProfiles' smart constructor.
data SearchProfiles = SearchProfiles'
  { -- | The filters to use to list a specified set of room profiles. Supported filter keys are ProfileName and Address. Required.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
    maxResults :: Core.Maybe Core.Natural,
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The sort order to use in listing the specified set of room profiles. Supported sort keys are ProfileName and Address.
    sortCriteria :: Core.Maybe [Types.Sort]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchProfiles' value with any optional fields omitted.
mkSearchProfiles ::
  SearchProfiles
mkSearchProfiles =
  SearchProfiles'
    { filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortCriteria = Core.Nothing
    }

-- | The filters to use to list a specified set of room profiles. Supported filter keys are ProfileName and Address. Required.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spFilters :: Lens.Lens' SearchProfiles (Core.Maybe [Types.Filter])
spFilters = Lens.field @"filters"
{-# DEPRECATED spFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spMaxResults :: Lens.Lens' SearchProfiles (Core.Maybe Core.Natural)
spMaxResults = Lens.field @"maxResults"
{-# DEPRECATED spMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spNextToken :: Lens.Lens' SearchProfiles (Core.Maybe Types.NextToken)
spNextToken = Lens.field @"nextToken"
{-# DEPRECATED spNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order to use in listing the specified set of room profiles. Supported sort keys are ProfileName and Address.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spSortCriteria :: Lens.Lens' SearchProfiles (Core.Maybe [Types.Sort])
spSortCriteria = Lens.field @"sortCriteria"
{-# DEPRECATED spSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

instance Core.FromJSON SearchProfiles where
  toJSON SearchProfiles {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortCriteria" Core..=) Core.<$> sortCriteria
          ]
      )

instance Core.AWSRequest SearchProfiles where
  type Rs SearchProfiles = SearchProfilesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.SearchProfiles")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchProfilesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Profiles")
            Core.<*> (x Core..:? "TotalCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager SearchProfiles where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"profiles" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkSearchProfilesResponse' smart constructor.
data SearchProfilesResponse = SearchProfilesResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The profiles that meet the specified set of filter criteria, in sort order.
    profiles :: Core.Maybe [Types.ProfileData],
    -- | The total number of room profiles returned.
    totalCount :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchProfilesResponse' value with any optional fields omitted.
mkSearchProfilesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SearchProfilesResponse
mkSearchProfilesResponse responseStatus =
  SearchProfilesResponse'
    { nextToken = Core.Nothing,
      profiles = Core.Nothing,
      totalCount = Core.Nothing,
      responseStatus
    }

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprrsNextToken :: Lens.Lens' SearchProfilesResponse (Core.Maybe Types.NextToken)
sprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED sprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The profiles that meet the specified set of filter criteria, in sort order.
--
-- /Note:/ Consider using 'profiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprrsProfiles :: Lens.Lens' SearchProfilesResponse (Core.Maybe [Types.ProfileData])
sprrsProfiles = Lens.field @"profiles"
{-# DEPRECATED sprrsProfiles "Use generic-lens or generic-optics with 'profiles' instead." #-}

-- | The total number of room profiles returned.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprrsTotalCount :: Lens.Lens' SearchProfilesResponse (Core.Maybe Core.Int)
sprrsTotalCount = Lens.field @"totalCount"
{-# DEPRECATED sprrsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprrsResponseStatus :: Lens.Lens' SearchProfilesResponse Core.Int
sprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
