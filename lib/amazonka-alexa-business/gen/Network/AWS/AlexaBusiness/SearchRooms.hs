{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SearchRooms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches rooms and lists the ones that meet a set of filter and sort criteria.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchRooms
  ( -- * Creating a request
    SearchRooms (..),
    mkSearchRooms,

    -- ** Request lenses
    srFilters,
    srMaxResults,
    srNextToken,
    srSortCriteria,

    -- * Destructuring the response
    SearchRoomsResponse (..),
    mkSearchRoomsResponse,

    -- ** Response lenses
    srrrsNextToken,
    srrrsRooms,
    srrrsTotalCount,
    srrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchRooms' smart constructor.
data SearchRooms = SearchRooms'
  { -- | The filters to use to list a specified set of rooms. The supported filter keys are RoomName and ProfileName.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
    maxResults :: Core.Maybe Core.Natural,
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The sort order to use in listing the specified set of rooms. The supported sort keys are RoomName and ProfileName.
    sortCriteria :: Core.Maybe [Types.Sort]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchRooms' value with any optional fields omitted.
mkSearchRooms ::
  SearchRooms
mkSearchRooms =
  SearchRooms'
    { filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortCriteria = Core.Nothing
    }

-- | The filters to use to list a specified set of rooms. The supported filter keys are RoomName and ProfileName.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srFilters :: Lens.Lens' SearchRooms (Core.Maybe [Types.Filter])
srFilters = Lens.field @"filters"
{-# DEPRECATED srFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srMaxResults :: Lens.Lens' SearchRooms (Core.Maybe Core.Natural)
srMaxResults = Lens.field @"maxResults"
{-# DEPRECATED srMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srNextToken :: Lens.Lens' SearchRooms (Core.Maybe Types.NextToken)
srNextToken = Lens.field @"nextToken"
{-# DEPRECATED srNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order to use in listing the specified set of rooms. The supported sort keys are RoomName and ProfileName.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSortCriteria :: Lens.Lens' SearchRooms (Core.Maybe [Types.Sort])
srSortCriteria = Lens.field @"sortCriteria"
{-# DEPRECATED srSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

instance Core.FromJSON SearchRooms where
  toJSON SearchRooms {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortCriteria" Core..=) Core.<$> sortCriteria
          ]
      )

instance Core.AWSRequest SearchRooms where
  type Rs SearchRooms = SearchRoomsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.SearchRooms")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchRoomsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Rooms")
            Core.<*> (x Core..:? "TotalCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager SearchRooms where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"rooms" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkSearchRoomsResponse' smart constructor.
data SearchRoomsResponse = SearchRoomsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The rooms that meet the specified set of filter criteria, in sort order.
    rooms :: Core.Maybe [Types.RoomData],
    -- | The total number of rooms returned.
    totalCount :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchRoomsResponse' value with any optional fields omitted.
mkSearchRoomsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SearchRoomsResponse
mkSearchRoomsResponse responseStatus =
  SearchRoomsResponse'
    { nextToken = Core.Nothing,
      rooms = Core.Nothing,
      totalCount = Core.Nothing,
      responseStatus
    }

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsNextToken :: Lens.Lens' SearchRoomsResponse (Core.Maybe Types.NextToken)
srrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED srrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The rooms that meet the specified set of filter criteria, in sort order.
--
-- /Note:/ Consider using 'rooms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsRooms :: Lens.Lens' SearchRoomsResponse (Core.Maybe [Types.RoomData])
srrrsRooms = Lens.field @"rooms"
{-# DEPRECATED srrrsRooms "Use generic-lens or generic-optics with 'rooms' instead." #-}

-- | The total number of rooms returned.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsTotalCount :: Lens.Lens' SearchRoomsResponse (Core.Maybe Core.Int)
srrrsTotalCount = Lens.field @"totalCount"
{-# DEPRECATED srrrsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrrsResponseStatus :: Lens.Lens' SearchRoomsResponse Core.Int
srrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
