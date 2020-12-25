{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListWorkteams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of private work teams that you have defined in a region. The list may be empty if no work team satisfies the filter specified in the @NameContains@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListWorkteams
  ( -- * Creating a request
    ListWorkteams (..),
    mkListWorkteams,

    -- ** Request lenses
    lwMaxResults,
    lwNameContains,
    lwNextToken,
    lwSortBy,
    lwSortOrder,

    -- * Destructuring the response
    ListWorkteamsResponse (..),
    mkListWorkteamsResponse,

    -- ** Response lenses
    lrsWorkteams,
    lrsNextToken,
    lrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListWorkteams' smart constructor.
data ListWorkteams = ListWorkteams'
  { -- | The maximum number of work teams to return in each page of the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A string in the work team's name. This filter returns only work teams whose name contains the specified string.
    nameContains :: Core.Maybe Types.NameContains,
    -- | If the result of the previous @ListWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The field to sort results by. The default is @CreationTime@ .
    sortBy :: Core.Maybe Types.ListWorkteamsSortByOptions,
    -- | The sort order for results. The default is @Ascending@ .
    sortOrder :: Core.Maybe Types.SortOrder
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListWorkteams' value with any optional fields omitted.
mkListWorkteams ::
  ListWorkteams
mkListWorkteams =
  ListWorkteams'
    { maxResults = Core.Nothing,
      nameContains = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | The maximum number of work teams to return in each page of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwMaxResults :: Lens.Lens' ListWorkteams (Core.Maybe Core.Natural)
lwMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lwMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A string in the work team's name. This filter returns only work teams whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwNameContains :: Lens.Lens' ListWorkteams (Core.Maybe Types.NameContains)
lwNameContains = Lens.field @"nameContains"
{-# DEPRECATED lwNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of the previous @ListWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwNextToken :: Lens.Lens' ListWorkteams (Core.Maybe Types.NextToken)
lwNextToken = Lens.field @"nextToken"
{-# DEPRECATED lwNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwSortBy :: Lens.Lens' ListWorkteams (Core.Maybe Types.ListWorkteamsSortByOptions)
lwSortBy = Lens.field @"sortBy"
{-# DEPRECATED lwSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwSortOrder :: Lens.Lens' ListWorkteams (Core.Maybe Types.SortOrder)
lwSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lwSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON ListWorkteams where
  toJSON ListWorkteams {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest ListWorkteams where
  type Rs ListWorkteams = ListWorkteamsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListWorkteams")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkteamsResponse'
            Core.<$> (x Core..:? "Workteams" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListWorkteams where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"workteams") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListWorkteamsResponse' smart constructor.
data ListWorkteamsResponse = ListWorkteamsResponse'
  { -- | An array of @Workteam@ objects, each describing a work team.
    workteams :: [Types.Workteam],
    -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListWorkteamsResponse' value with any optional fields omitted.
mkListWorkteamsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListWorkteamsResponse
mkListWorkteamsResponse responseStatus =
  ListWorkteamsResponse'
    { workteams = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @Workteam@ objects, each describing a work team.
--
-- /Note:/ Consider using 'workteams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsWorkteams :: Lens.Lens' ListWorkteamsResponse [Types.Workteam]
lrsWorkteams = Lens.field @"workteams"
{-# DEPRECATED lrsWorkteams "Use generic-lens or generic-optics with 'workteams' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListWorkteamsResponse (Core.Maybe Types.NextToken)
lrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListWorkteamsResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
