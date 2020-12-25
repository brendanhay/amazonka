{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListEntitiesDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the entity detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListEntitiesDetectionJobs
  ( -- * Creating a request
    ListEntitiesDetectionJobs (..),
    mkListEntitiesDetectionJobs,

    -- ** Request lenses
    ledjFilter,
    ledjMaxResults,
    ledjNextToken,

    -- * Destructuring the response
    ListEntitiesDetectionJobsResponse (..),
    mkListEntitiesDetectionJobsResponse,

    -- ** Response lenses
    ledjrrsEntitiesDetectionJobPropertiesList,
    ledjrrsNextToken,
    ledjrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListEntitiesDetectionJobs' smart constructor.
data ListEntitiesDetectionJobs = ListEntitiesDetectionJobs'
  { -- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
    filter :: Core.Maybe Types.EntitiesDetectionJobFilter,
    -- | The maximum number of results to return in each page. The default is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListEntitiesDetectionJobs' value with any optional fields omitted.
mkListEntitiesDetectionJobs ::
  ListEntitiesDetectionJobs
mkListEntitiesDetectionJobs =
  ListEntitiesDetectionJobs'
    { filter = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ledjFilter :: Lens.Lens' ListEntitiesDetectionJobs (Core.Maybe Types.EntitiesDetectionJobFilter)
ledjFilter = Lens.field @"filter"
{-# DEPRECATED ledjFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ledjMaxResults :: Lens.Lens' ListEntitiesDetectionJobs (Core.Maybe Core.Natural)
ledjMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ledjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ledjNextToken :: Lens.Lens' ListEntitiesDetectionJobs (Core.Maybe Types.String)
ledjNextToken = Lens.field @"nextToken"
{-# DEPRECATED ledjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListEntitiesDetectionJobs where
  toJSON ListEntitiesDetectionJobs {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filter" Core..=) Core.<$> filter,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListEntitiesDetectionJobs where
  type
    Rs ListEntitiesDetectionJobs =
      ListEntitiesDetectionJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Comprehend_20171127.ListEntitiesDetectionJobs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEntitiesDetectionJobsResponse'
            Core.<$> (x Core..:? "EntitiesDetectionJobPropertiesList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListEntitiesDetectionJobs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"entitiesDetectionJobPropertiesList" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListEntitiesDetectionJobsResponse' smart constructor.
data ListEntitiesDetectionJobsResponse = ListEntitiesDetectionJobsResponse'
  { -- | A list containing the properties of each job that is returned.
    entitiesDetectionJobPropertiesList :: Core.Maybe [Types.EntitiesDetectionJobProperties],
    -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListEntitiesDetectionJobsResponse' value with any optional fields omitted.
mkListEntitiesDetectionJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListEntitiesDetectionJobsResponse
mkListEntitiesDetectionJobsResponse responseStatus =
  ListEntitiesDetectionJobsResponse'
    { entitiesDetectionJobPropertiesList =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'entitiesDetectionJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ledjrrsEntitiesDetectionJobPropertiesList :: Lens.Lens' ListEntitiesDetectionJobsResponse (Core.Maybe [Types.EntitiesDetectionJobProperties])
ledjrrsEntitiesDetectionJobPropertiesList = Lens.field @"entitiesDetectionJobPropertiesList"
{-# DEPRECATED ledjrrsEntitiesDetectionJobPropertiesList "Use generic-lens or generic-optics with 'entitiesDetectionJobPropertiesList' instead." #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ledjrrsNextToken :: Lens.Lens' ListEntitiesDetectionJobsResponse (Core.Maybe Types.NextToken)
ledjrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ledjrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ledjrrsResponseStatus :: Lens.Lens' ListEntitiesDetectionJobsResponse Core.Int
ledjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ledjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
