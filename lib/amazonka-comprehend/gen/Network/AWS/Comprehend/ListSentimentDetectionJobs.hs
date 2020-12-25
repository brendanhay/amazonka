{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListSentimentDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of sentiment detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListSentimentDetectionJobs
  ( -- * Creating a request
    ListSentimentDetectionJobs (..),
    mkListSentimentDetectionJobs,

    -- ** Request lenses
    lsdjFilter,
    lsdjMaxResults,
    lsdjNextToken,

    -- * Destructuring the response
    ListSentimentDetectionJobsResponse (..),
    mkListSentimentDetectionJobsResponse,

    -- ** Response lenses
    lsdjrrsNextToken,
    lsdjrrsSentimentDetectionJobPropertiesList,
    lsdjrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSentimentDetectionJobs' smart constructor.
data ListSentimentDetectionJobs = ListSentimentDetectionJobs'
  { -- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
    filter :: Core.Maybe Types.SentimentDetectionJobFilter,
    -- | The maximum number of results to return in each page. The default is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListSentimentDetectionJobs' value with any optional fields omitted.
mkListSentimentDetectionJobs ::
  ListSentimentDetectionJobs
mkListSentimentDetectionJobs =
  ListSentimentDetectionJobs'
    { filter = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdjFilter :: Lens.Lens' ListSentimentDetectionJobs (Core.Maybe Types.SentimentDetectionJobFilter)
lsdjFilter = Lens.field @"filter"
{-# DEPRECATED lsdjFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdjMaxResults :: Lens.Lens' ListSentimentDetectionJobs (Core.Maybe Core.Natural)
lsdjMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lsdjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdjNextToken :: Lens.Lens' ListSentimentDetectionJobs (Core.Maybe Types.String)
lsdjNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsdjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListSentimentDetectionJobs where
  toJSON ListSentimentDetectionJobs {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filter" Core..=) Core.<$> filter,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListSentimentDetectionJobs where
  type
    Rs ListSentimentDetectionJobs =
      ListSentimentDetectionJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Comprehend_20171127.ListSentimentDetectionJobs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSentimentDetectionJobsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "SentimentDetectionJobPropertiesList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSentimentDetectionJobs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"sentimentDetectionJobPropertiesList"
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListSentimentDetectionJobsResponse' smart constructor.
data ListSentimentDetectionJobsResponse = ListSentimentDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Types.String,
    -- | A list containing the properties of each job that is returned.
    sentimentDetectionJobPropertiesList :: Core.Maybe [Types.SentimentDetectionJobProperties],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListSentimentDetectionJobsResponse' value with any optional fields omitted.
mkListSentimentDetectionJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSentimentDetectionJobsResponse
mkListSentimentDetectionJobsResponse responseStatus =
  ListSentimentDetectionJobsResponse'
    { nextToken = Core.Nothing,
      sentimentDetectionJobPropertiesList = Core.Nothing,
      responseStatus
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdjrrsNextToken :: Lens.Lens' ListSentimentDetectionJobsResponse (Core.Maybe Types.String)
lsdjrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsdjrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'sentimentDetectionJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdjrrsSentimentDetectionJobPropertiesList :: Lens.Lens' ListSentimentDetectionJobsResponse (Core.Maybe [Types.SentimentDetectionJobProperties])
lsdjrrsSentimentDetectionJobPropertiesList = Lens.field @"sentimentDetectionJobPropertiesList"
{-# DEPRECATED lsdjrrsSentimentDetectionJobPropertiesList "Use generic-lens or generic-optics with 'sentimentDetectionJobPropertiesList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdjrrsResponseStatus :: Lens.Lens' ListSentimentDetectionJobsResponse Core.Int
lsdjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsdjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
