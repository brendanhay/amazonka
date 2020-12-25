{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListDominantLanguageDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the dominant language detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListDominantLanguageDetectionJobs
  ( -- * Creating a request
    ListDominantLanguageDetectionJobs (..),
    mkListDominantLanguageDetectionJobs,

    -- ** Request lenses
    ldldjFilter,
    ldldjMaxResults,
    ldldjNextToken,

    -- * Destructuring the response
    ListDominantLanguageDetectionJobsResponse (..),
    mkListDominantLanguageDetectionJobsResponse,

    -- ** Response lenses
    ldldjrrsDominantLanguageDetectionJobPropertiesList,
    ldldjrrsNextToken,
    ldldjrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDominantLanguageDetectionJobs' smart constructor.
data ListDominantLanguageDetectionJobs = ListDominantLanguageDetectionJobs'
  { -- | Filters that jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
    filter :: Core.Maybe Types.DominantLanguageDetectionJobFilter,
    -- | The maximum number of results to return in each page. The default is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDominantLanguageDetectionJobs' value with any optional fields omitted.
mkListDominantLanguageDetectionJobs ::
  ListDominantLanguageDetectionJobs
mkListDominantLanguageDetectionJobs =
  ListDominantLanguageDetectionJobs'
    { filter = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Filters that jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjFilter :: Lens.Lens' ListDominantLanguageDetectionJobs (Core.Maybe Types.DominantLanguageDetectionJobFilter)
ldldjFilter = Lens.field @"filter"
{-# DEPRECATED ldldjFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjMaxResults :: Lens.Lens' ListDominantLanguageDetectionJobs (Core.Maybe Core.Natural)
ldldjMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ldldjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjNextToken :: Lens.Lens' ListDominantLanguageDetectionJobs (Core.Maybe Types.String)
ldldjNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldldjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListDominantLanguageDetectionJobs where
  toJSON ListDominantLanguageDetectionJobs {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filter" Core..=) Core.<$> filter,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListDominantLanguageDetectionJobs where
  type
    Rs ListDominantLanguageDetectionJobs =
      ListDominantLanguageDetectionJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Comprehend_20171127.ListDominantLanguageDetectionJobs"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDominantLanguageDetectionJobsResponse'
            Core.<$> (x Core..:? "DominantLanguageDetectionJobPropertiesList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDominantLanguageDetectionJobs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"dominantLanguageDetectionJobPropertiesList"
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListDominantLanguageDetectionJobsResponse' smart constructor.
data ListDominantLanguageDetectionJobsResponse = ListDominantLanguageDetectionJobsResponse'
  { -- | A list containing the properties of each job that is returned.
    dominantLanguageDetectionJobPropertiesList :: Core.Maybe [Types.DominantLanguageDetectionJobProperties],
    -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDominantLanguageDetectionJobsResponse' value with any optional fields omitted.
mkListDominantLanguageDetectionJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDominantLanguageDetectionJobsResponse
mkListDominantLanguageDetectionJobsResponse responseStatus =
  ListDominantLanguageDetectionJobsResponse'
    { dominantLanguageDetectionJobPropertiesList =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'dominantLanguageDetectionJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjrrsDominantLanguageDetectionJobPropertiesList :: Lens.Lens' ListDominantLanguageDetectionJobsResponse (Core.Maybe [Types.DominantLanguageDetectionJobProperties])
ldldjrrsDominantLanguageDetectionJobPropertiesList = Lens.field @"dominantLanguageDetectionJobPropertiesList"
{-# DEPRECATED ldldjrrsDominantLanguageDetectionJobPropertiesList "Use generic-lens or generic-optics with 'dominantLanguageDetectionJobPropertiesList' instead." #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjrrsNextToken :: Lens.Lens' ListDominantLanguageDetectionJobsResponse (Core.Maybe Types.String)
ldldjrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldldjrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjrrsResponseStatus :: Lens.Lens' ListDominantLanguageDetectionJobsResponse Core.Int
ldldjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldldjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
