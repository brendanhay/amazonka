{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListDocumentClassificationJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the documentation classification jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListDocumentClassificationJobs
  ( -- * Creating a request
    ListDocumentClassificationJobs (..),
    mkListDocumentClassificationJobs,

    -- ** Request lenses
    ldcjFilter,
    ldcjMaxResults,
    ldcjNextToken,

    -- * Destructuring the response
    ListDocumentClassificationJobsResponse (..),
    mkListDocumentClassificationJobsResponse,

    -- ** Response lenses
    ldcjrrsDocumentClassificationJobPropertiesList,
    ldcjrrsNextToken,
    ldcjrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDocumentClassificationJobs' smart constructor.
data ListDocumentClassificationJobs = ListDocumentClassificationJobs'
  { -- | Filters the jobs that are returned. You can filter jobs on their names, status, or the date and time that they were submitted. You can only set one filter at a time.
    filter :: Core.Maybe Types.DocumentClassificationJobFilter,
    -- | The maximum number of results to return in each page. The default is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDocumentClassificationJobs' value with any optional fields omitted.
mkListDocumentClassificationJobs ::
  ListDocumentClassificationJobs
mkListDocumentClassificationJobs =
  ListDocumentClassificationJobs'
    { filter = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Filters the jobs that are returned. You can filter jobs on their names, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcjFilter :: Lens.Lens' ListDocumentClassificationJobs (Core.Maybe Types.DocumentClassificationJobFilter)
ldcjFilter = Lens.field @"filter"
{-# DEPRECATED ldcjFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcjMaxResults :: Lens.Lens' ListDocumentClassificationJobs (Core.Maybe Core.Natural)
ldcjMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ldcjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcjNextToken :: Lens.Lens' ListDocumentClassificationJobs (Core.Maybe Types.String)
ldcjNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldcjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListDocumentClassificationJobs where
  toJSON ListDocumentClassificationJobs {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filter" Core..=) Core.<$> filter,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListDocumentClassificationJobs where
  type
    Rs ListDocumentClassificationJobs =
      ListDocumentClassificationJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Comprehend_20171127.ListDocumentClassificationJobs"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDocumentClassificationJobsResponse'
            Core.<$> (x Core..:? "DocumentClassificationJobPropertiesList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDocumentClassificationJobs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"documentClassificationJobPropertiesList"
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListDocumentClassificationJobsResponse' smart constructor.
data ListDocumentClassificationJobsResponse = ListDocumentClassificationJobsResponse'
  { -- | A list containing the properties of each job returned.
    documentClassificationJobPropertiesList :: Core.Maybe [Types.DocumentClassificationJobProperties],
    -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDocumentClassificationJobsResponse' value with any optional fields omitted.
mkListDocumentClassificationJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDocumentClassificationJobsResponse
mkListDocumentClassificationJobsResponse responseStatus =
  ListDocumentClassificationJobsResponse'
    { documentClassificationJobPropertiesList =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list containing the properties of each job returned.
--
-- /Note:/ Consider using 'documentClassificationJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcjrrsDocumentClassificationJobPropertiesList :: Lens.Lens' ListDocumentClassificationJobsResponse (Core.Maybe [Types.DocumentClassificationJobProperties])
ldcjrrsDocumentClassificationJobPropertiesList = Lens.field @"documentClassificationJobPropertiesList"
{-# DEPRECATED ldcjrrsDocumentClassificationJobPropertiesList "Use generic-lens or generic-optics with 'documentClassificationJobPropertiesList' instead." #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcjrrsNextToken :: Lens.Lens' ListDocumentClassificationJobsResponse (Core.Maybe Types.String)
ldcjrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldcjrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcjrrsResponseStatus :: Lens.Lens' ListDocumentClassificationJobsResponse Core.Int
ldcjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldcjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
