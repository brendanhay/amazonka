{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.ListTextTranslationJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the batch translation jobs that you have submitted.
module Network.AWS.Translate.ListTextTranslationJobs
  ( -- * Creating a request
    ListTextTranslationJobs (..),
    mkListTextTranslationJobs,

    -- ** Request lenses
    lttjFilter,
    lttjMaxResults,
    lttjNextToken,

    -- * Destructuring the response
    ListTextTranslationJobsResponse (..),
    mkListTextTranslationJobsResponse,

    -- ** Response lenses
    lttjrrsNextToken,
    lttjrrsTextTranslationJobPropertiesList,
    lttjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Translate.Types as Types

-- | /See:/ 'mkListTextTranslationJobs' smart constructor.
data ListTextTranslationJobs = ListTextTranslationJobs'
  { -- | The parameters that specify which batch translation jobs to retrieve. Filters include job name, job status, and submission time. You can only set one filter at a time.
    filter :: Core.Maybe Types.TextTranslationJobFilter,
    -- | The maximum number of results to return in each page. The default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to request the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTextTranslationJobs' value with any optional fields omitted.
mkListTextTranslationJobs ::
  ListTextTranslationJobs
mkListTextTranslationJobs =
  ListTextTranslationJobs'
    { filter = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The parameters that specify which batch translation jobs to retrieve. Filters include job name, job status, and submission time. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttjFilter :: Lens.Lens' ListTextTranslationJobs (Core.Maybe Types.TextTranslationJobFilter)
lttjFilter = Lens.field @"filter"
{-# DEPRECATED lttjFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttjMaxResults :: Lens.Lens' ListTextTranslationJobs (Core.Maybe Core.Natural)
lttjMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lttjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttjNextToken :: Lens.Lens' ListTextTranslationJobs (Core.Maybe Types.NextToken)
lttjNextToken = Lens.field @"nextToken"
{-# DEPRECATED lttjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListTextTranslationJobs where
  toJSON ListTextTranslationJobs {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filter" Core..=) Core.<$> filter,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListTextTranslationJobs where
  type Rs ListTextTranslationJobs = ListTextTranslationJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSShineFrontendService_20170701.ListTextTranslationJobs"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTextTranslationJobsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "TextTranslationJobPropertiesList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListTextTranslationJobsResponse' smart constructor.
data ListTextTranslationJobsResponse = ListTextTranslationJobsResponse'
  { -- | The token to use to retreive the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list containing the properties of each job that is returned.
    textTranslationJobPropertiesList :: Core.Maybe [Types.TextTranslationJobProperties],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTextTranslationJobsResponse' value with any optional fields omitted.
mkListTextTranslationJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTextTranslationJobsResponse
mkListTextTranslationJobsResponse responseStatus =
  ListTextTranslationJobsResponse'
    { nextToken = Core.Nothing,
      textTranslationJobPropertiesList = Core.Nothing,
      responseStatus
    }

-- | The token to use to retreive the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttjrrsNextToken :: Lens.Lens' ListTextTranslationJobsResponse (Core.Maybe Types.NextToken)
lttjrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lttjrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'textTranslationJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttjrrsTextTranslationJobPropertiesList :: Lens.Lens' ListTextTranslationJobsResponse (Core.Maybe [Types.TextTranslationJobProperties])
lttjrrsTextTranslationJobPropertiesList = Lens.field @"textTranslationJobPropertiesList"
{-# DEPRECATED lttjrrsTextTranslationJobPropertiesList "Use generic-lens or generic-optics with 'textTranslationJobPropertiesList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttjrrsResponseStatus :: Lens.Lens' ListTextTranslationJobsResponse Core.Int
lttjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lttjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
