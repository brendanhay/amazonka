{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListCandidatesForAutoMLJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the Candidates created for the job.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListCandidatesForAutoMLJob
  ( -- * Creating a request
    ListCandidatesForAutoMLJob (..),
    mkListCandidatesForAutoMLJob,

    -- ** Request lenses
    lcfamljAutoMLJobName,
    lcfamljCandidateNameEquals,
    lcfamljMaxResults,
    lcfamljNextToken,
    lcfamljSortBy,
    lcfamljSortOrder,
    lcfamljStatusEquals,

    -- * Destructuring the response
    ListCandidatesForAutoMLJobResponse (..),
    mkListCandidatesForAutoMLJobResponse,

    -- ** Response lenses
    lcfamljrrsCandidates,
    lcfamljrrsNextToken,
    lcfamljrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListCandidatesForAutoMLJob' smart constructor.
data ListCandidatesForAutoMLJob = ListCandidatesForAutoMLJob'
  { -- | List the Candidates created for the job by providing the job's name.
    autoMLJobName :: Types.AutoMLJobName,
    -- | List the Candidates for the job and filter by candidate name.
    candidateNameEquals :: Core.Maybe Types.CandidateNameEquals,
    -- | List the job's Candidates up to a specified limit.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The parameter by which to sort the results. The default is Descending.
    sortBy :: Core.Maybe Types.CandidateSortBy,
    -- | The sort order for the results. The default is Ascending.
    sortOrder :: Core.Maybe Types.AutoMLSortOrder,
    -- | List the Candidates for the job and filter by status.
    statusEquals :: Core.Maybe Types.CandidateStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCandidatesForAutoMLJob' value with any optional fields omitted.
mkListCandidatesForAutoMLJob ::
  -- | 'autoMLJobName'
  Types.AutoMLJobName ->
  ListCandidatesForAutoMLJob
mkListCandidatesForAutoMLJob autoMLJobName =
  ListCandidatesForAutoMLJob'
    { autoMLJobName,
      candidateNameEquals = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing,
      statusEquals = Core.Nothing
    }

-- | List the Candidates created for the job by providing the job's name.
--
-- /Note:/ Consider using 'autoMLJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljAutoMLJobName :: Lens.Lens' ListCandidatesForAutoMLJob Types.AutoMLJobName
lcfamljAutoMLJobName = Lens.field @"autoMLJobName"
{-# DEPRECATED lcfamljAutoMLJobName "Use generic-lens or generic-optics with 'autoMLJobName' instead." #-}

-- | List the Candidates for the job and filter by candidate name.
--
-- /Note:/ Consider using 'candidateNameEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljCandidateNameEquals :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Types.CandidateNameEquals)
lcfamljCandidateNameEquals = Lens.field @"candidateNameEquals"
{-# DEPRECATED lcfamljCandidateNameEquals "Use generic-lens or generic-optics with 'candidateNameEquals' instead." #-}

-- | List the job's Candidates up to a specified limit.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljMaxResults :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Core.Natural)
lcfamljMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcfamljMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljNextToken :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Types.NextToken)
lcfamljNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcfamljNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The parameter by which to sort the results. The default is Descending.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljSortBy :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Types.CandidateSortBy)
lcfamljSortBy = Lens.field @"sortBy"
{-# DEPRECATED lcfamljSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for the results. The default is Ascending.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljSortOrder :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Types.AutoMLSortOrder)
lcfamljSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lcfamljSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | List the Candidates for the job and filter by status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljStatusEquals :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Types.CandidateStatus)
lcfamljStatusEquals = Lens.field @"statusEquals"
{-# DEPRECATED lcfamljStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

instance Core.FromJSON ListCandidatesForAutoMLJob where
  toJSON ListCandidatesForAutoMLJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AutoMLJobName" Core..= autoMLJobName),
            ("CandidateNameEquals" Core..=) Core.<$> candidateNameEquals,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("StatusEquals" Core..=) Core.<$> statusEquals
          ]
      )

instance Core.AWSRequest ListCandidatesForAutoMLJob where
  type
    Rs ListCandidatesForAutoMLJob =
      ListCandidatesForAutoMLJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListCandidatesForAutoMLJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCandidatesForAutoMLJobResponse'
            Core.<$> (x Core..:? "Candidates" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListCandidatesForAutoMLJob where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"candidates") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListCandidatesForAutoMLJobResponse' smart constructor.
data ListCandidatesForAutoMLJobResponse = ListCandidatesForAutoMLJobResponse'
  { -- | Summaries about the Candidates.
    candidates :: [Types.AutoMLCandidate],
    -- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListCandidatesForAutoMLJobResponse' value with any optional fields omitted.
mkListCandidatesForAutoMLJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCandidatesForAutoMLJobResponse
mkListCandidatesForAutoMLJobResponse responseStatus =
  ListCandidatesForAutoMLJobResponse'
    { candidates = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Summaries about the Candidates.
--
-- /Note:/ Consider using 'candidates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljrrsCandidates :: Lens.Lens' ListCandidatesForAutoMLJobResponse [Types.AutoMLCandidate]
lcfamljrrsCandidates = Lens.field @"candidates"
{-# DEPRECATED lcfamljrrsCandidates "Use generic-lens or generic-optics with 'candidates' instead." #-}

-- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljrrsNextToken :: Lens.Lens' ListCandidatesForAutoMLJobResponse (Core.Maybe Types.NextToken)
lcfamljrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcfamljrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljrrsResponseStatus :: Lens.Lens' ListCandidatesForAutoMLJobResponse Core.Int
lcfamljrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcfamljrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
