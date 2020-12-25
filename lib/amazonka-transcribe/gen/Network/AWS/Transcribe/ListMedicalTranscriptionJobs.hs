{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.ListMedicalTranscriptionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists medical transcription jobs with a specified status or substring that matches their names.
module Network.AWS.Transcribe.ListMedicalTranscriptionJobs
  ( -- * Creating a request
    ListMedicalTranscriptionJobs (..),
    mkListMedicalTranscriptionJobs,

    -- ** Request lenses
    lmtjJobNameContains,
    lmtjMaxResults,
    lmtjNextToken,
    lmtjStatus,

    -- * Destructuring the response
    ListMedicalTranscriptionJobsResponse (..),
    mkListMedicalTranscriptionJobsResponse,

    -- ** Response lenses
    lmtjrrsMedicalTranscriptionJobSummaries,
    lmtjrrsNextToken,
    lmtjrrsStatus,
    lmtjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkListMedicalTranscriptionJobs' smart constructor.
data ListMedicalTranscriptionJobs = ListMedicalTranscriptionJobs'
  { -- | When specified, the jobs returned in the list are limited to jobs whose name contains the specified string.
    jobNameContains :: Core.Maybe Types.JobNameContains,
    -- | The maximum number of medical transcription jobs to return in the response. IF there are fewer results in the list, this response contains only the actual results.
    maxResults :: Core.Maybe Core.Natural,
    -- | If you a receive a truncated result in the previous request of @ListMedicalTranscriptionJobs@ , include @NextToken@ to fetch the next set of jobs.
    nextToken :: Core.Maybe Types.NextToken,
    -- | When specified, returns only medical transcription jobs with the specified status. Jobs are ordered by creation date, with the newest jobs returned first. If you don't specify a status, Amazon Transcribe Medical returns all transcription jobs ordered by creation date.
    status :: Core.Maybe Types.TranscriptionJobStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMedicalTranscriptionJobs' value with any optional fields omitted.
mkListMedicalTranscriptionJobs ::
  ListMedicalTranscriptionJobs
mkListMedicalTranscriptionJobs =
  ListMedicalTranscriptionJobs'
    { jobNameContains = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      status = Core.Nothing
    }

-- | When specified, the jobs returned in the list are limited to jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'jobNameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjJobNameContains :: Lens.Lens' ListMedicalTranscriptionJobs (Core.Maybe Types.JobNameContains)
lmtjJobNameContains = Lens.field @"jobNameContains"
{-# DEPRECATED lmtjJobNameContains "Use generic-lens or generic-optics with 'jobNameContains' instead." #-}

-- | The maximum number of medical transcription jobs to return in the response. IF there are fewer results in the list, this response contains only the actual results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjMaxResults :: Lens.Lens' ListMedicalTranscriptionJobs (Core.Maybe Core.Natural)
lmtjMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lmtjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If you a receive a truncated result in the previous request of @ListMedicalTranscriptionJobs@ , include @NextToken@ to fetch the next set of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjNextToken :: Lens.Lens' ListMedicalTranscriptionJobs (Core.Maybe Types.NextToken)
lmtjNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmtjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | When specified, returns only medical transcription jobs with the specified status. Jobs are ordered by creation date, with the newest jobs returned first. If you don't specify a status, Amazon Transcribe Medical returns all transcription jobs ordered by creation date.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjStatus :: Lens.Lens' ListMedicalTranscriptionJobs (Core.Maybe Types.TranscriptionJobStatus)
lmtjStatus = Lens.field @"status"
{-# DEPRECATED lmtjStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ListMedicalTranscriptionJobs where
  toJSON ListMedicalTranscriptionJobs {..} =
    Core.object
      ( Core.catMaybes
          [ ("JobNameContains" Core..=) Core.<$> jobNameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Status" Core..=) Core.<$> status
          ]
      )

instance Core.AWSRequest ListMedicalTranscriptionJobs where
  type
    Rs ListMedicalTranscriptionJobs =
      ListMedicalTranscriptionJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Transcribe.ListMedicalTranscriptionJobs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMedicalTranscriptionJobsResponse'
            Core.<$> (x Core..:? "MedicalTranscriptionJobSummaries")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListMedicalTranscriptionJobsResponse' smart constructor.
data ListMedicalTranscriptionJobsResponse = ListMedicalTranscriptionJobsResponse'
  { -- | A list of objects containing summary information for a transcription job.
    medicalTranscriptionJobSummaries :: Core.Maybe [Types.MedicalTranscriptionJobSummary],
    -- | The @ListMedicalTranscriptionJobs@ operation returns a page of jobs at a time. The maximum size of the page is set by the @MaxResults@ parameter. If the number of jobs exceeds what can fit on a page, Amazon Transcribe Medical returns the @NextPage@ token. Include the token in the next request to the @ListMedicalTranscriptionJobs@ operation to return in the next page of jobs.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The requested status of the medical transcription jobs returned.
    status :: Core.Maybe Types.TranscriptionJobStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListMedicalTranscriptionJobsResponse' value with any optional fields omitted.
mkListMedicalTranscriptionJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListMedicalTranscriptionJobsResponse
mkListMedicalTranscriptionJobsResponse responseStatus =
  ListMedicalTranscriptionJobsResponse'
    { medicalTranscriptionJobSummaries =
        Core.Nothing,
      nextToken = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | A list of objects containing summary information for a transcription job.
--
-- /Note:/ Consider using 'medicalTranscriptionJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjrrsMedicalTranscriptionJobSummaries :: Lens.Lens' ListMedicalTranscriptionJobsResponse (Core.Maybe [Types.MedicalTranscriptionJobSummary])
lmtjrrsMedicalTranscriptionJobSummaries = Lens.field @"medicalTranscriptionJobSummaries"
{-# DEPRECATED lmtjrrsMedicalTranscriptionJobSummaries "Use generic-lens or generic-optics with 'medicalTranscriptionJobSummaries' instead." #-}

-- | The @ListMedicalTranscriptionJobs@ operation returns a page of jobs at a time. The maximum size of the page is set by the @MaxResults@ parameter. If the number of jobs exceeds what can fit on a page, Amazon Transcribe Medical returns the @NextPage@ token. Include the token in the next request to the @ListMedicalTranscriptionJobs@ operation to return in the next page of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjrrsNextToken :: Lens.Lens' ListMedicalTranscriptionJobsResponse (Core.Maybe Types.NextToken)
lmtjrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmtjrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The requested status of the medical transcription jobs returned.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjrrsStatus :: Lens.Lens' ListMedicalTranscriptionJobsResponse (Core.Maybe Types.TranscriptionJobStatus)
lmtjrrsStatus = Lens.field @"status"
{-# DEPRECATED lmtjrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjrrsResponseStatus :: Lens.Lens' ListMedicalTranscriptionJobsResponse Core.Int
lmtjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lmtjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
