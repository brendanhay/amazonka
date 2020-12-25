{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ListJobsByPipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListJobsByPipeline operation gets a list of the jobs currently in a pipeline.
--
-- Elastic Transcoder returns all of the jobs currently in the specified pipeline. The response body contains one element for each job that satisfies the search criteria.
--
-- This operation returns paginated results.
module Network.AWS.ElasticTranscoder.ListJobsByPipeline
  ( -- * Creating a request
    ListJobsByPipeline (..),
    mkListJobsByPipeline,

    -- ** Request lenses
    ljbpPipelineId,
    ljbpAscending,
    ljbpPageToken,

    -- * Destructuring the response
    ListJobsByPipelineResponse (..),
    mkListJobsByPipelineResponse,

    -- ** Response lenses
    ljbprrsJobs,
    ljbprrsNextPageToken,
    ljbprrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @ListJobsByPipelineRequest@ structure.
--
-- /See:/ 'mkListJobsByPipeline' smart constructor.
data ListJobsByPipeline = ListJobsByPipeline'
  { -- | The ID of the pipeline for which you want to get job information.
    pipelineId :: Types.Id,
    -- | To list jobs in chronological order by the date and time that they were submitted, enter @true@ . To list jobs in reverse chronological order, enter @false@ .
    ascending :: Core.Maybe Types.Ascending,
    -- | When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
    pageToken :: Core.Maybe Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobsByPipeline' value with any optional fields omitted.
mkListJobsByPipeline ::
  -- | 'pipelineId'
  Types.Id ->
  ListJobsByPipeline
mkListJobsByPipeline pipelineId =
  ListJobsByPipeline'
    { pipelineId,
      ascending = Core.Nothing,
      pageToken = Core.Nothing
    }

-- | The ID of the pipeline for which you want to get job information.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbpPipelineId :: Lens.Lens' ListJobsByPipeline Types.Id
ljbpPipelineId = Lens.field @"pipelineId"
{-# DEPRECATED ljbpPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | To list jobs in chronological order by the date and time that they were submitted, enter @true@ . To list jobs in reverse chronological order, enter @false@ .
--
-- /Note:/ Consider using 'ascending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbpAscending :: Lens.Lens' ListJobsByPipeline (Core.Maybe Types.Ascending)
ljbpAscending = Lens.field @"ascending"
{-# DEPRECATED ljbpAscending "Use generic-lens or generic-optics with 'ascending' instead." #-}

-- | When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbpPageToken :: Lens.Lens' ListJobsByPipeline (Core.Maybe Types.Id)
ljbpPageToken = Lens.field @"pageToken"
{-# DEPRECATED ljbpPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.AWSRequest ListJobsByPipeline where
  type Rs ListJobsByPipeline = ListJobsByPipelineResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/2012-09-25/jobsByPipeline/" Core.<> (Core.toText pipelineId)),
        Core._rqQuery =
          Core.toQueryValue "Ascending" Core.<$> ascending
            Core.<> (Core.toQueryValue "PageToken" Core.<$> pageToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsByPipelineResponse'
            Core.<$> (x Core..:? "Jobs")
            Core.<*> (x Core..:? "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListJobsByPipeline where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"jobs" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | The @ListJobsByPipelineResponse@ structure.
--
-- /See:/ 'mkListJobsByPipelineResponse' smart constructor.
data ListJobsByPipelineResponse = ListJobsByPipelineResponse'
  { -- | An array of @Job@ objects that are in the specified pipeline.
    jobs :: Core.Maybe [Types.Job'],
    -- | A value that you use to access the second and subsequent pages of results, if any. When the jobs in the specified pipeline fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobsByPipelineResponse' value with any optional fields omitted.
mkListJobsByPipelineResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListJobsByPipelineResponse
mkListJobsByPipelineResponse responseStatus =
  ListJobsByPipelineResponse'
    { jobs = Core.Nothing,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | An array of @Job@ objects that are in the specified pipeline.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbprrsJobs :: Lens.Lens' ListJobsByPipelineResponse (Core.Maybe [Types.Job'])
ljbprrsJobs = Lens.field @"jobs"
{-# DEPRECATED ljbprrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | A value that you use to access the second and subsequent pages of results, if any. When the jobs in the specified pipeline fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbprrsNextPageToken :: Lens.Lens' ListJobsByPipelineResponse (Core.Maybe Types.NextPageToken)
ljbprrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED ljbprrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbprrsResponseStatus :: Lens.Lens' ListJobsByPipelineResponse Core.Int
ljbprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ljbprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
