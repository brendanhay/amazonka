{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ljbpAscending,
    ljbpPageToken,
    ljbpPipelineId,

    -- * Destructuring the response
    ListJobsByPipelineResponse (..),
    mkListJobsByPipelineResponse,

    -- ** Response lenses
    ljbprsNextPageToken,
    ljbprsJobs,
    ljbprsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @ListJobsByPipelineRequest@ structure.
--
-- /See:/ 'mkListJobsByPipeline' smart constructor.
data ListJobsByPipeline = ListJobsByPipeline'
  { ascending ::
      Lude.Maybe Lude.Text,
    pageToken :: Lude.Maybe Lude.Text,
    pipelineId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobsByPipeline' with the minimum fields required to make a request.
--
-- * 'ascending' - To list jobs in chronological order by the date and time that they were submitted, enter @true@ . To list jobs in reverse chronological order, enter @false@ .
-- * 'pageToken' - When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
-- * 'pipelineId' - The ID of the pipeline for which you want to get job information.
mkListJobsByPipeline ::
  -- | 'pipelineId'
  Lude.Text ->
  ListJobsByPipeline
mkListJobsByPipeline pPipelineId_ =
  ListJobsByPipeline'
    { ascending = Lude.Nothing,
      pageToken = Lude.Nothing,
      pipelineId = pPipelineId_
    }

-- | To list jobs in chronological order by the date and time that they were submitted, enter @true@ . To list jobs in reverse chronological order, enter @false@ .
--
-- /Note:/ Consider using 'ascending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbpAscending :: Lens.Lens' ListJobsByPipeline (Lude.Maybe Lude.Text)
ljbpAscending = Lens.lens (ascending :: ListJobsByPipeline -> Lude.Maybe Lude.Text) (\s a -> s {ascending = a} :: ListJobsByPipeline)
{-# DEPRECATED ljbpAscending "Use generic-lens or generic-optics with 'ascending' instead." #-}

-- | When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbpPageToken :: Lens.Lens' ListJobsByPipeline (Lude.Maybe Lude.Text)
ljbpPageToken = Lens.lens (pageToken :: ListJobsByPipeline -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListJobsByPipeline)
{-# DEPRECATED ljbpPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The ID of the pipeline for which you want to get job information.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbpPipelineId :: Lens.Lens' ListJobsByPipeline Lude.Text
ljbpPipelineId = Lens.lens (pipelineId :: ListJobsByPipeline -> Lude.Text) (\s a -> s {pipelineId = a} :: ListJobsByPipeline)
{-# DEPRECATED ljbpPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

instance Page.AWSPager ListJobsByPipeline where
  page rq rs
    | Page.stop (rs Lens.^. ljbprsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ljbprsJobs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ljbpPageToken Lens..~ rs Lens.^. ljbprsNextPageToken

instance Lude.AWSRequest ListJobsByPipeline where
  type Rs ListJobsByPipeline = ListJobsByPipelineResponse
  request = Req.get elasticTranscoderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListJobsByPipelineResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "Jobs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListJobsByPipeline where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListJobsByPipeline where
  toPath ListJobsByPipeline' {..} =
    Lude.mconcat
      ["/2012-09-25/jobsByPipeline/", Lude.toBS pipelineId]

instance Lude.ToQuery ListJobsByPipeline where
  toQuery ListJobsByPipeline' {..} =
    Lude.mconcat
      ["Ascending" Lude.=: ascending, "PageToken" Lude.=: pageToken]

-- | The @ListJobsByPipelineResponse@ structure.
--
-- /See:/ 'mkListJobsByPipelineResponse' smart constructor.
data ListJobsByPipelineResponse = ListJobsByPipelineResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    jobs :: Lude.Maybe [Job'],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobsByPipelineResponse' with the minimum fields required to make a request.
--
-- * 'jobs' - An array of @Job@ objects that are in the specified pipeline.
-- * 'nextPageToken' - A value that you use to access the second and subsequent pages of results, if any. When the jobs in the specified pipeline fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
-- * 'responseStatus' - The response status code.
mkListJobsByPipelineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListJobsByPipelineResponse
mkListJobsByPipelineResponse pResponseStatus_ =
  ListJobsByPipelineResponse'
    { nextPageToken = Lude.Nothing,
      jobs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that you use to access the second and subsequent pages of results, if any. When the jobs in the specified pipeline fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbprsNextPageToken :: Lens.Lens' ListJobsByPipelineResponse (Lude.Maybe Lude.Text)
ljbprsNextPageToken = Lens.lens (nextPageToken :: ListJobsByPipelineResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListJobsByPipelineResponse)
{-# DEPRECATED ljbprsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of @Job@ objects that are in the specified pipeline.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbprsJobs :: Lens.Lens' ListJobsByPipelineResponse (Lude.Maybe [Job'])
ljbprsJobs = Lens.lens (jobs :: ListJobsByPipelineResponse -> Lude.Maybe [Job']) (\s a -> s {jobs = a} :: ListJobsByPipelineResponse)
{-# DEPRECATED ljbprsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbprsResponseStatus :: Lens.Lens' ListJobsByPipelineResponse Lude.Int
ljbprsResponseStatus = Lens.lens (responseStatus :: ListJobsByPipelineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListJobsByPipelineResponse)
{-# DEPRECATED ljbprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
