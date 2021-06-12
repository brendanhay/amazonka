{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ListJobsByPipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListJobsByPipeline operation gets a list of the jobs currently in a
-- pipeline.
--
-- Elastic Transcoder returns all of the jobs currently in the specified
-- pipeline. The response body contains one element for each job that
-- satisfies the search criteria.
--
-- This operation returns paginated results.
module Network.AWS.ElasticTranscoder.ListJobsByPipeline
  ( -- * Creating a Request
    ListJobsByPipeline (..),
    newListJobsByPipeline,

    -- * Request Lenses
    listJobsByPipeline_ascending,
    listJobsByPipeline_pageToken,
    listJobsByPipeline_pipelineId,

    -- * Destructuring the Response
    ListJobsByPipelineResponse (..),
    newListJobsByPipelineResponse,

    -- * Response Lenses
    listJobsByPipelineResponse_nextPageToken,
    listJobsByPipelineResponse_jobs,
    listJobsByPipelineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @ListJobsByPipelineRequest@ structure.
--
-- /See:/ 'newListJobsByPipeline' smart constructor.
data ListJobsByPipeline = ListJobsByPipeline'
  { -- | To list jobs in chronological order by the date and time that they were
    -- submitted, enter @true@. To list jobs in reverse chronological order,
    -- enter @false@.
    ascending :: Core.Maybe Core.Text,
    -- | When Elastic Transcoder returns more than one page of results, use
    -- @pageToken@ in subsequent @GET@ requests to get each successive page of
    -- results.
    pageToken :: Core.Maybe Core.Text,
    -- | The ID of the pipeline for which you want to get job information.
    pipelineId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListJobsByPipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ascending', 'listJobsByPipeline_ascending' - To list jobs in chronological order by the date and time that they were
-- submitted, enter @true@. To list jobs in reverse chronological order,
-- enter @false@.
--
-- 'pageToken', 'listJobsByPipeline_pageToken' - When Elastic Transcoder returns more than one page of results, use
-- @pageToken@ in subsequent @GET@ requests to get each successive page of
-- results.
--
-- 'pipelineId', 'listJobsByPipeline_pipelineId' - The ID of the pipeline for which you want to get job information.
newListJobsByPipeline ::
  -- | 'pipelineId'
  Core.Text ->
  ListJobsByPipeline
newListJobsByPipeline pPipelineId_ =
  ListJobsByPipeline'
    { ascending = Core.Nothing,
      pageToken = Core.Nothing,
      pipelineId = pPipelineId_
    }

-- | To list jobs in chronological order by the date and time that they were
-- submitted, enter @true@. To list jobs in reverse chronological order,
-- enter @false@.
listJobsByPipeline_ascending :: Lens.Lens' ListJobsByPipeline (Core.Maybe Core.Text)
listJobsByPipeline_ascending = Lens.lens (\ListJobsByPipeline' {ascending} -> ascending) (\s@ListJobsByPipeline' {} a -> s {ascending = a} :: ListJobsByPipeline)

-- | When Elastic Transcoder returns more than one page of results, use
-- @pageToken@ in subsequent @GET@ requests to get each successive page of
-- results.
listJobsByPipeline_pageToken :: Lens.Lens' ListJobsByPipeline (Core.Maybe Core.Text)
listJobsByPipeline_pageToken = Lens.lens (\ListJobsByPipeline' {pageToken} -> pageToken) (\s@ListJobsByPipeline' {} a -> s {pageToken = a} :: ListJobsByPipeline)

-- | The ID of the pipeline for which you want to get job information.
listJobsByPipeline_pipelineId :: Lens.Lens' ListJobsByPipeline Core.Text
listJobsByPipeline_pipelineId = Lens.lens (\ListJobsByPipeline' {pipelineId} -> pipelineId) (\s@ListJobsByPipeline' {} a -> s {pipelineId = a} :: ListJobsByPipeline)

instance Core.AWSPager ListJobsByPipeline where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobsByPipelineResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobsByPipelineResponse_jobs Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listJobsByPipeline_pageToken
          Lens..~ rs
          Lens.^? listJobsByPipelineResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest ListJobsByPipeline where
  type
    AWSResponse ListJobsByPipeline =
      ListJobsByPipelineResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsByPipelineResponse'
            Core.<$> (x Core..?> "NextPageToken")
            Core.<*> (x Core..?> "Jobs" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListJobsByPipeline

instance Core.NFData ListJobsByPipeline

instance Core.ToHeaders ListJobsByPipeline where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListJobsByPipeline where
  toPath ListJobsByPipeline' {..} =
    Core.mconcat
      ["/2012-09-25/jobsByPipeline/", Core.toBS pipelineId]

instance Core.ToQuery ListJobsByPipeline where
  toQuery ListJobsByPipeline' {..} =
    Core.mconcat
      [ "Ascending" Core.=: ascending,
        "PageToken" Core.=: pageToken
      ]

-- | The @ListJobsByPipelineResponse@ structure.
--
-- /See:/ 'newListJobsByPipelineResponse' smart constructor.
data ListJobsByPipelineResponse = ListJobsByPipelineResponse'
  { -- | A value that you use to access the second and subsequent pages of
    -- results, if any. When the jobs in the specified pipeline fit on one page
    -- or when you\'ve reached the last page of results, the value of
    -- @NextPageToken@ is @null@.
    nextPageToken :: Core.Maybe Core.Text,
    -- | An array of @Job@ objects that are in the specified pipeline.
    jobs :: Core.Maybe [Job'],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListJobsByPipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listJobsByPipelineResponse_nextPageToken' - A value that you use to access the second and subsequent pages of
-- results, if any. When the jobs in the specified pipeline fit on one page
-- or when you\'ve reached the last page of results, the value of
-- @NextPageToken@ is @null@.
--
-- 'jobs', 'listJobsByPipelineResponse_jobs' - An array of @Job@ objects that are in the specified pipeline.
--
-- 'httpStatus', 'listJobsByPipelineResponse_httpStatus' - The response's http status code.
newListJobsByPipelineResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListJobsByPipelineResponse
newListJobsByPipelineResponse pHttpStatus_ =
  ListJobsByPipelineResponse'
    { nextPageToken =
        Core.Nothing,
      jobs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that you use to access the second and subsequent pages of
-- results, if any. When the jobs in the specified pipeline fit on one page
-- or when you\'ve reached the last page of results, the value of
-- @NextPageToken@ is @null@.
listJobsByPipelineResponse_nextPageToken :: Lens.Lens' ListJobsByPipelineResponse (Core.Maybe Core.Text)
listJobsByPipelineResponse_nextPageToken = Lens.lens (\ListJobsByPipelineResponse' {nextPageToken} -> nextPageToken) (\s@ListJobsByPipelineResponse' {} a -> s {nextPageToken = a} :: ListJobsByPipelineResponse)

-- | An array of @Job@ objects that are in the specified pipeline.
listJobsByPipelineResponse_jobs :: Lens.Lens' ListJobsByPipelineResponse (Core.Maybe [Job'])
listJobsByPipelineResponse_jobs = Lens.lens (\ListJobsByPipelineResponse' {jobs} -> jobs) (\s@ListJobsByPipelineResponse' {} a -> s {jobs = a} :: ListJobsByPipelineResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listJobsByPipelineResponse_httpStatus :: Lens.Lens' ListJobsByPipelineResponse Core.Int
listJobsByPipelineResponse_httpStatus = Lens.lens (\ListJobsByPipelineResponse' {httpStatus} -> httpStatus) (\s@ListJobsByPipelineResponse' {} a -> s {httpStatus = a} :: ListJobsByPipelineResponse)

instance Core.NFData ListJobsByPipelineResponse
