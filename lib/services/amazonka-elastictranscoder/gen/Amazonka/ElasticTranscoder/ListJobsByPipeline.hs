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
-- Module      : Amazonka.ElasticTranscoder.ListJobsByPipeline
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.ElasticTranscoder.ListJobsByPipeline
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The @ListJobsByPipelineRequest@ structure.
--
-- /See:/ 'newListJobsByPipeline' smart constructor.
data ListJobsByPipeline = ListJobsByPipeline'
  { -- | To list jobs in chronological order by the date and time that they were
    -- submitted, enter @true@. To list jobs in reverse chronological order,
    -- enter @false@.
    ascending :: Prelude.Maybe Prelude.Text,
    -- | When Elastic Transcoder returns more than one page of results, use
    -- @pageToken@ in subsequent @GET@ requests to get each successive page of
    -- results.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the pipeline for which you want to get job information.
    pipelineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListJobsByPipeline
newListJobsByPipeline pPipelineId_ =
  ListJobsByPipeline'
    { ascending = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      pipelineId = pPipelineId_
    }

-- | To list jobs in chronological order by the date and time that they were
-- submitted, enter @true@. To list jobs in reverse chronological order,
-- enter @false@.
listJobsByPipeline_ascending :: Lens.Lens' ListJobsByPipeline (Prelude.Maybe Prelude.Text)
listJobsByPipeline_ascending = Lens.lens (\ListJobsByPipeline' {ascending} -> ascending) (\s@ListJobsByPipeline' {} a -> s {ascending = a} :: ListJobsByPipeline)

-- | When Elastic Transcoder returns more than one page of results, use
-- @pageToken@ in subsequent @GET@ requests to get each successive page of
-- results.
listJobsByPipeline_pageToken :: Lens.Lens' ListJobsByPipeline (Prelude.Maybe Prelude.Text)
listJobsByPipeline_pageToken = Lens.lens (\ListJobsByPipeline' {pageToken} -> pageToken) (\s@ListJobsByPipeline' {} a -> s {pageToken = a} :: ListJobsByPipeline)

-- | The ID of the pipeline for which you want to get job information.
listJobsByPipeline_pipelineId :: Lens.Lens' ListJobsByPipeline Prelude.Text
listJobsByPipeline_pipelineId = Lens.lens (\ListJobsByPipeline' {pipelineId} -> pipelineId) (\s@ListJobsByPipeline' {} a -> s {pipelineId = a} :: ListJobsByPipeline)

instance Core.AWSPager ListJobsByPipeline where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobsByPipelineResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobsByPipelineResponse_jobs Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listJobsByPipeline_pageToken
          Lens..~ rs
          Lens.^? listJobsByPipelineResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListJobsByPipeline where
  type
    AWSResponse ListJobsByPipeline =
      ListJobsByPipelineResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsByPipelineResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> (x Data..?> "Jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobsByPipeline where
  hashWithSalt _salt ListJobsByPipeline' {..} =
    _salt `Prelude.hashWithSalt` ascending
      `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` pipelineId

instance Prelude.NFData ListJobsByPipeline where
  rnf ListJobsByPipeline' {..} =
    Prelude.rnf ascending
      `Prelude.seq` Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf pipelineId

instance Data.ToHeaders ListJobsByPipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListJobsByPipeline where
  toPath ListJobsByPipeline' {..} =
    Prelude.mconcat
      ["/2012-09-25/jobsByPipeline/", Data.toBS pipelineId]

instance Data.ToQuery ListJobsByPipeline where
  toQuery ListJobsByPipeline' {..} =
    Prelude.mconcat
      [ "Ascending" Data.=: ascending,
        "PageToken" Data.=: pageToken
      ]

-- | The @ListJobsByPipelineResponse@ structure.
--
-- /See:/ 'newListJobsByPipelineResponse' smart constructor.
data ListJobsByPipelineResponse = ListJobsByPipelineResponse'
  { -- | A value that you use to access the second and subsequent pages of
    -- results, if any. When the jobs in the specified pipeline fit on one page
    -- or when you\'ve reached the last page of results, the value of
    -- @NextPageToken@ is @null@.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @Job@ objects that are in the specified pipeline.
    jobs :: Prelude.Maybe [Job],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListJobsByPipelineResponse
newListJobsByPipelineResponse pHttpStatus_ =
  ListJobsByPipelineResponse'
    { nextPageToken =
        Prelude.Nothing,
      jobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that you use to access the second and subsequent pages of
-- results, if any. When the jobs in the specified pipeline fit on one page
-- or when you\'ve reached the last page of results, the value of
-- @NextPageToken@ is @null@.
listJobsByPipelineResponse_nextPageToken :: Lens.Lens' ListJobsByPipelineResponse (Prelude.Maybe Prelude.Text)
listJobsByPipelineResponse_nextPageToken = Lens.lens (\ListJobsByPipelineResponse' {nextPageToken} -> nextPageToken) (\s@ListJobsByPipelineResponse' {} a -> s {nextPageToken = a} :: ListJobsByPipelineResponse)

-- | An array of @Job@ objects that are in the specified pipeline.
listJobsByPipelineResponse_jobs :: Lens.Lens' ListJobsByPipelineResponse (Prelude.Maybe [Job])
listJobsByPipelineResponse_jobs = Lens.lens (\ListJobsByPipelineResponse' {jobs} -> jobs) (\s@ListJobsByPipelineResponse' {} a -> s {jobs = a} :: ListJobsByPipelineResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listJobsByPipelineResponse_httpStatus :: Lens.Lens' ListJobsByPipelineResponse Prelude.Int
listJobsByPipelineResponse_httpStatus = Lens.lens (\ListJobsByPipelineResponse' {httpStatus} -> httpStatus) (\s@ListJobsByPipelineResponse' {} a -> s {httpStatus = a} :: ListJobsByPipelineResponse)

instance Prelude.NFData ListJobsByPipelineResponse where
  rnf ListJobsByPipelineResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf jobs
      `Prelude.seq` Prelude.rnf httpStatus
