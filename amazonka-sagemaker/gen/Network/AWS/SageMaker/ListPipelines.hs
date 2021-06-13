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
-- Module      : Network.AWS.SageMaker.ListPipelines
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of pipelines.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListPipelines
  ( -- * Creating a Request
    ListPipelines (..),
    newListPipelines,

    -- * Request Lenses
    listPipelines_createdAfter,
    listPipelines_sortOrder,
    listPipelines_nextToken,
    listPipelines_createdBefore,
    listPipelines_maxResults,
    listPipelines_sortBy,
    listPipelines_pipelineNamePrefix,

    -- * Destructuring the Response
    ListPipelinesResponse (..),
    newListPipelinesResponse,

    -- * Response Lenses
    listPipelinesResponse_nextToken,
    listPipelinesResponse_pipelineSummaries,
    listPipelinesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListPipelines' smart constructor.
data ListPipelines = ListPipelines'
  { -- | A filter that returns the pipelines that were created after a specified
    -- time.
    createdAfter :: Prelude.Maybe Core.POSIX,
    -- | The sort order for results.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the result of the previous @ListPipelines@ request was truncated, the
    -- response includes a @NextToken@. To retrieve the next set of pipelines,
    -- use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns the pipelines that were created before a specified
    -- time.
    createdBefore :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of pipelines to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The field by which to sort results. The default is @CreatedTime@.
    sortBy :: Prelude.Maybe SortPipelinesBy,
    -- | The prefix of the pipeline name.
    pipelineNamePrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPipelines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'listPipelines_createdAfter' - A filter that returns the pipelines that were created after a specified
-- time.
--
-- 'sortOrder', 'listPipelines_sortOrder' - The sort order for results.
--
-- 'nextToken', 'listPipelines_nextToken' - If the result of the previous @ListPipelines@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of pipelines,
-- use the token in the next request.
--
-- 'createdBefore', 'listPipelines_createdBefore' - A filter that returns the pipelines that were created before a specified
-- time.
--
-- 'maxResults', 'listPipelines_maxResults' - The maximum number of pipelines to return in the response.
--
-- 'sortBy', 'listPipelines_sortBy' - The field by which to sort results. The default is @CreatedTime@.
--
-- 'pipelineNamePrefix', 'listPipelines_pipelineNamePrefix' - The prefix of the pipeline name.
newListPipelines ::
  ListPipelines
newListPipelines =
  ListPipelines'
    { createdAfter = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      pipelineNamePrefix = Prelude.Nothing
    }

-- | A filter that returns the pipelines that were created after a specified
-- time.
listPipelines_createdAfter :: Lens.Lens' ListPipelines (Prelude.Maybe Prelude.UTCTime)
listPipelines_createdAfter = Lens.lens (\ListPipelines' {createdAfter} -> createdAfter) (\s@ListPipelines' {} a -> s {createdAfter = a} :: ListPipelines) Prelude.. Lens.mapping Core._Time

-- | The sort order for results.
listPipelines_sortOrder :: Lens.Lens' ListPipelines (Prelude.Maybe SortOrder)
listPipelines_sortOrder = Lens.lens (\ListPipelines' {sortOrder} -> sortOrder) (\s@ListPipelines' {} a -> s {sortOrder = a} :: ListPipelines)

-- | If the result of the previous @ListPipelines@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of pipelines,
-- use the token in the next request.
listPipelines_nextToken :: Lens.Lens' ListPipelines (Prelude.Maybe Prelude.Text)
listPipelines_nextToken = Lens.lens (\ListPipelines' {nextToken} -> nextToken) (\s@ListPipelines' {} a -> s {nextToken = a} :: ListPipelines)

-- | A filter that returns the pipelines that were created before a specified
-- time.
listPipelines_createdBefore :: Lens.Lens' ListPipelines (Prelude.Maybe Prelude.UTCTime)
listPipelines_createdBefore = Lens.lens (\ListPipelines' {createdBefore} -> createdBefore) (\s@ListPipelines' {} a -> s {createdBefore = a} :: ListPipelines) Prelude.. Lens.mapping Core._Time

-- | The maximum number of pipelines to return in the response.
listPipelines_maxResults :: Lens.Lens' ListPipelines (Prelude.Maybe Prelude.Natural)
listPipelines_maxResults = Lens.lens (\ListPipelines' {maxResults} -> maxResults) (\s@ListPipelines' {} a -> s {maxResults = a} :: ListPipelines)

-- | The field by which to sort results. The default is @CreatedTime@.
listPipelines_sortBy :: Lens.Lens' ListPipelines (Prelude.Maybe SortPipelinesBy)
listPipelines_sortBy = Lens.lens (\ListPipelines' {sortBy} -> sortBy) (\s@ListPipelines' {} a -> s {sortBy = a} :: ListPipelines)

-- | The prefix of the pipeline name.
listPipelines_pipelineNamePrefix :: Lens.Lens' ListPipelines (Prelude.Maybe Prelude.Text)
listPipelines_pipelineNamePrefix = Lens.lens (\ListPipelines' {pipelineNamePrefix} -> pipelineNamePrefix) (\s@ListPipelines' {} a -> s {pipelineNamePrefix = a} :: ListPipelines)

instance Core.AWSPager ListPipelines where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPipelinesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPipelinesResponse_pipelineSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPipelines_nextToken
          Lens..~ rs
          Lens.^? listPipelinesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListPipelines where
  type
    AWSResponse ListPipelines =
      ListPipelinesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelinesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "PipelineSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPipelines

instance Prelude.NFData ListPipelines

instance Core.ToHeaders ListPipelines where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListPipelines" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPipelines where
  toJSON ListPipelines' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CreatedAfter" Core..=) Prelude.<$> createdAfter,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("CreatedBefore" Core..=) Prelude.<$> createdBefore,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("PipelineNamePrefix" Core..=)
              Prelude.<$> pipelineNamePrefix
          ]
      )

instance Core.ToPath ListPipelines where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPipelines where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { -- | If the result of the previous @ListPipelines@ request was truncated, the
    -- response includes a @NextToken@. To retrieve the next set of pipelines,
    -- use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Contains a sorted list of @PipelineSummary@ objects matching the
    -- specified filters. Each @PipelineSummary@ consists of PipelineArn,
    -- PipelineName, ExperimentName, PipelineDescription, CreationTime,
    -- LastModifiedTime, LastRunTime, and RoleArn. This list can be empty.
    pipelineSummaries :: Prelude.Maybe [PipelineSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPipelinesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPipelinesResponse_nextToken' - If the result of the previous @ListPipelines@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of pipelines,
-- use the token in the next request.
--
-- 'pipelineSummaries', 'listPipelinesResponse_pipelineSummaries' - Contains a sorted list of @PipelineSummary@ objects matching the
-- specified filters. Each @PipelineSummary@ consists of PipelineArn,
-- PipelineName, ExperimentName, PipelineDescription, CreationTime,
-- LastModifiedTime, LastRunTime, and RoleArn. This list can be empty.
--
-- 'httpStatus', 'listPipelinesResponse_httpStatus' - The response's http status code.
newListPipelinesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPipelinesResponse
newListPipelinesResponse pHttpStatus_ =
  ListPipelinesResponse'
    { nextToken = Prelude.Nothing,
      pipelineSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the result of the previous @ListPipelines@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of pipelines,
-- use the token in the next request.
listPipelinesResponse_nextToken :: Lens.Lens' ListPipelinesResponse (Prelude.Maybe Prelude.Text)
listPipelinesResponse_nextToken = Lens.lens (\ListPipelinesResponse' {nextToken} -> nextToken) (\s@ListPipelinesResponse' {} a -> s {nextToken = a} :: ListPipelinesResponse)

-- | Contains a sorted list of @PipelineSummary@ objects matching the
-- specified filters. Each @PipelineSummary@ consists of PipelineArn,
-- PipelineName, ExperimentName, PipelineDescription, CreationTime,
-- LastModifiedTime, LastRunTime, and RoleArn. This list can be empty.
listPipelinesResponse_pipelineSummaries :: Lens.Lens' ListPipelinesResponse (Prelude.Maybe [PipelineSummary])
listPipelinesResponse_pipelineSummaries = Lens.lens (\ListPipelinesResponse' {pipelineSummaries} -> pipelineSummaries) (\s@ListPipelinesResponse' {} a -> s {pipelineSummaries = a} :: ListPipelinesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPipelinesResponse_httpStatus :: Lens.Lens' ListPipelinesResponse Prelude.Int
listPipelinesResponse_httpStatus = Lens.lens (\ListPipelinesResponse' {httpStatus} -> httpStatus) (\s@ListPipelinesResponse' {} a -> s {httpStatus = a} :: ListPipelinesResponse)

instance Prelude.NFData ListPipelinesResponse
