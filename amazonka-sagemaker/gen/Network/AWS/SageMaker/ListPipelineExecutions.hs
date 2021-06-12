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
-- Module      : Network.AWS.SageMaker.ListPipelineExecutions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the pipeline executions.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListPipelineExecutions
  ( -- * Creating a Request
    ListPipelineExecutions (..),
    newListPipelineExecutions,

    -- * Request Lenses
    listPipelineExecutions_createdAfter,
    listPipelineExecutions_sortOrder,
    listPipelineExecutions_nextToken,
    listPipelineExecutions_createdBefore,
    listPipelineExecutions_maxResults,
    listPipelineExecutions_sortBy,
    listPipelineExecutions_pipelineName,

    -- * Destructuring the Response
    ListPipelineExecutionsResponse (..),
    newListPipelineExecutionsResponse,

    -- * Response Lenses
    listPipelineExecutionsResponse_nextToken,
    listPipelineExecutionsResponse_pipelineExecutionSummaries,
    listPipelineExecutionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListPipelineExecutions' smart constructor.
data ListPipelineExecutions = ListPipelineExecutions'
  { -- | A filter that returns the pipeline executions that were created after a
    -- specified time.
    createdAfter :: Core.Maybe Core.POSIX,
    -- | The sort order for results.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the result of the previous @ListPipelineExecutions@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of pipeline executions, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | A filter that returns the pipeline executions that were created before a
    -- specified time.
    createdBefore :: Core.Maybe Core.POSIX,
    -- | The maximum number of pipeline executions to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The field by which to sort results. The default is @CreatedTime@.
    sortBy :: Core.Maybe SortPipelineExecutionsBy,
    -- | The name of the pipeline.
    pipelineName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPipelineExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'listPipelineExecutions_createdAfter' - A filter that returns the pipeline executions that were created after a
-- specified time.
--
-- 'sortOrder', 'listPipelineExecutions_sortOrder' - The sort order for results.
--
-- 'nextToken', 'listPipelineExecutions_nextToken' - If the result of the previous @ListPipelineExecutions@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline executions, use the token in the next request.
--
-- 'createdBefore', 'listPipelineExecutions_createdBefore' - A filter that returns the pipeline executions that were created before a
-- specified time.
--
-- 'maxResults', 'listPipelineExecutions_maxResults' - The maximum number of pipeline executions to return in the response.
--
-- 'sortBy', 'listPipelineExecutions_sortBy' - The field by which to sort results. The default is @CreatedTime@.
--
-- 'pipelineName', 'listPipelineExecutions_pipelineName' - The name of the pipeline.
newListPipelineExecutions ::
  -- | 'pipelineName'
  Core.Text ->
  ListPipelineExecutions
newListPipelineExecutions pPipelineName_ =
  ListPipelineExecutions'
    { createdAfter =
        Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      createdBefore = Core.Nothing,
      maxResults = Core.Nothing,
      sortBy = Core.Nothing,
      pipelineName = pPipelineName_
    }

-- | A filter that returns the pipeline executions that were created after a
-- specified time.
listPipelineExecutions_createdAfter :: Lens.Lens' ListPipelineExecutions (Core.Maybe Core.UTCTime)
listPipelineExecutions_createdAfter = Lens.lens (\ListPipelineExecutions' {createdAfter} -> createdAfter) (\s@ListPipelineExecutions' {} a -> s {createdAfter = a} :: ListPipelineExecutions) Core.. Lens.mapping Core._Time

-- | The sort order for results.
listPipelineExecutions_sortOrder :: Lens.Lens' ListPipelineExecutions (Core.Maybe SortOrder)
listPipelineExecutions_sortOrder = Lens.lens (\ListPipelineExecutions' {sortOrder} -> sortOrder) (\s@ListPipelineExecutions' {} a -> s {sortOrder = a} :: ListPipelineExecutions)

-- | If the result of the previous @ListPipelineExecutions@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline executions, use the token in the next request.
listPipelineExecutions_nextToken :: Lens.Lens' ListPipelineExecutions (Core.Maybe Core.Text)
listPipelineExecutions_nextToken = Lens.lens (\ListPipelineExecutions' {nextToken} -> nextToken) (\s@ListPipelineExecutions' {} a -> s {nextToken = a} :: ListPipelineExecutions)

-- | A filter that returns the pipeline executions that were created before a
-- specified time.
listPipelineExecutions_createdBefore :: Lens.Lens' ListPipelineExecutions (Core.Maybe Core.UTCTime)
listPipelineExecutions_createdBefore = Lens.lens (\ListPipelineExecutions' {createdBefore} -> createdBefore) (\s@ListPipelineExecutions' {} a -> s {createdBefore = a} :: ListPipelineExecutions) Core.. Lens.mapping Core._Time

-- | The maximum number of pipeline executions to return in the response.
listPipelineExecutions_maxResults :: Lens.Lens' ListPipelineExecutions (Core.Maybe Core.Natural)
listPipelineExecutions_maxResults = Lens.lens (\ListPipelineExecutions' {maxResults} -> maxResults) (\s@ListPipelineExecutions' {} a -> s {maxResults = a} :: ListPipelineExecutions)

-- | The field by which to sort results. The default is @CreatedTime@.
listPipelineExecutions_sortBy :: Lens.Lens' ListPipelineExecutions (Core.Maybe SortPipelineExecutionsBy)
listPipelineExecutions_sortBy = Lens.lens (\ListPipelineExecutions' {sortBy} -> sortBy) (\s@ListPipelineExecutions' {} a -> s {sortBy = a} :: ListPipelineExecutions)

-- | The name of the pipeline.
listPipelineExecutions_pipelineName :: Lens.Lens' ListPipelineExecutions Core.Text
listPipelineExecutions_pipelineName = Lens.lens (\ListPipelineExecutions' {pipelineName} -> pipelineName) (\s@ListPipelineExecutions' {} a -> s {pipelineName = a} :: ListPipelineExecutions)

instance Core.AWSPager ListPipelineExecutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPipelineExecutionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPipelineExecutionsResponse_pipelineExecutionSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPipelineExecutions_nextToken
          Lens..~ rs
          Lens.^? listPipelineExecutionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListPipelineExecutions where
  type
    AWSResponse ListPipelineExecutions =
      ListPipelineExecutionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelineExecutionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "PipelineExecutionSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPipelineExecutions

instance Core.NFData ListPipelineExecutions

instance Core.ToHeaders ListPipelineExecutions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListPipelineExecutions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListPipelineExecutions where
  toJSON ListPipelineExecutions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreatedAfter" Core..=) Core.<$> createdAfter,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("CreatedBefore" Core..=) Core.<$> createdBefore,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("SortBy" Core..=) Core.<$> sortBy,
            Core.Just ("PipelineName" Core..= pipelineName)
          ]
      )

instance Core.ToPath ListPipelineExecutions where
  toPath = Core.const "/"

instance Core.ToQuery ListPipelineExecutions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListPipelineExecutionsResponse' smart constructor.
data ListPipelineExecutionsResponse = ListPipelineExecutionsResponse'
  { -- | If the result of the previous @ListPipelineExecutions@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of pipeline executions, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | Contains a sorted list of pipeline execution summary objects matching
    -- the specified filters. Each run summary includes the Amazon Resource
    -- Name (ARN) of the pipeline execution, the run date, and the status. This
    -- list can be empty.
    pipelineExecutionSummaries :: Core.Maybe [PipelineExecutionSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPipelineExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPipelineExecutionsResponse_nextToken' - If the result of the previous @ListPipelineExecutions@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline executions, use the token in the next request.
--
-- 'pipelineExecutionSummaries', 'listPipelineExecutionsResponse_pipelineExecutionSummaries' - Contains a sorted list of pipeline execution summary objects matching
-- the specified filters. Each run summary includes the Amazon Resource
-- Name (ARN) of the pipeline execution, the run date, and the status. This
-- list can be empty.
--
-- 'httpStatus', 'listPipelineExecutionsResponse_httpStatus' - The response's http status code.
newListPipelineExecutionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPipelineExecutionsResponse
newListPipelineExecutionsResponse pHttpStatus_ =
  ListPipelineExecutionsResponse'
    { nextToken =
        Core.Nothing,
      pipelineExecutionSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the result of the previous @ListPipelineExecutions@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline executions, use the token in the next request.
listPipelineExecutionsResponse_nextToken :: Lens.Lens' ListPipelineExecutionsResponse (Core.Maybe Core.Text)
listPipelineExecutionsResponse_nextToken = Lens.lens (\ListPipelineExecutionsResponse' {nextToken} -> nextToken) (\s@ListPipelineExecutionsResponse' {} a -> s {nextToken = a} :: ListPipelineExecutionsResponse)

-- | Contains a sorted list of pipeline execution summary objects matching
-- the specified filters. Each run summary includes the Amazon Resource
-- Name (ARN) of the pipeline execution, the run date, and the status. This
-- list can be empty.
listPipelineExecutionsResponse_pipelineExecutionSummaries :: Lens.Lens' ListPipelineExecutionsResponse (Core.Maybe [PipelineExecutionSummary])
listPipelineExecutionsResponse_pipelineExecutionSummaries = Lens.lens (\ListPipelineExecutionsResponse' {pipelineExecutionSummaries} -> pipelineExecutionSummaries) (\s@ListPipelineExecutionsResponse' {} a -> s {pipelineExecutionSummaries = a} :: ListPipelineExecutionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPipelineExecutionsResponse_httpStatus :: Lens.Lens' ListPipelineExecutionsResponse Core.Int
listPipelineExecutionsResponse_httpStatus = Lens.lens (\ListPipelineExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListPipelineExecutionsResponse' {} a -> s {httpStatus = a} :: ListPipelineExecutionsResponse)

instance Core.NFData ListPipelineExecutionsResponse
