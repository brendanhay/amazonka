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
-- Module      : Network.AWS.IoTAnalytics.ListPipelines
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of pipelines.
--
-- This operation returns paginated results.
module Network.AWS.IoTAnalytics.ListPipelines
  ( -- * Creating a Request
    ListPipelines (..),
    newListPipelines,

    -- * Request Lenses
    listPipelines_nextToken,
    listPipelines_maxResults,

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
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPipelines' smart constructor.
data ListPipelines = ListPipelines'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in this request.
    --
    -- The default value is 100.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPipelines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPipelines_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listPipelines_maxResults' - The maximum number of results to return in this request.
--
-- The default value is 100.
newListPipelines ::
  ListPipelines
newListPipelines =
  ListPipelines'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results.
listPipelines_nextToken :: Lens.Lens' ListPipelines (Core.Maybe Core.Text)
listPipelines_nextToken = Lens.lens (\ListPipelines' {nextToken} -> nextToken) (\s@ListPipelines' {} a -> s {nextToken = a} :: ListPipelines)

-- | The maximum number of results to return in this request.
--
-- The default value is 100.
listPipelines_maxResults :: Lens.Lens' ListPipelines (Core.Maybe Core.Natural)
listPipelines_maxResults = Lens.lens (\ListPipelines' {maxResults} -> maxResults) (\s@ListPipelines' {} a -> s {maxResults = a} :: ListPipelines)

instance Core.AWSPager ListPipelines where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPipelinesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPipelinesResponse_pipelineSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPipelines_nextToken
          Lens..~ rs
          Lens.^? listPipelinesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListPipelines where
  type
    AWSResponse ListPipelines =
      ListPipelinesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelinesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "pipelineSummaries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPipelines

instance Core.NFData ListPipelines

instance Core.ToHeaders ListPipelines where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListPipelines where
  toPath = Core.const "/pipelines"

instance Core.ToQuery ListPipelines where
  toQuery ListPipelines' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @PipelineSummary@ objects.
    pipelineSummaries :: Core.Maybe [PipelineSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPipelinesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPipelinesResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'pipelineSummaries', 'listPipelinesResponse_pipelineSummaries' - A list of @PipelineSummary@ objects.
--
-- 'httpStatus', 'listPipelinesResponse_httpStatus' - The response's http status code.
newListPipelinesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPipelinesResponse
newListPipelinesResponse pHttpStatus_ =
  ListPipelinesResponse'
    { nextToken = Core.Nothing,
      pipelineSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listPipelinesResponse_nextToken :: Lens.Lens' ListPipelinesResponse (Core.Maybe Core.Text)
listPipelinesResponse_nextToken = Lens.lens (\ListPipelinesResponse' {nextToken} -> nextToken) (\s@ListPipelinesResponse' {} a -> s {nextToken = a} :: ListPipelinesResponse)

-- | A list of @PipelineSummary@ objects.
listPipelinesResponse_pipelineSummaries :: Lens.Lens' ListPipelinesResponse (Core.Maybe [PipelineSummary])
listPipelinesResponse_pipelineSummaries = Lens.lens (\ListPipelinesResponse' {pipelineSummaries} -> pipelineSummaries) (\s@ListPipelinesResponse' {} a -> s {pipelineSummaries = a} :: ListPipelinesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPipelinesResponse_httpStatus :: Lens.Lens' ListPipelinesResponse Core.Int
listPipelinesResponse_httpStatus = Lens.lens (\ListPipelinesResponse' {httpStatus} -> httpStatus) (\s@ListPipelinesResponse' {} a -> s {httpStatus = a} :: ListPipelinesResponse)

instance Core.NFData ListPipelinesResponse
