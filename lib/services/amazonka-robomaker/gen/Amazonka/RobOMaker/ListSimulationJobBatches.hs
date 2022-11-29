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
-- Module      : Amazonka.RobOMaker.ListSimulationJobBatches
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list simulation job batches. You can optionally provide
-- filters to retrieve specific simulation batch jobs.
--
-- This operation returns paginated results.
module Amazonka.RobOMaker.ListSimulationJobBatches
  ( -- * Creating a Request
    ListSimulationJobBatches (..),
    newListSimulationJobBatches,

    -- * Request Lenses
    listSimulationJobBatches_nextToken,
    listSimulationJobBatches_filters,
    listSimulationJobBatches_maxResults,

    -- * Destructuring the Response
    ListSimulationJobBatchesResponse (..),
    newListSimulationJobBatchesResponse,

    -- * Response Lenses
    listSimulationJobBatchesResponse_nextToken,
    listSimulationJobBatchesResponse_simulationJobBatchSummaries,
    listSimulationJobBatchesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newListSimulationJobBatches' smart constructor.
data ListSimulationJobBatches = ListSimulationJobBatches'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call
    -- @ListSimulationJobBatches@ again and assign that token to the request
    -- object\'s @nextToken@ parameter. If there are no remaining results, the
    -- previous response object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Optional filters to limit results.
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | When this parameter is used, @ListSimulationJobBatches@ only returns
    -- @maxResults@ results in a single page along with a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @ListSimulationJobBatches@ request with the returned
    -- @nextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSimulationJobBatches' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSimulationJobBatches_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListSimulationJobBatches@ again and assign that token to the request
-- object\'s @nextToken@ parameter. If there are no remaining results, the
-- previous response object\'s NextToken parameter is set to null.
--
-- 'filters', 'listSimulationJobBatches_filters' - Optional filters to limit results.
--
-- 'maxResults', 'listSimulationJobBatches_maxResults' - When this parameter is used, @ListSimulationJobBatches@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListSimulationJobBatches@ request with the returned
-- @nextToken@ value.
newListSimulationJobBatches ::
  ListSimulationJobBatches
newListSimulationJobBatches =
  ListSimulationJobBatches'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListSimulationJobBatches@ again and assign that token to the request
-- object\'s @nextToken@ parameter. If there are no remaining results, the
-- previous response object\'s NextToken parameter is set to null.
listSimulationJobBatches_nextToken :: Lens.Lens' ListSimulationJobBatches (Prelude.Maybe Prelude.Text)
listSimulationJobBatches_nextToken = Lens.lens (\ListSimulationJobBatches' {nextToken} -> nextToken) (\s@ListSimulationJobBatches' {} a -> s {nextToken = a} :: ListSimulationJobBatches)

-- | Optional filters to limit results.
listSimulationJobBatches_filters :: Lens.Lens' ListSimulationJobBatches (Prelude.Maybe (Prelude.NonEmpty Filter))
listSimulationJobBatches_filters = Lens.lens (\ListSimulationJobBatches' {filters} -> filters) (\s@ListSimulationJobBatches' {} a -> s {filters = a} :: ListSimulationJobBatches) Prelude.. Lens.mapping Lens.coerced

-- | When this parameter is used, @ListSimulationJobBatches@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListSimulationJobBatches@ request with the returned
-- @nextToken@ value.
listSimulationJobBatches_maxResults :: Lens.Lens' ListSimulationJobBatches (Prelude.Maybe Prelude.Int)
listSimulationJobBatches_maxResults = Lens.lens (\ListSimulationJobBatches' {maxResults} -> maxResults) (\s@ListSimulationJobBatches' {} a -> s {maxResults = a} :: ListSimulationJobBatches)

instance Core.AWSPager ListSimulationJobBatches where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSimulationJobBatchesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSimulationJobBatchesResponse_simulationJobBatchSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSimulationJobBatches_nextToken
          Lens..~ rs
          Lens.^? listSimulationJobBatchesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSimulationJobBatches where
  type
    AWSResponse ListSimulationJobBatches =
      ListSimulationJobBatchesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSimulationJobBatchesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "simulationJobBatchSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSimulationJobBatches where
  hashWithSalt _salt ListSimulationJobBatches' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListSimulationJobBatches where
  rnf ListSimulationJobBatches' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListSimulationJobBatches where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSimulationJobBatches where
  toJSON ListSimulationJobBatches' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("filters" Core..=) Prelude.<$> filters,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListSimulationJobBatches where
  toPath = Prelude.const "/listSimulationJobBatches"

instance Core.ToQuery ListSimulationJobBatches where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSimulationJobBatchesResponse' smart constructor.
data ListSimulationJobBatchesResponse = ListSimulationJobBatchesResponse'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call
    -- @ListSimulationJobBatches@ again and assign that token to the request
    -- object\'s @nextToken@ parameter. If there are no remaining results, the
    -- previous response object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of simulation job batch summaries.
    simulationJobBatchSummaries :: Prelude.Maybe [SimulationJobBatchSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSimulationJobBatchesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSimulationJobBatchesResponse_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListSimulationJobBatches@ again and assign that token to the request
-- object\'s @nextToken@ parameter. If there are no remaining results, the
-- previous response object\'s NextToken parameter is set to null.
--
-- 'simulationJobBatchSummaries', 'listSimulationJobBatchesResponse_simulationJobBatchSummaries' - A list of simulation job batch summaries.
--
-- 'httpStatus', 'listSimulationJobBatchesResponse_httpStatus' - The response's http status code.
newListSimulationJobBatchesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSimulationJobBatchesResponse
newListSimulationJobBatchesResponse pHttpStatus_ =
  ListSimulationJobBatchesResponse'
    { nextToken =
        Prelude.Nothing,
      simulationJobBatchSummaries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call
-- @ListSimulationJobBatches@ again and assign that token to the request
-- object\'s @nextToken@ parameter. If there are no remaining results, the
-- previous response object\'s NextToken parameter is set to null.
listSimulationJobBatchesResponse_nextToken :: Lens.Lens' ListSimulationJobBatchesResponse (Prelude.Maybe Prelude.Text)
listSimulationJobBatchesResponse_nextToken = Lens.lens (\ListSimulationJobBatchesResponse' {nextToken} -> nextToken) (\s@ListSimulationJobBatchesResponse' {} a -> s {nextToken = a} :: ListSimulationJobBatchesResponse)

-- | A list of simulation job batch summaries.
listSimulationJobBatchesResponse_simulationJobBatchSummaries :: Lens.Lens' ListSimulationJobBatchesResponse (Prelude.Maybe [SimulationJobBatchSummary])
listSimulationJobBatchesResponse_simulationJobBatchSummaries = Lens.lens (\ListSimulationJobBatchesResponse' {simulationJobBatchSummaries} -> simulationJobBatchSummaries) (\s@ListSimulationJobBatchesResponse' {} a -> s {simulationJobBatchSummaries = a} :: ListSimulationJobBatchesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSimulationJobBatchesResponse_httpStatus :: Lens.Lens' ListSimulationJobBatchesResponse Prelude.Int
listSimulationJobBatchesResponse_httpStatus = Lens.lens (\ListSimulationJobBatchesResponse' {httpStatus} -> httpStatus) (\s@ListSimulationJobBatchesResponse' {} a -> s {httpStatus = a} :: ListSimulationJobBatchesResponse)

instance
  Prelude.NFData
    ListSimulationJobBatchesResponse
  where
  rnf ListSimulationJobBatchesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf simulationJobBatchSummaries
      `Prelude.seq` Prelude.rnf httpStatus
