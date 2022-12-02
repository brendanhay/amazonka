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
-- Module      : Amazonka.XRay.BatchGetTraces
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of traces specified by ID. Each trace is a collection
-- of segment documents that originates from a single request. Use
-- @GetTraceSummaries@ to get a list of trace IDs.
--
-- This operation returns paginated results.
module Amazonka.XRay.BatchGetTraces
  ( -- * Creating a Request
    BatchGetTraces (..),
    newBatchGetTraces,

    -- * Request Lenses
    batchGetTraces_nextToken,
    batchGetTraces_traceIds,

    -- * Destructuring the Response
    BatchGetTracesResponse (..),
    newBatchGetTracesResponse,

    -- * Response Lenses
    batchGetTracesResponse_nextToken,
    batchGetTracesResponse_traces,
    batchGetTracesResponse_unprocessedTraceIds,
    batchGetTracesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newBatchGetTraces' smart constructor.
data BatchGetTraces = BatchGetTraces'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specify the trace IDs of requests for which to retrieve segments.
    traceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetTraces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchGetTraces_nextToken' - Pagination token.
--
-- 'traceIds', 'batchGetTraces_traceIds' - Specify the trace IDs of requests for which to retrieve segments.
newBatchGetTraces ::
  BatchGetTraces
newBatchGetTraces =
  BatchGetTraces'
    { nextToken = Prelude.Nothing,
      traceIds = Prelude.mempty
    }

-- | Pagination token.
batchGetTraces_nextToken :: Lens.Lens' BatchGetTraces (Prelude.Maybe Prelude.Text)
batchGetTraces_nextToken = Lens.lens (\BatchGetTraces' {nextToken} -> nextToken) (\s@BatchGetTraces' {} a -> s {nextToken = a} :: BatchGetTraces)

-- | Specify the trace IDs of requests for which to retrieve segments.
batchGetTraces_traceIds :: Lens.Lens' BatchGetTraces [Prelude.Text]
batchGetTraces_traceIds = Lens.lens (\BatchGetTraces' {traceIds} -> traceIds) (\s@BatchGetTraces' {} a -> s {traceIds = a} :: BatchGetTraces) Prelude.. Lens.coerced

instance Core.AWSPager BatchGetTraces where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? batchGetTracesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? batchGetTracesResponse_traces Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& batchGetTraces_nextToken
          Lens..~ rs
          Lens.^? batchGetTracesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest BatchGetTraces where
  type
    AWSResponse BatchGetTraces =
      BatchGetTracesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetTracesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Traces" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "UnprocessedTraceIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetTraces where
  hashWithSalt _salt BatchGetTraces' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` traceIds

instance Prelude.NFData BatchGetTraces where
  rnf BatchGetTraces' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf traceIds

instance Data.ToHeaders BatchGetTraces where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON BatchGetTraces where
  toJSON BatchGetTraces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("TraceIds" Data..= traceIds)
          ]
      )

instance Data.ToPath BatchGetTraces where
  toPath = Prelude.const "/Traces"

instance Data.ToQuery BatchGetTraces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetTracesResponse' smart constructor.
data BatchGetTracesResponse = BatchGetTracesResponse'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Full traces for the specified requests.
    traces :: Prelude.Maybe [Trace],
    -- | Trace IDs of requests that haven\'t been processed.
    unprocessedTraceIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetTracesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchGetTracesResponse_nextToken' - Pagination token.
--
-- 'traces', 'batchGetTracesResponse_traces' - Full traces for the specified requests.
--
-- 'unprocessedTraceIds', 'batchGetTracesResponse_unprocessedTraceIds' - Trace IDs of requests that haven\'t been processed.
--
-- 'httpStatus', 'batchGetTracesResponse_httpStatus' - The response's http status code.
newBatchGetTracesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetTracesResponse
newBatchGetTracesResponse pHttpStatus_ =
  BatchGetTracesResponse'
    { nextToken =
        Prelude.Nothing,
      traces = Prelude.Nothing,
      unprocessedTraceIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
batchGetTracesResponse_nextToken :: Lens.Lens' BatchGetTracesResponse (Prelude.Maybe Prelude.Text)
batchGetTracesResponse_nextToken = Lens.lens (\BatchGetTracesResponse' {nextToken} -> nextToken) (\s@BatchGetTracesResponse' {} a -> s {nextToken = a} :: BatchGetTracesResponse)

-- | Full traces for the specified requests.
batchGetTracesResponse_traces :: Lens.Lens' BatchGetTracesResponse (Prelude.Maybe [Trace])
batchGetTracesResponse_traces = Lens.lens (\BatchGetTracesResponse' {traces} -> traces) (\s@BatchGetTracesResponse' {} a -> s {traces = a} :: BatchGetTracesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Trace IDs of requests that haven\'t been processed.
batchGetTracesResponse_unprocessedTraceIds :: Lens.Lens' BatchGetTracesResponse (Prelude.Maybe [Prelude.Text])
batchGetTracesResponse_unprocessedTraceIds = Lens.lens (\BatchGetTracesResponse' {unprocessedTraceIds} -> unprocessedTraceIds) (\s@BatchGetTracesResponse' {} a -> s {unprocessedTraceIds = a} :: BatchGetTracesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetTracesResponse_httpStatus :: Lens.Lens' BatchGetTracesResponse Prelude.Int
batchGetTracesResponse_httpStatus = Lens.lens (\BatchGetTracesResponse' {httpStatus} -> httpStatus) (\s@BatchGetTracesResponse' {} a -> s {httpStatus = a} :: BatchGetTracesResponse)

instance Prelude.NFData BatchGetTracesResponse where
  rnf BatchGetTracesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf traces
      `Prelude.seq` Prelude.rnf unprocessedTraceIds
      `Prelude.seq` Prelude.rnf httpStatus
