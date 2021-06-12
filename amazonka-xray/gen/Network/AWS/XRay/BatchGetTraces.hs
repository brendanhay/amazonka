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
-- Module      : Network.AWS.XRay.BatchGetTraces
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.XRay.BatchGetTraces
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
    batchGetTracesResponse_unprocessedTraceIds,
    batchGetTracesResponse_traces,
    batchGetTracesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newBatchGetTraces' smart constructor.
data BatchGetTraces = BatchGetTraces'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | Specify the trace IDs of requests for which to retrieve segments.
    traceIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      traceIds = Core.mempty
    }

-- | Pagination token.
batchGetTraces_nextToken :: Lens.Lens' BatchGetTraces (Core.Maybe Core.Text)
batchGetTraces_nextToken = Lens.lens (\BatchGetTraces' {nextToken} -> nextToken) (\s@BatchGetTraces' {} a -> s {nextToken = a} :: BatchGetTraces)

-- | Specify the trace IDs of requests for which to retrieve segments.
batchGetTraces_traceIds :: Lens.Lens' BatchGetTraces [Core.Text]
batchGetTraces_traceIds = Lens.lens (\BatchGetTraces' {traceIds} -> traceIds) (\s@BatchGetTraces' {} a -> s {traceIds = a} :: BatchGetTraces) Core.. Lens._Coerce

instance Core.AWSPager BatchGetTraces where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? batchGetTracesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? batchGetTracesResponse_traces Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& batchGetTraces_nextToken
          Lens..~ rs
          Lens.^? batchGetTracesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest BatchGetTraces where
  type
    AWSResponse BatchGetTraces =
      BatchGetTracesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetTracesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "UnprocessedTraceIds"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "Traces" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchGetTraces

instance Core.NFData BatchGetTraces

instance Core.ToHeaders BatchGetTraces where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON BatchGetTraces where
  toJSON BatchGetTraces' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            Core.Just ("TraceIds" Core..= traceIds)
          ]
      )

instance Core.ToPath BatchGetTraces where
  toPath = Core.const "/Traces"

instance Core.ToQuery BatchGetTraces where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchGetTracesResponse' smart constructor.
data BatchGetTracesResponse = BatchGetTracesResponse'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | Trace IDs of requests that haven\'t been processed.
    unprocessedTraceIds :: Core.Maybe [Core.Text],
    -- | Full traces for the specified requests.
    traces :: Core.Maybe [Trace],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'unprocessedTraceIds', 'batchGetTracesResponse_unprocessedTraceIds' - Trace IDs of requests that haven\'t been processed.
--
-- 'traces', 'batchGetTracesResponse_traces' - Full traces for the specified requests.
--
-- 'httpStatus', 'batchGetTracesResponse_httpStatus' - The response's http status code.
newBatchGetTracesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchGetTracesResponse
newBatchGetTracesResponse pHttpStatus_ =
  BatchGetTracesResponse'
    { nextToken = Core.Nothing,
      unprocessedTraceIds = Core.Nothing,
      traces = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
batchGetTracesResponse_nextToken :: Lens.Lens' BatchGetTracesResponse (Core.Maybe Core.Text)
batchGetTracesResponse_nextToken = Lens.lens (\BatchGetTracesResponse' {nextToken} -> nextToken) (\s@BatchGetTracesResponse' {} a -> s {nextToken = a} :: BatchGetTracesResponse)

-- | Trace IDs of requests that haven\'t been processed.
batchGetTracesResponse_unprocessedTraceIds :: Lens.Lens' BatchGetTracesResponse (Core.Maybe [Core.Text])
batchGetTracesResponse_unprocessedTraceIds = Lens.lens (\BatchGetTracesResponse' {unprocessedTraceIds} -> unprocessedTraceIds) (\s@BatchGetTracesResponse' {} a -> s {unprocessedTraceIds = a} :: BatchGetTracesResponse) Core.. Lens.mapping Lens._Coerce

-- | Full traces for the specified requests.
batchGetTracesResponse_traces :: Lens.Lens' BatchGetTracesResponse (Core.Maybe [Trace])
batchGetTracesResponse_traces = Lens.lens (\BatchGetTracesResponse' {traces} -> traces) (\s@BatchGetTracesResponse' {} a -> s {traces = a} :: BatchGetTracesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetTracesResponse_httpStatus :: Lens.Lens' BatchGetTracesResponse Core.Int
batchGetTracesResponse_httpStatus = Lens.lens (\BatchGetTracesResponse' {httpStatus} -> httpStatus) (\s@BatchGetTracesResponse' {} a -> s {httpStatus = a} :: BatchGetTracesResponse)

instance Core.NFData BatchGetTracesResponse
