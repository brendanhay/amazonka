{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newBatchGetTraces' smart constructor.
data BatchGetTraces = BatchGetTraces'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specify the trace IDs of requests for which to retrieve segments.
    traceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
batchGetTraces_traceIds = Lens.lens (\BatchGetTraces' {traceIds} -> traceIds) (\s@BatchGetTraces' {} a -> s {traceIds = a} :: BatchGetTraces) Prelude.. Prelude._Coerce

instance Pager.AWSPager BatchGetTraces where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? batchGetTracesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? batchGetTracesResponse_traces Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& batchGetTraces_nextToken
          Lens..~ rs
          Lens.^? batchGetTracesResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest BatchGetTraces where
  type Rs BatchGetTraces = BatchGetTracesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetTracesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "UnprocessedTraceIds"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "Traces" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetTraces

instance Prelude.NFData BatchGetTraces

instance Prelude.ToHeaders BatchGetTraces where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON BatchGetTraces where
  toJSON BatchGetTraces' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            Prelude.Just ("TraceIds" Prelude..= traceIds)
          ]
      )

instance Prelude.ToPath BatchGetTraces where
  toPath = Prelude.const "/Traces"

instance Prelude.ToQuery BatchGetTraces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetTracesResponse' smart constructor.
data BatchGetTracesResponse = BatchGetTracesResponse'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Trace IDs of requests that haven\'t been processed.
    unprocessedTraceIds :: Prelude.Maybe [Prelude.Text],
    -- | Full traces for the specified requests.
    traces :: Prelude.Maybe [Trace],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  BatchGetTracesResponse
newBatchGetTracesResponse pHttpStatus_ =
  BatchGetTracesResponse'
    { nextToken =
        Prelude.Nothing,
      unprocessedTraceIds = Prelude.Nothing,
      traces = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
batchGetTracesResponse_nextToken :: Lens.Lens' BatchGetTracesResponse (Prelude.Maybe Prelude.Text)
batchGetTracesResponse_nextToken = Lens.lens (\BatchGetTracesResponse' {nextToken} -> nextToken) (\s@BatchGetTracesResponse' {} a -> s {nextToken = a} :: BatchGetTracesResponse)

-- | Trace IDs of requests that haven\'t been processed.
batchGetTracesResponse_unprocessedTraceIds :: Lens.Lens' BatchGetTracesResponse (Prelude.Maybe [Prelude.Text])
batchGetTracesResponse_unprocessedTraceIds = Lens.lens (\BatchGetTracesResponse' {unprocessedTraceIds} -> unprocessedTraceIds) (\s@BatchGetTracesResponse' {} a -> s {unprocessedTraceIds = a} :: BatchGetTracesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Full traces for the specified requests.
batchGetTracesResponse_traces :: Lens.Lens' BatchGetTracesResponse (Prelude.Maybe [Trace])
batchGetTracesResponse_traces = Lens.lens (\BatchGetTracesResponse' {traces} -> traces) (\s@BatchGetTracesResponse' {} a -> s {traces = a} :: BatchGetTracesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
batchGetTracesResponse_httpStatus :: Lens.Lens' BatchGetTracesResponse Prelude.Int
batchGetTracesResponse_httpStatus = Lens.lens (\BatchGetTracesResponse' {httpStatus} -> httpStatus) (\s@BatchGetTracesResponse' {} a -> s {httpStatus = a} :: BatchGetTracesResponse)

instance Prelude.NFData BatchGetTracesResponse
