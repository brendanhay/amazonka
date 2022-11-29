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
-- Module      : Amazonka.SESV2.BatchGetMetricData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves batches of metric data collected based on your sending
-- activity.
--
-- You can execute this operation no more than 16 times per second, and
-- with at most 160 queries from the batches per second (cumulative).
module Amazonka.SESV2.BatchGetMetricData
  ( -- * Creating a Request
    BatchGetMetricData (..),
    newBatchGetMetricData,

    -- * Request Lenses
    batchGetMetricData_queries,

    -- * Destructuring the Response
    BatchGetMetricDataResponse (..),
    newBatchGetMetricDataResponse,

    -- * Response Lenses
    batchGetMetricDataResponse_errors,
    batchGetMetricDataResponse_results,
    batchGetMetricDataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | Represents a request to retrieve a batch of metric data.
--
-- /See:/ 'newBatchGetMetricData' smart constructor.
data BatchGetMetricData = BatchGetMetricData'
  { -- | A list of queries for metrics to be retrieved.
    queries :: Prelude.NonEmpty BatchGetMetricDataQuery
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetMetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queries', 'batchGetMetricData_queries' - A list of queries for metrics to be retrieved.
newBatchGetMetricData ::
  -- | 'queries'
  Prelude.NonEmpty BatchGetMetricDataQuery ->
  BatchGetMetricData
newBatchGetMetricData pQueries_ =
  BatchGetMetricData'
    { queries =
        Lens.coerced Lens.# pQueries_
    }

-- | A list of queries for metrics to be retrieved.
batchGetMetricData_queries :: Lens.Lens' BatchGetMetricData (Prelude.NonEmpty BatchGetMetricDataQuery)
batchGetMetricData_queries = Lens.lens (\BatchGetMetricData' {queries} -> queries) (\s@BatchGetMetricData' {} a -> s {queries = a} :: BatchGetMetricData) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetMetricData where
  type
    AWSResponse BatchGetMetricData =
      BatchGetMetricDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetMetricDataResponse'
            Prelude.<$> (x Core..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetMetricData where
  hashWithSalt _salt BatchGetMetricData' {..} =
    _salt `Prelude.hashWithSalt` queries

instance Prelude.NFData BatchGetMetricData where
  rnf BatchGetMetricData' {..} = Prelude.rnf queries

instance Core.ToHeaders BatchGetMetricData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetMetricData where
  toJSON BatchGetMetricData' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Queries" Core..= queries)]
      )

instance Core.ToPath BatchGetMetricData where
  toPath = Prelude.const "/v2/email/metrics/batch"

instance Core.ToQuery BatchGetMetricData where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of processing your metric data batch request
--
-- /See:/ 'newBatchGetMetricDataResponse' smart constructor.
data BatchGetMetricDataResponse = BatchGetMetricDataResponse'
  { -- | A list of @MetricDataError@ encountered while processing your metric
    -- data batch request.
    errors :: Prelude.Maybe [MetricDataError],
    -- | A list of successfully retrieved @MetricDataResult@.
    results :: Prelude.Maybe [MetricDataResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetMetricDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchGetMetricDataResponse_errors' - A list of @MetricDataError@ encountered while processing your metric
-- data batch request.
--
-- 'results', 'batchGetMetricDataResponse_results' - A list of successfully retrieved @MetricDataResult@.
--
-- 'httpStatus', 'batchGetMetricDataResponse_httpStatus' - The response's http status code.
newBatchGetMetricDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetMetricDataResponse
newBatchGetMetricDataResponse pHttpStatus_ =
  BatchGetMetricDataResponse'
    { errors =
        Prelude.Nothing,
      results = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @MetricDataError@ encountered while processing your metric
-- data batch request.
batchGetMetricDataResponse_errors :: Lens.Lens' BatchGetMetricDataResponse (Prelude.Maybe [MetricDataError])
batchGetMetricDataResponse_errors = Lens.lens (\BatchGetMetricDataResponse' {errors} -> errors) (\s@BatchGetMetricDataResponse' {} a -> s {errors = a} :: BatchGetMetricDataResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of successfully retrieved @MetricDataResult@.
batchGetMetricDataResponse_results :: Lens.Lens' BatchGetMetricDataResponse (Prelude.Maybe [MetricDataResult])
batchGetMetricDataResponse_results = Lens.lens (\BatchGetMetricDataResponse' {results} -> results) (\s@BatchGetMetricDataResponse' {} a -> s {results = a} :: BatchGetMetricDataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetMetricDataResponse_httpStatus :: Lens.Lens' BatchGetMetricDataResponse Prelude.Int
batchGetMetricDataResponse_httpStatus = Lens.lens (\BatchGetMetricDataResponse' {httpStatus} -> httpStatus) (\s@BatchGetMetricDataResponse' {} a -> s {httpStatus = a} :: BatchGetMetricDataResponse)

instance Prelude.NFData BatchGetMetricDataResponse where
  rnf BatchGetMetricDataResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf results
      `Prelude.seq` Prelude.rnf httpStatus
