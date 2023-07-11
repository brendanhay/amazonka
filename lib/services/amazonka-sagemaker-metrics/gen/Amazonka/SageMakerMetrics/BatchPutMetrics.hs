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
-- Module      : Amazonka.SageMakerMetrics.BatchPutMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to ingest training metrics into SageMaker. These metrics can be
-- visualized in SageMaker Studio and retrieved with the @GetMetrics@ API.
module Amazonka.SageMakerMetrics.BatchPutMetrics
  ( -- * Creating a Request
    BatchPutMetrics (..),
    newBatchPutMetrics,

    -- * Request Lenses
    batchPutMetrics_trialComponentName,
    batchPutMetrics_metricData,

    -- * Destructuring the Response
    BatchPutMetricsResponse (..),
    newBatchPutMetricsResponse,

    -- * Response Lenses
    batchPutMetricsResponse_errors,
    batchPutMetricsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerMetrics.Types

-- | /See:/ 'newBatchPutMetrics' smart constructor.
data BatchPutMetrics = BatchPutMetrics'
  { -- | The name of the Trial Component to associate with the metrics.
    trialComponentName :: Prelude.Text,
    -- | A list of raw metric values to put.
    metricData :: Prelude.NonEmpty RawMetricData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialComponentName', 'batchPutMetrics_trialComponentName' - The name of the Trial Component to associate with the metrics.
--
-- 'metricData', 'batchPutMetrics_metricData' - A list of raw metric values to put.
newBatchPutMetrics ::
  -- | 'trialComponentName'
  Prelude.Text ->
  -- | 'metricData'
  Prelude.NonEmpty RawMetricData ->
  BatchPutMetrics
newBatchPutMetrics pTrialComponentName_ pMetricData_ =
  BatchPutMetrics'
    { trialComponentName =
        pTrialComponentName_,
      metricData = Lens.coerced Lens.# pMetricData_
    }

-- | The name of the Trial Component to associate with the metrics.
batchPutMetrics_trialComponentName :: Lens.Lens' BatchPutMetrics Prelude.Text
batchPutMetrics_trialComponentName = Lens.lens (\BatchPutMetrics' {trialComponentName} -> trialComponentName) (\s@BatchPutMetrics' {} a -> s {trialComponentName = a} :: BatchPutMetrics)

-- | A list of raw metric values to put.
batchPutMetrics_metricData :: Lens.Lens' BatchPutMetrics (Prelude.NonEmpty RawMetricData)
batchPutMetrics_metricData = Lens.lens (\BatchPutMetrics' {metricData} -> metricData) (\s@BatchPutMetrics' {} a -> s {metricData = a} :: BatchPutMetrics) Prelude.. Lens.coerced

instance Core.AWSRequest BatchPutMetrics where
  type
    AWSResponse BatchPutMetrics =
      BatchPutMetricsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchPutMetricsResponse'
            Prelude.<$> (x Data..?> "Errors")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchPutMetrics where
  hashWithSalt _salt BatchPutMetrics' {..} =
    _salt
      `Prelude.hashWithSalt` trialComponentName
      `Prelude.hashWithSalt` metricData

instance Prelude.NFData BatchPutMetrics where
  rnf BatchPutMetrics' {..} =
    Prelude.rnf trialComponentName
      `Prelude.seq` Prelude.rnf metricData

instance Data.ToHeaders BatchPutMetrics where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON BatchPutMetrics where
  toJSON BatchPutMetrics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TrialComponentName" Data..= trialComponentName),
            Prelude.Just ("MetricData" Data..= metricData)
          ]
      )

instance Data.ToPath BatchPutMetrics where
  toPath = Prelude.const "/BatchPutMetrics"

instance Data.ToQuery BatchPutMetrics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchPutMetricsResponse' smart constructor.
data BatchPutMetricsResponse = BatchPutMetricsResponse'
  { -- | Lists any errors that occur when inserting metric data.
    errors :: Prelude.Maybe (Prelude.NonEmpty BatchPutMetricsError),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchPutMetricsResponse_errors' - Lists any errors that occur when inserting metric data.
--
-- 'httpStatus', 'batchPutMetricsResponse_httpStatus' - The response's http status code.
newBatchPutMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchPutMetricsResponse
newBatchPutMetricsResponse pHttpStatus_ =
  BatchPutMetricsResponse'
    { errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists any errors that occur when inserting metric data.
batchPutMetricsResponse_errors :: Lens.Lens' BatchPutMetricsResponse (Prelude.Maybe (Prelude.NonEmpty BatchPutMetricsError))
batchPutMetricsResponse_errors = Lens.lens (\BatchPutMetricsResponse' {errors} -> errors) (\s@BatchPutMetricsResponse' {} a -> s {errors = a} :: BatchPutMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchPutMetricsResponse_httpStatus :: Lens.Lens' BatchPutMetricsResponse Prelude.Int
batchPutMetricsResponse_httpStatus = Lens.lens (\BatchPutMetricsResponse' {httpStatus} -> httpStatus) (\s@BatchPutMetricsResponse' {} a -> s {httpStatus = a} :: BatchPutMetricsResponse)

instance Prelude.NFData BatchPutMetricsResponse where
  rnf BatchPutMetricsResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
