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
-- Module      : Amazonka.LookoutMetrics.GetDataQualityMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the requested data quality metrics.
module Amazonka.LookoutMetrics.GetDataQualityMetrics
  ( -- * Creating a Request
    GetDataQualityMetrics (..),
    newGetDataQualityMetrics,

    -- * Request Lenses
    getDataQualityMetrics_metricSetArn,
    getDataQualityMetrics_anomalyDetectorArn,

    -- * Destructuring the Response
    GetDataQualityMetricsResponse (..),
    newGetDataQualityMetricsResponse,

    -- * Response Lenses
    getDataQualityMetricsResponse_anomalyDetectorDataQualityMetricList,
    getDataQualityMetricsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataQualityMetrics' smart constructor.
data GetDataQualityMetrics = GetDataQualityMetrics'
  { -- | The Amazon Resource Name (ARN) of a specific data quality metric set.
    metricSetArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the anomaly detector that you want to
    -- investigate.
    anomalyDetectorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataQualityMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricSetArn', 'getDataQualityMetrics_metricSetArn' - The Amazon Resource Name (ARN) of a specific data quality metric set.
--
-- 'anomalyDetectorArn', 'getDataQualityMetrics_anomalyDetectorArn' - The Amazon Resource Name (ARN) of the anomaly detector that you want to
-- investigate.
newGetDataQualityMetrics ::
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  GetDataQualityMetrics
newGetDataQualityMetrics pAnomalyDetectorArn_ =
  GetDataQualityMetrics'
    { metricSetArn =
        Prelude.Nothing,
      anomalyDetectorArn = pAnomalyDetectorArn_
    }

-- | The Amazon Resource Name (ARN) of a specific data quality metric set.
getDataQualityMetrics_metricSetArn :: Lens.Lens' GetDataQualityMetrics (Prelude.Maybe Prelude.Text)
getDataQualityMetrics_metricSetArn = Lens.lens (\GetDataQualityMetrics' {metricSetArn} -> metricSetArn) (\s@GetDataQualityMetrics' {} a -> s {metricSetArn = a} :: GetDataQualityMetrics)

-- | The Amazon Resource Name (ARN) of the anomaly detector that you want to
-- investigate.
getDataQualityMetrics_anomalyDetectorArn :: Lens.Lens' GetDataQualityMetrics Prelude.Text
getDataQualityMetrics_anomalyDetectorArn = Lens.lens (\GetDataQualityMetrics' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@GetDataQualityMetrics' {} a -> s {anomalyDetectorArn = a} :: GetDataQualityMetrics)

instance Core.AWSRequest GetDataQualityMetrics where
  type
    AWSResponse GetDataQualityMetrics =
      GetDataQualityMetricsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataQualityMetricsResponse'
            Prelude.<$> ( x Data..?> "AnomalyDetectorDataQualityMetricList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataQualityMetrics where
  hashWithSalt _salt GetDataQualityMetrics' {..} =
    _salt `Prelude.hashWithSalt` metricSetArn
      `Prelude.hashWithSalt` anomalyDetectorArn

instance Prelude.NFData GetDataQualityMetrics where
  rnf GetDataQualityMetrics' {..} =
    Prelude.rnf metricSetArn
      `Prelude.seq` Prelude.rnf anomalyDetectorArn

instance Data.ToHeaders GetDataQualityMetrics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDataQualityMetrics where
  toJSON GetDataQualityMetrics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MetricSetArn" Data..=) Prelude.<$> metricSetArn,
            Prelude.Just
              ("AnomalyDetectorArn" Data..= anomalyDetectorArn)
          ]
      )

instance Data.ToPath GetDataQualityMetrics where
  toPath = Prelude.const "/GetDataQualityMetrics"

instance Data.ToQuery GetDataQualityMetrics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataQualityMetricsResponse' smart constructor.
data GetDataQualityMetricsResponse = GetDataQualityMetricsResponse'
  { -- | A list of the data quality metrics for the @AnomalyDetectorArn@ that you
    -- requested.
    anomalyDetectorDataQualityMetricList :: Prelude.Maybe [AnomalyDetectorDataQualityMetric],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataQualityMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyDetectorDataQualityMetricList', 'getDataQualityMetricsResponse_anomalyDetectorDataQualityMetricList' - A list of the data quality metrics for the @AnomalyDetectorArn@ that you
-- requested.
--
-- 'httpStatus', 'getDataQualityMetricsResponse_httpStatus' - The response's http status code.
newGetDataQualityMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataQualityMetricsResponse
newGetDataQualityMetricsResponse pHttpStatus_ =
  GetDataQualityMetricsResponse'
    { anomalyDetectorDataQualityMetricList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the data quality metrics for the @AnomalyDetectorArn@ that you
-- requested.
getDataQualityMetricsResponse_anomalyDetectorDataQualityMetricList :: Lens.Lens' GetDataQualityMetricsResponse (Prelude.Maybe [AnomalyDetectorDataQualityMetric])
getDataQualityMetricsResponse_anomalyDetectorDataQualityMetricList = Lens.lens (\GetDataQualityMetricsResponse' {anomalyDetectorDataQualityMetricList} -> anomalyDetectorDataQualityMetricList) (\s@GetDataQualityMetricsResponse' {} a -> s {anomalyDetectorDataQualityMetricList = a} :: GetDataQualityMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDataQualityMetricsResponse_httpStatus :: Lens.Lens' GetDataQualityMetricsResponse Prelude.Int
getDataQualityMetricsResponse_httpStatus = Lens.lens (\GetDataQualityMetricsResponse' {httpStatus} -> httpStatus) (\s@GetDataQualityMetricsResponse' {} a -> s {httpStatus = a} :: GetDataQualityMetricsResponse)

instance Prelude.NFData GetDataQualityMetricsResponse where
  rnf GetDataQualityMetricsResponse' {..} =
    Prelude.rnf anomalyDetectorDataQualityMetricList
      `Prelude.seq` Prelude.rnf httpStatus
