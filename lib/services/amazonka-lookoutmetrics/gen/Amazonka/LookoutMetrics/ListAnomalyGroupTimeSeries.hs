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
-- Module      : Amazonka.LookoutMetrics.ListAnomalyGroupTimeSeries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of anomalous metrics for a measure in an anomaly group.
module Amazonka.LookoutMetrics.ListAnomalyGroupTimeSeries
  ( -- * Creating a Request
    ListAnomalyGroupTimeSeries (..),
    newListAnomalyGroupTimeSeries,

    -- * Request Lenses
    listAnomalyGroupTimeSeries_maxResults,
    listAnomalyGroupTimeSeries_nextToken,
    listAnomalyGroupTimeSeries_anomalyDetectorArn,
    listAnomalyGroupTimeSeries_anomalyGroupId,
    listAnomalyGroupTimeSeries_metricName,

    -- * Destructuring the Response
    ListAnomalyGroupTimeSeriesResponse (..),
    newListAnomalyGroupTimeSeriesResponse,

    -- * Response Lenses
    listAnomalyGroupTimeSeriesResponse_anomalyGroupId,
    listAnomalyGroupTimeSeriesResponse_metricName,
    listAnomalyGroupTimeSeriesResponse_nextToken,
    listAnomalyGroupTimeSeriesResponse_timeSeriesList,
    listAnomalyGroupTimeSeriesResponse_timestampList,
    listAnomalyGroupTimeSeriesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAnomalyGroupTimeSeries' smart constructor.
data ListAnomalyGroupTimeSeries = ListAnomalyGroupTimeSeries'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the anomaly detector.
    anomalyDetectorArn :: Prelude.Text,
    -- | The ID of the anomaly group.
    anomalyGroupId :: Prelude.Text,
    -- | The name of the measure field.
    metricName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnomalyGroupTimeSeries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAnomalyGroupTimeSeries_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listAnomalyGroupTimeSeries_nextToken' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'anomalyDetectorArn', 'listAnomalyGroupTimeSeries_anomalyDetectorArn' - The Amazon Resource Name (ARN) of the anomaly detector.
--
-- 'anomalyGroupId', 'listAnomalyGroupTimeSeries_anomalyGroupId' - The ID of the anomaly group.
--
-- 'metricName', 'listAnomalyGroupTimeSeries_metricName' - The name of the measure field.
newListAnomalyGroupTimeSeries ::
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  -- | 'anomalyGroupId'
  Prelude.Text ->
  -- | 'metricName'
  Prelude.Text ->
  ListAnomalyGroupTimeSeries
newListAnomalyGroupTimeSeries
  pAnomalyDetectorArn_
  pAnomalyGroupId_
  pMetricName_ =
    ListAnomalyGroupTimeSeries'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        anomalyDetectorArn = pAnomalyDetectorArn_,
        anomalyGroupId = pAnomalyGroupId_,
        metricName = pMetricName_
      }

-- | The maximum number of results to return.
listAnomalyGroupTimeSeries_maxResults :: Lens.Lens' ListAnomalyGroupTimeSeries (Prelude.Maybe Prelude.Natural)
listAnomalyGroupTimeSeries_maxResults = Lens.lens (\ListAnomalyGroupTimeSeries' {maxResults} -> maxResults) (\s@ListAnomalyGroupTimeSeries' {} a -> s {maxResults = a} :: ListAnomalyGroupTimeSeries)

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listAnomalyGroupTimeSeries_nextToken :: Lens.Lens' ListAnomalyGroupTimeSeries (Prelude.Maybe Prelude.Text)
listAnomalyGroupTimeSeries_nextToken = Lens.lens (\ListAnomalyGroupTimeSeries' {nextToken} -> nextToken) (\s@ListAnomalyGroupTimeSeries' {} a -> s {nextToken = a} :: ListAnomalyGroupTimeSeries)

-- | The Amazon Resource Name (ARN) of the anomaly detector.
listAnomalyGroupTimeSeries_anomalyDetectorArn :: Lens.Lens' ListAnomalyGroupTimeSeries Prelude.Text
listAnomalyGroupTimeSeries_anomalyDetectorArn = Lens.lens (\ListAnomalyGroupTimeSeries' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@ListAnomalyGroupTimeSeries' {} a -> s {anomalyDetectorArn = a} :: ListAnomalyGroupTimeSeries)

-- | The ID of the anomaly group.
listAnomalyGroupTimeSeries_anomalyGroupId :: Lens.Lens' ListAnomalyGroupTimeSeries Prelude.Text
listAnomalyGroupTimeSeries_anomalyGroupId = Lens.lens (\ListAnomalyGroupTimeSeries' {anomalyGroupId} -> anomalyGroupId) (\s@ListAnomalyGroupTimeSeries' {} a -> s {anomalyGroupId = a} :: ListAnomalyGroupTimeSeries)

-- | The name of the measure field.
listAnomalyGroupTimeSeries_metricName :: Lens.Lens' ListAnomalyGroupTimeSeries Prelude.Text
listAnomalyGroupTimeSeries_metricName = Lens.lens (\ListAnomalyGroupTimeSeries' {metricName} -> metricName) (\s@ListAnomalyGroupTimeSeries' {} a -> s {metricName = a} :: ListAnomalyGroupTimeSeries)

instance Core.AWSRequest ListAnomalyGroupTimeSeries where
  type
    AWSResponse ListAnomalyGroupTimeSeries =
      ListAnomalyGroupTimeSeriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAnomalyGroupTimeSeriesResponse'
            Prelude.<$> (x Data..?> "AnomalyGroupId")
            Prelude.<*> (x Data..?> "MetricName")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "TimeSeriesList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "TimestampList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAnomalyGroupTimeSeries where
  hashWithSalt _salt ListAnomalyGroupTimeSeries' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` anomalyDetectorArn
      `Prelude.hashWithSalt` anomalyGroupId
      `Prelude.hashWithSalt` metricName

instance Prelude.NFData ListAnomalyGroupTimeSeries where
  rnf ListAnomalyGroupTimeSeries' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf anomalyGroupId
      `Prelude.seq` Prelude.rnf metricName

instance Data.ToHeaders ListAnomalyGroupTimeSeries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAnomalyGroupTimeSeries where
  toJSON ListAnomalyGroupTimeSeries' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("AnomalyDetectorArn" Data..= anomalyDetectorArn),
            Prelude.Just
              ("AnomalyGroupId" Data..= anomalyGroupId),
            Prelude.Just ("MetricName" Data..= metricName)
          ]
      )

instance Data.ToPath ListAnomalyGroupTimeSeries where
  toPath = Prelude.const "/ListAnomalyGroupTimeSeries"

instance Data.ToQuery ListAnomalyGroupTimeSeries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAnomalyGroupTimeSeriesResponse' smart constructor.
data ListAnomalyGroupTimeSeriesResponse = ListAnomalyGroupTimeSeriesResponse'
  { -- | The ID of the anomaly group.
    anomalyGroupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the measure field.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of anomalous metrics.
    timeSeriesList :: Prelude.Maybe [TimeSeries],
    -- | Timestamps for the anomalous metrics.
    timestampList :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnomalyGroupTimeSeriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyGroupId', 'listAnomalyGroupTimeSeriesResponse_anomalyGroupId' - The ID of the anomaly group.
--
-- 'metricName', 'listAnomalyGroupTimeSeriesResponse_metricName' - The name of the measure field.
--
-- 'nextToken', 'listAnomalyGroupTimeSeriesResponse_nextToken' - The pagination token that\'s included if more results are available.
--
-- 'timeSeriesList', 'listAnomalyGroupTimeSeriesResponse_timeSeriesList' - A list of anomalous metrics.
--
-- 'timestampList', 'listAnomalyGroupTimeSeriesResponse_timestampList' - Timestamps for the anomalous metrics.
--
-- 'httpStatus', 'listAnomalyGroupTimeSeriesResponse_httpStatus' - The response's http status code.
newListAnomalyGroupTimeSeriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAnomalyGroupTimeSeriesResponse
newListAnomalyGroupTimeSeriesResponse pHttpStatus_ =
  ListAnomalyGroupTimeSeriesResponse'
    { anomalyGroupId =
        Prelude.Nothing,
      metricName = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      timeSeriesList = Prelude.Nothing,
      timestampList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the anomaly group.
listAnomalyGroupTimeSeriesResponse_anomalyGroupId :: Lens.Lens' ListAnomalyGroupTimeSeriesResponse (Prelude.Maybe Prelude.Text)
listAnomalyGroupTimeSeriesResponse_anomalyGroupId = Lens.lens (\ListAnomalyGroupTimeSeriesResponse' {anomalyGroupId} -> anomalyGroupId) (\s@ListAnomalyGroupTimeSeriesResponse' {} a -> s {anomalyGroupId = a} :: ListAnomalyGroupTimeSeriesResponse)

-- | The name of the measure field.
listAnomalyGroupTimeSeriesResponse_metricName :: Lens.Lens' ListAnomalyGroupTimeSeriesResponse (Prelude.Maybe Prelude.Text)
listAnomalyGroupTimeSeriesResponse_metricName = Lens.lens (\ListAnomalyGroupTimeSeriesResponse' {metricName} -> metricName) (\s@ListAnomalyGroupTimeSeriesResponse' {} a -> s {metricName = a} :: ListAnomalyGroupTimeSeriesResponse)

-- | The pagination token that\'s included if more results are available.
listAnomalyGroupTimeSeriesResponse_nextToken :: Lens.Lens' ListAnomalyGroupTimeSeriesResponse (Prelude.Maybe Prelude.Text)
listAnomalyGroupTimeSeriesResponse_nextToken = Lens.lens (\ListAnomalyGroupTimeSeriesResponse' {nextToken} -> nextToken) (\s@ListAnomalyGroupTimeSeriesResponse' {} a -> s {nextToken = a} :: ListAnomalyGroupTimeSeriesResponse)

-- | A list of anomalous metrics.
listAnomalyGroupTimeSeriesResponse_timeSeriesList :: Lens.Lens' ListAnomalyGroupTimeSeriesResponse (Prelude.Maybe [TimeSeries])
listAnomalyGroupTimeSeriesResponse_timeSeriesList = Lens.lens (\ListAnomalyGroupTimeSeriesResponse' {timeSeriesList} -> timeSeriesList) (\s@ListAnomalyGroupTimeSeriesResponse' {} a -> s {timeSeriesList = a} :: ListAnomalyGroupTimeSeriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Timestamps for the anomalous metrics.
listAnomalyGroupTimeSeriesResponse_timestampList :: Lens.Lens' ListAnomalyGroupTimeSeriesResponse (Prelude.Maybe [Prelude.Text])
listAnomalyGroupTimeSeriesResponse_timestampList = Lens.lens (\ListAnomalyGroupTimeSeriesResponse' {timestampList} -> timestampList) (\s@ListAnomalyGroupTimeSeriesResponse' {} a -> s {timestampList = a} :: ListAnomalyGroupTimeSeriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAnomalyGroupTimeSeriesResponse_httpStatus :: Lens.Lens' ListAnomalyGroupTimeSeriesResponse Prelude.Int
listAnomalyGroupTimeSeriesResponse_httpStatus = Lens.lens (\ListAnomalyGroupTimeSeriesResponse' {httpStatus} -> httpStatus) (\s@ListAnomalyGroupTimeSeriesResponse' {} a -> s {httpStatus = a} :: ListAnomalyGroupTimeSeriesResponse)

instance
  Prelude.NFData
    ListAnomalyGroupTimeSeriesResponse
  where
  rnf ListAnomalyGroupTimeSeriesResponse' {..} =
    Prelude.rnf anomalyGroupId
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf timeSeriesList
      `Prelude.seq` Prelude.rnf timestampList
      `Prelude.seq` Prelude.rnf httpStatus
