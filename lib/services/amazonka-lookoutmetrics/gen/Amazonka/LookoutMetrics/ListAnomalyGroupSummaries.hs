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
-- Module      : Amazonka.LookoutMetrics.ListAnomalyGroupSummaries
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of anomaly groups.
module Amazonka.LookoutMetrics.ListAnomalyGroupSummaries
  ( -- * Creating a Request
    ListAnomalyGroupSummaries (..),
    newListAnomalyGroupSummaries,

    -- * Request Lenses
    listAnomalyGroupSummaries_nextToken,
    listAnomalyGroupSummaries_maxResults,
    listAnomalyGroupSummaries_anomalyDetectorArn,
    listAnomalyGroupSummaries_sensitivityThreshold,

    -- * Destructuring the Response
    ListAnomalyGroupSummariesResponse (..),
    newListAnomalyGroupSummariesResponse,

    -- * Response Lenses
    listAnomalyGroupSummariesResponse_anomalyGroupSummaryList,
    listAnomalyGroupSummariesResponse_nextToken,
    listAnomalyGroupSummariesResponse_anomalyGroupStatistics,
    listAnomalyGroupSummariesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAnomalyGroupSummaries' smart constructor.
data ListAnomalyGroupSummaries = ListAnomalyGroupSummaries'
  { -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the anomaly detector.
    anomalyDetectorArn :: Prelude.Text,
    -- | The minimum severity score for inclusion in the output.
    sensitivityThreshold :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnomalyGroupSummaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAnomalyGroupSummaries_nextToken' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'maxResults', 'listAnomalyGroupSummaries_maxResults' - The maximum number of results to return.
--
-- 'anomalyDetectorArn', 'listAnomalyGroupSummaries_anomalyDetectorArn' - The Amazon Resource Name (ARN) of the anomaly detector.
--
-- 'sensitivityThreshold', 'listAnomalyGroupSummaries_sensitivityThreshold' - The minimum severity score for inclusion in the output.
newListAnomalyGroupSummaries ::
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  -- | 'sensitivityThreshold'
  Prelude.Natural ->
  ListAnomalyGroupSummaries
newListAnomalyGroupSummaries
  pAnomalyDetectorArn_
  pSensitivityThreshold_ =
    ListAnomalyGroupSummaries'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        anomalyDetectorArn = pAnomalyDetectorArn_,
        sensitivityThreshold = pSensitivityThreshold_
      }

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listAnomalyGroupSummaries_nextToken :: Lens.Lens' ListAnomalyGroupSummaries (Prelude.Maybe Prelude.Text)
listAnomalyGroupSummaries_nextToken = Lens.lens (\ListAnomalyGroupSummaries' {nextToken} -> nextToken) (\s@ListAnomalyGroupSummaries' {} a -> s {nextToken = a} :: ListAnomalyGroupSummaries)

-- | The maximum number of results to return.
listAnomalyGroupSummaries_maxResults :: Lens.Lens' ListAnomalyGroupSummaries (Prelude.Maybe Prelude.Natural)
listAnomalyGroupSummaries_maxResults = Lens.lens (\ListAnomalyGroupSummaries' {maxResults} -> maxResults) (\s@ListAnomalyGroupSummaries' {} a -> s {maxResults = a} :: ListAnomalyGroupSummaries)

-- | The Amazon Resource Name (ARN) of the anomaly detector.
listAnomalyGroupSummaries_anomalyDetectorArn :: Lens.Lens' ListAnomalyGroupSummaries Prelude.Text
listAnomalyGroupSummaries_anomalyDetectorArn = Lens.lens (\ListAnomalyGroupSummaries' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@ListAnomalyGroupSummaries' {} a -> s {anomalyDetectorArn = a} :: ListAnomalyGroupSummaries)

-- | The minimum severity score for inclusion in the output.
listAnomalyGroupSummaries_sensitivityThreshold :: Lens.Lens' ListAnomalyGroupSummaries Prelude.Natural
listAnomalyGroupSummaries_sensitivityThreshold = Lens.lens (\ListAnomalyGroupSummaries' {sensitivityThreshold} -> sensitivityThreshold) (\s@ListAnomalyGroupSummaries' {} a -> s {sensitivityThreshold = a} :: ListAnomalyGroupSummaries)

instance Core.AWSRequest ListAnomalyGroupSummaries where
  type
    AWSResponse ListAnomalyGroupSummaries =
      ListAnomalyGroupSummariesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAnomalyGroupSummariesResponse'
            Prelude.<$> ( x Data..?> "AnomalyGroupSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "AnomalyGroupStatistics")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAnomalyGroupSummaries where
  hashWithSalt _salt ListAnomalyGroupSummaries' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` anomalyDetectorArn
      `Prelude.hashWithSalt` sensitivityThreshold

instance Prelude.NFData ListAnomalyGroupSummaries where
  rnf ListAnomalyGroupSummaries' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf sensitivityThreshold

instance Data.ToHeaders ListAnomalyGroupSummaries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAnomalyGroupSummaries where
  toJSON ListAnomalyGroupSummaries' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just
              ("AnomalyDetectorArn" Data..= anomalyDetectorArn),
            Prelude.Just
              ( "SensitivityThreshold"
                  Data..= sensitivityThreshold
              )
          ]
      )

instance Data.ToPath ListAnomalyGroupSummaries where
  toPath = Prelude.const "/ListAnomalyGroupSummaries"

instance Data.ToQuery ListAnomalyGroupSummaries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAnomalyGroupSummariesResponse' smart constructor.
data ListAnomalyGroupSummariesResponse = ListAnomalyGroupSummariesResponse'
  { -- | A list of anomaly group summaries.
    anomalyGroupSummaryList :: Prelude.Maybe [AnomalyGroupSummary],
    -- | The pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Aggregated details about the anomaly groups.
    anomalyGroupStatistics :: Prelude.Maybe AnomalyGroupStatistics,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnomalyGroupSummariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyGroupSummaryList', 'listAnomalyGroupSummariesResponse_anomalyGroupSummaryList' - A list of anomaly group summaries.
--
-- 'nextToken', 'listAnomalyGroupSummariesResponse_nextToken' - The pagination token that\'s included if more results are available.
--
-- 'anomalyGroupStatistics', 'listAnomalyGroupSummariesResponse_anomalyGroupStatistics' - Aggregated details about the anomaly groups.
--
-- 'httpStatus', 'listAnomalyGroupSummariesResponse_httpStatus' - The response's http status code.
newListAnomalyGroupSummariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAnomalyGroupSummariesResponse
newListAnomalyGroupSummariesResponse pHttpStatus_ =
  ListAnomalyGroupSummariesResponse'
    { anomalyGroupSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      anomalyGroupStatistics = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of anomaly group summaries.
listAnomalyGroupSummariesResponse_anomalyGroupSummaryList :: Lens.Lens' ListAnomalyGroupSummariesResponse (Prelude.Maybe [AnomalyGroupSummary])
listAnomalyGroupSummariesResponse_anomalyGroupSummaryList = Lens.lens (\ListAnomalyGroupSummariesResponse' {anomalyGroupSummaryList} -> anomalyGroupSummaryList) (\s@ListAnomalyGroupSummariesResponse' {} a -> s {anomalyGroupSummaryList = a} :: ListAnomalyGroupSummariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s included if more results are available.
listAnomalyGroupSummariesResponse_nextToken :: Lens.Lens' ListAnomalyGroupSummariesResponse (Prelude.Maybe Prelude.Text)
listAnomalyGroupSummariesResponse_nextToken = Lens.lens (\ListAnomalyGroupSummariesResponse' {nextToken} -> nextToken) (\s@ListAnomalyGroupSummariesResponse' {} a -> s {nextToken = a} :: ListAnomalyGroupSummariesResponse)

-- | Aggregated details about the anomaly groups.
listAnomalyGroupSummariesResponse_anomalyGroupStatistics :: Lens.Lens' ListAnomalyGroupSummariesResponse (Prelude.Maybe AnomalyGroupStatistics)
listAnomalyGroupSummariesResponse_anomalyGroupStatistics = Lens.lens (\ListAnomalyGroupSummariesResponse' {anomalyGroupStatistics} -> anomalyGroupStatistics) (\s@ListAnomalyGroupSummariesResponse' {} a -> s {anomalyGroupStatistics = a} :: ListAnomalyGroupSummariesResponse)

-- | The response's http status code.
listAnomalyGroupSummariesResponse_httpStatus :: Lens.Lens' ListAnomalyGroupSummariesResponse Prelude.Int
listAnomalyGroupSummariesResponse_httpStatus = Lens.lens (\ListAnomalyGroupSummariesResponse' {httpStatus} -> httpStatus) (\s@ListAnomalyGroupSummariesResponse' {} a -> s {httpStatus = a} :: ListAnomalyGroupSummariesResponse)

instance
  Prelude.NFData
    ListAnomalyGroupSummariesResponse
  where
  rnf ListAnomalyGroupSummariesResponse' {..} =
    Prelude.rnf anomalyGroupSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf anomalyGroupStatistics
      `Prelude.seq` Prelude.rnf httpStatus
