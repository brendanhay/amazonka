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
-- Module      : Amazonka.LookoutMetrics.ListAnomalyGroupRelatedMetrics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of measures that are potential causes or effects of an
-- anomaly group.
module Amazonka.LookoutMetrics.ListAnomalyGroupRelatedMetrics
  ( -- * Creating a Request
    ListAnomalyGroupRelatedMetrics (..),
    newListAnomalyGroupRelatedMetrics,

    -- * Request Lenses
    listAnomalyGroupRelatedMetrics_nextToken,
    listAnomalyGroupRelatedMetrics_maxResults,
    listAnomalyGroupRelatedMetrics_relationshipTypeFilter,
    listAnomalyGroupRelatedMetrics_anomalyDetectorArn,
    listAnomalyGroupRelatedMetrics_anomalyGroupId,

    -- * Destructuring the Response
    ListAnomalyGroupRelatedMetricsResponse (..),
    newListAnomalyGroupRelatedMetricsResponse,

    -- * Response Lenses
    listAnomalyGroupRelatedMetricsResponse_nextToken,
    listAnomalyGroupRelatedMetricsResponse_interMetricImpactList,
    listAnomalyGroupRelatedMetricsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAnomalyGroupRelatedMetrics' smart constructor.
data ListAnomalyGroupRelatedMetrics = ListAnomalyGroupRelatedMetrics'
  { -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filter for potential causes (@CAUSE_OF_INPUT_ANOMALY_GROUP@) or
    -- downstream effects (@EFFECT_OF_INPUT_ANOMALY_GROUP@) of the anomaly
    -- group.
    relationshipTypeFilter :: Prelude.Maybe RelationshipType,
    -- | The Amazon Resource Name (ARN) of the anomaly detector.
    anomalyDetectorArn :: Prelude.Text,
    -- | The ID of the anomaly group.
    anomalyGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnomalyGroupRelatedMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAnomalyGroupRelatedMetrics_nextToken' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'maxResults', 'listAnomalyGroupRelatedMetrics_maxResults' - The maximum number of results to return.
--
-- 'relationshipTypeFilter', 'listAnomalyGroupRelatedMetrics_relationshipTypeFilter' - Filter for potential causes (@CAUSE_OF_INPUT_ANOMALY_GROUP@) or
-- downstream effects (@EFFECT_OF_INPUT_ANOMALY_GROUP@) of the anomaly
-- group.
--
-- 'anomalyDetectorArn', 'listAnomalyGroupRelatedMetrics_anomalyDetectorArn' - The Amazon Resource Name (ARN) of the anomaly detector.
--
-- 'anomalyGroupId', 'listAnomalyGroupRelatedMetrics_anomalyGroupId' - The ID of the anomaly group.
newListAnomalyGroupRelatedMetrics ::
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  -- | 'anomalyGroupId'
  Prelude.Text ->
  ListAnomalyGroupRelatedMetrics
newListAnomalyGroupRelatedMetrics
  pAnomalyDetectorArn_
  pAnomalyGroupId_ =
    ListAnomalyGroupRelatedMetrics'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        relationshipTypeFilter = Prelude.Nothing,
        anomalyDetectorArn = pAnomalyDetectorArn_,
        anomalyGroupId = pAnomalyGroupId_
      }

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
listAnomalyGroupRelatedMetrics_nextToken :: Lens.Lens' ListAnomalyGroupRelatedMetrics (Prelude.Maybe Prelude.Text)
listAnomalyGroupRelatedMetrics_nextToken = Lens.lens (\ListAnomalyGroupRelatedMetrics' {nextToken} -> nextToken) (\s@ListAnomalyGroupRelatedMetrics' {} a -> s {nextToken = a} :: ListAnomalyGroupRelatedMetrics)

-- | The maximum number of results to return.
listAnomalyGroupRelatedMetrics_maxResults :: Lens.Lens' ListAnomalyGroupRelatedMetrics (Prelude.Maybe Prelude.Natural)
listAnomalyGroupRelatedMetrics_maxResults = Lens.lens (\ListAnomalyGroupRelatedMetrics' {maxResults} -> maxResults) (\s@ListAnomalyGroupRelatedMetrics' {} a -> s {maxResults = a} :: ListAnomalyGroupRelatedMetrics)

-- | Filter for potential causes (@CAUSE_OF_INPUT_ANOMALY_GROUP@) or
-- downstream effects (@EFFECT_OF_INPUT_ANOMALY_GROUP@) of the anomaly
-- group.
listAnomalyGroupRelatedMetrics_relationshipTypeFilter :: Lens.Lens' ListAnomalyGroupRelatedMetrics (Prelude.Maybe RelationshipType)
listAnomalyGroupRelatedMetrics_relationshipTypeFilter = Lens.lens (\ListAnomalyGroupRelatedMetrics' {relationshipTypeFilter} -> relationshipTypeFilter) (\s@ListAnomalyGroupRelatedMetrics' {} a -> s {relationshipTypeFilter = a} :: ListAnomalyGroupRelatedMetrics)

-- | The Amazon Resource Name (ARN) of the anomaly detector.
listAnomalyGroupRelatedMetrics_anomalyDetectorArn :: Lens.Lens' ListAnomalyGroupRelatedMetrics Prelude.Text
listAnomalyGroupRelatedMetrics_anomalyDetectorArn = Lens.lens (\ListAnomalyGroupRelatedMetrics' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@ListAnomalyGroupRelatedMetrics' {} a -> s {anomalyDetectorArn = a} :: ListAnomalyGroupRelatedMetrics)

-- | The ID of the anomaly group.
listAnomalyGroupRelatedMetrics_anomalyGroupId :: Lens.Lens' ListAnomalyGroupRelatedMetrics Prelude.Text
listAnomalyGroupRelatedMetrics_anomalyGroupId = Lens.lens (\ListAnomalyGroupRelatedMetrics' {anomalyGroupId} -> anomalyGroupId) (\s@ListAnomalyGroupRelatedMetrics' {} a -> s {anomalyGroupId = a} :: ListAnomalyGroupRelatedMetrics)

instance
  Core.AWSRequest
    ListAnomalyGroupRelatedMetrics
  where
  type
    AWSResponse ListAnomalyGroupRelatedMetrics =
      ListAnomalyGroupRelatedMetricsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAnomalyGroupRelatedMetricsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "InterMetricImpactList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAnomalyGroupRelatedMetrics
  where
  hashWithSalt
    _salt
    ListAnomalyGroupRelatedMetrics' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` relationshipTypeFilter
        `Prelude.hashWithSalt` anomalyDetectorArn
        `Prelude.hashWithSalt` anomalyGroupId

instance
  Prelude.NFData
    ListAnomalyGroupRelatedMetrics
  where
  rnf ListAnomalyGroupRelatedMetrics' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf relationshipTypeFilter
      `Prelude.seq` Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf anomalyGroupId

instance
  Data.ToHeaders
    ListAnomalyGroupRelatedMetrics
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAnomalyGroupRelatedMetrics where
  toJSON ListAnomalyGroupRelatedMetrics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("RelationshipTypeFilter" Data..=)
              Prelude.<$> relationshipTypeFilter,
            Prelude.Just
              ("AnomalyDetectorArn" Data..= anomalyDetectorArn),
            Prelude.Just
              ("AnomalyGroupId" Data..= anomalyGroupId)
          ]
      )

instance Data.ToPath ListAnomalyGroupRelatedMetrics where
  toPath =
    Prelude.const "/ListAnomalyGroupRelatedMetrics"

instance Data.ToQuery ListAnomalyGroupRelatedMetrics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAnomalyGroupRelatedMetricsResponse' smart constructor.
data ListAnomalyGroupRelatedMetricsResponse = ListAnomalyGroupRelatedMetricsResponse'
  { -- | The pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Aggregated details about the measures contributing to the anomaly group,
    -- and the measures potentially impacted by the anomaly group.
    interMetricImpactList :: Prelude.Maybe [InterMetricImpactDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnomalyGroupRelatedMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAnomalyGroupRelatedMetricsResponse_nextToken' - The pagination token that\'s included if more results are available.
--
-- 'interMetricImpactList', 'listAnomalyGroupRelatedMetricsResponse_interMetricImpactList' - Aggregated details about the measures contributing to the anomaly group,
-- and the measures potentially impacted by the anomaly group.
--
-- 'httpStatus', 'listAnomalyGroupRelatedMetricsResponse_httpStatus' - The response's http status code.
newListAnomalyGroupRelatedMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAnomalyGroupRelatedMetricsResponse
newListAnomalyGroupRelatedMetricsResponse
  pHttpStatus_ =
    ListAnomalyGroupRelatedMetricsResponse'
      { nextToken =
          Prelude.Nothing,
        interMetricImpactList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The pagination token that\'s included if more results are available.
listAnomalyGroupRelatedMetricsResponse_nextToken :: Lens.Lens' ListAnomalyGroupRelatedMetricsResponse (Prelude.Maybe Prelude.Text)
listAnomalyGroupRelatedMetricsResponse_nextToken = Lens.lens (\ListAnomalyGroupRelatedMetricsResponse' {nextToken} -> nextToken) (\s@ListAnomalyGroupRelatedMetricsResponse' {} a -> s {nextToken = a} :: ListAnomalyGroupRelatedMetricsResponse)

-- | Aggregated details about the measures contributing to the anomaly group,
-- and the measures potentially impacted by the anomaly group.
listAnomalyGroupRelatedMetricsResponse_interMetricImpactList :: Lens.Lens' ListAnomalyGroupRelatedMetricsResponse (Prelude.Maybe [InterMetricImpactDetails])
listAnomalyGroupRelatedMetricsResponse_interMetricImpactList = Lens.lens (\ListAnomalyGroupRelatedMetricsResponse' {interMetricImpactList} -> interMetricImpactList) (\s@ListAnomalyGroupRelatedMetricsResponse' {} a -> s {interMetricImpactList = a} :: ListAnomalyGroupRelatedMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAnomalyGroupRelatedMetricsResponse_httpStatus :: Lens.Lens' ListAnomalyGroupRelatedMetricsResponse Prelude.Int
listAnomalyGroupRelatedMetricsResponse_httpStatus = Lens.lens (\ListAnomalyGroupRelatedMetricsResponse' {httpStatus} -> httpStatus) (\s@ListAnomalyGroupRelatedMetricsResponse' {} a -> s {httpStatus = a} :: ListAnomalyGroupRelatedMetricsResponse)

instance
  Prelude.NFData
    ListAnomalyGroupRelatedMetricsResponse
  where
  rnf ListAnomalyGroupRelatedMetricsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf interMetricImpactList
      `Prelude.seq` Prelude.rnf httpStatus
