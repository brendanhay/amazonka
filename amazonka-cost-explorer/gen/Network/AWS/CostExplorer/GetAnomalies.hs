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
-- Module      : Network.AWS.CostExplorer.GetAnomalies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all of the cost anomalies detected on your account, during the
-- time period specified by the @DateInterval@ object.
module Network.AWS.CostExplorer.GetAnomalies
  ( -- * Creating a Request
    GetAnomalies (..),
    newGetAnomalies,

    -- * Request Lenses
    getAnomalies_maxResults,
    getAnomalies_nextPageToken,
    getAnomalies_monitorArn,
    getAnomalies_feedback,
    getAnomalies_totalImpact,
    getAnomalies_dateInterval,

    -- * Destructuring the Response
    GetAnomaliesResponse (..),
    newGetAnomaliesResponse,

    -- * Response Lenses
    getAnomaliesResponse_nextPageToken,
    getAnomaliesResponse_httpStatus,
    getAnomaliesResponse_anomalies,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAnomalies' smart constructor.
data GetAnomalies = GetAnomalies'
  { -- | The number of entries a paginated response contains.
    maxResults :: Core.Maybe Core.Int,
    -- | The token to retrieve the next set of results. AWS provides the token
    -- when the response from a previous call has more results than the maximum
    -- page size.
    nextPageToken :: Core.Maybe Core.Text,
    -- | Retrieves all of the cost anomalies detected for a specific cost anomaly
    -- monitor Amazon Resource Name (ARN).
    monitorArn :: Core.Maybe Core.Text,
    -- | Filters anomaly results by the feedback field on the anomaly object.
    feedback :: Core.Maybe AnomalyFeedbackType,
    -- | Filters anomaly results by the total impact field on the anomaly object.
    -- For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve
    -- anomalies, with an estimated dollar impact greater than 200.
    totalImpact :: Core.Maybe TotalImpactFilter,
    -- | Assigns the start and end dates for retrieving cost anomalies. The
    -- returned anomaly object will have an @AnomalyEndDate@ in the specified
    -- time range.
    dateInterval :: AnomalyDateInterval
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAnomalies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getAnomalies_maxResults' - The number of entries a paginated response contains.
--
-- 'nextPageToken', 'getAnomalies_nextPageToken' - The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
--
-- 'monitorArn', 'getAnomalies_monitorArn' - Retrieves all of the cost anomalies detected for a specific cost anomaly
-- monitor Amazon Resource Name (ARN).
--
-- 'feedback', 'getAnomalies_feedback' - Filters anomaly results by the feedback field on the anomaly object.
--
-- 'totalImpact', 'getAnomalies_totalImpact' - Filters anomaly results by the total impact field on the anomaly object.
-- For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve
-- anomalies, with an estimated dollar impact greater than 200.
--
-- 'dateInterval', 'getAnomalies_dateInterval' - Assigns the start and end dates for retrieving cost anomalies. The
-- returned anomaly object will have an @AnomalyEndDate@ in the specified
-- time range.
newGetAnomalies ::
  -- | 'dateInterval'
  AnomalyDateInterval ->
  GetAnomalies
newGetAnomalies pDateInterval_ =
  GetAnomalies'
    { maxResults = Core.Nothing,
      nextPageToken = Core.Nothing,
      monitorArn = Core.Nothing,
      feedback = Core.Nothing,
      totalImpact = Core.Nothing,
      dateInterval = pDateInterval_
    }

-- | The number of entries a paginated response contains.
getAnomalies_maxResults :: Lens.Lens' GetAnomalies (Core.Maybe Core.Int)
getAnomalies_maxResults = Lens.lens (\GetAnomalies' {maxResults} -> maxResults) (\s@GetAnomalies' {} a -> s {maxResults = a} :: GetAnomalies)

-- | The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
getAnomalies_nextPageToken :: Lens.Lens' GetAnomalies (Core.Maybe Core.Text)
getAnomalies_nextPageToken = Lens.lens (\GetAnomalies' {nextPageToken} -> nextPageToken) (\s@GetAnomalies' {} a -> s {nextPageToken = a} :: GetAnomalies)

-- | Retrieves all of the cost anomalies detected for a specific cost anomaly
-- monitor Amazon Resource Name (ARN).
getAnomalies_monitorArn :: Lens.Lens' GetAnomalies (Core.Maybe Core.Text)
getAnomalies_monitorArn = Lens.lens (\GetAnomalies' {monitorArn} -> monitorArn) (\s@GetAnomalies' {} a -> s {monitorArn = a} :: GetAnomalies)

-- | Filters anomaly results by the feedback field on the anomaly object.
getAnomalies_feedback :: Lens.Lens' GetAnomalies (Core.Maybe AnomalyFeedbackType)
getAnomalies_feedback = Lens.lens (\GetAnomalies' {feedback} -> feedback) (\s@GetAnomalies' {} a -> s {feedback = a} :: GetAnomalies)

-- | Filters anomaly results by the total impact field on the anomaly object.
-- For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve
-- anomalies, with an estimated dollar impact greater than 200.
getAnomalies_totalImpact :: Lens.Lens' GetAnomalies (Core.Maybe TotalImpactFilter)
getAnomalies_totalImpact = Lens.lens (\GetAnomalies' {totalImpact} -> totalImpact) (\s@GetAnomalies' {} a -> s {totalImpact = a} :: GetAnomalies)

-- | Assigns the start and end dates for retrieving cost anomalies. The
-- returned anomaly object will have an @AnomalyEndDate@ in the specified
-- time range.
getAnomalies_dateInterval :: Lens.Lens' GetAnomalies AnomalyDateInterval
getAnomalies_dateInterval = Lens.lens (\GetAnomalies' {dateInterval} -> dateInterval) (\s@GetAnomalies' {} a -> s {dateInterval = a} :: GetAnomalies)

instance Core.AWSRequest GetAnomalies where
  type AWSResponse GetAnomalies = GetAnomaliesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAnomaliesResponse'
            Core.<$> (x Core..?> "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "Anomalies" Core..!@ Core.mempty)
      )

instance Core.Hashable GetAnomalies

instance Core.NFData GetAnomalies

instance Core.ToHeaders GetAnomalies where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetAnomalies" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAnomalies where
  toJSON GetAnomalies' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextPageToken" Core..=) Core.<$> nextPageToken,
            ("MonitorArn" Core..=) Core.<$> monitorArn,
            ("Feedback" Core..=) Core.<$> feedback,
            ("TotalImpact" Core..=) Core.<$> totalImpact,
            Core.Just ("DateInterval" Core..= dateInterval)
          ]
      )

instance Core.ToPath GetAnomalies where
  toPath = Core.const "/"

instance Core.ToQuery GetAnomalies where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAnomaliesResponse' smart constructor.
data GetAnomaliesResponse = GetAnomaliesResponse'
  { -- | The token to retrieve the next set of results. AWS provides the token
    -- when the response from a previous call has more results than the maximum
    -- page size.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of cost anomalies.
    anomalies :: [Anomaly]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAnomaliesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getAnomaliesResponse_nextPageToken' - The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
--
-- 'httpStatus', 'getAnomaliesResponse_httpStatus' - The response's http status code.
--
-- 'anomalies', 'getAnomaliesResponse_anomalies' - A list of cost anomalies.
newGetAnomaliesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAnomaliesResponse
newGetAnomaliesResponse pHttpStatus_ =
  GetAnomaliesResponse'
    { nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      anomalies = Core.mempty
    }

-- | The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
getAnomaliesResponse_nextPageToken :: Lens.Lens' GetAnomaliesResponse (Core.Maybe Core.Text)
getAnomaliesResponse_nextPageToken = Lens.lens (\GetAnomaliesResponse' {nextPageToken} -> nextPageToken) (\s@GetAnomaliesResponse' {} a -> s {nextPageToken = a} :: GetAnomaliesResponse)

-- | The response's http status code.
getAnomaliesResponse_httpStatus :: Lens.Lens' GetAnomaliesResponse Core.Int
getAnomaliesResponse_httpStatus = Lens.lens (\GetAnomaliesResponse' {httpStatus} -> httpStatus) (\s@GetAnomaliesResponse' {} a -> s {httpStatus = a} :: GetAnomaliesResponse)

-- | A list of cost anomalies.
getAnomaliesResponse_anomalies :: Lens.Lens' GetAnomaliesResponse [Anomaly]
getAnomaliesResponse_anomalies = Lens.lens (\GetAnomaliesResponse' {anomalies} -> anomalies) (\s@GetAnomaliesResponse' {} a -> s {anomalies = a} :: GetAnomaliesResponse) Core.. Lens._Coerce

instance Core.NFData GetAnomaliesResponse
