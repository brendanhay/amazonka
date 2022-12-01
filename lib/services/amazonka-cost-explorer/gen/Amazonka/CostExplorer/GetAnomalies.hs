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
-- Module      : Amazonka.CostExplorer.GetAnomalies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all of the cost anomalies detected on your account during the
-- time period that\'s specified by the @DateInterval@ object.
module Amazonka.CostExplorer.GetAnomalies
  ( -- * Creating a Request
    GetAnomalies (..),
    newGetAnomalies,

    -- * Request Lenses
    getAnomalies_nextPageToken,
    getAnomalies_totalImpact,
    getAnomalies_monitorArn,
    getAnomalies_feedback,
    getAnomalies_maxResults,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAnomalies' smart constructor.
data GetAnomalies = GetAnomalies'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | Filters anomaly results by the total impact field on the anomaly object.
    -- For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve
    -- anomalies, with an estimated dollar impact greater than 200.
    totalImpact :: Prelude.Maybe TotalImpactFilter,
    -- | Retrieves all of the cost anomalies detected for a specific cost anomaly
    -- monitor Amazon Resource Name (ARN).
    monitorArn :: Prelude.Maybe Prelude.Text,
    -- | Filters anomaly results by the feedback field on the anomaly object.
    feedback :: Prelude.Maybe AnomalyFeedbackType,
    -- | The number of entries a paginated response contains.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Assigns the start and end dates for retrieving cost anomalies. The
    -- returned anomaly object will have an @AnomalyEndDate@ in the specified
    -- time range.
    dateInterval :: AnomalyDateInterval
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAnomalies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getAnomalies_nextPageToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'totalImpact', 'getAnomalies_totalImpact' - Filters anomaly results by the total impact field on the anomaly object.
-- For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve
-- anomalies, with an estimated dollar impact greater than 200.
--
-- 'monitorArn', 'getAnomalies_monitorArn' - Retrieves all of the cost anomalies detected for a specific cost anomaly
-- monitor Amazon Resource Name (ARN).
--
-- 'feedback', 'getAnomalies_feedback' - Filters anomaly results by the feedback field on the anomaly object.
--
-- 'maxResults', 'getAnomalies_maxResults' - The number of entries a paginated response contains.
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
    { nextPageToken = Prelude.Nothing,
      totalImpact = Prelude.Nothing,
      monitorArn = Prelude.Nothing,
      feedback = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      dateInterval = pDateInterval_
    }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getAnomalies_nextPageToken :: Lens.Lens' GetAnomalies (Prelude.Maybe Prelude.Text)
getAnomalies_nextPageToken = Lens.lens (\GetAnomalies' {nextPageToken} -> nextPageToken) (\s@GetAnomalies' {} a -> s {nextPageToken = a} :: GetAnomalies)

-- | Filters anomaly results by the total impact field on the anomaly object.
-- For example, you can filter anomalies @GREATER_THAN 200.00@ to retrieve
-- anomalies, with an estimated dollar impact greater than 200.
getAnomalies_totalImpact :: Lens.Lens' GetAnomalies (Prelude.Maybe TotalImpactFilter)
getAnomalies_totalImpact = Lens.lens (\GetAnomalies' {totalImpact} -> totalImpact) (\s@GetAnomalies' {} a -> s {totalImpact = a} :: GetAnomalies)

-- | Retrieves all of the cost anomalies detected for a specific cost anomaly
-- monitor Amazon Resource Name (ARN).
getAnomalies_monitorArn :: Lens.Lens' GetAnomalies (Prelude.Maybe Prelude.Text)
getAnomalies_monitorArn = Lens.lens (\GetAnomalies' {monitorArn} -> monitorArn) (\s@GetAnomalies' {} a -> s {monitorArn = a} :: GetAnomalies)

-- | Filters anomaly results by the feedback field on the anomaly object.
getAnomalies_feedback :: Lens.Lens' GetAnomalies (Prelude.Maybe AnomalyFeedbackType)
getAnomalies_feedback = Lens.lens (\GetAnomalies' {feedback} -> feedback) (\s@GetAnomalies' {} a -> s {feedback = a} :: GetAnomalies)

-- | The number of entries a paginated response contains.
getAnomalies_maxResults :: Lens.Lens' GetAnomalies (Prelude.Maybe Prelude.Int)
getAnomalies_maxResults = Lens.lens (\GetAnomalies' {maxResults} -> maxResults) (\s@GetAnomalies' {} a -> s {maxResults = a} :: GetAnomalies)

-- | Assigns the start and end dates for retrieving cost anomalies. The
-- returned anomaly object will have an @AnomalyEndDate@ in the specified
-- time range.
getAnomalies_dateInterval :: Lens.Lens' GetAnomalies AnomalyDateInterval
getAnomalies_dateInterval = Lens.lens (\GetAnomalies' {dateInterval} -> dateInterval) (\s@GetAnomalies' {} a -> s {dateInterval = a} :: GetAnomalies)

instance Core.AWSRequest GetAnomalies where
  type AWSResponse GetAnomalies = GetAnomaliesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAnomaliesResponse'
            Prelude.<$> (x Core..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Anomalies" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetAnomalies where
  hashWithSalt _salt GetAnomalies' {..} =
    _salt `Prelude.hashWithSalt` nextPageToken
      `Prelude.hashWithSalt` totalImpact
      `Prelude.hashWithSalt` monitorArn
      `Prelude.hashWithSalt` feedback
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` dateInterval

instance Prelude.NFData GetAnomalies where
  rnf GetAnomalies' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf totalImpact
      `Prelude.seq` Prelude.rnf monitorArn
      `Prelude.seq` Prelude.rnf feedback
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf dateInterval

instance Core.ToHeaders GetAnomalies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetAnomalies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetAnomalies where
  toJSON GetAnomalies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextPageToken" Core..=) Prelude.<$> nextPageToken,
            ("TotalImpact" Core..=) Prelude.<$> totalImpact,
            ("MonitorArn" Core..=) Prelude.<$> monitorArn,
            ("Feedback" Core..=) Prelude.<$> feedback,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("DateInterval" Core..= dateInterval)
          ]
      )

instance Core.ToPath GetAnomalies where
  toPath = Prelude.const "/"

instance Core.ToQuery GetAnomalies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAnomaliesResponse' smart constructor.
data GetAnomaliesResponse = GetAnomaliesResponse'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of cost anomalies.
    anomalies :: [Anomaly]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAnomaliesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getAnomaliesResponse_nextPageToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'httpStatus', 'getAnomaliesResponse_httpStatus' - The response's http status code.
--
-- 'anomalies', 'getAnomaliesResponse_anomalies' - A list of cost anomalies.
newGetAnomaliesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAnomaliesResponse
newGetAnomaliesResponse pHttpStatus_ =
  GetAnomaliesResponse'
    { nextPageToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      anomalies = Prelude.mempty
    }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getAnomaliesResponse_nextPageToken :: Lens.Lens' GetAnomaliesResponse (Prelude.Maybe Prelude.Text)
getAnomaliesResponse_nextPageToken = Lens.lens (\GetAnomaliesResponse' {nextPageToken} -> nextPageToken) (\s@GetAnomaliesResponse' {} a -> s {nextPageToken = a} :: GetAnomaliesResponse)

-- | The response's http status code.
getAnomaliesResponse_httpStatus :: Lens.Lens' GetAnomaliesResponse Prelude.Int
getAnomaliesResponse_httpStatus = Lens.lens (\GetAnomaliesResponse' {httpStatus} -> httpStatus) (\s@GetAnomaliesResponse' {} a -> s {httpStatus = a} :: GetAnomaliesResponse)

-- | A list of cost anomalies.
getAnomaliesResponse_anomalies :: Lens.Lens' GetAnomaliesResponse [Anomaly]
getAnomaliesResponse_anomalies = Lens.lens (\GetAnomaliesResponse' {anomalies} -> anomalies) (\s@GetAnomaliesResponse' {} a -> s {anomalies = a} :: GetAnomaliesResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetAnomaliesResponse where
  rnf GetAnomaliesResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf anomalies
