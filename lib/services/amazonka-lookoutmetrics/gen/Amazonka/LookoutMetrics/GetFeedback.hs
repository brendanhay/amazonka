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
-- Module      : Amazonka.LookoutMetrics.GetFeedback
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get feedback for an anomaly group.
module Amazonka.LookoutMetrics.GetFeedback
  ( -- * Creating a Request
    GetFeedback (..),
    newGetFeedback,

    -- * Request Lenses
    getFeedback_maxResults,
    getFeedback_nextToken,
    getFeedback_anomalyDetectorArn,
    getFeedback_anomalyGroupTimeSeriesFeedback,

    -- * Destructuring the Response
    GetFeedbackResponse (..),
    newGetFeedbackResponse,

    -- * Response Lenses
    getFeedbackResponse_anomalyGroupTimeSeriesFeedback,
    getFeedbackResponse_nextToken,
    getFeedbackResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFeedback' smart constructor.
data GetFeedback = GetFeedback'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token that\'s returned by a previous request to
    -- retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the anomaly detector.
    anomalyDetectorArn :: Prelude.Text,
    -- | The anomalous metric and group ID.
    anomalyGroupTimeSeriesFeedback :: AnomalyGroupTimeSeries
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getFeedback_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'getFeedback_nextToken' - Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
--
-- 'anomalyDetectorArn', 'getFeedback_anomalyDetectorArn' - The Amazon Resource Name (ARN) of the anomaly detector.
--
-- 'anomalyGroupTimeSeriesFeedback', 'getFeedback_anomalyGroupTimeSeriesFeedback' - The anomalous metric and group ID.
newGetFeedback ::
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  -- | 'anomalyGroupTimeSeriesFeedback'
  AnomalyGroupTimeSeries ->
  GetFeedback
newGetFeedback
  pAnomalyDetectorArn_
  pAnomalyGroupTimeSeriesFeedback_ =
    GetFeedback'
      { maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        anomalyDetectorArn = pAnomalyDetectorArn_,
        anomalyGroupTimeSeriesFeedback =
          pAnomalyGroupTimeSeriesFeedback_
      }

-- | The maximum number of results to return.
getFeedback_maxResults :: Lens.Lens' GetFeedback (Prelude.Maybe Prelude.Natural)
getFeedback_maxResults = Lens.lens (\GetFeedback' {maxResults} -> maxResults) (\s@GetFeedback' {} a -> s {maxResults = a} :: GetFeedback)

-- | Specify the pagination token that\'s returned by a previous request to
-- retrieve the next page of results.
getFeedback_nextToken :: Lens.Lens' GetFeedback (Prelude.Maybe Prelude.Text)
getFeedback_nextToken = Lens.lens (\GetFeedback' {nextToken} -> nextToken) (\s@GetFeedback' {} a -> s {nextToken = a} :: GetFeedback)

-- | The Amazon Resource Name (ARN) of the anomaly detector.
getFeedback_anomalyDetectorArn :: Lens.Lens' GetFeedback Prelude.Text
getFeedback_anomalyDetectorArn = Lens.lens (\GetFeedback' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@GetFeedback' {} a -> s {anomalyDetectorArn = a} :: GetFeedback)

-- | The anomalous metric and group ID.
getFeedback_anomalyGroupTimeSeriesFeedback :: Lens.Lens' GetFeedback AnomalyGroupTimeSeries
getFeedback_anomalyGroupTimeSeriesFeedback = Lens.lens (\GetFeedback' {anomalyGroupTimeSeriesFeedback} -> anomalyGroupTimeSeriesFeedback) (\s@GetFeedback' {} a -> s {anomalyGroupTimeSeriesFeedback = a} :: GetFeedback)

instance Core.AWSRequest GetFeedback where
  type AWSResponse GetFeedback = GetFeedbackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFeedbackResponse'
            Prelude.<$> ( x
                            Data..?> "AnomalyGroupTimeSeriesFeedback"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFeedback where
  hashWithSalt _salt GetFeedback' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` anomalyDetectorArn
      `Prelude.hashWithSalt` anomalyGroupTimeSeriesFeedback

instance Prelude.NFData GetFeedback where
  rnf GetFeedback' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf anomalyGroupTimeSeriesFeedback

instance Data.ToHeaders GetFeedback where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetFeedback where
  toJSON GetFeedback' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("AnomalyDetectorArn" Data..= anomalyDetectorArn),
            Prelude.Just
              ( "AnomalyGroupTimeSeriesFeedback"
                  Data..= anomalyGroupTimeSeriesFeedback
              )
          ]
      )

instance Data.ToPath GetFeedback where
  toPath = Prelude.const "/GetFeedback"

instance Data.ToQuery GetFeedback where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFeedbackResponse' smart constructor.
data GetFeedbackResponse = GetFeedbackResponse'
  { -- | Feedback for an anomalous metric.
    anomalyGroupTimeSeriesFeedback :: Prelude.Maybe [TimeSeriesFeedback],
    -- | The pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFeedbackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyGroupTimeSeriesFeedback', 'getFeedbackResponse_anomalyGroupTimeSeriesFeedback' - Feedback for an anomalous metric.
--
-- 'nextToken', 'getFeedbackResponse_nextToken' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'getFeedbackResponse_httpStatus' - The response's http status code.
newGetFeedbackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFeedbackResponse
newGetFeedbackResponse pHttpStatus_ =
  GetFeedbackResponse'
    { anomalyGroupTimeSeriesFeedback =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Feedback for an anomalous metric.
getFeedbackResponse_anomalyGroupTimeSeriesFeedback :: Lens.Lens' GetFeedbackResponse (Prelude.Maybe [TimeSeriesFeedback])
getFeedbackResponse_anomalyGroupTimeSeriesFeedback = Lens.lens (\GetFeedbackResponse' {anomalyGroupTimeSeriesFeedback} -> anomalyGroupTimeSeriesFeedback) (\s@GetFeedbackResponse' {} a -> s {anomalyGroupTimeSeriesFeedback = a} :: GetFeedbackResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s included if more results are available.
getFeedbackResponse_nextToken :: Lens.Lens' GetFeedbackResponse (Prelude.Maybe Prelude.Text)
getFeedbackResponse_nextToken = Lens.lens (\GetFeedbackResponse' {nextToken} -> nextToken) (\s@GetFeedbackResponse' {} a -> s {nextToken = a} :: GetFeedbackResponse)

-- | The response's http status code.
getFeedbackResponse_httpStatus :: Lens.Lens' GetFeedbackResponse Prelude.Int
getFeedbackResponse_httpStatus = Lens.lens (\GetFeedbackResponse' {httpStatus} -> httpStatus) (\s@GetFeedbackResponse' {} a -> s {httpStatus = a} :: GetFeedbackResponse)

instance Prelude.NFData GetFeedbackResponse where
  rnf GetFeedbackResponse' {..} =
    Prelude.rnf anomalyGroupTimeSeriesFeedback
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
