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
-- Module      : Amazonka.LookoutMetrics.ListAlerts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the alerts attached to a detector.
--
-- Amazon Lookout for Metrics API actions are eventually consistent. If you
-- do a read operation on a resource immediately after creating or
-- modifying it, use retries to allow time for the write operation to
-- complete.
module Amazonka.LookoutMetrics.ListAlerts
  ( -- * Creating a Request
    ListAlerts (..),
    newListAlerts,

    -- * Request Lenses
    listAlerts_anomalyDetectorArn,
    listAlerts_maxResults,
    listAlerts_nextToken,

    -- * Destructuring the Response
    ListAlertsResponse (..),
    newListAlertsResponse,

    -- * Response Lenses
    listAlertsResponse_alertSummaryList,
    listAlertsResponse_nextToken,
    listAlertsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAlerts' smart constructor.
data ListAlerts = ListAlerts'
  { -- | The ARN of the alert\'s detector.
    anomalyDetectorArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that will be displayed by the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the result of the previous request is truncated, the response
    -- includes a @NextToken@. To retrieve the next set of results, use the
    -- token in the next request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAlerts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyDetectorArn', 'listAlerts_anomalyDetectorArn' - The ARN of the alert\'s detector.
--
-- 'maxResults', 'listAlerts_maxResults' - The maximum number of results that will be displayed by the request.
--
-- 'nextToken', 'listAlerts_nextToken' - If the result of the previous request is truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
newListAlerts ::
  ListAlerts
newListAlerts =
  ListAlerts'
    { anomalyDetectorArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The ARN of the alert\'s detector.
listAlerts_anomalyDetectorArn :: Lens.Lens' ListAlerts (Prelude.Maybe Prelude.Text)
listAlerts_anomalyDetectorArn = Lens.lens (\ListAlerts' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@ListAlerts' {} a -> s {anomalyDetectorArn = a} :: ListAlerts)

-- | The maximum number of results that will be displayed by the request.
listAlerts_maxResults :: Lens.Lens' ListAlerts (Prelude.Maybe Prelude.Natural)
listAlerts_maxResults = Lens.lens (\ListAlerts' {maxResults} -> maxResults) (\s@ListAlerts' {} a -> s {maxResults = a} :: ListAlerts)

-- | If the result of the previous request is truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
listAlerts_nextToken :: Lens.Lens' ListAlerts (Prelude.Maybe Prelude.Text)
listAlerts_nextToken = Lens.lens (\ListAlerts' {nextToken} -> nextToken) (\s@ListAlerts' {} a -> s {nextToken = a} :: ListAlerts)

instance Core.AWSRequest ListAlerts where
  type AWSResponse ListAlerts = ListAlertsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAlertsResponse'
            Prelude.<$> ( x Data..?> "AlertSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAlerts where
  hashWithSalt _salt ListAlerts' {..} =
    _salt `Prelude.hashWithSalt` anomalyDetectorArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListAlerts where
  rnf ListAlerts' {..} =
    Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListAlerts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAlerts where
  toJSON ListAlerts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnomalyDetectorArn" Data..=)
              Prelude.<$> anomalyDetectorArn,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListAlerts where
  toPath = Prelude.const "/ListAlerts"

instance Data.ToQuery ListAlerts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAlertsResponse' smart constructor.
data ListAlertsResponse = ListAlertsResponse'
  { -- | Contains information about an alert.
    alertSummaryList :: Prelude.Maybe [AlertSummary],
    -- | If the response is truncated, the service returns this token. To
    -- retrieve the next set of results, use this token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAlertsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alertSummaryList', 'listAlertsResponse_alertSummaryList' - Contains information about an alert.
--
-- 'nextToken', 'listAlertsResponse_nextToken' - If the response is truncated, the service returns this token. To
-- retrieve the next set of results, use this token in the next request.
--
-- 'httpStatus', 'listAlertsResponse_httpStatus' - The response's http status code.
newListAlertsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAlertsResponse
newListAlertsResponse pHttpStatus_ =
  ListAlertsResponse'
    { alertSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains information about an alert.
listAlertsResponse_alertSummaryList :: Lens.Lens' ListAlertsResponse (Prelude.Maybe [AlertSummary])
listAlertsResponse_alertSummaryList = Lens.lens (\ListAlertsResponse' {alertSummaryList} -> alertSummaryList) (\s@ListAlertsResponse' {} a -> s {alertSummaryList = a} :: ListAlertsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, the service returns this token. To
-- retrieve the next set of results, use this token in the next request.
listAlertsResponse_nextToken :: Lens.Lens' ListAlertsResponse (Prelude.Maybe Prelude.Text)
listAlertsResponse_nextToken = Lens.lens (\ListAlertsResponse' {nextToken} -> nextToken) (\s@ListAlertsResponse' {} a -> s {nextToken = a} :: ListAlertsResponse)

-- | The response's http status code.
listAlertsResponse_httpStatus :: Lens.Lens' ListAlertsResponse Prelude.Int
listAlertsResponse_httpStatus = Lens.lens (\ListAlertsResponse' {httpStatus} -> httpStatus) (\s@ListAlertsResponse' {} a -> s {httpStatus = a} :: ListAlertsResponse)

instance Prelude.NFData ListAlertsResponse where
  rnf ListAlertsResponse' {..} =
    Prelude.rnf alertSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
