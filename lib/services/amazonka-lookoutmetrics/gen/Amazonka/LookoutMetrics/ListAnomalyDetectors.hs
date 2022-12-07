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
-- Module      : Amazonka.LookoutMetrics.ListAnomalyDetectors
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the detectors in the current AWS Region.
--
-- Amazon Lookout for Metrics API actions are eventually consistent. If you
-- do a read operation on a resource immediately after creating or
-- modifying it, use retries to allow time for the write operation to
-- complete.
module Amazonka.LookoutMetrics.ListAnomalyDetectors
  ( -- * Creating a Request
    ListAnomalyDetectors (..),
    newListAnomalyDetectors,

    -- * Request Lenses
    listAnomalyDetectors_nextToken,
    listAnomalyDetectors_maxResults,

    -- * Destructuring the Response
    ListAnomalyDetectorsResponse (..),
    newListAnomalyDetectorsResponse,

    -- * Response Lenses
    listAnomalyDetectorsResponse_nextToken,
    listAnomalyDetectorsResponse_anomalyDetectorSummaryList,
    listAnomalyDetectorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAnomalyDetectors' smart constructor.
data ListAnomalyDetectors = ListAnomalyDetectors'
  { -- | If the result of the previous request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of results, use the
    -- token in the next request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnomalyDetectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAnomalyDetectors_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
--
-- 'maxResults', 'listAnomalyDetectors_maxResults' - The maximum number of results to return.
newListAnomalyDetectors ::
  ListAnomalyDetectors
newListAnomalyDetectors =
  ListAnomalyDetectors'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
listAnomalyDetectors_nextToken :: Lens.Lens' ListAnomalyDetectors (Prelude.Maybe Prelude.Text)
listAnomalyDetectors_nextToken = Lens.lens (\ListAnomalyDetectors' {nextToken} -> nextToken) (\s@ListAnomalyDetectors' {} a -> s {nextToken = a} :: ListAnomalyDetectors)

-- | The maximum number of results to return.
listAnomalyDetectors_maxResults :: Lens.Lens' ListAnomalyDetectors (Prelude.Maybe Prelude.Natural)
listAnomalyDetectors_maxResults = Lens.lens (\ListAnomalyDetectors' {maxResults} -> maxResults) (\s@ListAnomalyDetectors' {} a -> s {maxResults = a} :: ListAnomalyDetectors)

instance Core.AWSRequest ListAnomalyDetectors where
  type
    AWSResponse ListAnomalyDetectors =
      ListAnomalyDetectorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAnomalyDetectorsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "AnomalyDetectorSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAnomalyDetectors where
  hashWithSalt _salt ListAnomalyDetectors' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListAnomalyDetectors where
  rnf ListAnomalyDetectors' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListAnomalyDetectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAnomalyDetectors where
  toJSON ListAnomalyDetectors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListAnomalyDetectors where
  toPath = Prelude.const "/ListAnomalyDetectors"

instance Data.ToQuery ListAnomalyDetectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAnomalyDetectorsResponse' smart constructor.
data ListAnomalyDetectorsResponse = ListAnomalyDetectorsResponse'
  { -- | If the response is truncated, the service returns this token. To
    -- retrieve the next set of results, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of anomaly detectors in the account in the current region.
    anomalyDetectorSummaryList :: Prelude.Maybe [AnomalyDetectorSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnomalyDetectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAnomalyDetectorsResponse_nextToken' - If the response is truncated, the service returns this token. To
-- retrieve the next set of results, use the token in the next request.
--
-- 'anomalyDetectorSummaryList', 'listAnomalyDetectorsResponse_anomalyDetectorSummaryList' - A list of anomaly detectors in the account in the current region.
--
-- 'httpStatus', 'listAnomalyDetectorsResponse_httpStatus' - The response's http status code.
newListAnomalyDetectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAnomalyDetectorsResponse
newListAnomalyDetectorsResponse pHttpStatus_ =
  ListAnomalyDetectorsResponse'
    { nextToken =
        Prelude.Nothing,
      anomalyDetectorSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, the service returns this token. To
-- retrieve the next set of results, use the token in the next request.
listAnomalyDetectorsResponse_nextToken :: Lens.Lens' ListAnomalyDetectorsResponse (Prelude.Maybe Prelude.Text)
listAnomalyDetectorsResponse_nextToken = Lens.lens (\ListAnomalyDetectorsResponse' {nextToken} -> nextToken) (\s@ListAnomalyDetectorsResponse' {} a -> s {nextToken = a} :: ListAnomalyDetectorsResponse)

-- | A list of anomaly detectors in the account in the current region.
listAnomalyDetectorsResponse_anomalyDetectorSummaryList :: Lens.Lens' ListAnomalyDetectorsResponse (Prelude.Maybe [AnomalyDetectorSummary])
listAnomalyDetectorsResponse_anomalyDetectorSummaryList = Lens.lens (\ListAnomalyDetectorsResponse' {anomalyDetectorSummaryList} -> anomalyDetectorSummaryList) (\s@ListAnomalyDetectorsResponse' {} a -> s {anomalyDetectorSummaryList = a} :: ListAnomalyDetectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAnomalyDetectorsResponse_httpStatus :: Lens.Lens' ListAnomalyDetectorsResponse Prelude.Int
listAnomalyDetectorsResponse_httpStatus = Lens.lens (\ListAnomalyDetectorsResponse' {httpStatus} -> httpStatus) (\s@ListAnomalyDetectorsResponse' {} a -> s {httpStatus = a} :: ListAnomalyDetectorsResponse)

instance Prelude.NFData ListAnomalyDetectorsResponse where
  rnf ListAnomalyDetectorsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf anomalyDetectorSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
