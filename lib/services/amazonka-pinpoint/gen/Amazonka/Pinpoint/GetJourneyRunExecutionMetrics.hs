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
-- Module      : Amazonka.Pinpoint.GetJourneyRunExecutionMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard run execution
-- metric that applies to a journey.
module Amazonka.Pinpoint.GetJourneyRunExecutionMetrics
  ( -- * Creating a Request
    GetJourneyRunExecutionMetrics (..),
    newGetJourneyRunExecutionMetrics,

    -- * Request Lenses
    getJourneyRunExecutionMetrics_nextToken,
    getJourneyRunExecutionMetrics_pageSize,
    getJourneyRunExecutionMetrics_runId,
    getJourneyRunExecutionMetrics_applicationId,
    getJourneyRunExecutionMetrics_journeyId,

    -- * Destructuring the Response
    GetJourneyRunExecutionMetricsResponse (..),
    newGetJourneyRunExecutionMetricsResponse,

    -- * Response Lenses
    getJourneyRunExecutionMetricsResponse_httpStatus,
    getJourneyRunExecutionMetricsResponse_journeyRunExecutionMetricsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetJourneyRunExecutionMetrics' smart constructor.
data GetJourneyRunExecutionMetrics = GetJourneyRunExecutionMetrics'
  { -- | The string that specifies which page of results to return in a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the journey run.
    runId :: Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    -- | The unique identifier for the journey.
    journeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJourneyRunExecutionMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getJourneyRunExecutionMetrics_nextToken' - The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'pageSize', 'getJourneyRunExecutionMetrics_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'runId', 'getJourneyRunExecutionMetrics_runId' - The unique identifier for the journey run.
--
-- 'applicationId', 'getJourneyRunExecutionMetrics_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'journeyId', 'getJourneyRunExecutionMetrics_journeyId' - The unique identifier for the journey.
newGetJourneyRunExecutionMetrics ::
  -- | 'runId'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'journeyId'
  Prelude.Text ->
  GetJourneyRunExecutionMetrics
newGetJourneyRunExecutionMetrics
  pRunId_
  pApplicationId_
  pJourneyId_ =
    GetJourneyRunExecutionMetrics'
      { nextToken =
          Prelude.Nothing,
        pageSize = Prelude.Nothing,
        runId = pRunId_,
        applicationId = pApplicationId_,
        journeyId = pJourneyId_
      }

-- | The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getJourneyRunExecutionMetrics_nextToken :: Lens.Lens' GetJourneyRunExecutionMetrics (Prelude.Maybe Prelude.Text)
getJourneyRunExecutionMetrics_nextToken = Lens.lens (\GetJourneyRunExecutionMetrics' {nextToken} -> nextToken) (\s@GetJourneyRunExecutionMetrics' {} a -> s {nextToken = a} :: GetJourneyRunExecutionMetrics)

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getJourneyRunExecutionMetrics_pageSize :: Lens.Lens' GetJourneyRunExecutionMetrics (Prelude.Maybe Prelude.Text)
getJourneyRunExecutionMetrics_pageSize = Lens.lens (\GetJourneyRunExecutionMetrics' {pageSize} -> pageSize) (\s@GetJourneyRunExecutionMetrics' {} a -> s {pageSize = a} :: GetJourneyRunExecutionMetrics)

-- | The unique identifier for the journey run.
getJourneyRunExecutionMetrics_runId :: Lens.Lens' GetJourneyRunExecutionMetrics Prelude.Text
getJourneyRunExecutionMetrics_runId = Lens.lens (\GetJourneyRunExecutionMetrics' {runId} -> runId) (\s@GetJourneyRunExecutionMetrics' {} a -> s {runId = a} :: GetJourneyRunExecutionMetrics)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getJourneyRunExecutionMetrics_applicationId :: Lens.Lens' GetJourneyRunExecutionMetrics Prelude.Text
getJourneyRunExecutionMetrics_applicationId = Lens.lens (\GetJourneyRunExecutionMetrics' {applicationId} -> applicationId) (\s@GetJourneyRunExecutionMetrics' {} a -> s {applicationId = a} :: GetJourneyRunExecutionMetrics)

-- | The unique identifier for the journey.
getJourneyRunExecutionMetrics_journeyId :: Lens.Lens' GetJourneyRunExecutionMetrics Prelude.Text
getJourneyRunExecutionMetrics_journeyId = Lens.lens (\GetJourneyRunExecutionMetrics' {journeyId} -> journeyId) (\s@GetJourneyRunExecutionMetrics' {} a -> s {journeyId = a} :: GetJourneyRunExecutionMetrics)

instance
  Core.AWSRequest
    GetJourneyRunExecutionMetrics
  where
  type
    AWSResponse GetJourneyRunExecutionMetrics =
      GetJourneyRunExecutionMetricsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJourneyRunExecutionMetricsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance
  Prelude.Hashable
    GetJourneyRunExecutionMetrics
  where
  hashWithSalt _salt GetJourneyRunExecutionMetrics' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` runId
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` journeyId

instance Prelude.NFData GetJourneyRunExecutionMetrics where
  rnf GetJourneyRunExecutionMetrics' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf journeyId

instance Data.ToHeaders GetJourneyRunExecutionMetrics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetJourneyRunExecutionMetrics where
  toPath GetJourneyRunExecutionMetrics' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/journeys/",
        Data.toBS journeyId,
        "/runs/",
        Data.toBS runId,
        "/execution-metrics"
      ]

instance Data.ToQuery GetJourneyRunExecutionMetrics where
  toQuery GetJourneyRunExecutionMetrics' {..} =
    Prelude.mconcat
      [ "next-token" Data.=: nextToken,
        "page-size" Data.=: pageSize
      ]

-- | /See:/ 'newGetJourneyRunExecutionMetricsResponse' smart constructor.
data GetJourneyRunExecutionMetricsResponse = GetJourneyRunExecutionMetricsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    journeyRunExecutionMetricsResponse :: JourneyRunExecutionMetricsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJourneyRunExecutionMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getJourneyRunExecutionMetricsResponse_httpStatus' - The response's http status code.
--
-- 'journeyRunExecutionMetricsResponse', 'getJourneyRunExecutionMetricsResponse_journeyRunExecutionMetricsResponse' - Undocumented member.
newGetJourneyRunExecutionMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'journeyRunExecutionMetricsResponse'
  JourneyRunExecutionMetricsResponse ->
  GetJourneyRunExecutionMetricsResponse
newGetJourneyRunExecutionMetricsResponse
  pHttpStatus_
  pJourneyRunExecutionMetricsResponse_ =
    GetJourneyRunExecutionMetricsResponse'
      { httpStatus =
          pHttpStatus_,
        journeyRunExecutionMetricsResponse =
          pJourneyRunExecutionMetricsResponse_
      }

-- | The response's http status code.
getJourneyRunExecutionMetricsResponse_httpStatus :: Lens.Lens' GetJourneyRunExecutionMetricsResponse Prelude.Int
getJourneyRunExecutionMetricsResponse_httpStatus = Lens.lens (\GetJourneyRunExecutionMetricsResponse' {httpStatus} -> httpStatus) (\s@GetJourneyRunExecutionMetricsResponse' {} a -> s {httpStatus = a} :: GetJourneyRunExecutionMetricsResponse)

-- | Undocumented member.
getJourneyRunExecutionMetricsResponse_journeyRunExecutionMetricsResponse :: Lens.Lens' GetJourneyRunExecutionMetricsResponse JourneyRunExecutionMetricsResponse
getJourneyRunExecutionMetricsResponse_journeyRunExecutionMetricsResponse = Lens.lens (\GetJourneyRunExecutionMetricsResponse' {journeyRunExecutionMetricsResponse} -> journeyRunExecutionMetricsResponse) (\s@GetJourneyRunExecutionMetricsResponse' {} a -> s {journeyRunExecutionMetricsResponse = a} :: GetJourneyRunExecutionMetricsResponse)

instance
  Prelude.NFData
    GetJourneyRunExecutionMetricsResponse
  where
  rnf GetJourneyRunExecutionMetricsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf journeyRunExecutionMetricsResponse
