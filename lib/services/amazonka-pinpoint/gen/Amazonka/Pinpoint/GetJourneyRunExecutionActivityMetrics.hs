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
-- Module      : Amazonka.Pinpoint.GetJourneyRunExecutionActivityMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard run execution
-- metric that applies to a journey activity.
module Amazonka.Pinpoint.GetJourneyRunExecutionActivityMetrics
  ( -- * Creating a Request
    GetJourneyRunExecutionActivityMetrics (..),
    newGetJourneyRunExecutionActivityMetrics,

    -- * Request Lenses
    getJourneyRunExecutionActivityMetrics_nextToken,
    getJourneyRunExecutionActivityMetrics_pageSize,
    getJourneyRunExecutionActivityMetrics_runId,
    getJourneyRunExecutionActivityMetrics_journeyActivityId,
    getJourneyRunExecutionActivityMetrics_journeyId,
    getJourneyRunExecutionActivityMetrics_applicationId,

    -- * Destructuring the Response
    GetJourneyRunExecutionActivityMetricsResponse (..),
    newGetJourneyRunExecutionActivityMetricsResponse,

    -- * Response Lenses
    getJourneyRunExecutionActivityMetricsResponse_httpStatus,
    getJourneyRunExecutionActivityMetricsResponse_journeyRunExecutionActivityMetricsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetJourneyRunExecutionActivityMetrics' smart constructor.
data GetJourneyRunExecutionActivityMetrics = GetJourneyRunExecutionActivityMetrics'
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
    -- | The unique identifier for the journey activity.
    journeyActivityId :: Prelude.Text,
    -- | The unique identifier for the journey.
    journeyId :: Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJourneyRunExecutionActivityMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getJourneyRunExecutionActivityMetrics_nextToken' - The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'pageSize', 'getJourneyRunExecutionActivityMetrics_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'runId', 'getJourneyRunExecutionActivityMetrics_runId' - The unique identifier for the journey run.
--
-- 'journeyActivityId', 'getJourneyRunExecutionActivityMetrics_journeyActivityId' - The unique identifier for the journey activity.
--
-- 'journeyId', 'getJourneyRunExecutionActivityMetrics_journeyId' - The unique identifier for the journey.
--
-- 'applicationId', 'getJourneyRunExecutionActivityMetrics_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetJourneyRunExecutionActivityMetrics ::
  -- | 'runId'
  Prelude.Text ->
  -- | 'journeyActivityId'
  Prelude.Text ->
  -- | 'journeyId'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  GetJourneyRunExecutionActivityMetrics
newGetJourneyRunExecutionActivityMetrics
  pRunId_
  pJourneyActivityId_
  pJourneyId_
  pApplicationId_ =
    GetJourneyRunExecutionActivityMetrics'
      { nextToken =
          Prelude.Nothing,
        pageSize = Prelude.Nothing,
        runId = pRunId_,
        journeyActivityId =
          pJourneyActivityId_,
        journeyId = pJourneyId_,
        applicationId = pApplicationId_
      }

-- | The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getJourneyRunExecutionActivityMetrics_nextToken :: Lens.Lens' GetJourneyRunExecutionActivityMetrics (Prelude.Maybe Prelude.Text)
getJourneyRunExecutionActivityMetrics_nextToken = Lens.lens (\GetJourneyRunExecutionActivityMetrics' {nextToken} -> nextToken) (\s@GetJourneyRunExecutionActivityMetrics' {} a -> s {nextToken = a} :: GetJourneyRunExecutionActivityMetrics)

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getJourneyRunExecutionActivityMetrics_pageSize :: Lens.Lens' GetJourneyRunExecutionActivityMetrics (Prelude.Maybe Prelude.Text)
getJourneyRunExecutionActivityMetrics_pageSize = Lens.lens (\GetJourneyRunExecutionActivityMetrics' {pageSize} -> pageSize) (\s@GetJourneyRunExecutionActivityMetrics' {} a -> s {pageSize = a} :: GetJourneyRunExecutionActivityMetrics)

-- | The unique identifier for the journey run.
getJourneyRunExecutionActivityMetrics_runId :: Lens.Lens' GetJourneyRunExecutionActivityMetrics Prelude.Text
getJourneyRunExecutionActivityMetrics_runId = Lens.lens (\GetJourneyRunExecutionActivityMetrics' {runId} -> runId) (\s@GetJourneyRunExecutionActivityMetrics' {} a -> s {runId = a} :: GetJourneyRunExecutionActivityMetrics)

-- | The unique identifier for the journey activity.
getJourneyRunExecutionActivityMetrics_journeyActivityId :: Lens.Lens' GetJourneyRunExecutionActivityMetrics Prelude.Text
getJourneyRunExecutionActivityMetrics_journeyActivityId = Lens.lens (\GetJourneyRunExecutionActivityMetrics' {journeyActivityId} -> journeyActivityId) (\s@GetJourneyRunExecutionActivityMetrics' {} a -> s {journeyActivityId = a} :: GetJourneyRunExecutionActivityMetrics)

-- | The unique identifier for the journey.
getJourneyRunExecutionActivityMetrics_journeyId :: Lens.Lens' GetJourneyRunExecutionActivityMetrics Prelude.Text
getJourneyRunExecutionActivityMetrics_journeyId = Lens.lens (\GetJourneyRunExecutionActivityMetrics' {journeyId} -> journeyId) (\s@GetJourneyRunExecutionActivityMetrics' {} a -> s {journeyId = a} :: GetJourneyRunExecutionActivityMetrics)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getJourneyRunExecutionActivityMetrics_applicationId :: Lens.Lens' GetJourneyRunExecutionActivityMetrics Prelude.Text
getJourneyRunExecutionActivityMetrics_applicationId = Lens.lens (\GetJourneyRunExecutionActivityMetrics' {applicationId} -> applicationId) (\s@GetJourneyRunExecutionActivityMetrics' {} a -> s {applicationId = a} :: GetJourneyRunExecutionActivityMetrics)

instance
  Core.AWSRequest
    GetJourneyRunExecutionActivityMetrics
  where
  type
    AWSResponse
      GetJourneyRunExecutionActivityMetrics =
      GetJourneyRunExecutionActivityMetricsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJourneyRunExecutionActivityMetricsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance
  Prelude.Hashable
    GetJourneyRunExecutionActivityMetrics
  where
  hashWithSalt
    _salt
    GetJourneyRunExecutionActivityMetrics' {..} =
      _salt
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` pageSize
        `Prelude.hashWithSalt` runId
        `Prelude.hashWithSalt` journeyActivityId
        `Prelude.hashWithSalt` journeyId
        `Prelude.hashWithSalt` applicationId

instance
  Prelude.NFData
    GetJourneyRunExecutionActivityMetrics
  where
  rnf GetJourneyRunExecutionActivityMetrics' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf journeyActivityId
      `Prelude.seq` Prelude.rnf journeyId
      `Prelude.seq` Prelude.rnf applicationId

instance
  Data.ToHeaders
    GetJourneyRunExecutionActivityMetrics
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

instance
  Data.ToPath
    GetJourneyRunExecutionActivityMetrics
  where
  toPath GetJourneyRunExecutionActivityMetrics' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/journeys/",
        Data.toBS journeyId,
        "/runs/",
        Data.toBS runId,
        "/activities/",
        Data.toBS journeyActivityId,
        "/execution-metrics"
      ]

instance
  Data.ToQuery
    GetJourneyRunExecutionActivityMetrics
  where
  toQuery GetJourneyRunExecutionActivityMetrics' {..} =
    Prelude.mconcat
      [ "next-token" Data.=: nextToken,
        "page-size" Data.=: pageSize
      ]

-- | /See:/ 'newGetJourneyRunExecutionActivityMetricsResponse' smart constructor.
data GetJourneyRunExecutionActivityMetricsResponse = GetJourneyRunExecutionActivityMetricsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    journeyRunExecutionActivityMetricsResponse :: JourneyRunExecutionActivityMetricsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJourneyRunExecutionActivityMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getJourneyRunExecutionActivityMetricsResponse_httpStatus' - The response's http status code.
--
-- 'journeyRunExecutionActivityMetricsResponse', 'getJourneyRunExecutionActivityMetricsResponse_journeyRunExecutionActivityMetricsResponse' - Undocumented member.
newGetJourneyRunExecutionActivityMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'journeyRunExecutionActivityMetricsResponse'
  JourneyRunExecutionActivityMetricsResponse ->
  GetJourneyRunExecutionActivityMetricsResponse
newGetJourneyRunExecutionActivityMetricsResponse
  pHttpStatus_
  pJourneyRunExecutionActivityMetricsResponse_ =
    GetJourneyRunExecutionActivityMetricsResponse'
      { httpStatus =
          pHttpStatus_,
        journeyRunExecutionActivityMetricsResponse =
          pJourneyRunExecutionActivityMetricsResponse_
      }

-- | The response's http status code.
getJourneyRunExecutionActivityMetricsResponse_httpStatus :: Lens.Lens' GetJourneyRunExecutionActivityMetricsResponse Prelude.Int
getJourneyRunExecutionActivityMetricsResponse_httpStatus = Lens.lens (\GetJourneyRunExecutionActivityMetricsResponse' {httpStatus} -> httpStatus) (\s@GetJourneyRunExecutionActivityMetricsResponse' {} a -> s {httpStatus = a} :: GetJourneyRunExecutionActivityMetricsResponse)

-- | Undocumented member.
getJourneyRunExecutionActivityMetricsResponse_journeyRunExecutionActivityMetricsResponse :: Lens.Lens' GetJourneyRunExecutionActivityMetricsResponse JourneyRunExecutionActivityMetricsResponse
getJourneyRunExecutionActivityMetricsResponse_journeyRunExecutionActivityMetricsResponse = Lens.lens (\GetJourneyRunExecutionActivityMetricsResponse' {journeyRunExecutionActivityMetricsResponse} -> journeyRunExecutionActivityMetricsResponse) (\s@GetJourneyRunExecutionActivityMetricsResponse' {} a -> s {journeyRunExecutionActivityMetricsResponse = a} :: GetJourneyRunExecutionActivityMetricsResponse)

instance
  Prelude.NFData
    GetJourneyRunExecutionActivityMetricsResponse
  where
  rnf
    GetJourneyRunExecutionActivityMetricsResponse' {..} =
      Prelude.rnf httpStatus
        `Prelude.seq` Prelude.rnf
          journeyRunExecutionActivityMetricsResponse
