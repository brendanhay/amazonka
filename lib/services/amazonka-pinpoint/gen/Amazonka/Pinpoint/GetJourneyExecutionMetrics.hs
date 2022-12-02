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
-- Module      : Amazonka.Pinpoint.GetJourneyExecutionMetrics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard execution metric
-- that applies to a journey.
module Amazonka.Pinpoint.GetJourneyExecutionMetrics
  ( -- * Creating a Request
    GetJourneyExecutionMetrics (..),
    newGetJourneyExecutionMetrics,

    -- * Request Lenses
    getJourneyExecutionMetrics_nextToken,
    getJourneyExecutionMetrics_pageSize,
    getJourneyExecutionMetrics_applicationId,
    getJourneyExecutionMetrics_journeyId,

    -- * Destructuring the Response
    GetJourneyExecutionMetricsResponse (..),
    newGetJourneyExecutionMetricsResponse,

    -- * Response Lenses
    getJourneyExecutionMetricsResponse_httpStatus,
    getJourneyExecutionMetricsResponse_journeyExecutionMetricsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetJourneyExecutionMetrics' smart constructor.
data GetJourneyExecutionMetrics = GetJourneyExecutionMetrics'
  { -- | The string that specifies which page of results to return in a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    -- | The unique identifier for the journey.
    journeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJourneyExecutionMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getJourneyExecutionMetrics_nextToken' - The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'pageSize', 'getJourneyExecutionMetrics_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'applicationId', 'getJourneyExecutionMetrics_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'journeyId', 'getJourneyExecutionMetrics_journeyId' - The unique identifier for the journey.
newGetJourneyExecutionMetrics ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'journeyId'
  Prelude.Text ->
  GetJourneyExecutionMetrics
newGetJourneyExecutionMetrics
  pApplicationId_
  pJourneyId_ =
    GetJourneyExecutionMetrics'
      { nextToken =
          Prelude.Nothing,
        pageSize = Prelude.Nothing,
        applicationId = pApplicationId_,
        journeyId = pJourneyId_
      }

-- | The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getJourneyExecutionMetrics_nextToken :: Lens.Lens' GetJourneyExecutionMetrics (Prelude.Maybe Prelude.Text)
getJourneyExecutionMetrics_nextToken = Lens.lens (\GetJourneyExecutionMetrics' {nextToken} -> nextToken) (\s@GetJourneyExecutionMetrics' {} a -> s {nextToken = a} :: GetJourneyExecutionMetrics)

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getJourneyExecutionMetrics_pageSize :: Lens.Lens' GetJourneyExecutionMetrics (Prelude.Maybe Prelude.Text)
getJourneyExecutionMetrics_pageSize = Lens.lens (\GetJourneyExecutionMetrics' {pageSize} -> pageSize) (\s@GetJourneyExecutionMetrics' {} a -> s {pageSize = a} :: GetJourneyExecutionMetrics)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getJourneyExecutionMetrics_applicationId :: Lens.Lens' GetJourneyExecutionMetrics Prelude.Text
getJourneyExecutionMetrics_applicationId = Lens.lens (\GetJourneyExecutionMetrics' {applicationId} -> applicationId) (\s@GetJourneyExecutionMetrics' {} a -> s {applicationId = a} :: GetJourneyExecutionMetrics)

-- | The unique identifier for the journey.
getJourneyExecutionMetrics_journeyId :: Lens.Lens' GetJourneyExecutionMetrics Prelude.Text
getJourneyExecutionMetrics_journeyId = Lens.lens (\GetJourneyExecutionMetrics' {journeyId} -> journeyId) (\s@GetJourneyExecutionMetrics' {} a -> s {journeyId = a} :: GetJourneyExecutionMetrics)

instance Core.AWSRequest GetJourneyExecutionMetrics where
  type
    AWSResponse GetJourneyExecutionMetrics =
      GetJourneyExecutionMetricsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJourneyExecutionMetricsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetJourneyExecutionMetrics where
  hashWithSalt _salt GetJourneyExecutionMetrics' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` journeyId

instance Prelude.NFData GetJourneyExecutionMetrics where
  rnf GetJourneyExecutionMetrics' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf journeyId

instance Data.ToHeaders GetJourneyExecutionMetrics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetJourneyExecutionMetrics where
  toPath GetJourneyExecutionMetrics' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/journeys/",
        Data.toBS journeyId,
        "/execution-metrics"
      ]

instance Data.ToQuery GetJourneyExecutionMetrics where
  toQuery GetJourneyExecutionMetrics' {..} =
    Prelude.mconcat
      [ "next-token" Data.=: nextToken,
        "page-size" Data.=: pageSize
      ]

-- | /See:/ 'newGetJourneyExecutionMetricsResponse' smart constructor.
data GetJourneyExecutionMetricsResponse = GetJourneyExecutionMetricsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    journeyExecutionMetricsResponse :: JourneyExecutionMetricsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJourneyExecutionMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getJourneyExecutionMetricsResponse_httpStatus' - The response's http status code.
--
-- 'journeyExecutionMetricsResponse', 'getJourneyExecutionMetricsResponse_journeyExecutionMetricsResponse' - Undocumented member.
newGetJourneyExecutionMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'journeyExecutionMetricsResponse'
  JourneyExecutionMetricsResponse ->
  GetJourneyExecutionMetricsResponse
newGetJourneyExecutionMetricsResponse
  pHttpStatus_
  pJourneyExecutionMetricsResponse_ =
    GetJourneyExecutionMetricsResponse'
      { httpStatus =
          pHttpStatus_,
        journeyExecutionMetricsResponse =
          pJourneyExecutionMetricsResponse_
      }

-- | The response's http status code.
getJourneyExecutionMetricsResponse_httpStatus :: Lens.Lens' GetJourneyExecutionMetricsResponse Prelude.Int
getJourneyExecutionMetricsResponse_httpStatus = Lens.lens (\GetJourneyExecutionMetricsResponse' {httpStatus} -> httpStatus) (\s@GetJourneyExecutionMetricsResponse' {} a -> s {httpStatus = a} :: GetJourneyExecutionMetricsResponse)

-- | Undocumented member.
getJourneyExecutionMetricsResponse_journeyExecutionMetricsResponse :: Lens.Lens' GetJourneyExecutionMetricsResponse JourneyExecutionMetricsResponse
getJourneyExecutionMetricsResponse_journeyExecutionMetricsResponse = Lens.lens (\GetJourneyExecutionMetricsResponse' {journeyExecutionMetricsResponse} -> journeyExecutionMetricsResponse) (\s@GetJourneyExecutionMetricsResponse' {} a -> s {journeyExecutionMetricsResponse = a} :: GetJourneyExecutionMetricsResponse)

instance
  Prelude.NFData
    GetJourneyExecutionMetricsResponse
  where
  rnf GetJourneyExecutionMetricsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf journeyExecutionMetricsResponse
