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
-- Module      : Network.AWS.Pinpoint.GetJourneyExecutionMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard execution metric
-- that applies to a journey.
module Network.AWS.Pinpoint.GetJourneyExecutionMetrics
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetJourneyExecutionMetrics' smart constructor.
data GetJourneyExecutionMetrics = GetJourneyExecutionMetrics'
  { -- | The string that specifies which page of results to return in a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The unique identifier for the journey.
    journeyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'journeyId'
  Core.Text ->
  GetJourneyExecutionMetrics
newGetJourneyExecutionMetrics
  pApplicationId_
  pJourneyId_ =
    GetJourneyExecutionMetrics'
      { nextToken =
          Core.Nothing,
        pageSize = Core.Nothing,
        applicationId = pApplicationId_,
        journeyId = pJourneyId_
      }

-- | The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getJourneyExecutionMetrics_nextToken :: Lens.Lens' GetJourneyExecutionMetrics (Core.Maybe Core.Text)
getJourneyExecutionMetrics_nextToken = Lens.lens (\GetJourneyExecutionMetrics' {nextToken} -> nextToken) (\s@GetJourneyExecutionMetrics' {} a -> s {nextToken = a} :: GetJourneyExecutionMetrics)

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getJourneyExecutionMetrics_pageSize :: Lens.Lens' GetJourneyExecutionMetrics (Core.Maybe Core.Text)
getJourneyExecutionMetrics_pageSize = Lens.lens (\GetJourneyExecutionMetrics' {pageSize} -> pageSize) (\s@GetJourneyExecutionMetrics' {} a -> s {pageSize = a} :: GetJourneyExecutionMetrics)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getJourneyExecutionMetrics_applicationId :: Lens.Lens' GetJourneyExecutionMetrics Core.Text
getJourneyExecutionMetrics_applicationId = Lens.lens (\GetJourneyExecutionMetrics' {applicationId} -> applicationId) (\s@GetJourneyExecutionMetrics' {} a -> s {applicationId = a} :: GetJourneyExecutionMetrics)

-- | The unique identifier for the journey.
getJourneyExecutionMetrics_journeyId :: Lens.Lens' GetJourneyExecutionMetrics Core.Text
getJourneyExecutionMetrics_journeyId = Lens.lens (\GetJourneyExecutionMetrics' {journeyId} -> journeyId) (\s@GetJourneyExecutionMetrics' {} a -> s {journeyId = a} :: GetJourneyExecutionMetrics)

instance Core.AWSRequest GetJourneyExecutionMetrics where
  type
    AWSResponse GetJourneyExecutionMetrics =
      GetJourneyExecutionMetricsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJourneyExecutionMetricsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetJourneyExecutionMetrics

instance Core.NFData GetJourneyExecutionMetrics

instance Core.ToHeaders GetJourneyExecutionMetrics where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetJourneyExecutionMetrics where
  toPath GetJourneyExecutionMetrics' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/journeys/",
        Core.toBS journeyId,
        "/execution-metrics"
      ]

instance Core.ToQuery GetJourneyExecutionMetrics where
  toQuery GetJourneyExecutionMetrics' {..} =
    Core.mconcat
      [ "next-token" Core.=: nextToken,
        "page-size" Core.=: pageSize
      ]

-- | /See:/ 'newGetJourneyExecutionMetricsResponse' smart constructor.
data GetJourneyExecutionMetricsResponse = GetJourneyExecutionMetricsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    journeyExecutionMetricsResponse :: JourneyExecutionMetricsResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
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
getJourneyExecutionMetricsResponse_httpStatus :: Lens.Lens' GetJourneyExecutionMetricsResponse Core.Int
getJourneyExecutionMetricsResponse_httpStatus = Lens.lens (\GetJourneyExecutionMetricsResponse' {httpStatus} -> httpStatus) (\s@GetJourneyExecutionMetricsResponse' {} a -> s {httpStatus = a} :: GetJourneyExecutionMetricsResponse)

-- | Undocumented member.
getJourneyExecutionMetricsResponse_journeyExecutionMetricsResponse :: Lens.Lens' GetJourneyExecutionMetricsResponse JourneyExecutionMetricsResponse
getJourneyExecutionMetricsResponse_journeyExecutionMetricsResponse = Lens.lens (\GetJourneyExecutionMetricsResponse' {journeyExecutionMetricsResponse} -> journeyExecutionMetricsResponse) (\s@GetJourneyExecutionMetricsResponse' {} a -> s {journeyExecutionMetricsResponse = a} :: GetJourneyExecutionMetricsResponse)

instance
  Core.NFData
    GetJourneyExecutionMetricsResponse
