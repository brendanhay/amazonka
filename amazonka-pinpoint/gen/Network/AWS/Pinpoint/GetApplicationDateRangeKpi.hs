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
-- Module      : Network.AWS.Pinpoint.GetApplicationDateRangeKpi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard metric that
-- applies to an application.
module Network.AWS.Pinpoint.GetApplicationDateRangeKpi
  ( -- * Creating a Request
    GetApplicationDateRangeKpi (..),
    newGetApplicationDateRangeKpi,

    -- * Request Lenses
    getApplicationDateRangeKpi_nextToken,
    getApplicationDateRangeKpi_pageSize,
    getApplicationDateRangeKpi_startTime,
    getApplicationDateRangeKpi_endTime,
    getApplicationDateRangeKpi_applicationId,
    getApplicationDateRangeKpi_kpiName,

    -- * Destructuring the Response
    GetApplicationDateRangeKpiResponse (..),
    newGetApplicationDateRangeKpiResponse,

    -- * Response Lenses
    getApplicationDateRangeKpiResponse_httpStatus,
    getApplicationDateRangeKpiResponse_applicationDateRangeKpiResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetApplicationDateRangeKpi' smart constructor.
data GetApplicationDateRangeKpi = GetApplicationDateRangeKpi'
  { -- | The string that specifies which page of results to return in a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The first date and time to retrieve data for, as part of an inclusive
    -- date range that filters the query results. This value should be in
    -- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
    -- example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value
    -- should also be fewer than 90 days from the current day.
    startTime :: Core.Maybe Core.POSIX,
    -- | The last date and time to retrieve data for, as part of an inclusive
    -- date range that filters the query results. This value should be in
    -- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
    -- example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
    endTime :: Core.Maybe Core.POSIX,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The name of the metric, also referred to as a /key performance indicator
    -- (KPI)/, to retrieve data for. This value describes the associated metric
    -- and consists of two or more terms, which are comprised of lowercase
    -- alphanumeric characters, separated by a hyphen. Examples are
    -- email-open-rate and successful-delivery-rate. For a list of valid
    -- values, see the
    -- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
    kpiName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApplicationDateRangeKpi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getApplicationDateRangeKpi_nextToken' - The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'pageSize', 'getApplicationDateRangeKpi_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'startTime', 'getApplicationDateRangeKpi_startTime' - The first date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value
-- should also be fewer than 90 days from the current day.
--
-- 'endTime', 'getApplicationDateRangeKpi_endTime' - The last date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
--
-- 'applicationId', 'getApplicationDateRangeKpi_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'kpiName', 'getApplicationDateRangeKpi_kpiName' - The name of the metric, also referred to as a /key performance indicator
-- (KPI)/, to retrieve data for. This value describes the associated metric
-- and consists of two or more terms, which are comprised of lowercase
-- alphanumeric characters, separated by a hyphen. Examples are
-- email-open-rate and successful-delivery-rate. For a list of valid
-- values, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
newGetApplicationDateRangeKpi ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'kpiName'
  Core.Text ->
  GetApplicationDateRangeKpi
newGetApplicationDateRangeKpi
  pApplicationId_
  pKpiName_ =
    GetApplicationDateRangeKpi'
      { nextToken =
          Core.Nothing,
        pageSize = Core.Nothing,
        startTime = Core.Nothing,
        endTime = Core.Nothing,
        applicationId = pApplicationId_,
        kpiName = pKpiName_
      }

-- | The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getApplicationDateRangeKpi_nextToken :: Lens.Lens' GetApplicationDateRangeKpi (Core.Maybe Core.Text)
getApplicationDateRangeKpi_nextToken = Lens.lens (\GetApplicationDateRangeKpi' {nextToken} -> nextToken) (\s@GetApplicationDateRangeKpi' {} a -> s {nextToken = a} :: GetApplicationDateRangeKpi)

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getApplicationDateRangeKpi_pageSize :: Lens.Lens' GetApplicationDateRangeKpi (Core.Maybe Core.Text)
getApplicationDateRangeKpi_pageSize = Lens.lens (\GetApplicationDateRangeKpi' {pageSize} -> pageSize) (\s@GetApplicationDateRangeKpi' {} a -> s {pageSize = a} :: GetApplicationDateRangeKpi)

-- | The first date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value
-- should also be fewer than 90 days from the current day.
getApplicationDateRangeKpi_startTime :: Lens.Lens' GetApplicationDateRangeKpi (Core.Maybe Core.UTCTime)
getApplicationDateRangeKpi_startTime = Lens.lens (\GetApplicationDateRangeKpi' {startTime} -> startTime) (\s@GetApplicationDateRangeKpi' {} a -> s {startTime = a} :: GetApplicationDateRangeKpi) Core.. Lens.mapping Core._Time

-- | The last date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
getApplicationDateRangeKpi_endTime :: Lens.Lens' GetApplicationDateRangeKpi (Core.Maybe Core.UTCTime)
getApplicationDateRangeKpi_endTime = Lens.lens (\GetApplicationDateRangeKpi' {endTime} -> endTime) (\s@GetApplicationDateRangeKpi' {} a -> s {endTime = a} :: GetApplicationDateRangeKpi) Core.. Lens.mapping Core._Time

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getApplicationDateRangeKpi_applicationId :: Lens.Lens' GetApplicationDateRangeKpi Core.Text
getApplicationDateRangeKpi_applicationId = Lens.lens (\GetApplicationDateRangeKpi' {applicationId} -> applicationId) (\s@GetApplicationDateRangeKpi' {} a -> s {applicationId = a} :: GetApplicationDateRangeKpi)

-- | The name of the metric, also referred to as a /key performance indicator
-- (KPI)/, to retrieve data for. This value describes the associated metric
-- and consists of two or more terms, which are comprised of lowercase
-- alphanumeric characters, separated by a hyphen. Examples are
-- email-open-rate and successful-delivery-rate. For a list of valid
-- values, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
getApplicationDateRangeKpi_kpiName :: Lens.Lens' GetApplicationDateRangeKpi Core.Text
getApplicationDateRangeKpi_kpiName = Lens.lens (\GetApplicationDateRangeKpi' {kpiName} -> kpiName) (\s@GetApplicationDateRangeKpi' {} a -> s {kpiName = a} :: GetApplicationDateRangeKpi)

instance Core.AWSRequest GetApplicationDateRangeKpi where
  type
    AWSResponse GetApplicationDateRangeKpi =
      GetApplicationDateRangeKpiResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationDateRangeKpiResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetApplicationDateRangeKpi

instance Core.NFData GetApplicationDateRangeKpi

instance Core.ToHeaders GetApplicationDateRangeKpi where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetApplicationDateRangeKpi where
  toPath GetApplicationDateRangeKpi' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/kpis/daterange/",
        Core.toBS kpiName
      ]

instance Core.ToQuery GetApplicationDateRangeKpi where
  toQuery GetApplicationDateRangeKpi' {..} =
    Core.mconcat
      [ "next-token" Core.=: nextToken,
        "page-size" Core.=: pageSize,
        "start-time" Core.=: startTime,
        "end-time" Core.=: endTime
      ]

-- | /See:/ 'newGetApplicationDateRangeKpiResponse' smart constructor.
data GetApplicationDateRangeKpiResponse = GetApplicationDateRangeKpiResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    applicationDateRangeKpiResponse :: ApplicationDateRangeKpiResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApplicationDateRangeKpiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getApplicationDateRangeKpiResponse_httpStatus' - The response's http status code.
--
-- 'applicationDateRangeKpiResponse', 'getApplicationDateRangeKpiResponse_applicationDateRangeKpiResponse' - Undocumented member.
newGetApplicationDateRangeKpiResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'applicationDateRangeKpiResponse'
  ApplicationDateRangeKpiResponse ->
  GetApplicationDateRangeKpiResponse
newGetApplicationDateRangeKpiResponse
  pHttpStatus_
  pApplicationDateRangeKpiResponse_ =
    GetApplicationDateRangeKpiResponse'
      { httpStatus =
          pHttpStatus_,
        applicationDateRangeKpiResponse =
          pApplicationDateRangeKpiResponse_
      }

-- | The response's http status code.
getApplicationDateRangeKpiResponse_httpStatus :: Lens.Lens' GetApplicationDateRangeKpiResponse Core.Int
getApplicationDateRangeKpiResponse_httpStatus = Lens.lens (\GetApplicationDateRangeKpiResponse' {httpStatus} -> httpStatus) (\s@GetApplicationDateRangeKpiResponse' {} a -> s {httpStatus = a} :: GetApplicationDateRangeKpiResponse)

-- | Undocumented member.
getApplicationDateRangeKpiResponse_applicationDateRangeKpiResponse :: Lens.Lens' GetApplicationDateRangeKpiResponse ApplicationDateRangeKpiResponse
getApplicationDateRangeKpiResponse_applicationDateRangeKpiResponse = Lens.lens (\GetApplicationDateRangeKpiResponse' {applicationDateRangeKpiResponse} -> applicationDateRangeKpiResponse) (\s@GetApplicationDateRangeKpiResponse' {} a -> s {applicationDateRangeKpiResponse = a} :: GetApplicationDateRangeKpiResponse)

instance
  Core.NFData
    GetApplicationDateRangeKpiResponse
