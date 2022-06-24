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
-- Module      : Amazonka.Pinpoint.GetJourneyDateRangeKpi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard engagement metric
-- that applies to a journey.
module Amazonka.Pinpoint.GetJourneyDateRangeKpi
  ( -- * Creating a Request
    GetJourneyDateRangeKpi (..),
    newGetJourneyDateRangeKpi,

    -- * Request Lenses
    getJourneyDateRangeKpi_nextToken,
    getJourneyDateRangeKpi_endTime,
    getJourneyDateRangeKpi_pageSize,
    getJourneyDateRangeKpi_startTime,
    getJourneyDateRangeKpi_journeyId,
    getJourneyDateRangeKpi_applicationId,
    getJourneyDateRangeKpi_kpiName,

    -- * Destructuring the Response
    GetJourneyDateRangeKpiResponse (..),
    newGetJourneyDateRangeKpiResponse,

    -- * Response Lenses
    getJourneyDateRangeKpiResponse_httpStatus,
    getJourneyDateRangeKpiResponse_journeyDateRangeKpiResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetJourneyDateRangeKpi' smart constructor.
data GetJourneyDateRangeKpi = GetJourneyDateRangeKpi'
  { -- | The string that specifies which page of results to return in a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The last date and time to retrieve data for, as part of an inclusive
    -- date range that filters the query results. This value should be in
    -- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
    -- example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Prelude.Maybe Prelude.Text,
    -- | The first date and time to retrieve data for, as part of an inclusive
    -- date range that filters the query results. This value should be in
    -- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
    -- example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value
    -- should also be fewer than 90 days from the current day.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier for the journey.
    journeyId :: Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    -- | The name of the metric, also referred to as a /key performance indicator
    -- (KPI)/, to retrieve data for. This value describes the associated metric
    -- and consists of two or more terms, which are comprised of lowercase
    -- alphanumeric characters, separated by a hyphen. Examples are
    -- email-open-rate and successful-delivery-rate. For a list of valid
    -- values, see the
    -- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
    kpiName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJourneyDateRangeKpi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getJourneyDateRangeKpi_nextToken' - The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'endTime', 'getJourneyDateRangeKpi_endTime' - The last date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
--
-- 'pageSize', 'getJourneyDateRangeKpi_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'startTime', 'getJourneyDateRangeKpi_startTime' - The first date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value
-- should also be fewer than 90 days from the current day.
--
-- 'journeyId', 'getJourneyDateRangeKpi_journeyId' - The unique identifier for the journey.
--
-- 'applicationId', 'getJourneyDateRangeKpi_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'kpiName', 'getJourneyDateRangeKpi_kpiName' - The name of the metric, also referred to as a /key performance indicator
-- (KPI)/, to retrieve data for. This value describes the associated metric
-- and consists of two or more terms, which are comprised of lowercase
-- alphanumeric characters, separated by a hyphen. Examples are
-- email-open-rate and successful-delivery-rate. For a list of valid
-- values, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
newGetJourneyDateRangeKpi ::
  -- | 'journeyId'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'kpiName'
  Prelude.Text ->
  GetJourneyDateRangeKpi
newGetJourneyDateRangeKpi
  pJourneyId_
  pApplicationId_
  pKpiName_ =
    GetJourneyDateRangeKpi'
      { nextToken =
          Prelude.Nothing,
        endTime = Prelude.Nothing,
        pageSize = Prelude.Nothing,
        startTime = Prelude.Nothing,
        journeyId = pJourneyId_,
        applicationId = pApplicationId_,
        kpiName = pKpiName_
      }

-- | The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getJourneyDateRangeKpi_nextToken :: Lens.Lens' GetJourneyDateRangeKpi (Prelude.Maybe Prelude.Text)
getJourneyDateRangeKpi_nextToken = Lens.lens (\GetJourneyDateRangeKpi' {nextToken} -> nextToken) (\s@GetJourneyDateRangeKpi' {} a -> s {nextToken = a} :: GetJourneyDateRangeKpi)

-- | The last date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
getJourneyDateRangeKpi_endTime :: Lens.Lens' GetJourneyDateRangeKpi (Prelude.Maybe Prelude.UTCTime)
getJourneyDateRangeKpi_endTime = Lens.lens (\GetJourneyDateRangeKpi' {endTime} -> endTime) (\s@GetJourneyDateRangeKpi' {} a -> s {endTime = a} :: GetJourneyDateRangeKpi) Prelude.. Lens.mapping Core._Time

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getJourneyDateRangeKpi_pageSize :: Lens.Lens' GetJourneyDateRangeKpi (Prelude.Maybe Prelude.Text)
getJourneyDateRangeKpi_pageSize = Lens.lens (\GetJourneyDateRangeKpi' {pageSize} -> pageSize) (\s@GetJourneyDateRangeKpi' {} a -> s {pageSize = a} :: GetJourneyDateRangeKpi)

-- | The first date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value
-- should also be fewer than 90 days from the current day.
getJourneyDateRangeKpi_startTime :: Lens.Lens' GetJourneyDateRangeKpi (Prelude.Maybe Prelude.UTCTime)
getJourneyDateRangeKpi_startTime = Lens.lens (\GetJourneyDateRangeKpi' {startTime} -> startTime) (\s@GetJourneyDateRangeKpi' {} a -> s {startTime = a} :: GetJourneyDateRangeKpi) Prelude.. Lens.mapping Core._Time

-- | The unique identifier for the journey.
getJourneyDateRangeKpi_journeyId :: Lens.Lens' GetJourneyDateRangeKpi Prelude.Text
getJourneyDateRangeKpi_journeyId = Lens.lens (\GetJourneyDateRangeKpi' {journeyId} -> journeyId) (\s@GetJourneyDateRangeKpi' {} a -> s {journeyId = a} :: GetJourneyDateRangeKpi)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getJourneyDateRangeKpi_applicationId :: Lens.Lens' GetJourneyDateRangeKpi Prelude.Text
getJourneyDateRangeKpi_applicationId = Lens.lens (\GetJourneyDateRangeKpi' {applicationId} -> applicationId) (\s@GetJourneyDateRangeKpi' {} a -> s {applicationId = a} :: GetJourneyDateRangeKpi)

-- | The name of the metric, also referred to as a /key performance indicator
-- (KPI)/, to retrieve data for. This value describes the associated metric
-- and consists of two or more terms, which are comprised of lowercase
-- alphanumeric characters, separated by a hyphen. Examples are
-- email-open-rate and successful-delivery-rate. For a list of valid
-- values, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
getJourneyDateRangeKpi_kpiName :: Lens.Lens' GetJourneyDateRangeKpi Prelude.Text
getJourneyDateRangeKpi_kpiName = Lens.lens (\GetJourneyDateRangeKpi' {kpiName} -> kpiName) (\s@GetJourneyDateRangeKpi' {} a -> s {kpiName = a} :: GetJourneyDateRangeKpi)

instance Core.AWSRequest GetJourneyDateRangeKpi where
  type
    AWSResponse GetJourneyDateRangeKpi =
      GetJourneyDateRangeKpiResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJourneyDateRangeKpiResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetJourneyDateRangeKpi where
  hashWithSalt _salt GetJourneyDateRangeKpi' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` journeyId
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` kpiName

instance Prelude.NFData GetJourneyDateRangeKpi where
  rnf GetJourneyDateRangeKpi' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf journeyId
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf kpiName

instance Core.ToHeaders GetJourneyDateRangeKpi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetJourneyDateRangeKpi where
  toPath GetJourneyDateRangeKpi' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/journeys/",
        Core.toBS journeyId,
        "/kpis/daterange/",
        Core.toBS kpiName
      ]

instance Core.ToQuery GetJourneyDateRangeKpi where
  toQuery GetJourneyDateRangeKpi' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "end-time" Core.=: endTime,
        "page-size" Core.=: pageSize,
        "start-time" Core.=: startTime
      ]

-- | /See:/ 'newGetJourneyDateRangeKpiResponse' smart constructor.
data GetJourneyDateRangeKpiResponse = GetJourneyDateRangeKpiResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    journeyDateRangeKpiResponse :: JourneyDateRangeKpiResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJourneyDateRangeKpiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getJourneyDateRangeKpiResponse_httpStatus' - The response's http status code.
--
-- 'journeyDateRangeKpiResponse', 'getJourneyDateRangeKpiResponse_journeyDateRangeKpiResponse' - Undocumented member.
newGetJourneyDateRangeKpiResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'journeyDateRangeKpiResponse'
  JourneyDateRangeKpiResponse ->
  GetJourneyDateRangeKpiResponse
newGetJourneyDateRangeKpiResponse
  pHttpStatus_
  pJourneyDateRangeKpiResponse_ =
    GetJourneyDateRangeKpiResponse'
      { httpStatus =
          pHttpStatus_,
        journeyDateRangeKpiResponse =
          pJourneyDateRangeKpiResponse_
      }

-- | The response's http status code.
getJourneyDateRangeKpiResponse_httpStatus :: Lens.Lens' GetJourneyDateRangeKpiResponse Prelude.Int
getJourneyDateRangeKpiResponse_httpStatus = Lens.lens (\GetJourneyDateRangeKpiResponse' {httpStatus} -> httpStatus) (\s@GetJourneyDateRangeKpiResponse' {} a -> s {httpStatus = a} :: GetJourneyDateRangeKpiResponse)

-- | Undocumented member.
getJourneyDateRangeKpiResponse_journeyDateRangeKpiResponse :: Lens.Lens' GetJourneyDateRangeKpiResponse JourneyDateRangeKpiResponse
getJourneyDateRangeKpiResponse_journeyDateRangeKpiResponse = Lens.lens (\GetJourneyDateRangeKpiResponse' {journeyDateRangeKpiResponse} -> journeyDateRangeKpiResponse) (\s@GetJourneyDateRangeKpiResponse' {} a -> s {journeyDateRangeKpiResponse = a} :: GetJourneyDateRangeKpiResponse)

instance
  Prelude.NFData
    GetJourneyDateRangeKpiResponse
  where
  rnf GetJourneyDateRangeKpiResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf journeyDateRangeKpiResponse
