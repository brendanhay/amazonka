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
-- Module      : Amazonka.Pinpoint.GetApplicationDateRangeKpi
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard metric that
-- applies to an application.
module Amazonka.Pinpoint.GetApplicationDateRangeKpi
  ( -- * Creating a Request
    GetApplicationDateRangeKpi (..),
    newGetApplicationDateRangeKpi,

    -- * Request Lenses
    getApplicationDateRangeKpi_endTime,
    getApplicationDateRangeKpi_nextToken,
    getApplicationDateRangeKpi_pageSize,
    getApplicationDateRangeKpi_startTime,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetApplicationDateRangeKpi' smart constructor.
data GetApplicationDateRangeKpi = GetApplicationDateRangeKpi'
  { -- | The last date and time to retrieve data for, as part of an inclusive
    -- date range that filters the query results. This value should be in
    -- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
    -- example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The string that specifies which page of results to return in a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Prelude.Maybe Prelude.Text,
    -- | The first date and time to retrieve data for, as part of an inclusive
    -- date range that filters the query results. This value should be in
    -- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
    -- example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value
    -- should also be fewer than 90 days from the current day.
    startTime :: Prelude.Maybe Data.ISO8601,
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
-- Create a value of 'GetApplicationDateRangeKpi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'getApplicationDateRangeKpi_endTime' - The last date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
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
  Prelude.Text ->
  -- | 'kpiName'
  Prelude.Text ->
  GetApplicationDateRangeKpi
newGetApplicationDateRangeKpi
  pApplicationId_
  pKpiName_ =
    GetApplicationDateRangeKpi'
      { endTime =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        pageSize = Prelude.Nothing,
        startTime = Prelude.Nothing,
        applicationId = pApplicationId_,
        kpiName = pKpiName_
      }

-- | The last date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
getApplicationDateRangeKpi_endTime :: Lens.Lens' GetApplicationDateRangeKpi (Prelude.Maybe Prelude.UTCTime)
getApplicationDateRangeKpi_endTime = Lens.lens (\GetApplicationDateRangeKpi' {endTime} -> endTime) (\s@GetApplicationDateRangeKpi' {} a -> s {endTime = a} :: GetApplicationDateRangeKpi) Prelude.. Lens.mapping Data._Time

-- | The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getApplicationDateRangeKpi_nextToken :: Lens.Lens' GetApplicationDateRangeKpi (Prelude.Maybe Prelude.Text)
getApplicationDateRangeKpi_nextToken = Lens.lens (\GetApplicationDateRangeKpi' {nextToken} -> nextToken) (\s@GetApplicationDateRangeKpi' {} a -> s {nextToken = a} :: GetApplicationDateRangeKpi)

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getApplicationDateRangeKpi_pageSize :: Lens.Lens' GetApplicationDateRangeKpi (Prelude.Maybe Prelude.Text)
getApplicationDateRangeKpi_pageSize = Lens.lens (\GetApplicationDateRangeKpi' {pageSize} -> pageSize) (\s@GetApplicationDateRangeKpi' {} a -> s {pageSize = a} :: GetApplicationDateRangeKpi)

-- | The first date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value
-- should also be fewer than 90 days from the current day.
getApplicationDateRangeKpi_startTime :: Lens.Lens' GetApplicationDateRangeKpi (Prelude.Maybe Prelude.UTCTime)
getApplicationDateRangeKpi_startTime = Lens.lens (\GetApplicationDateRangeKpi' {startTime} -> startTime) (\s@GetApplicationDateRangeKpi' {} a -> s {startTime = a} :: GetApplicationDateRangeKpi) Prelude.. Lens.mapping Data._Time

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getApplicationDateRangeKpi_applicationId :: Lens.Lens' GetApplicationDateRangeKpi Prelude.Text
getApplicationDateRangeKpi_applicationId = Lens.lens (\GetApplicationDateRangeKpi' {applicationId} -> applicationId) (\s@GetApplicationDateRangeKpi' {} a -> s {applicationId = a} :: GetApplicationDateRangeKpi)

-- | The name of the metric, also referred to as a /key performance indicator
-- (KPI)/, to retrieve data for. This value describes the associated metric
-- and consists of two or more terms, which are comprised of lowercase
-- alphanumeric characters, separated by a hyphen. Examples are
-- email-open-rate and successful-delivery-rate. For a list of valid
-- values, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
getApplicationDateRangeKpi_kpiName :: Lens.Lens' GetApplicationDateRangeKpi Prelude.Text
getApplicationDateRangeKpi_kpiName = Lens.lens (\GetApplicationDateRangeKpi' {kpiName} -> kpiName) (\s@GetApplicationDateRangeKpi' {} a -> s {kpiName = a} :: GetApplicationDateRangeKpi)

instance Core.AWSRequest GetApplicationDateRangeKpi where
  type
    AWSResponse GetApplicationDateRangeKpi =
      GetApplicationDateRangeKpiResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationDateRangeKpiResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetApplicationDateRangeKpi where
  hashWithSalt _salt GetApplicationDateRangeKpi' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` kpiName

instance Prelude.NFData GetApplicationDateRangeKpi where
  rnf GetApplicationDateRangeKpi' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf kpiName

instance Data.ToHeaders GetApplicationDateRangeKpi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetApplicationDateRangeKpi where
  toPath GetApplicationDateRangeKpi' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/kpis/daterange/",
        Data.toBS kpiName
      ]

instance Data.ToQuery GetApplicationDateRangeKpi where
  toQuery GetApplicationDateRangeKpi' {..} =
    Prelude.mconcat
      [ "end-time" Data.=: endTime,
        "next-token" Data.=: nextToken,
        "page-size" Data.=: pageSize,
        "start-time" Data.=: startTime
      ]

-- | /See:/ 'newGetApplicationDateRangeKpiResponse' smart constructor.
data GetApplicationDateRangeKpiResponse = GetApplicationDateRangeKpiResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    applicationDateRangeKpiResponse :: ApplicationDateRangeKpiResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
getApplicationDateRangeKpiResponse_httpStatus :: Lens.Lens' GetApplicationDateRangeKpiResponse Prelude.Int
getApplicationDateRangeKpiResponse_httpStatus = Lens.lens (\GetApplicationDateRangeKpiResponse' {httpStatus} -> httpStatus) (\s@GetApplicationDateRangeKpiResponse' {} a -> s {httpStatus = a} :: GetApplicationDateRangeKpiResponse)

-- | Undocumented member.
getApplicationDateRangeKpiResponse_applicationDateRangeKpiResponse :: Lens.Lens' GetApplicationDateRangeKpiResponse ApplicationDateRangeKpiResponse
getApplicationDateRangeKpiResponse_applicationDateRangeKpiResponse = Lens.lens (\GetApplicationDateRangeKpiResponse' {applicationDateRangeKpiResponse} -> applicationDateRangeKpiResponse) (\s@GetApplicationDateRangeKpiResponse' {} a -> s {applicationDateRangeKpiResponse = a} :: GetApplicationDateRangeKpiResponse)

instance
  Prelude.NFData
    GetApplicationDateRangeKpiResponse
  where
  rnf GetApplicationDateRangeKpiResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationDateRangeKpiResponse
