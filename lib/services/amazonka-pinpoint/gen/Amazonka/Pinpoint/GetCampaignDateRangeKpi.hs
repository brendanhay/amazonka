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
-- Module      : Amazonka.Pinpoint.GetCampaignDateRangeKpi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) pre-aggregated data for a standard metric that
-- applies to a campaign.
module Amazonka.Pinpoint.GetCampaignDateRangeKpi
  ( -- * Creating a Request
    GetCampaignDateRangeKpi (..),
    newGetCampaignDateRangeKpi,

    -- * Request Lenses
    getCampaignDateRangeKpi_nextToken,
    getCampaignDateRangeKpi_endTime,
    getCampaignDateRangeKpi_pageSize,
    getCampaignDateRangeKpi_startTime,
    getCampaignDateRangeKpi_applicationId,
    getCampaignDateRangeKpi_kpiName,
    getCampaignDateRangeKpi_campaignId,

    -- * Destructuring the Response
    GetCampaignDateRangeKpiResponse (..),
    newGetCampaignDateRangeKpiResponse,

    -- * Response Lenses
    getCampaignDateRangeKpiResponse_httpStatus,
    getCampaignDateRangeKpiResponse_campaignDateRangeKpiResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCampaignDateRangeKpi' smart constructor.
data GetCampaignDateRangeKpi = GetCampaignDateRangeKpi'
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
    kpiName :: Prelude.Text,
    -- | The unique identifier for the campaign.
    campaignId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCampaignDateRangeKpi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCampaignDateRangeKpi_nextToken' - The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'endTime', 'getCampaignDateRangeKpi_endTime' - The last date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
--
-- 'pageSize', 'getCampaignDateRangeKpi_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'startTime', 'getCampaignDateRangeKpi_startTime' - The first date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value
-- should also be fewer than 90 days from the current day.
--
-- 'applicationId', 'getCampaignDateRangeKpi_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'kpiName', 'getCampaignDateRangeKpi_kpiName' - The name of the metric, also referred to as a /key performance indicator
-- (KPI)/, to retrieve data for. This value describes the associated metric
-- and consists of two or more terms, which are comprised of lowercase
-- alphanumeric characters, separated by a hyphen. Examples are
-- email-open-rate and successful-delivery-rate. For a list of valid
-- values, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
--
-- 'campaignId', 'getCampaignDateRangeKpi_campaignId' - The unique identifier for the campaign.
newGetCampaignDateRangeKpi ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'kpiName'
  Prelude.Text ->
  -- | 'campaignId'
  Prelude.Text ->
  GetCampaignDateRangeKpi
newGetCampaignDateRangeKpi
  pApplicationId_
  pKpiName_
  pCampaignId_ =
    GetCampaignDateRangeKpi'
      { nextToken =
          Prelude.Nothing,
        endTime = Prelude.Nothing,
        pageSize = Prelude.Nothing,
        startTime = Prelude.Nothing,
        applicationId = pApplicationId_,
        kpiName = pKpiName_,
        campaignId = pCampaignId_
      }

-- | The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getCampaignDateRangeKpi_nextToken :: Lens.Lens' GetCampaignDateRangeKpi (Prelude.Maybe Prelude.Text)
getCampaignDateRangeKpi_nextToken = Lens.lens (\GetCampaignDateRangeKpi' {nextToken} -> nextToken) (\s@GetCampaignDateRangeKpi' {} a -> s {nextToken = a} :: GetCampaignDateRangeKpi)

-- | The last date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-26T20:00:00Z for 8:00 PM UTC July 26, 2019.
getCampaignDateRangeKpi_endTime :: Lens.Lens' GetCampaignDateRangeKpi (Prelude.Maybe Prelude.UTCTime)
getCampaignDateRangeKpi_endTime = Lens.lens (\GetCampaignDateRangeKpi' {endTime} -> endTime) (\s@GetCampaignDateRangeKpi' {} a -> s {endTime = a} :: GetCampaignDateRangeKpi) Prelude.. Lens.mapping Core._Time

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getCampaignDateRangeKpi_pageSize :: Lens.Lens' GetCampaignDateRangeKpi (Prelude.Maybe Prelude.Text)
getCampaignDateRangeKpi_pageSize = Lens.lens (\GetCampaignDateRangeKpi' {pageSize} -> pageSize) (\s@GetCampaignDateRangeKpi' {} a -> s {pageSize = a} :: GetCampaignDateRangeKpi)

-- | The first date and time to retrieve data for, as part of an inclusive
-- date range that filters the query results. This value should be in
-- extended ISO 8601 format and use Coordinated Universal Time (UTC), for
-- example: 2019-07-19T20:00:00Z for 8:00 PM UTC July 19, 2019. This value
-- should also be fewer than 90 days from the current day.
getCampaignDateRangeKpi_startTime :: Lens.Lens' GetCampaignDateRangeKpi (Prelude.Maybe Prelude.UTCTime)
getCampaignDateRangeKpi_startTime = Lens.lens (\GetCampaignDateRangeKpi' {startTime} -> startTime) (\s@GetCampaignDateRangeKpi' {} a -> s {startTime = a} :: GetCampaignDateRangeKpi) Prelude.. Lens.mapping Core._Time

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getCampaignDateRangeKpi_applicationId :: Lens.Lens' GetCampaignDateRangeKpi Prelude.Text
getCampaignDateRangeKpi_applicationId = Lens.lens (\GetCampaignDateRangeKpi' {applicationId} -> applicationId) (\s@GetCampaignDateRangeKpi' {} a -> s {applicationId = a} :: GetCampaignDateRangeKpi)

-- | The name of the metric, also referred to as a /key performance indicator
-- (KPI)/, to retrieve data for. This value describes the associated metric
-- and consists of two or more terms, which are comprised of lowercase
-- alphanumeric characters, separated by a hyphen. Examples are
-- email-open-rate and successful-delivery-rate. For a list of valid
-- values, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
getCampaignDateRangeKpi_kpiName :: Lens.Lens' GetCampaignDateRangeKpi Prelude.Text
getCampaignDateRangeKpi_kpiName = Lens.lens (\GetCampaignDateRangeKpi' {kpiName} -> kpiName) (\s@GetCampaignDateRangeKpi' {} a -> s {kpiName = a} :: GetCampaignDateRangeKpi)

-- | The unique identifier for the campaign.
getCampaignDateRangeKpi_campaignId :: Lens.Lens' GetCampaignDateRangeKpi Prelude.Text
getCampaignDateRangeKpi_campaignId = Lens.lens (\GetCampaignDateRangeKpi' {campaignId} -> campaignId) (\s@GetCampaignDateRangeKpi' {} a -> s {campaignId = a} :: GetCampaignDateRangeKpi)

instance Core.AWSRequest GetCampaignDateRangeKpi where
  type
    AWSResponse GetCampaignDateRangeKpi =
      GetCampaignDateRangeKpiResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCampaignDateRangeKpiResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetCampaignDateRangeKpi where
  hashWithSalt _salt GetCampaignDateRangeKpi' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` kpiName
      `Prelude.hashWithSalt` campaignId

instance Prelude.NFData GetCampaignDateRangeKpi where
  rnf GetCampaignDateRangeKpi' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf kpiName
      `Prelude.seq` Prelude.rnf campaignId

instance Core.ToHeaders GetCampaignDateRangeKpi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetCampaignDateRangeKpi where
  toPath GetCampaignDateRangeKpi' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/campaigns/",
        Core.toBS campaignId,
        "/kpis/daterange/",
        Core.toBS kpiName
      ]

instance Core.ToQuery GetCampaignDateRangeKpi where
  toQuery GetCampaignDateRangeKpi' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "end-time" Core.=: endTime,
        "page-size" Core.=: pageSize,
        "start-time" Core.=: startTime
      ]

-- | /See:/ 'newGetCampaignDateRangeKpiResponse' smart constructor.
data GetCampaignDateRangeKpiResponse = GetCampaignDateRangeKpiResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    campaignDateRangeKpiResponse :: CampaignDateRangeKpiResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCampaignDateRangeKpiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getCampaignDateRangeKpiResponse_httpStatus' - The response's http status code.
--
-- 'campaignDateRangeKpiResponse', 'getCampaignDateRangeKpiResponse_campaignDateRangeKpiResponse' - Undocumented member.
newGetCampaignDateRangeKpiResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'campaignDateRangeKpiResponse'
  CampaignDateRangeKpiResponse ->
  GetCampaignDateRangeKpiResponse
newGetCampaignDateRangeKpiResponse
  pHttpStatus_
  pCampaignDateRangeKpiResponse_ =
    GetCampaignDateRangeKpiResponse'
      { httpStatus =
          pHttpStatus_,
        campaignDateRangeKpiResponse =
          pCampaignDateRangeKpiResponse_
      }

-- | The response's http status code.
getCampaignDateRangeKpiResponse_httpStatus :: Lens.Lens' GetCampaignDateRangeKpiResponse Prelude.Int
getCampaignDateRangeKpiResponse_httpStatus = Lens.lens (\GetCampaignDateRangeKpiResponse' {httpStatus} -> httpStatus) (\s@GetCampaignDateRangeKpiResponse' {} a -> s {httpStatus = a} :: GetCampaignDateRangeKpiResponse)

-- | Undocumented member.
getCampaignDateRangeKpiResponse_campaignDateRangeKpiResponse :: Lens.Lens' GetCampaignDateRangeKpiResponse CampaignDateRangeKpiResponse
getCampaignDateRangeKpiResponse_campaignDateRangeKpiResponse = Lens.lens (\GetCampaignDateRangeKpiResponse' {campaignDateRangeKpiResponse} -> campaignDateRangeKpiResponse) (\s@GetCampaignDateRangeKpiResponse' {} a -> s {campaignDateRangeKpiResponse = a} :: GetCampaignDateRangeKpiResponse)

instance
  Prelude.NFData
    GetCampaignDateRangeKpiResponse
  where
  rnf GetCampaignDateRangeKpiResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf campaignDateRangeKpiResponse
