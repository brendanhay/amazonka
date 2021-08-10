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
-- Module      : Network.AWS.SESv2.GetDomainStatisticsReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve inbox placement and engagement rates for the domains that you
-- use to send email.
module Network.AWS.SESv2.GetDomainStatisticsReport
  ( -- * Creating a Request
    GetDomainStatisticsReport (..),
    newGetDomainStatisticsReport,

    -- * Request Lenses
    getDomainStatisticsReport_domain,
    getDomainStatisticsReport_startDate,
    getDomainStatisticsReport_endDate,

    -- * Destructuring the Response
    GetDomainStatisticsReportResponse (..),
    newGetDomainStatisticsReportResponse,

    -- * Response Lenses
    getDomainStatisticsReportResponse_httpStatus,
    getDomainStatisticsReportResponse_overallVolume,
    getDomainStatisticsReportResponse_dailyVolumes,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | A request to obtain deliverability metrics for a domain.
--
-- /See:/ 'newGetDomainStatisticsReport' smart constructor.
data GetDomainStatisticsReport = GetDomainStatisticsReport'
  { -- | The domain that you want to obtain deliverability metrics for.
    domain :: Prelude.Text,
    -- | The first day (in Unix time) that you want to obtain domain
    -- deliverability metrics for.
    startDate :: Core.POSIX,
    -- | The last day (in Unix time) that you want to obtain domain
    -- deliverability metrics for. The @EndDate@ that you specify has to be
    -- less than or equal to 30 days after the @StartDate@.
    endDate :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainStatisticsReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'getDomainStatisticsReport_domain' - The domain that you want to obtain deliverability metrics for.
--
-- 'startDate', 'getDomainStatisticsReport_startDate' - The first day (in Unix time) that you want to obtain domain
-- deliverability metrics for.
--
-- 'endDate', 'getDomainStatisticsReport_endDate' - The last day (in Unix time) that you want to obtain domain
-- deliverability metrics for. The @EndDate@ that you specify has to be
-- less than or equal to 30 days after the @StartDate@.
newGetDomainStatisticsReport ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'startDate'
  Prelude.UTCTime ->
  -- | 'endDate'
  Prelude.UTCTime ->
  GetDomainStatisticsReport
newGetDomainStatisticsReport
  pDomain_
  pStartDate_
  pEndDate_ =
    GetDomainStatisticsReport'
      { domain = pDomain_,
        startDate = Core._Time Lens.# pStartDate_,
        endDate = Core._Time Lens.# pEndDate_
      }

-- | The domain that you want to obtain deliverability metrics for.
getDomainStatisticsReport_domain :: Lens.Lens' GetDomainStatisticsReport Prelude.Text
getDomainStatisticsReport_domain = Lens.lens (\GetDomainStatisticsReport' {domain} -> domain) (\s@GetDomainStatisticsReport' {} a -> s {domain = a} :: GetDomainStatisticsReport)

-- | The first day (in Unix time) that you want to obtain domain
-- deliverability metrics for.
getDomainStatisticsReport_startDate :: Lens.Lens' GetDomainStatisticsReport Prelude.UTCTime
getDomainStatisticsReport_startDate = Lens.lens (\GetDomainStatisticsReport' {startDate} -> startDate) (\s@GetDomainStatisticsReport' {} a -> s {startDate = a} :: GetDomainStatisticsReport) Prelude.. Core._Time

-- | The last day (in Unix time) that you want to obtain domain
-- deliverability metrics for. The @EndDate@ that you specify has to be
-- less than or equal to 30 days after the @StartDate@.
getDomainStatisticsReport_endDate :: Lens.Lens' GetDomainStatisticsReport Prelude.UTCTime
getDomainStatisticsReport_endDate = Lens.lens (\GetDomainStatisticsReport' {endDate} -> endDate) (\s@GetDomainStatisticsReport' {} a -> s {endDate = a} :: GetDomainStatisticsReport) Prelude.. Core._Time

instance Core.AWSRequest GetDomainStatisticsReport where
  type
    AWSResponse GetDomainStatisticsReport =
      GetDomainStatisticsReportResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainStatisticsReportResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "OverallVolume")
            Prelude.<*> (x Core..?> "DailyVolumes" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetDomainStatisticsReport

instance Prelude.NFData GetDomainStatisticsReport

instance Core.ToHeaders GetDomainStatisticsReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetDomainStatisticsReport where
  toPath GetDomainStatisticsReport' {..} =
    Prelude.mconcat
      [ "/v2/email/deliverability-dashboard/statistics-report/",
        Core.toBS domain
      ]

instance Core.ToQuery GetDomainStatisticsReport where
  toQuery GetDomainStatisticsReport' {..} =
    Prelude.mconcat
      [ "StartDate" Core.=: startDate,
        "EndDate" Core.=: endDate
      ]

-- | An object that includes statistics that are related to the domain that
-- you specified.
--
-- /See:/ 'newGetDomainStatisticsReportResponse' smart constructor.
data GetDomainStatisticsReportResponse = GetDomainStatisticsReportResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object that contains deliverability metrics for the domain that you
    -- specified. The data in this object is a summary of all of the data that
    -- was collected from the @StartDate@ to the @EndDate@.
    overallVolume :: OverallVolume,
    -- | An object that contains deliverability metrics for the domain that you
    -- specified. This object contains data for each day, starting on the
    -- @StartDate@ and ending on the @EndDate@.
    dailyVolumes :: [DailyVolume]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainStatisticsReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getDomainStatisticsReportResponse_httpStatus' - The response's http status code.
--
-- 'overallVolume', 'getDomainStatisticsReportResponse_overallVolume' - An object that contains deliverability metrics for the domain that you
-- specified. The data in this object is a summary of all of the data that
-- was collected from the @StartDate@ to the @EndDate@.
--
-- 'dailyVolumes', 'getDomainStatisticsReportResponse_dailyVolumes' - An object that contains deliverability metrics for the domain that you
-- specified. This object contains data for each day, starting on the
-- @StartDate@ and ending on the @EndDate@.
newGetDomainStatisticsReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'overallVolume'
  OverallVolume ->
  GetDomainStatisticsReportResponse
newGetDomainStatisticsReportResponse
  pHttpStatus_
  pOverallVolume_ =
    GetDomainStatisticsReportResponse'
      { httpStatus =
          pHttpStatus_,
        overallVolume = pOverallVolume_,
        dailyVolumes = Prelude.mempty
      }

-- | The response's http status code.
getDomainStatisticsReportResponse_httpStatus :: Lens.Lens' GetDomainStatisticsReportResponse Prelude.Int
getDomainStatisticsReportResponse_httpStatus = Lens.lens (\GetDomainStatisticsReportResponse' {httpStatus} -> httpStatus) (\s@GetDomainStatisticsReportResponse' {} a -> s {httpStatus = a} :: GetDomainStatisticsReportResponse)

-- | An object that contains deliverability metrics for the domain that you
-- specified. The data in this object is a summary of all of the data that
-- was collected from the @StartDate@ to the @EndDate@.
getDomainStatisticsReportResponse_overallVolume :: Lens.Lens' GetDomainStatisticsReportResponse OverallVolume
getDomainStatisticsReportResponse_overallVolume = Lens.lens (\GetDomainStatisticsReportResponse' {overallVolume} -> overallVolume) (\s@GetDomainStatisticsReportResponse' {} a -> s {overallVolume = a} :: GetDomainStatisticsReportResponse)

-- | An object that contains deliverability metrics for the domain that you
-- specified. This object contains data for each day, starting on the
-- @StartDate@ and ending on the @EndDate@.
getDomainStatisticsReportResponse_dailyVolumes :: Lens.Lens' GetDomainStatisticsReportResponse [DailyVolume]
getDomainStatisticsReportResponse_dailyVolumes = Lens.lens (\GetDomainStatisticsReportResponse' {dailyVolumes} -> dailyVolumes) (\s@GetDomainStatisticsReportResponse' {} a -> s {dailyVolumes = a} :: GetDomainStatisticsReportResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    GetDomainStatisticsReportResponse
