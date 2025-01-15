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
-- Module      : Amazonka.CodeBuild.GetReportGroupTrend
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Analyzes and accumulates test report values for the specified test
-- reports.
module Amazonka.CodeBuild.GetReportGroupTrend
  ( -- * Creating a Request
    GetReportGroupTrend (..),
    newGetReportGroupTrend,

    -- * Request Lenses
    getReportGroupTrend_numOfReports,
    getReportGroupTrend_reportGroupArn,
    getReportGroupTrend_trendField,

    -- * Destructuring the Response
    GetReportGroupTrendResponse (..),
    newGetReportGroupTrendResponse,

    -- * Response Lenses
    getReportGroupTrendResponse_rawData,
    getReportGroupTrendResponse_stats,
    getReportGroupTrendResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetReportGroupTrend' smart constructor.
data GetReportGroupTrend = GetReportGroupTrend'
  { -- | The number of reports to analyze. This operation always retrieves the
    -- most recent reports.
    --
    -- If this parameter is omitted, the most recent 100 reports are analyzed.
    numOfReports :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the report group that contains the reports to analyze.
    reportGroupArn :: Prelude.Text,
    -- | The test report value to accumulate. This must be one of the following
    -- values:
    --
    -- [Test reports:]
    --     [DURATION]
    --         Accumulate the test run times for the specified reports.
    --
    --     [PASS_RATE]
    --         Accumulate the percentage of tests that passed for the specified
    --         test reports.
    --
    --     [TOTAL]
    --         Accumulate the total number of tests for the specified test
    --         reports.
    --
    -- [Code coverage reports:]
    --     [BRANCH_COVERAGE]
    --         Accumulate the branch coverage percentages for the specified
    --         test reports.
    --
    --     [BRANCHES_COVERED]
    --         Accumulate the branches covered values for the specified test
    --         reports.
    --
    --     [BRANCHES_MISSED]
    --         Accumulate the branches missed values for the specified test
    --         reports.
    --
    --     [LINE_COVERAGE]
    --         Accumulate the line coverage percentages for the specified test
    --         reports.
    --
    --     [LINES_COVERED]
    --         Accumulate the lines covered values for the specified test
    --         reports.
    --
    --     [LINES_MISSED]
    --         Accumulate the lines not covered values for the specified test
    --         reports.
    trendField :: ReportGroupTrendFieldType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReportGroupTrend' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numOfReports', 'getReportGroupTrend_numOfReports' - The number of reports to analyze. This operation always retrieves the
-- most recent reports.
--
-- If this parameter is omitted, the most recent 100 reports are analyzed.
--
-- 'reportGroupArn', 'getReportGroupTrend_reportGroupArn' - The ARN of the report group that contains the reports to analyze.
--
-- 'trendField', 'getReportGroupTrend_trendField' - The test report value to accumulate. This must be one of the following
-- values:
--
-- [Test reports:]
--     [DURATION]
--         Accumulate the test run times for the specified reports.
--
--     [PASS_RATE]
--         Accumulate the percentage of tests that passed for the specified
--         test reports.
--
--     [TOTAL]
--         Accumulate the total number of tests for the specified test
--         reports.
--
-- [Code coverage reports:]
--     [BRANCH_COVERAGE]
--         Accumulate the branch coverage percentages for the specified
--         test reports.
--
--     [BRANCHES_COVERED]
--         Accumulate the branches covered values for the specified test
--         reports.
--
--     [BRANCHES_MISSED]
--         Accumulate the branches missed values for the specified test
--         reports.
--
--     [LINE_COVERAGE]
--         Accumulate the line coverage percentages for the specified test
--         reports.
--
--     [LINES_COVERED]
--         Accumulate the lines covered values for the specified test
--         reports.
--
--     [LINES_MISSED]
--         Accumulate the lines not covered values for the specified test
--         reports.
newGetReportGroupTrend ::
  -- | 'reportGroupArn'
  Prelude.Text ->
  -- | 'trendField'
  ReportGroupTrendFieldType ->
  GetReportGroupTrend
newGetReportGroupTrend pReportGroupArn_ pTrendField_ =
  GetReportGroupTrend'
    { numOfReports =
        Prelude.Nothing,
      reportGroupArn = pReportGroupArn_,
      trendField = pTrendField_
    }

-- | The number of reports to analyze. This operation always retrieves the
-- most recent reports.
--
-- If this parameter is omitted, the most recent 100 reports are analyzed.
getReportGroupTrend_numOfReports :: Lens.Lens' GetReportGroupTrend (Prelude.Maybe Prelude.Natural)
getReportGroupTrend_numOfReports = Lens.lens (\GetReportGroupTrend' {numOfReports} -> numOfReports) (\s@GetReportGroupTrend' {} a -> s {numOfReports = a} :: GetReportGroupTrend)

-- | The ARN of the report group that contains the reports to analyze.
getReportGroupTrend_reportGroupArn :: Lens.Lens' GetReportGroupTrend Prelude.Text
getReportGroupTrend_reportGroupArn = Lens.lens (\GetReportGroupTrend' {reportGroupArn} -> reportGroupArn) (\s@GetReportGroupTrend' {} a -> s {reportGroupArn = a} :: GetReportGroupTrend)

-- | The test report value to accumulate. This must be one of the following
-- values:
--
-- [Test reports:]
--     [DURATION]
--         Accumulate the test run times for the specified reports.
--
--     [PASS_RATE]
--         Accumulate the percentage of tests that passed for the specified
--         test reports.
--
--     [TOTAL]
--         Accumulate the total number of tests for the specified test
--         reports.
--
-- [Code coverage reports:]
--     [BRANCH_COVERAGE]
--         Accumulate the branch coverage percentages for the specified
--         test reports.
--
--     [BRANCHES_COVERED]
--         Accumulate the branches covered values for the specified test
--         reports.
--
--     [BRANCHES_MISSED]
--         Accumulate the branches missed values for the specified test
--         reports.
--
--     [LINE_COVERAGE]
--         Accumulate the line coverage percentages for the specified test
--         reports.
--
--     [LINES_COVERED]
--         Accumulate the lines covered values for the specified test
--         reports.
--
--     [LINES_MISSED]
--         Accumulate the lines not covered values for the specified test
--         reports.
getReportGroupTrend_trendField :: Lens.Lens' GetReportGroupTrend ReportGroupTrendFieldType
getReportGroupTrend_trendField = Lens.lens (\GetReportGroupTrend' {trendField} -> trendField) (\s@GetReportGroupTrend' {} a -> s {trendField = a} :: GetReportGroupTrend)

instance Core.AWSRequest GetReportGroupTrend where
  type
    AWSResponse GetReportGroupTrend =
      GetReportGroupTrendResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReportGroupTrendResponse'
            Prelude.<$> (x Data..?> "rawData" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "stats")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetReportGroupTrend where
  hashWithSalt _salt GetReportGroupTrend' {..} =
    _salt
      `Prelude.hashWithSalt` numOfReports
      `Prelude.hashWithSalt` reportGroupArn
      `Prelude.hashWithSalt` trendField

instance Prelude.NFData GetReportGroupTrend where
  rnf GetReportGroupTrend' {..} =
    Prelude.rnf numOfReports `Prelude.seq`
      Prelude.rnf reportGroupArn `Prelude.seq`
        Prelude.rnf trendField

instance Data.ToHeaders GetReportGroupTrend where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.GetReportGroupTrend" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetReportGroupTrend where
  toJSON GetReportGroupTrend' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("numOfReports" Data..=) Prelude.<$> numOfReports,
            Prelude.Just
              ("reportGroupArn" Data..= reportGroupArn),
            Prelude.Just ("trendField" Data..= trendField)
          ]
      )

instance Data.ToPath GetReportGroupTrend where
  toPath = Prelude.const "/"

instance Data.ToQuery GetReportGroupTrend where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReportGroupTrendResponse' smart constructor.
data GetReportGroupTrendResponse = GetReportGroupTrendResponse'
  { -- | An array that contains the raw data for each report.
    rawData :: Prelude.Maybe [ReportWithRawData],
    -- | Contains the accumulated trend data.
    stats :: Prelude.Maybe ReportGroupTrendStats,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReportGroupTrendResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rawData', 'getReportGroupTrendResponse_rawData' - An array that contains the raw data for each report.
--
-- 'stats', 'getReportGroupTrendResponse_stats' - Contains the accumulated trend data.
--
-- 'httpStatus', 'getReportGroupTrendResponse_httpStatus' - The response's http status code.
newGetReportGroupTrendResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetReportGroupTrendResponse
newGetReportGroupTrendResponse pHttpStatus_ =
  GetReportGroupTrendResponse'
    { rawData =
        Prelude.Nothing,
      stats = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array that contains the raw data for each report.
getReportGroupTrendResponse_rawData :: Lens.Lens' GetReportGroupTrendResponse (Prelude.Maybe [ReportWithRawData])
getReportGroupTrendResponse_rawData = Lens.lens (\GetReportGroupTrendResponse' {rawData} -> rawData) (\s@GetReportGroupTrendResponse' {} a -> s {rawData = a} :: GetReportGroupTrendResponse) Prelude.. Lens.mapping Lens.coerced

-- | Contains the accumulated trend data.
getReportGroupTrendResponse_stats :: Lens.Lens' GetReportGroupTrendResponse (Prelude.Maybe ReportGroupTrendStats)
getReportGroupTrendResponse_stats = Lens.lens (\GetReportGroupTrendResponse' {stats} -> stats) (\s@GetReportGroupTrendResponse' {} a -> s {stats = a} :: GetReportGroupTrendResponse)

-- | The response's http status code.
getReportGroupTrendResponse_httpStatus :: Lens.Lens' GetReportGroupTrendResponse Prelude.Int
getReportGroupTrendResponse_httpStatus = Lens.lens (\GetReportGroupTrendResponse' {httpStatus} -> httpStatus) (\s@GetReportGroupTrendResponse' {} a -> s {httpStatus = a} :: GetReportGroupTrendResponse)

instance Prelude.NFData GetReportGroupTrendResponse where
  rnf GetReportGroupTrendResponse' {..} =
    Prelude.rnf rawData `Prelude.seq`
      Prelude.rnf stats `Prelude.seq`
        Prelude.rnf httpStatus
