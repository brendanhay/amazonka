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
-- Module      : Amazonka.CodeGuruProfiler.GetFindingsReportAccountSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_FindingsReportSummary.html FindingsReportSummary>
-- objects that contain analysis results for all profiling groups in your
-- AWS account.
module Amazonka.CodeGuruProfiler.GetFindingsReportAccountSummary
  ( -- * Creating a Request
    GetFindingsReportAccountSummary (..),
    newGetFindingsReportAccountSummary,

    -- * Request Lenses
    getFindingsReportAccountSummary_nextToken,
    getFindingsReportAccountSummary_maxResults,
    getFindingsReportAccountSummary_dailyReportsOnly,

    -- * Destructuring the Response
    GetFindingsReportAccountSummaryResponse (..),
    newGetFindingsReportAccountSummaryResponse,

    -- * Response Lenses
    getFindingsReportAccountSummaryResponse_nextToken,
    getFindingsReportAccountSummaryResponse_httpStatus,
    getFindingsReportAccountSummaryResponse_reportSummaries,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The structure representing the GetFindingsReportAccountSummaryRequest.
--
-- /See:/ 'newGetFindingsReportAccountSummary' smart constructor.
data GetFindingsReportAccountSummary = GetFindingsReportAccountSummary'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @GetFindingsReportAccountSummary@ request where @maxResults@ was used
    -- and the results exceeded the value of that parameter. Pagination
    -- continues from the end of the previous results that returned the
    -- @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results returned by
    -- @ GetFindingsReportAccountSummary@ in paginated output. When this
    -- parameter is used, @GetFindingsReportAccountSummary@ only returns
    -- @maxResults@ results in a single page along with a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @GetFindingsReportAccountSummary@ request with the
    -- returned @nextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A @Boolean@ value indicating whether to only return reports from daily
    -- profiles. If set to @True@, only analysis data from daily profiles is
    -- returned. If set to @False@, analysis data is returned from smaller time
    -- windows (for example, one hour).
    dailyReportsOnly :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindingsReportAccountSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getFindingsReportAccountSummary_nextToken' - The @nextToken@ value returned from a previous paginated
-- @GetFindingsReportAccountSummary@ request where @maxResults@ was used
-- and the results exceeded the value of that parameter. Pagination
-- continues from the end of the previous results that returned the
-- @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'maxResults', 'getFindingsReportAccountSummary_maxResults' - The maximum number of results returned by
-- @ GetFindingsReportAccountSummary@ in paginated output. When this
-- parameter is used, @GetFindingsReportAccountSummary@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @GetFindingsReportAccountSummary@ request with the
-- returned @nextToken@ value.
--
-- 'dailyReportsOnly', 'getFindingsReportAccountSummary_dailyReportsOnly' - A @Boolean@ value indicating whether to only return reports from daily
-- profiles. If set to @True@, only analysis data from daily profiles is
-- returned. If set to @False@, analysis data is returned from smaller time
-- windows (for example, one hour).
newGetFindingsReportAccountSummary ::
  GetFindingsReportAccountSummary
newGetFindingsReportAccountSummary =
  GetFindingsReportAccountSummary'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      dailyReportsOnly = Prelude.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated
-- @GetFindingsReportAccountSummary@ request where @maxResults@ was used
-- and the results exceeded the value of that parameter. Pagination
-- continues from the end of the previous results that returned the
-- @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
getFindingsReportAccountSummary_nextToken :: Lens.Lens' GetFindingsReportAccountSummary (Prelude.Maybe Prelude.Text)
getFindingsReportAccountSummary_nextToken = Lens.lens (\GetFindingsReportAccountSummary' {nextToken} -> nextToken) (\s@GetFindingsReportAccountSummary' {} a -> s {nextToken = a} :: GetFindingsReportAccountSummary)

-- | The maximum number of results returned by
-- @ GetFindingsReportAccountSummary@ in paginated output. When this
-- parameter is used, @GetFindingsReportAccountSummary@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @GetFindingsReportAccountSummary@ request with the
-- returned @nextToken@ value.
getFindingsReportAccountSummary_maxResults :: Lens.Lens' GetFindingsReportAccountSummary (Prelude.Maybe Prelude.Natural)
getFindingsReportAccountSummary_maxResults = Lens.lens (\GetFindingsReportAccountSummary' {maxResults} -> maxResults) (\s@GetFindingsReportAccountSummary' {} a -> s {maxResults = a} :: GetFindingsReportAccountSummary)

-- | A @Boolean@ value indicating whether to only return reports from daily
-- profiles. If set to @True@, only analysis data from daily profiles is
-- returned. If set to @False@, analysis data is returned from smaller time
-- windows (for example, one hour).
getFindingsReportAccountSummary_dailyReportsOnly :: Lens.Lens' GetFindingsReportAccountSummary (Prelude.Maybe Prelude.Bool)
getFindingsReportAccountSummary_dailyReportsOnly = Lens.lens (\GetFindingsReportAccountSummary' {dailyReportsOnly} -> dailyReportsOnly) (\s@GetFindingsReportAccountSummary' {} a -> s {dailyReportsOnly = a} :: GetFindingsReportAccountSummary)

instance
  Core.AWSRequest
    GetFindingsReportAccountSummary
  where
  type
    AWSResponse GetFindingsReportAccountSummary =
      GetFindingsReportAccountSummaryResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFindingsReportAccountSummaryResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "reportSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    GetFindingsReportAccountSummary
  where
  hashWithSalt
    _salt
    GetFindingsReportAccountSummary' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` dailyReportsOnly

instance
  Prelude.NFData
    GetFindingsReportAccountSummary
  where
  rnf GetFindingsReportAccountSummary' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf dailyReportsOnly

instance
  Core.ToHeaders
    GetFindingsReportAccountSummary
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetFindingsReportAccountSummary where
  toPath = Prelude.const "/internal/findingsReports"

instance Core.ToQuery GetFindingsReportAccountSummary where
  toQuery GetFindingsReportAccountSummary' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "dailyReportsOnly" Core.=: dailyReportsOnly
      ]

-- | The structure representing the GetFindingsReportAccountSummaryResponse.
--
-- /See:/ 'newGetFindingsReportAccountSummaryResponse' smart constructor.
data GetFindingsReportAccountSummaryResponse = GetFindingsReportAccountSummaryResponse'
  { -- | The @nextToken@ value to include in a future
    -- @GetFindingsReportAccountSummary@ request. When the results of a
    -- @GetFindingsReportAccountSummary@ request exceed @maxResults@, this
    -- value can be used to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The return list of
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_FindingsReportSummary.html FindingsReportSummary>
    -- objects taht contain summaries of analysis results for all profiling
    -- groups in your AWS account.
    reportSummaries :: [FindingsReportSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindingsReportAccountSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getFindingsReportAccountSummaryResponse_nextToken' - The @nextToken@ value to include in a future
-- @GetFindingsReportAccountSummary@ request. When the results of a
-- @GetFindingsReportAccountSummary@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'getFindingsReportAccountSummaryResponse_httpStatus' - The response's http status code.
--
-- 'reportSummaries', 'getFindingsReportAccountSummaryResponse_reportSummaries' - The return list of
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_FindingsReportSummary.html FindingsReportSummary>
-- objects taht contain summaries of analysis results for all profiling
-- groups in your AWS account.
newGetFindingsReportAccountSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFindingsReportAccountSummaryResponse
newGetFindingsReportAccountSummaryResponse
  pHttpStatus_ =
    GetFindingsReportAccountSummaryResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        reportSummaries = Prelude.mempty
      }

-- | The @nextToken@ value to include in a future
-- @GetFindingsReportAccountSummary@ request. When the results of a
-- @GetFindingsReportAccountSummary@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getFindingsReportAccountSummaryResponse_nextToken :: Lens.Lens' GetFindingsReportAccountSummaryResponse (Prelude.Maybe Prelude.Text)
getFindingsReportAccountSummaryResponse_nextToken = Lens.lens (\GetFindingsReportAccountSummaryResponse' {nextToken} -> nextToken) (\s@GetFindingsReportAccountSummaryResponse' {} a -> s {nextToken = a} :: GetFindingsReportAccountSummaryResponse)

-- | The response's http status code.
getFindingsReportAccountSummaryResponse_httpStatus :: Lens.Lens' GetFindingsReportAccountSummaryResponse Prelude.Int
getFindingsReportAccountSummaryResponse_httpStatus = Lens.lens (\GetFindingsReportAccountSummaryResponse' {httpStatus} -> httpStatus) (\s@GetFindingsReportAccountSummaryResponse' {} a -> s {httpStatus = a} :: GetFindingsReportAccountSummaryResponse)

-- | The return list of
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_FindingsReportSummary.html FindingsReportSummary>
-- objects taht contain summaries of analysis results for all profiling
-- groups in your AWS account.
getFindingsReportAccountSummaryResponse_reportSummaries :: Lens.Lens' GetFindingsReportAccountSummaryResponse [FindingsReportSummary]
getFindingsReportAccountSummaryResponse_reportSummaries = Lens.lens (\GetFindingsReportAccountSummaryResponse' {reportSummaries} -> reportSummaries) (\s@GetFindingsReportAccountSummaryResponse' {} a -> s {reportSummaries = a} :: GetFindingsReportAccountSummaryResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    GetFindingsReportAccountSummaryResponse
  where
  rnf GetFindingsReportAccountSummaryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf reportSummaries
