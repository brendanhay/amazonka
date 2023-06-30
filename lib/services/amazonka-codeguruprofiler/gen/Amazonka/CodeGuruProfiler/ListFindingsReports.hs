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
-- Module      : Amazonka.CodeGuruProfiler.ListFindingsReports
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the available reports for a given profiling group and time range.
module Amazonka.CodeGuruProfiler.ListFindingsReports
  ( -- * Creating a Request
    ListFindingsReports (..),
    newListFindingsReports,

    -- * Request Lenses
    listFindingsReports_dailyReportsOnly,
    listFindingsReports_maxResults,
    listFindingsReports_nextToken,
    listFindingsReports_endTime,
    listFindingsReports_profilingGroupName,
    listFindingsReports_startTime,

    -- * Destructuring the Response
    ListFindingsReportsResponse (..),
    newListFindingsReportsResponse,

    -- * Response Lenses
    listFindingsReportsResponse_nextToken,
    listFindingsReportsResponse_httpStatus,
    listFindingsReportsResponse_findingsReportSummaries,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The structure representing the ListFindingsReportsRequest.
--
-- /See:/ 'newListFindingsReports' smart constructor.
data ListFindingsReports = ListFindingsReports'
  { -- | A @Boolean@ value indicating whether to only return reports from daily
    -- profiles. If set to @True@, only analysis data from daily profiles is
    -- returned. If set to @False@, analysis data is returned from smaller time
    -- windows (for example, one hour).
    dailyReportsOnly :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of report results returned by @ListFindingsReports@
    -- in paginated output. When this parameter is used, @ListFindingsReports@
    -- only returns @maxResults@ results in a single page along with a
    -- @nextToken@ response element. The remaining results of the initial
    -- request can be seen by sending another @ListFindingsReports@ request
    -- with the returned @nextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ value returned from a previous paginated
    -- @ListFindingsReportsRequest@ request where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The end time of the profile to get analysis data about. You must specify
    -- @startTime@ and @endTime@. This is specified using the ISO 8601 format.
    -- For example, 2020-06-01T13:15:02.001Z represents 1 millisecond past June
    -- 1, 2020 1:15:02 PM UTC.
    endTime :: Data.ISO8601,
    -- | The name of the profiling group from which to search for analysis data.
    profilingGroupName :: Prelude.Text,
    -- | The start time of the profile to get analysis data about. You must
    -- specify @startTime@ and @endTime@. This is specified using the ISO 8601
    -- format. For example, 2020-06-01T13:15:02.001Z represents 1 millisecond
    -- past June 1, 2020 1:15:02 PM UTC.
    startTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFindingsReports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dailyReportsOnly', 'listFindingsReports_dailyReportsOnly' - A @Boolean@ value indicating whether to only return reports from daily
-- profiles. If set to @True@, only analysis data from daily profiles is
-- returned. If set to @False@, analysis data is returned from smaller time
-- windows (for example, one hour).
--
-- 'maxResults', 'listFindingsReports_maxResults' - The maximum number of report results returned by @ListFindingsReports@
-- in paginated output. When this parameter is used, @ListFindingsReports@
-- only returns @maxResults@ results in a single page along with a
-- @nextToken@ response element. The remaining results of the initial
-- request can be seen by sending another @ListFindingsReports@ request
-- with the returned @nextToken@ value.
--
-- 'nextToken', 'listFindingsReports_nextToken' - The @nextToken@ value returned from a previous paginated
-- @ListFindingsReportsRequest@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'endTime', 'listFindingsReports_endTime' - The end time of the profile to get analysis data about. You must specify
-- @startTime@ and @endTime@. This is specified using the ISO 8601 format.
-- For example, 2020-06-01T13:15:02.001Z represents 1 millisecond past June
-- 1, 2020 1:15:02 PM UTC.
--
-- 'profilingGroupName', 'listFindingsReports_profilingGroupName' - The name of the profiling group from which to search for analysis data.
--
-- 'startTime', 'listFindingsReports_startTime' - The start time of the profile to get analysis data about. You must
-- specify @startTime@ and @endTime@. This is specified using the ISO 8601
-- format. For example, 2020-06-01T13:15:02.001Z represents 1 millisecond
-- past June 1, 2020 1:15:02 PM UTC.
newListFindingsReports ::
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'profilingGroupName'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  ListFindingsReports
newListFindingsReports
  pEndTime_
  pProfilingGroupName_
  pStartTime_ =
    ListFindingsReports'
      { dailyReportsOnly =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        endTime = Data._Time Lens.# pEndTime_,
        profilingGroupName = pProfilingGroupName_,
        startTime = Data._Time Lens.# pStartTime_
      }

-- | A @Boolean@ value indicating whether to only return reports from daily
-- profiles. If set to @True@, only analysis data from daily profiles is
-- returned. If set to @False@, analysis data is returned from smaller time
-- windows (for example, one hour).
listFindingsReports_dailyReportsOnly :: Lens.Lens' ListFindingsReports (Prelude.Maybe Prelude.Bool)
listFindingsReports_dailyReportsOnly = Lens.lens (\ListFindingsReports' {dailyReportsOnly} -> dailyReportsOnly) (\s@ListFindingsReports' {} a -> s {dailyReportsOnly = a} :: ListFindingsReports)

-- | The maximum number of report results returned by @ListFindingsReports@
-- in paginated output. When this parameter is used, @ListFindingsReports@
-- only returns @maxResults@ results in a single page along with a
-- @nextToken@ response element. The remaining results of the initial
-- request can be seen by sending another @ListFindingsReports@ request
-- with the returned @nextToken@ value.
listFindingsReports_maxResults :: Lens.Lens' ListFindingsReports (Prelude.Maybe Prelude.Natural)
listFindingsReports_maxResults = Lens.lens (\ListFindingsReports' {maxResults} -> maxResults) (\s@ListFindingsReports' {} a -> s {maxResults = a} :: ListFindingsReports)

-- | The @nextToken@ value returned from a previous paginated
-- @ListFindingsReportsRequest@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listFindingsReports_nextToken :: Lens.Lens' ListFindingsReports (Prelude.Maybe Prelude.Text)
listFindingsReports_nextToken = Lens.lens (\ListFindingsReports' {nextToken} -> nextToken) (\s@ListFindingsReports' {} a -> s {nextToken = a} :: ListFindingsReports)

-- | The end time of the profile to get analysis data about. You must specify
-- @startTime@ and @endTime@. This is specified using the ISO 8601 format.
-- For example, 2020-06-01T13:15:02.001Z represents 1 millisecond past June
-- 1, 2020 1:15:02 PM UTC.
listFindingsReports_endTime :: Lens.Lens' ListFindingsReports Prelude.UTCTime
listFindingsReports_endTime = Lens.lens (\ListFindingsReports' {endTime} -> endTime) (\s@ListFindingsReports' {} a -> s {endTime = a} :: ListFindingsReports) Prelude.. Data._Time

-- | The name of the profiling group from which to search for analysis data.
listFindingsReports_profilingGroupName :: Lens.Lens' ListFindingsReports Prelude.Text
listFindingsReports_profilingGroupName = Lens.lens (\ListFindingsReports' {profilingGroupName} -> profilingGroupName) (\s@ListFindingsReports' {} a -> s {profilingGroupName = a} :: ListFindingsReports)

-- | The start time of the profile to get analysis data about. You must
-- specify @startTime@ and @endTime@. This is specified using the ISO 8601
-- format. For example, 2020-06-01T13:15:02.001Z represents 1 millisecond
-- past June 1, 2020 1:15:02 PM UTC.
listFindingsReports_startTime :: Lens.Lens' ListFindingsReports Prelude.UTCTime
listFindingsReports_startTime = Lens.lens (\ListFindingsReports' {startTime} -> startTime) (\s@ListFindingsReports' {} a -> s {startTime = a} :: ListFindingsReports) Prelude.. Data._Time

instance Core.AWSRequest ListFindingsReports where
  type
    AWSResponse ListFindingsReports =
      ListFindingsReportsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFindingsReportsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "findingsReportSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListFindingsReports where
  hashWithSalt _salt ListFindingsReports' {..} =
    _salt
      `Prelude.hashWithSalt` dailyReportsOnly
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` profilingGroupName
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ListFindingsReports where
  rnf ListFindingsReports' {..} =
    Prelude.rnf dailyReportsOnly
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf profilingGroupName
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToHeaders ListFindingsReports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListFindingsReports where
  toPath ListFindingsReports' {..} =
    Prelude.mconcat
      [ "/internal/profilingGroups/",
        Data.toBS profilingGroupName,
        "/findingsReports"
      ]

instance Data.ToQuery ListFindingsReports where
  toQuery ListFindingsReports' {..} =
    Prelude.mconcat
      [ "dailyReportsOnly" Data.=: dailyReportsOnly,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "endTime" Data.=: endTime,
        "startTime" Data.=: startTime
      ]

-- | The structure representing the ListFindingsReportsResponse.
--
-- /See:/ 'newListFindingsReportsResponse' smart constructor.
data ListFindingsReportsResponse = ListFindingsReportsResponse'
  { -- | The @nextToken@ value to include in a future @ListFindingsReports@
    -- request. When the results of a @ListFindingsReports@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of analysis results summaries.
    findingsReportSummaries :: [FindingsReportSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFindingsReportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFindingsReportsResponse_nextToken' - The @nextToken@ value to include in a future @ListFindingsReports@
-- request. When the results of a @ListFindingsReports@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'httpStatus', 'listFindingsReportsResponse_httpStatus' - The response's http status code.
--
-- 'findingsReportSummaries', 'listFindingsReportsResponse_findingsReportSummaries' - The list of analysis results summaries.
newListFindingsReportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFindingsReportsResponse
newListFindingsReportsResponse pHttpStatus_ =
  ListFindingsReportsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      findingsReportSummaries = Prelude.mempty
    }

-- | The @nextToken@ value to include in a future @ListFindingsReports@
-- request. When the results of a @ListFindingsReports@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
listFindingsReportsResponse_nextToken :: Lens.Lens' ListFindingsReportsResponse (Prelude.Maybe Prelude.Text)
listFindingsReportsResponse_nextToken = Lens.lens (\ListFindingsReportsResponse' {nextToken} -> nextToken) (\s@ListFindingsReportsResponse' {} a -> s {nextToken = a} :: ListFindingsReportsResponse)

-- | The response's http status code.
listFindingsReportsResponse_httpStatus :: Lens.Lens' ListFindingsReportsResponse Prelude.Int
listFindingsReportsResponse_httpStatus = Lens.lens (\ListFindingsReportsResponse' {httpStatus} -> httpStatus) (\s@ListFindingsReportsResponse' {} a -> s {httpStatus = a} :: ListFindingsReportsResponse)

-- | The list of analysis results summaries.
listFindingsReportsResponse_findingsReportSummaries :: Lens.Lens' ListFindingsReportsResponse [FindingsReportSummary]
listFindingsReportsResponse_findingsReportSummaries = Lens.lens (\ListFindingsReportsResponse' {findingsReportSummaries} -> findingsReportSummaries) (\s@ListFindingsReportsResponse' {} a -> s {findingsReportSummaries = a} :: ListFindingsReportsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListFindingsReportsResponse where
  rnf ListFindingsReportsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf findingsReportSummaries
