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
-- Module      : Amazonka.WellArchitected.GetConsolidatedReport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a consolidated report of your workloads.
--
-- You can optionally choose to include workloads that have been shared
-- with you.
module Amazonka.WellArchitected.GetConsolidatedReport
  ( -- * Creating a Request
    GetConsolidatedReport (..),
    newGetConsolidatedReport,

    -- * Request Lenses
    getConsolidatedReport_includeSharedResources,
    getConsolidatedReport_maxResults,
    getConsolidatedReport_nextToken,
    getConsolidatedReport_format,

    -- * Destructuring the Response
    GetConsolidatedReportResponse (..),
    newGetConsolidatedReportResponse,

    -- * Response Lenses
    getConsolidatedReportResponse_base64String,
    getConsolidatedReportResponse_metrics,
    getConsolidatedReportResponse_nextToken,
    getConsolidatedReportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newGetConsolidatedReport' smart constructor.
data GetConsolidatedReport = GetConsolidatedReport'
  { -- | Set to @true@ to have shared resources included in the report.
    includeSharedResources :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return for this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The format of the consolidated report.
    --
    -- For @PDF@, @Base64String@ is returned. For @JSON@, @Metrics@ is
    -- returned.
    format :: ReportFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConsolidatedReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeSharedResources', 'getConsolidatedReport_includeSharedResources' - Set to @true@ to have shared resources included in the report.
--
-- 'maxResults', 'getConsolidatedReport_maxResults' - The maximum number of results to return for this request.
--
-- 'nextToken', 'getConsolidatedReport_nextToken' - Undocumented member.
--
-- 'format', 'getConsolidatedReport_format' - The format of the consolidated report.
--
-- For @PDF@, @Base64String@ is returned. For @JSON@, @Metrics@ is
-- returned.
newGetConsolidatedReport ::
  -- | 'format'
  ReportFormat ->
  GetConsolidatedReport
newGetConsolidatedReport pFormat_ =
  GetConsolidatedReport'
    { includeSharedResources =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      format = pFormat_
    }

-- | Set to @true@ to have shared resources included in the report.
getConsolidatedReport_includeSharedResources :: Lens.Lens' GetConsolidatedReport (Prelude.Maybe Prelude.Bool)
getConsolidatedReport_includeSharedResources = Lens.lens (\GetConsolidatedReport' {includeSharedResources} -> includeSharedResources) (\s@GetConsolidatedReport' {} a -> s {includeSharedResources = a} :: GetConsolidatedReport)

-- | The maximum number of results to return for this request.
getConsolidatedReport_maxResults :: Lens.Lens' GetConsolidatedReport (Prelude.Maybe Prelude.Natural)
getConsolidatedReport_maxResults = Lens.lens (\GetConsolidatedReport' {maxResults} -> maxResults) (\s@GetConsolidatedReport' {} a -> s {maxResults = a} :: GetConsolidatedReport)

-- | Undocumented member.
getConsolidatedReport_nextToken :: Lens.Lens' GetConsolidatedReport (Prelude.Maybe Prelude.Text)
getConsolidatedReport_nextToken = Lens.lens (\GetConsolidatedReport' {nextToken} -> nextToken) (\s@GetConsolidatedReport' {} a -> s {nextToken = a} :: GetConsolidatedReport)

-- | The format of the consolidated report.
--
-- For @PDF@, @Base64String@ is returned. For @JSON@, @Metrics@ is
-- returned.
getConsolidatedReport_format :: Lens.Lens' GetConsolidatedReport ReportFormat
getConsolidatedReport_format = Lens.lens (\GetConsolidatedReport' {format} -> format) (\s@GetConsolidatedReport' {} a -> s {format = a} :: GetConsolidatedReport)

instance Core.AWSRequest GetConsolidatedReport where
  type
    AWSResponse GetConsolidatedReport =
      GetConsolidatedReportResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConsolidatedReportResponse'
            Prelude.<$> (x Data..?> "Base64String")
            Prelude.<*> (x Data..?> "Metrics" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConsolidatedReport where
  hashWithSalt _salt GetConsolidatedReport' {..} =
    _salt
      `Prelude.hashWithSalt` includeSharedResources
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` format

instance Prelude.NFData GetConsolidatedReport where
  rnf GetConsolidatedReport' {..} =
    Prelude.rnf includeSharedResources
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf format

instance Data.ToHeaders GetConsolidatedReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetConsolidatedReport where
  toPath = Prelude.const "/consolidatedReport"

instance Data.ToQuery GetConsolidatedReport where
  toQuery GetConsolidatedReport' {..} =
    Prelude.mconcat
      [ "IncludeSharedResources"
          Data.=: includeSharedResources,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "Format" Data.=: format
      ]

-- | /See:/ 'newGetConsolidatedReportResponse' smart constructor.
data GetConsolidatedReportResponse = GetConsolidatedReportResponse'
  { base64String :: Prelude.Maybe Prelude.Text,
    -- | The metrics that make up the consolidated report.
    --
    -- Only returned when @JSON@ format is requested.
    metrics :: Prelude.Maybe [ConsolidatedReportMetric],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConsolidatedReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'base64String', 'getConsolidatedReportResponse_base64String' - Undocumented member.
--
-- 'metrics', 'getConsolidatedReportResponse_metrics' - The metrics that make up the consolidated report.
--
-- Only returned when @JSON@ format is requested.
--
-- 'nextToken', 'getConsolidatedReportResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'getConsolidatedReportResponse_httpStatus' - The response's http status code.
newGetConsolidatedReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConsolidatedReportResponse
newGetConsolidatedReportResponse pHttpStatus_ =
  GetConsolidatedReportResponse'
    { base64String =
        Prelude.Nothing,
      metrics = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getConsolidatedReportResponse_base64String :: Lens.Lens' GetConsolidatedReportResponse (Prelude.Maybe Prelude.Text)
getConsolidatedReportResponse_base64String = Lens.lens (\GetConsolidatedReportResponse' {base64String} -> base64String) (\s@GetConsolidatedReportResponse' {} a -> s {base64String = a} :: GetConsolidatedReportResponse)

-- | The metrics that make up the consolidated report.
--
-- Only returned when @JSON@ format is requested.
getConsolidatedReportResponse_metrics :: Lens.Lens' GetConsolidatedReportResponse (Prelude.Maybe [ConsolidatedReportMetric])
getConsolidatedReportResponse_metrics = Lens.lens (\GetConsolidatedReportResponse' {metrics} -> metrics) (\s@GetConsolidatedReportResponse' {} a -> s {metrics = a} :: GetConsolidatedReportResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getConsolidatedReportResponse_nextToken :: Lens.Lens' GetConsolidatedReportResponse (Prelude.Maybe Prelude.Text)
getConsolidatedReportResponse_nextToken = Lens.lens (\GetConsolidatedReportResponse' {nextToken} -> nextToken) (\s@GetConsolidatedReportResponse' {} a -> s {nextToken = a} :: GetConsolidatedReportResponse)

-- | The response's http status code.
getConsolidatedReportResponse_httpStatus :: Lens.Lens' GetConsolidatedReportResponse Prelude.Int
getConsolidatedReportResponse_httpStatus = Lens.lens (\GetConsolidatedReportResponse' {httpStatus} -> httpStatus) (\s@GetConsolidatedReportResponse' {} a -> s {httpStatus = a} :: GetConsolidatedReportResponse)

instance Prelude.NFData GetConsolidatedReportResponse where
  rnf GetConsolidatedReportResponse' {..} =
    Prelude.rnf base64String
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
