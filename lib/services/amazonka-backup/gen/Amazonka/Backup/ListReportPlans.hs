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
-- Module      : Amazonka.Backup.ListReportPlans
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your report plans. For detailed information about a
-- single report plan, use @DescribeReportPlan@.
module Amazonka.Backup.ListReportPlans
  ( -- * Creating a Request
    ListReportPlans (..),
    newListReportPlans,

    -- * Request Lenses
    listReportPlans_maxResults,
    listReportPlans_nextToken,

    -- * Destructuring the Response
    ListReportPlansResponse (..),
    newListReportPlansResponse,

    -- * Response Lenses
    listReportPlansResponse_nextToken,
    listReportPlansResponse_reportPlans,
    listReportPlansResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReportPlans' smart constructor.
data ListReportPlans = ListReportPlans'
  { -- | The number of desired results from 1 to 1000. Optional. If unspecified,
    -- the query will return 1 MB of data.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReportPlans' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listReportPlans_maxResults' - The number of desired results from 1 to 1000. Optional. If unspecified,
-- the query will return 1 MB of data.
--
-- 'nextToken', 'listReportPlans_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
newListReportPlans ::
  ListReportPlans
newListReportPlans =
  ListReportPlans'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The number of desired results from 1 to 1000. Optional. If unspecified,
-- the query will return 1 MB of data.
listReportPlans_maxResults :: Lens.Lens' ListReportPlans (Prelude.Maybe Prelude.Natural)
listReportPlans_maxResults = Lens.lens (\ListReportPlans' {maxResults} -> maxResults) (\s@ListReportPlans' {} a -> s {maxResults = a} :: ListReportPlans)

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listReportPlans_nextToken :: Lens.Lens' ListReportPlans (Prelude.Maybe Prelude.Text)
listReportPlans_nextToken = Lens.lens (\ListReportPlans' {nextToken} -> nextToken) (\s@ListReportPlans' {} a -> s {nextToken = a} :: ListReportPlans)

instance Core.AWSRequest ListReportPlans where
  type
    AWSResponse ListReportPlans =
      ListReportPlansResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReportPlansResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ReportPlans" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReportPlans where
  hashWithSalt _salt ListReportPlans' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListReportPlans where
  rnf ListReportPlans' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListReportPlans where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListReportPlans where
  toPath = Prelude.const "/audit/report-plans"

instance Data.ToQuery ListReportPlans where
  toQuery ListReportPlans' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListReportPlansResponse' smart constructor.
data ListReportPlansResponse = ListReportPlansResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of your report plans with detailed information for each plan.
    -- This information includes the Amazon Resource Name (ARN), report plan
    -- name, description, settings, delivery channel, deployment status,
    -- creation time, and last times the report plan attempted to and
    -- successfully ran.
    reportPlans :: Prelude.Maybe [ReportPlan],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReportPlansResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReportPlansResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'reportPlans', 'listReportPlansResponse_reportPlans' - A list of your report plans with detailed information for each plan.
-- This information includes the Amazon Resource Name (ARN), report plan
-- name, description, settings, delivery channel, deployment status,
-- creation time, and last times the report plan attempted to and
-- successfully ran.
--
-- 'httpStatus', 'listReportPlansResponse_httpStatus' - The response's http status code.
newListReportPlansResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReportPlansResponse
newListReportPlansResponse pHttpStatus_ =
  ListReportPlansResponse'
    { nextToken =
        Prelude.Nothing,
      reportPlans = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listReportPlansResponse_nextToken :: Lens.Lens' ListReportPlansResponse (Prelude.Maybe Prelude.Text)
listReportPlansResponse_nextToken = Lens.lens (\ListReportPlansResponse' {nextToken} -> nextToken) (\s@ListReportPlansResponse' {} a -> s {nextToken = a} :: ListReportPlansResponse)

-- | A list of your report plans with detailed information for each plan.
-- This information includes the Amazon Resource Name (ARN), report plan
-- name, description, settings, delivery channel, deployment status,
-- creation time, and last times the report plan attempted to and
-- successfully ran.
listReportPlansResponse_reportPlans :: Lens.Lens' ListReportPlansResponse (Prelude.Maybe [ReportPlan])
listReportPlansResponse_reportPlans = Lens.lens (\ListReportPlansResponse' {reportPlans} -> reportPlans) (\s@ListReportPlansResponse' {} a -> s {reportPlans = a} :: ListReportPlansResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReportPlansResponse_httpStatus :: Lens.Lens' ListReportPlansResponse Prelude.Int
listReportPlansResponse_httpStatus = Lens.lens (\ListReportPlansResponse' {httpStatus} -> httpStatus) (\s@ListReportPlansResponse' {} a -> s {httpStatus = a} :: ListReportPlansResponse)

instance Prelude.NFData ListReportPlansResponse where
  rnf ListReportPlansResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reportPlans
      `Prelude.seq` Prelude.rnf httpStatus
