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
-- Module      : Amazonka.CodeBuild.BatchGetReports
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of reports.
module Amazonka.CodeBuild.BatchGetReports
  ( -- * Creating a Request
    BatchGetReports (..),
    newBatchGetReports,

    -- * Request Lenses
    batchGetReports_reportArns,

    -- * Destructuring the Response
    BatchGetReportsResponse (..),
    newBatchGetReportsResponse,

    -- * Response Lenses
    batchGetReportsResponse_reportsNotFound,
    batchGetReportsResponse_reports,
    batchGetReportsResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetReports' smart constructor.
data BatchGetReports = BatchGetReports'
  { -- | An array of ARNs that identify the @Report@ objects to return.
    reportArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetReports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportArns', 'batchGetReports_reportArns' - An array of ARNs that identify the @Report@ objects to return.
newBatchGetReports ::
  -- | 'reportArns'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetReports
newBatchGetReports pReportArns_ =
  BatchGetReports'
    { reportArns =
        Lens.coerced Lens.# pReportArns_
    }

-- | An array of ARNs that identify the @Report@ objects to return.
batchGetReports_reportArns :: Lens.Lens' BatchGetReports (Prelude.NonEmpty Prelude.Text)
batchGetReports_reportArns = Lens.lens (\BatchGetReports' {reportArns} -> reportArns) (\s@BatchGetReports' {} a -> s {reportArns = a} :: BatchGetReports) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetReports where
  type
    AWSResponse BatchGetReports =
      BatchGetReportsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetReportsResponse'
            Prelude.<$> (x Core..?> "reportsNotFound")
            Prelude.<*> (x Core..?> "reports")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetReports where
  hashWithSalt _salt BatchGetReports' {..} =
    _salt `Prelude.hashWithSalt` reportArns

instance Prelude.NFData BatchGetReports where
  rnf BatchGetReports' {..} = Prelude.rnf reportArns

instance Core.ToHeaders BatchGetReports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.BatchGetReports" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetReports where
  toJSON BatchGetReports' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("reportArns" Core..= reportArns)]
      )

instance Core.ToPath BatchGetReports where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchGetReports where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetReportsResponse' smart constructor.
data BatchGetReportsResponse = BatchGetReportsResponse'
  { -- | An array of ARNs passed to @BatchGetReportGroups@ that are not
    -- associated with a @Report@.
    reportsNotFound :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The array of @Report@ objects returned by @BatchGetReports@.
    reports :: Prelude.Maybe (Prelude.NonEmpty Report),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetReportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportsNotFound', 'batchGetReportsResponse_reportsNotFound' - An array of ARNs passed to @BatchGetReportGroups@ that are not
-- associated with a @Report@.
--
-- 'reports', 'batchGetReportsResponse_reports' - The array of @Report@ objects returned by @BatchGetReports@.
--
-- 'httpStatus', 'batchGetReportsResponse_httpStatus' - The response's http status code.
newBatchGetReportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetReportsResponse
newBatchGetReportsResponse pHttpStatus_ =
  BatchGetReportsResponse'
    { reportsNotFound =
        Prelude.Nothing,
      reports = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of ARNs passed to @BatchGetReportGroups@ that are not
-- associated with a @Report@.
batchGetReportsResponse_reportsNotFound :: Lens.Lens' BatchGetReportsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetReportsResponse_reportsNotFound = Lens.lens (\BatchGetReportsResponse' {reportsNotFound} -> reportsNotFound) (\s@BatchGetReportsResponse' {} a -> s {reportsNotFound = a} :: BatchGetReportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The array of @Report@ objects returned by @BatchGetReports@.
batchGetReportsResponse_reports :: Lens.Lens' BatchGetReportsResponse (Prelude.Maybe (Prelude.NonEmpty Report))
batchGetReportsResponse_reports = Lens.lens (\BatchGetReportsResponse' {reports} -> reports) (\s@BatchGetReportsResponse' {} a -> s {reports = a} :: BatchGetReportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetReportsResponse_httpStatus :: Lens.Lens' BatchGetReportsResponse Prelude.Int
batchGetReportsResponse_httpStatus = Lens.lens (\BatchGetReportsResponse' {httpStatus} -> httpStatus) (\s@BatchGetReportsResponse' {} a -> s {httpStatus = a} :: BatchGetReportsResponse)

instance Prelude.NFData BatchGetReportsResponse where
  rnf BatchGetReportsResponse' {..} =
    Prelude.rnf reportsNotFound
      `Prelude.seq` Prelude.rnf reports
      `Prelude.seq` Prelude.rnf httpStatus
