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
-- Module      : Network.AWS.CodeBuild.BatchGetReports
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of reports.
module Network.AWS.CodeBuild.BatchGetReports
  ( -- * Creating a Request
    BatchGetReports (..),
    newBatchGetReports,

    -- * Request Lenses
    batchGetReports_reportArns,

    -- * Destructuring the Response
    BatchGetReportsResponse (..),
    newBatchGetReportsResponse,

    -- * Response Lenses
    batchGetReportsResponse_reports,
    batchGetReportsResponse_reportsNotFound,
    batchGetReportsResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetReports' smart constructor.
data BatchGetReports = BatchGetReports'
  { -- | An array of ARNs that identify the @Report@ objects to return.
    reportArns :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Core.Text ->
  BatchGetReports
newBatchGetReports pReportArns_ =
  BatchGetReports'
    { reportArns =
        Lens._Coerce Lens.# pReportArns_
    }

-- | An array of ARNs that identify the @Report@ objects to return.
batchGetReports_reportArns :: Lens.Lens' BatchGetReports (Core.NonEmpty Core.Text)
batchGetReports_reportArns = Lens.lens (\BatchGetReports' {reportArns} -> reportArns) (\s@BatchGetReports' {} a -> s {reportArns = a} :: BatchGetReports) Core.. Lens._Coerce

instance Core.AWSRequest BatchGetReports where
  type
    AWSResponse BatchGetReports =
      BatchGetReportsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetReportsResponse'
            Core.<$> (x Core..?> "reports")
            Core.<*> (x Core..?> "reportsNotFound")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchGetReports

instance Core.NFData BatchGetReports

instance Core.ToHeaders BatchGetReports where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.BatchGetReports" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchGetReports where
  toJSON BatchGetReports' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("reportArns" Core..= reportArns)]
      )

instance Core.ToPath BatchGetReports where
  toPath = Core.const "/"

instance Core.ToQuery BatchGetReports where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchGetReportsResponse' smart constructor.
data BatchGetReportsResponse = BatchGetReportsResponse'
  { -- | The array of @Report@ objects returned by @BatchGetReports@.
    reports :: Core.Maybe (Core.NonEmpty Report),
    -- | An array of ARNs passed to @BatchGetReportGroups@ that are not
    -- associated with a @Report@.
    reportsNotFound :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetReportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reports', 'batchGetReportsResponse_reports' - The array of @Report@ objects returned by @BatchGetReports@.
--
-- 'reportsNotFound', 'batchGetReportsResponse_reportsNotFound' - An array of ARNs passed to @BatchGetReportGroups@ that are not
-- associated with a @Report@.
--
-- 'httpStatus', 'batchGetReportsResponse_httpStatus' - The response's http status code.
newBatchGetReportsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchGetReportsResponse
newBatchGetReportsResponse pHttpStatus_ =
  BatchGetReportsResponse'
    { reports = Core.Nothing,
      reportsNotFound = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The array of @Report@ objects returned by @BatchGetReports@.
batchGetReportsResponse_reports :: Lens.Lens' BatchGetReportsResponse (Core.Maybe (Core.NonEmpty Report))
batchGetReportsResponse_reports = Lens.lens (\BatchGetReportsResponse' {reports} -> reports) (\s@BatchGetReportsResponse' {} a -> s {reports = a} :: BatchGetReportsResponse) Core.. Lens.mapping Lens._Coerce

-- | An array of ARNs passed to @BatchGetReportGroups@ that are not
-- associated with a @Report@.
batchGetReportsResponse_reportsNotFound :: Lens.Lens' BatchGetReportsResponse (Core.Maybe (Core.NonEmpty Core.Text))
batchGetReportsResponse_reportsNotFound = Lens.lens (\BatchGetReportsResponse' {reportsNotFound} -> reportsNotFound) (\s@BatchGetReportsResponse' {} a -> s {reportsNotFound = a} :: BatchGetReportsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetReportsResponse_httpStatus :: Lens.Lens' BatchGetReportsResponse Core.Int
batchGetReportsResponse_httpStatus = Lens.lens (\BatchGetReportsResponse' {httpStatus} -> httpStatus) (\s@BatchGetReportsResponse' {} a -> s {httpStatus = a} :: BatchGetReportsResponse)

instance Core.NFData BatchGetReportsResponse
