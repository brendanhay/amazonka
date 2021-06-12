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
-- Module      : Network.AWS.CodeBuild.BatchGetReportGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of report groups.
module Network.AWS.CodeBuild.BatchGetReportGroups
  ( -- * Creating a Request
    BatchGetReportGroups (..),
    newBatchGetReportGroups,

    -- * Request Lenses
    batchGetReportGroups_reportGroupArns,

    -- * Destructuring the Response
    BatchGetReportGroupsResponse (..),
    newBatchGetReportGroupsResponse,

    -- * Response Lenses
    batchGetReportGroupsResponse_reportGroupsNotFound,
    batchGetReportGroupsResponse_reportGroups,
    batchGetReportGroupsResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetReportGroups' smart constructor.
data BatchGetReportGroups = BatchGetReportGroups'
  { -- | An array of report group ARNs that identify the report groups to return.
    reportGroupArns :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetReportGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportGroupArns', 'batchGetReportGroups_reportGroupArns' - An array of report group ARNs that identify the report groups to return.
newBatchGetReportGroups ::
  -- | 'reportGroupArns'
  Core.NonEmpty Core.Text ->
  BatchGetReportGroups
newBatchGetReportGroups pReportGroupArns_ =
  BatchGetReportGroups'
    { reportGroupArns =
        Lens._Coerce Lens.# pReportGroupArns_
    }

-- | An array of report group ARNs that identify the report groups to return.
batchGetReportGroups_reportGroupArns :: Lens.Lens' BatchGetReportGroups (Core.NonEmpty Core.Text)
batchGetReportGroups_reportGroupArns = Lens.lens (\BatchGetReportGroups' {reportGroupArns} -> reportGroupArns) (\s@BatchGetReportGroups' {} a -> s {reportGroupArns = a} :: BatchGetReportGroups) Core.. Lens._Coerce

instance Core.AWSRequest BatchGetReportGroups where
  type
    AWSResponse BatchGetReportGroups =
      BatchGetReportGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetReportGroupsResponse'
            Core.<$> (x Core..?> "reportGroupsNotFound")
            Core.<*> (x Core..?> "reportGroups")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchGetReportGroups

instance Core.NFData BatchGetReportGroups

instance Core.ToHeaders BatchGetReportGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.BatchGetReportGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchGetReportGroups where
  toJSON BatchGetReportGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("reportGroupArns" Core..= reportGroupArns)
          ]
      )

instance Core.ToPath BatchGetReportGroups where
  toPath = Core.const "/"

instance Core.ToQuery BatchGetReportGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchGetReportGroupsResponse' smart constructor.
data BatchGetReportGroupsResponse = BatchGetReportGroupsResponse'
  { -- | An array of ARNs passed to @BatchGetReportGroups@ that are not
    -- associated with a @ReportGroup@.
    reportGroupsNotFound :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The array of report groups returned by @BatchGetReportGroups@.
    reportGroups :: Core.Maybe (Core.NonEmpty ReportGroup),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetReportGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportGroupsNotFound', 'batchGetReportGroupsResponse_reportGroupsNotFound' - An array of ARNs passed to @BatchGetReportGroups@ that are not
-- associated with a @ReportGroup@.
--
-- 'reportGroups', 'batchGetReportGroupsResponse_reportGroups' - The array of report groups returned by @BatchGetReportGroups@.
--
-- 'httpStatus', 'batchGetReportGroupsResponse_httpStatus' - The response's http status code.
newBatchGetReportGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchGetReportGroupsResponse
newBatchGetReportGroupsResponse pHttpStatus_ =
  BatchGetReportGroupsResponse'
    { reportGroupsNotFound =
        Core.Nothing,
      reportGroups = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of ARNs passed to @BatchGetReportGroups@ that are not
-- associated with a @ReportGroup@.
batchGetReportGroupsResponse_reportGroupsNotFound :: Lens.Lens' BatchGetReportGroupsResponse (Core.Maybe (Core.NonEmpty Core.Text))
batchGetReportGroupsResponse_reportGroupsNotFound = Lens.lens (\BatchGetReportGroupsResponse' {reportGroupsNotFound} -> reportGroupsNotFound) (\s@BatchGetReportGroupsResponse' {} a -> s {reportGroupsNotFound = a} :: BatchGetReportGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The array of report groups returned by @BatchGetReportGroups@.
batchGetReportGroupsResponse_reportGroups :: Lens.Lens' BatchGetReportGroupsResponse (Core.Maybe (Core.NonEmpty ReportGroup))
batchGetReportGroupsResponse_reportGroups = Lens.lens (\BatchGetReportGroupsResponse' {reportGroups} -> reportGroups) (\s@BatchGetReportGroupsResponse' {} a -> s {reportGroups = a} :: BatchGetReportGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetReportGroupsResponse_httpStatus :: Lens.Lens' BatchGetReportGroupsResponse Core.Int
batchGetReportGroupsResponse_httpStatus = Lens.lens (\BatchGetReportGroupsResponse' {httpStatus} -> httpStatus) (\s@BatchGetReportGroupsResponse' {} a -> s {httpStatus = a} :: BatchGetReportGroupsResponse)

instance Core.NFData BatchGetReportGroupsResponse
