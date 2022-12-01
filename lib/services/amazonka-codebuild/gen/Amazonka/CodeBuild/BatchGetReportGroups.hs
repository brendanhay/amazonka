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
-- Module      : Amazonka.CodeBuild.BatchGetReportGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of report groups.
module Amazonka.CodeBuild.BatchGetReportGroups
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

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetReportGroups' smart constructor.
data BatchGetReportGroups = BatchGetReportGroups'
  { -- | An array of report group ARNs that identify the report groups to return.
    reportGroupArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty Prelude.Text ->
  BatchGetReportGroups
newBatchGetReportGroups pReportGroupArns_ =
  BatchGetReportGroups'
    { reportGroupArns =
        Lens.coerced Lens.# pReportGroupArns_
    }

-- | An array of report group ARNs that identify the report groups to return.
batchGetReportGroups_reportGroupArns :: Lens.Lens' BatchGetReportGroups (Prelude.NonEmpty Prelude.Text)
batchGetReportGroups_reportGroupArns = Lens.lens (\BatchGetReportGroups' {reportGroupArns} -> reportGroupArns) (\s@BatchGetReportGroups' {} a -> s {reportGroupArns = a} :: BatchGetReportGroups) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetReportGroups where
  type
    AWSResponse BatchGetReportGroups =
      BatchGetReportGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetReportGroupsResponse'
            Prelude.<$> (x Core..?> "reportGroupsNotFound")
            Prelude.<*> (x Core..?> "reportGroups")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetReportGroups where
  hashWithSalt _salt BatchGetReportGroups' {..} =
    _salt `Prelude.hashWithSalt` reportGroupArns

instance Prelude.NFData BatchGetReportGroups where
  rnf BatchGetReportGroups' {..} =
    Prelude.rnf reportGroupArns

instance Core.ToHeaders BatchGetReportGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.BatchGetReportGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetReportGroups where
  toJSON BatchGetReportGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("reportGroupArns" Core..= reportGroupArns)
          ]
      )

instance Core.ToPath BatchGetReportGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchGetReportGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetReportGroupsResponse' smart constructor.
data BatchGetReportGroupsResponse = BatchGetReportGroupsResponse'
  { -- | An array of ARNs passed to @BatchGetReportGroups@ that are not
    -- associated with a @ReportGroup@.
    reportGroupsNotFound :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The array of report groups returned by @BatchGetReportGroups@.
    reportGroups :: Prelude.Maybe (Prelude.NonEmpty ReportGroup),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchGetReportGroupsResponse
newBatchGetReportGroupsResponse pHttpStatus_ =
  BatchGetReportGroupsResponse'
    { reportGroupsNotFound =
        Prelude.Nothing,
      reportGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of ARNs passed to @BatchGetReportGroups@ that are not
-- associated with a @ReportGroup@.
batchGetReportGroupsResponse_reportGroupsNotFound :: Lens.Lens' BatchGetReportGroupsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetReportGroupsResponse_reportGroupsNotFound = Lens.lens (\BatchGetReportGroupsResponse' {reportGroupsNotFound} -> reportGroupsNotFound) (\s@BatchGetReportGroupsResponse' {} a -> s {reportGroupsNotFound = a} :: BatchGetReportGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The array of report groups returned by @BatchGetReportGroups@.
batchGetReportGroupsResponse_reportGroups :: Lens.Lens' BatchGetReportGroupsResponse (Prelude.Maybe (Prelude.NonEmpty ReportGroup))
batchGetReportGroupsResponse_reportGroups = Lens.lens (\BatchGetReportGroupsResponse' {reportGroups} -> reportGroups) (\s@BatchGetReportGroupsResponse' {} a -> s {reportGroups = a} :: BatchGetReportGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetReportGroupsResponse_httpStatus :: Lens.Lens' BatchGetReportGroupsResponse Prelude.Int
batchGetReportGroupsResponse_httpStatus = Lens.lens (\BatchGetReportGroupsResponse' {httpStatus} -> httpStatus) (\s@BatchGetReportGroupsResponse' {} a -> s {httpStatus = a} :: BatchGetReportGroupsResponse)

instance Prelude.NFData BatchGetReportGroupsResponse where
  rnf BatchGetReportGroupsResponse' {..} =
    Prelude.rnf reportGroupsNotFound
      `Prelude.seq` Prelude.rnf reportGroups
      `Prelude.seq` Prelude.rnf httpStatus
