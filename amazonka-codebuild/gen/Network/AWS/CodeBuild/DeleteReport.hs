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
-- Module      : Network.AWS.CodeBuild.DeleteReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a report.
module Network.AWS.CodeBuild.DeleteReport
  ( -- * Creating a Request
    DeleteReport (..),
    newDeleteReport,

    -- * Request Lenses
    deleteReport_arn,

    -- * Destructuring the Response
    DeleteReportResponse (..),
    newDeleteReportResponse,

    -- * Response Lenses
    deleteReportResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteReport' smart constructor.
data DeleteReport = DeleteReport'
  { -- | The ARN of the report to delete.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteReport_arn' - The ARN of the report to delete.
newDeleteReport ::
  -- | 'arn'
  Core.Text ->
  DeleteReport
newDeleteReport pArn_ = DeleteReport' {arn = pArn_}

-- | The ARN of the report to delete.
deleteReport_arn :: Lens.Lens' DeleteReport Core.Text
deleteReport_arn = Lens.lens (\DeleteReport' {arn} -> arn) (\s@DeleteReport' {} a -> s {arn = a} :: DeleteReport)

instance Core.AWSRequest DeleteReport where
  type AWSResponse DeleteReport = DeleteReportResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReportResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteReport

instance Core.NFData DeleteReport

instance Core.ToHeaders DeleteReport where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.DeleteReport" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteReport where
  toJSON DeleteReport' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath DeleteReport where
  toPath = Core.const "/"

instance Core.ToQuery DeleteReport where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteReportResponse' smart constructor.
data DeleteReportResponse = DeleteReportResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteReportResponse_httpStatus' - The response's http status code.
newDeleteReportResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteReportResponse
newDeleteReportResponse pHttpStatus_ =
  DeleteReportResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteReportResponse_httpStatus :: Lens.Lens' DeleteReportResponse Core.Int
deleteReportResponse_httpStatus = Lens.lens (\DeleteReportResponse' {httpStatus} -> httpStatus) (\s@DeleteReportResponse' {} a -> s {httpStatus = a} :: DeleteReportResponse)

instance Core.NFData DeleteReportResponse
