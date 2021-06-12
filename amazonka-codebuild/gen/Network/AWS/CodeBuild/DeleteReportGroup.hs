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
-- Module      : Network.AWS.CodeBuild.DeleteReportGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a report group. Before you delete a report group, you must
-- delete its reports.
module Network.AWS.CodeBuild.DeleteReportGroup
  ( -- * Creating a Request
    DeleteReportGroup (..),
    newDeleteReportGroup,

    -- * Request Lenses
    deleteReportGroup_deleteReports,
    deleteReportGroup_arn,

    -- * Destructuring the Response
    DeleteReportGroupResponse (..),
    newDeleteReportGroupResponse,

    -- * Response Lenses
    deleteReportGroupResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteReportGroup' smart constructor.
data DeleteReportGroup = DeleteReportGroup'
  { -- | If @true@, deletes any reports that belong to a report group before
    -- deleting the report group.
    --
    -- If @false@, you must delete any reports in the report group. Use
    -- <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_ListReportsForReportGroup.html ListReportsForReportGroup>
    -- to get the reports in a report group. Use
    -- <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_DeleteReport.html DeleteReport>
    -- to delete the reports. If you call @DeleteReportGroup@ for a report
    -- group that contains one or more reports, an exception is thrown.
    deleteReports :: Core.Maybe Core.Bool,
    -- | The ARN of the report group to delete.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteReportGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteReports', 'deleteReportGroup_deleteReports' - If @true@, deletes any reports that belong to a report group before
-- deleting the report group.
--
-- If @false@, you must delete any reports in the report group. Use
-- <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_ListReportsForReportGroup.html ListReportsForReportGroup>
-- to get the reports in a report group. Use
-- <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_DeleteReport.html DeleteReport>
-- to delete the reports. If you call @DeleteReportGroup@ for a report
-- group that contains one or more reports, an exception is thrown.
--
-- 'arn', 'deleteReportGroup_arn' - The ARN of the report group to delete.
newDeleteReportGroup ::
  -- | 'arn'
  Core.Text ->
  DeleteReportGroup
newDeleteReportGroup pArn_ =
  DeleteReportGroup'
    { deleteReports = Core.Nothing,
      arn = pArn_
    }

-- | If @true@, deletes any reports that belong to a report group before
-- deleting the report group.
--
-- If @false@, you must delete any reports in the report group. Use
-- <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_ListReportsForReportGroup.html ListReportsForReportGroup>
-- to get the reports in a report group. Use
-- <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_DeleteReport.html DeleteReport>
-- to delete the reports. If you call @DeleteReportGroup@ for a report
-- group that contains one or more reports, an exception is thrown.
deleteReportGroup_deleteReports :: Lens.Lens' DeleteReportGroup (Core.Maybe Core.Bool)
deleteReportGroup_deleteReports = Lens.lens (\DeleteReportGroup' {deleteReports} -> deleteReports) (\s@DeleteReportGroup' {} a -> s {deleteReports = a} :: DeleteReportGroup)

-- | The ARN of the report group to delete.
deleteReportGroup_arn :: Lens.Lens' DeleteReportGroup Core.Text
deleteReportGroup_arn = Lens.lens (\DeleteReportGroup' {arn} -> arn) (\s@DeleteReportGroup' {} a -> s {arn = a} :: DeleteReportGroup)

instance Core.AWSRequest DeleteReportGroup where
  type
    AWSResponse DeleteReportGroup =
      DeleteReportGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReportGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteReportGroup

instance Core.NFData DeleteReportGroup

instance Core.ToHeaders DeleteReportGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.DeleteReportGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteReportGroup where
  toJSON DeleteReportGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("deleteReports" Core..=) Core.<$> deleteReports,
            Core.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath DeleteReportGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteReportGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteReportGroupResponse' smart constructor.
data DeleteReportGroupResponse = DeleteReportGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteReportGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteReportGroupResponse_httpStatus' - The response's http status code.
newDeleteReportGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteReportGroupResponse
newDeleteReportGroupResponse pHttpStatus_ =
  DeleteReportGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteReportGroupResponse_httpStatus :: Lens.Lens' DeleteReportGroupResponse Core.Int
deleteReportGroupResponse_httpStatus = Lens.lens (\DeleteReportGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteReportGroupResponse' {} a -> s {httpStatus = a} :: DeleteReportGroupResponse)

instance Core.NFData DeleteReportGroupResponse
