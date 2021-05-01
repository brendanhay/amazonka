{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    deleteReports :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the report group to delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteReportGroup
newDeleteReportGroup pArn_ =
  DeleteReportGroup'
    { deleteReports = Prelude.Nothing,
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
deleteReportGroup_deleteReports :: Lens.Lens' DeleteReportGroup (Prelude.Maybe Prelude.Bool)
deleteReportGroup_deleteReports = Lens.lens (\DeleteReportGroup' {deleteReports} -> deleteReports) (\s@DeleteReportGroup' {} a -> s {deleteReports = a} :: DeleteReportGroup)

-- | The ARN of the report group to delete.
deleteReportGroup_arn :: Lens.Lens' DeleteReportGroup Prelude.Text
deleteReportGroup_arn = Lens.lens (\DeleteReportGroup' {arn} -> arn) (\s@DeleteReportGroup' {} a -> s {arn = a} :: DeleteReportGroup)

instance Prelude.AWSRequest DeleteReportGroup where
  type Rs DeleteReportGroup = DeleteReportGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReportGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReportGroup

instance Prelude.NFData DeleteReportGroup

instance Prelude.ToHeaders DeleteReportGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeBuild_20161006.DeleteReportGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteReportGroup where
  toJSON DeleteReportGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("deleteReports" Prelude..=)
              Prelude.<$> deleteReports,
            Prelude.Just ("arn" Prelude..= arn)
          ]
      )

instance Prelude.ToPath DeleteReportGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteReportGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteReportGroupResponse' smart constructor.
data DeleteReportGroupResponse = DeleteReportGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteReportGroupResponse
newDeleteReportGroupResponse pHttpStatus_ =
  DeleteReportGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteReportGroupResponse_httpStatus :: Lens.Lens' DeleteReportGroupResponse Prelude.Int
deleteReportGroupResponse_httpStatus = Lens.lens (\DeleteReportGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteReportGroupResponse' {} a -> s {httpStatus = a} :: DeleteReportGroupResponse)

instance Prelude.NFData DeleteReportGroupResponse
