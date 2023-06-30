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
-- Module      : Amazonka.CodeBuild.DeleteReportGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a report group. Before you delete a report group, you must
-- delete its reports.
module Amazonka.CodeBuild.DeleteReportGroup
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

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteReportGroup where
  type
    AWSResponse DeleteReportGroup =
      DeleteReportGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReportGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReportGroup where
  hashWithSalt _salt DeleteReportGroup' {..} =
    _salt
      `Prelude.hashWithSalt` deleteReports
      `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteReportGroup where
  rnf DeleteReportGroup' {..} =
    Prelude.rnf deleteReports
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders DeleteReportGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.DeleteReportGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteReportGroup where
  toJSON DeleteReportGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deleteReports" Data..=) Prelude.<$> deleteReports,
            Prelude.Just ("arn" Data..= arn)
          ]
      )

instance Data.ToPath DeleteReportGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteReportGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteReportGroupResponse' smart constructor.
data DeleteReportGroupResponse = DeleteReportGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteReportGroupResponse where
  rnf DeleteReportGroupResponse' {..} =
    Prelude.rnf httpStatus
