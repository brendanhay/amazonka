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
-- Module      : Amazonka.AuditManager.DeleteAssessmentReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an assessment report from an assessment in Audit Manager.
module Amazonka.AuditManager.DeleteAssessmentReport
  ( -- * Creating a Request
    DeleteAssessmentReport (..),
    newDeleteAssessmentReport,

    -- * Request Lenses
    deleteAssessmentReport_assessmentId,
    deleteAssessmentReport_assessmentReportId,

    -- * Destructuring the Response
    DeleteAssessmentReportResponse (..),
    newDeleteAssessmentReportResponse,

    -- * Response Lenses
    deleteAssessmentReportResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAssessmentReport' smart constructor.
data DeleteAssessmentReport = DeleteAssessmentReport'
  { -- | The identifier for the specified assessment.
    assessmentId :: Prelude.Text,
    -- | The unique identifier for the assessment report.
    assessmentReportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssessmentReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentId', 'deleteAssessmentReport_assessmentId' - The identifier for the specified assessment.
--
-- 'assessmentReportId', 'deleteAssessmentReport_assessmentReportId' - The unique identifier for the assessment report.
newDeleteAssessmentReport ::
  -- | 'assessmentId'
  Prelude.Text ->
  -- | 'assessmentReportId'
  Prelude.Text ->
  DeleteAssessmentReport
newDeleteAssessmentReport
  pAssessmentId_
  pAssessmentReportId_ =
    DeleteAssessmentReport'
      { assessmentId =
          pAssessmentId_,
        assessmentReportId = pAssessmentReportId_
      }

-- | The identifier for the specified assessment.
deleteAssessmentReport_assessmentId :: Lens.Lens' DeleteAssessmentReport Prelude.Text
deleteAssessmentReport_assessmentId = Lens.lens (\DeleteAssessmentReport' {assessmentId} -> assessmentId) (\s@DeleteAssessmentReport' {} a -> s {assessmentId = a} :: DeleteAssessmentReport)

-- | The unique identifier for the assessment report.
deleteAssessmentReport_assessmentReportId :: Lens.Lens' DeleteAssessmentReport Prelude.Text
deleteAssessmentReport_assessmentReportId = Lens.lens (\DeleteAssessmentReport' {assessmentReportId} -> assessmentReportId) (\s@DeleteAssessmentReport' {} a -> s {assessmentReportId = a} :: DeleteAssessmentReport)

instance Core.AWSRequest DeleteAssessmentReport where
  type
    AWSResponse DeleteAssessmentReport =
      DeleteAssessmentReportResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAssessmentReportResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAssessmentReport where
  hashWithSalt salt' DeleteAssessmentReport' {..} =
    salt' `Prelude.hashWithSalt` assessmentReportId
      `Prelude.hashWithSalt` assessmentId

instance Prelude.NFData DeleteAssessmentReport where
  rnf DeleteAssessmentReport' {..} =
    Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf assessmentReportId

instance Core.ToHeaders DeleteAssessmentReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteAssessmentReport where
  toPath DeleteAssessmentReport' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Core.toBS assessmentId,
        "/reports/",
        Core.toBS assessmentReportId
      ]

instance Core.ToQuery DeleteAssessmentReport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAssessmentReportResponse' smart constructor.
data DeleteAssessmentReportResponse = DeleteAssessmentReportResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssessmentReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAssessmentReportResponse_httpStatus' - The response's http status code.
newDeleteAssessmentReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAssessmentReportResponse
newDeleteAssessmentReportResponse pHttpStatus_ =
  DeleteAssessmentReportResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAssessmentReportResponse_httpStatus :: Lens.Lens' DeleteAssessmentReportResponse Prelude.Int
deleteAssessmentReportResponse_httpStatus = Lens.lens (\DeleteAssessmentReportResponse' {httpStatus} -> httpStatus) (\s@DeleteAssessmentReportResponse' {} a -> s {httpStatus = a} :: DeleteAssessmentReportResponse)

instance
  Prelude.NFData
    DeleteAssessmentReportResponse
  where
  rnf DeleteAssessmentReportResponse' {..} =
    Prelude.rnf httpStatus
