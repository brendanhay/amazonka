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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an assessment report in Audit Manager.
--
-- When you run the @DeleteAssessmentReport@ operation, Audit Manager
-- attempts to delete the following data:
--
-- 1.  The specified assessment report that’s stored in your S3 bucket
--
-- 2.  The associated metadata that’s stored in Audit Manager
--
-- If Audit Manager can’t access the assessment report in your S3 bucket,
-- the report isn’t deleted. In this event, the @DeleteAssessmentReport@
-- operation doesn’t fail. Instead, it proceeds to delete the associated
-- metadata only. You must then delete the assessment report from the S3
-- bucket yourself.
--
-- This scenario happens when Audit Manager receives a @403 (Forbidden)@ or
-- @404 (Not Found)@ error from Amazon S3. To avoid this, make sure that
-- your S3 bucket is available, and that you configured the correct
-- permissions for Audit Manager to delete resources in your S3 bucket. For
-- an example permissions policy that you can use, see
-- <https://docs.aws.amazon.com/audit-manager/latest/userguide/security_iam_id-based-policy-examples.html#full-administrator-access-assessment-report-destination Assessment report destination permissions>
-- in the /Audit Manager User Guide/. For information about the issues that
-- could cause a @403 (Forbidden)@ or @404 (Not Found@) error from Amazon
-- S3, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#ErrorCodeList List of Error Codes>
-- in the /Amazon Simple Storage Service API Reference/.
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAssessmentReport' smart constructor.
data DeleteAssessmentReport = DeleteAssessmentReport'
  { -- | The unique identifier for the assessment.
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
-- 'assessmentId', 'deleteAssessmentReport_assessmentId' - The unique identifier for the assessment.
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

-- | The unique identifier for the assessment.
deleteAssessmentReport_assessmentId :: Lens.Lens' DeleteAssessmentReport Prelude.Text
deleteAssessmentReport_assessmentId = Lens.lens (\DeleteAssessmentReport' {assessmentId} -> assessmentId) (\s@DeleteAssessmentReport' {} a -> s {assessmentId = a} :: DeleteAssessmentReport)

-- | The unique identifier for the assessment report.
deleteAssessmentReport_assessmentReportId :: Lens.Lens' DeleteAssessmentReport Prelude.Text
deleteAssessmentReport_assessmentReportId = Lens.lens (\DeleteAssessmentReport' {assessmentReportId} -> assessmentReportId) (\s@DeleteAssessmentReport' {} a -> s {assessmentReportId = a} :: DeleteAssessmentReport)

instance Core.AWSRequest DeleteAssessmentReport where
  type
    AWSResponse DeleteAssessmentReport =
      DeleteAssessmentReportResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAssessmentReportResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAssessmentReport where
  hashWithSalt _salt DeleteAssessmentReport' {..} =
    _salt `Prelude.hashWithSalt` assessmentId
      `Prelude.hashWithSalt` assessmentReportId

instance Prelude.NFData DeleteAssessmentReport where
  rnf DeleteAssessmentReport' {..} =
    Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf assessmentReportId

instance Data.ToHeaders DeleteAssessmentReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAssessmentReport where
  toPath DeleteAssessmentReport' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Data.toBS assessmentId,
        "/reports/",
        Data.toBS assessmentReportId
      ]

instance Data.ToQuery DeleteAssessmentReport where
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
