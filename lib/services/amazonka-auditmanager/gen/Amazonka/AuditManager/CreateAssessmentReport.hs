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
-- Module      : Amazonka.AuditManager.CreateAssessmentReport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an assessment report for the specified assessment.
module Amazonka.AuditManager.CreateAssessmentReport
  ( -- * Creating a Request
    CreateAssessmentReport (..),
    newCreateAssessmentReport,

    -- * Request Lenses
    createAssessmentReport_description,
    createAssessmentReport_queryStatement,
    createAssessmentReport_name,
    createAssessmentReport_assessmentId,

    -- * Destructuring the Response
    CreateAssessmentReportResponse (..),
    newCreateAssessmentReportResponse,

    -- * Response Lenses
    createAssessmentReportResponse_assessmentReport,
    createAssessmentReportResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAssessmentReport' smart constructor.
data CreateAssessmentReport = CreateAssessmentReport'
  { -- | The description of the assessment report.
    description :: Prelude.Maybe Prelude.Text,
    -- | A SQL statement that represents an evidence finder query.
    --
    -- Provide this parameter when you want to generate an assessment report
    -- from the results of an evidence finder search query. When you use this
    -- parameter, Audit Manager generates a one-time report using only the
    -- evidence from the query output. This report does not include any
    -- assessment evidence that was manually
    -- <https://docs.aws.amazon.com/userguide/generate-assessment-report.html#generate-assessment-report-include-evidence added to a report using the console>,
    -- or
    -- <https://docs.aws.amazon.com/APIReference-evidenceFinder/API_BatchAssociateAssessmentReportEvidence.html associated with a report using the API>.
    --
    -- To use this parameter, the
    -- <https://docs.aws.amazon.com/APIReference-evidenceFinder/API_EvidenceFinderSetup.html#auditmanager-Type-EvidenceFinderSetup-enablementStatus enablementStatus>
    -- of evidence finder must be @ENABLED@.
    --
    -- For examples and help resolving @queryStatement@ validation exceptions,
    -- see
    -- <https://docs.aws.amazon.com/audit-manager/latest/userguide/evidence-finder-issues.html#querystatement-exceptions Troubleshooting evidence finder issues>
    -- in the AWS Audit Manager User Guide.
    queryStatement :: Prelude.Maybe Prelude.Text,
    -- | The name of the new assessment report.
    name :: Prelude.Text,
    -- | The identifier for the assessment.
    assessmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssessmentReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createAssessmentReport_description' - The description of the assessment report.
--
-- 'queryStatement', 'createAssessmentReport_queryStatement' - A SQL statement that represents an evidence finder query.
--
-- Provide this parameter when you want to generate an assessment report
-- from the results of an evidence finder search query. When you use this
-- parameter, Audit Manager generates a one-time report using only the
-- evidence from the query output. This report does not include any
-- assessment evidence that was manually
-- <https://docs.aws.amazon.com/userguide/generate-assessment-report.html#generate-assessment-report-include-evidence added to a report using the console>,
-- or
-- <https://docs.aws.amazon.com/APIReference-evidenceFinder/API_BatchAssociateAssessmentReportEvidence.html associated with a report using the API>.
--
-- To use this parameter, the
-- <https://docs.aws.amazon.com/APIReference-evidenceFinder/API_EvidenceFinderSetup.html#auditmanager-Type-EvidenceFinderSetup-enablementStatus enablementStatus>
-- of evidence finder must be @ENABLED@.
--
-- For examples and help resolving @queryStatement@ validation exceptions,
-- see
-- <https://docs.aws.amazon.com/audit-manager/latest/userguide/evidence-finder-issues.html#querystatement-exceptions Troubleshooting evidence finder issues>
-- in the AWS Audit Manager User Guide.
--
-- 'name', 'createAssessmentReport_name' - The name of the new assessment report.
--
-- 'assessmentId', 'createAssessmentReport_assessmentId' - The identifier for the assessment.
newCreateAssessmentReport ::
  -- | 'name'
  Prelude.Text ->
  -- | 'assessmentId'
  Prelude.Text ->
  CreateAssessmentReport
newCreateAssessmentReport pName_ pAssessmentId_ =
  CreateAssessmentReport'
    { description =
        Prelude.Nothing,
      queryStatement = Prelude.Nothing,
      name = pName_,
      assessmentId = pAssessmentId_
    }

-- | The description of the assessment report.
createAssessmentReport_description :: Lens.Lens' CreateAssessmentReport (Prelude.Maybe Prelude.Text)
createAssessmentReport_description = Lens.lens (\CreateAssessmentReport' {description} -> description) (\s@CreateAssessmentReport' {} a -> s {description = a} :: CreateAssessmentReport)

-- | A SQL statement that represents an evidence finder query.
--
-- Provide this parameter when you want to generate an assessment report
-- from the results of an evidence finder search query. When you use this
-- parameter, Audit Manager generates a one-time report using only the
-- evidence from the query output. This report does not include any
-- assessment evidence that was manually
-- <https://docs.aws.amazon.com/userguide/generate-assessment-report.html#generate-assessment-report-include-evidence added to a report using the console>,
-- or
-- <https://docs.aws.amazon.com/APIReference-evidenceFinder/API_BatchAssociateAssessmentReportEvidence.html associated with a report using the API>.
--
-- To use this parameter, the
-- <https://docs.aws.amazon.com/APIReference-evidenceFinder/API_EvidenceFinderSetup.html#auditmanager-Type-EvidenceFinderSetup-enablementStatus enablementStatus>
-- of evidence finder must be @ENABLED@.
--
-- For examples and help resolving @queryStatement@ validation exceptions,
-- see
-- <https://docs.aws.amazon.com/audit-manager/latest/userguide/evidence-finder-issues.html#querystatement-exceptions Troubleshooting evidence finder issues>
-- in the AWS Audit Manager User Guide.
createAssessmentReport_queryStatement :: Lens.Lens' CreateAssessmentReport (Prelude.Maybe Prelude.Text)
createAssessmentReport_queryStatement = Lens.lens (\CreateAssessmentReport' {queryStatement} -> queryStatement) (\s@CreateAssessmentReport' {} a -> s {queryStatement = a} :: CreateAssessmentReport)

-- | The name of the new assessment report.
createAssessmentReport_name :: Lens.Lens' CreateAssessmentReport Prelude.Text
createAssessmentReport_name = Lens.lens (\CreateAssessmentReport' {name} -> name) (\s@CreateAssessmentReport' {} a -> s {name = a} :: CreateAssessmentReport)

-- | The identifier for the assessment.
createAssessmentReport_assessmentId :: Lens.Lens' CreateAssessmentReport Prelude.Text
createAssessmentReport_assessmentId = Lens.lens (\CreateAssessmentReport' {assessmentId} -> assessmentId) (\s@CreateAssessmentReport' {} a -> s {assessmentId = a} :: CreateAssessmentReport)

instance Core.AWSRequest CreateAssessmentReport where
  type
    AWSResponse CreateAssessmentReport =
      CreateAssessmentReportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssessmentReportResponse'
            Prelude.<$> (x Core..?> "assessmentReport")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAssessmentReport where
  hashWithSalt _salt CreateAssessmentReport' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` queryStatement
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` assessmentId

instance Prelude.NFData CreateAssessmentReport where
  rnf CreateAssessmentReport' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf queryStatement
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf assessmentId

instance Core.ToHeaders CreateAssessmentReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateAssessmentReport where
  toJSON CreateAssessmentReport' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("description" Core..=) Prelude.<$> description,
            ("queryStatement" Core..=)
              Prelude.<$> queryStatement,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateAssessmentReport where
  toPath CreateAssessmentReport' {..} =
    Prelude.mconcat
      ["/assessments/", Core.toBS assessmentId, "/reports"]

instance Core.ToQuery CreateAssessmentReport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAssessmentReportResponse' smart constructor.
data CreateAssessmentReportResponse = CreateAssessmentReportResponse'
  { -- | The new assessment report that the @CreateAssessmentReport@ API
    -- returned.
    assessmentReport :: Prelude.Maybe AssessmentReport,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssessmentReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentReport', 'createAssessmentReportResponse_assessmentReport' - The new assessment report that the @CreateAssessmentReport@ API
-- returned.
--
-- 'httpStatus', 'createAssessmentReportResponse_httpStatus' - The response's http status code.
newCreateAssessmentReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAssessmentReportResponse
newCreateAssessmentReportResponse pHttpStatus_ =
  CreateAssessmentReportResponse'
    { assessmentReport =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new assessment report that the @CreateAssessmentReport@ API
-- returned.
createAssessmentReportResponse_assessmentReport :: Lens.Lens' CreateAssessmentReportResponse (Prelude.Maybe AssessmentReport)
createAssessmentReportResponse_assessmentReport = Lens.lens (\CreateAssessmentReportResponse' {assessmentReport} -> assessmentReport) (\s@CreateAssessmentReportResponse' {} a -> s {assessmentReport = a} :: CreateAssessmentReportResponse)

-- | The response's http status code.
createAssessmentReportResponse_httpStatus :: Lens.Lens' CreateAssessmentReportResponse Prelude.Int
createAssessmentReportResponse_httpStatus = Lens.lens (\CreateAssessmentReportResponse' {httpStatus} -> httpStatus) (\s@CreateAssessmentReportResponse' {} a -> s {httpStatus = a} :: CreateAssessmentReportResponse)

instance
  Prelude.NFData
    CreateAssessmentReportResponse
  where
  rnf CreateAssessmentReportResponse' {..} =
    Prelude.rnf assessmentReport
      `Prelude.seq` Prelude.rnf httpStatus
