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
-- Module      : Network.AWS.AuditManager.CreateAssessmentReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an assessment report for the specified assessment.
module Network.AWS.AuditManager.CreateAssessmentReport
  ( -- * Creating a Request
    CreateAssessmentReport (..),
    newCreateAssessmentReport,

    -- * Request Lenses
    createAssessmentReport_description,
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

import Network.AWS.AuditManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAssessmentReport' smart constructor.
data CreateAssessmentReport = CreateAssessmentReport'
  { -- | The description of the assessment report.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the new assessment report.
    name :: Prelude.Text,
    -- | The identifier for the specified assessment.
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
-- 'name', 'createAssessmentReport_name' - The name of the new assessment report.
--
-- 'assessmentId', 'createAssessmentReport_assessmentId' - The identifier for the specified assessment.
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
      name = pName_,
      assessmentId = pAssessmentId_
    }

-- | The description of the assessment report.
createAssessmentReport_description :: Lens.Lens' CreateAssessmentReport (Prelude.Maybe Prelude.Text)
createAssessmentReport_description = Lens.lens (\CreateAssessmentReport' {description} -> description) (\s@CreateAssessmentReport' {} a -> s {description = a} :: CreateAssessmentReport)

-- | The name of the new assessment report.
createAssessmentReport_name :: Lens.Lens' CreateAssessmentReport Prelude.Text
createAssessmentReport_name = Lens.lens (\CreateAssessmentReport' {name} -> name) (\s@CreateAssessmentReport' {} a -> s {name = a} :: CreateAssessmentReport)

-- | The identifier for the specified assessment.
createAssessmentReport_assessmentId :: Lens.Lens' CreateAssessmentReport Prelude.Text
createAssessmentReport_assessmentId = Lens.lens (\CreateAssessmentReport' {assessmentId} -> assessmentId) (\s@CreateAssessmentReport' {} a -> s {assessmentId = a} :: CreateAssessmentReport)

instance Core.AWSRequest CreateAssessmentReport where
  type
    AWSResponse CreateAssessmentReport =
      CreateAssessmentReportResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssessmentReportResponse'
            Prelude.<$> (x Core..?> "assessmentReport")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAssessmentReport

instance Prelude.NFData CreateAssessmentReport

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
  { -- | The new assessment report returned by the @CreateAssessmentReport@ API.
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
-- 'assessmentReport', 'createAssessmentReportResponse_assessmentReport' - The new assessment report returned by the @CreateAssessmentReport@ API.
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

-- | The new assessment report returned by the @CreateAssessmentReport@ API.
createAssessmentReportResponse_assessmentReport :: Lens.Lens' CreateAssessmentReportResponse (Prelude.Maybe AssessmentReport)
createAssessmentReportResponse_assessmentReport = Lens.lens (\CreateAssessmentReportResponse' {assessmentReport} -> assessmentReport) (\s@CreateAssessmentReportResponse' {} a -> s {assessmentReport = a} :: CreateAssessmentReportResponse)

-- | The response's http status code.
createAssessmentReportResponse_httpStatus :: Lens.Lens' CreateAssessmentReportResponse Prelude.Int
createAssessmentReportResponse_httpStatus = Lens.lens (\CreateAssessmentReportResponse' {httpStatus} -> httpStatus) (\s@CreateAssessmentReportResponse' {} a -> s {httpStatus = a} :: CreateAssessmentReportResponse)

instance
  Prelude.NFData
    CreateAssessmentReportResponse
