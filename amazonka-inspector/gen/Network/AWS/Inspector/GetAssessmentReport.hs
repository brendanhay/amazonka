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
-- Module      : Network.AWS.Inspector.GetAssessmentReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces an assessment report that includes detailed and comprehensive
-- results of a specified assessment run.
module Network.AWS.Inspector.GetAssessmentReport
  ( -- * Creating a Request
    GetAssessmentReport (..),
    newGetAssessmentReport,

    -- * Request Lenses
    getAssessmentReport_assessmentRunArn,
    getAssessmentReport_reportFileFormat,
    getAssessmentReport_reportType,

    -- * Destructuring the Response
    GetAssessmentReportResponse (..),
    newGetAssessmentReportResponse,

    -- * Response Lenses
    getAssessmentReportResponse_url,
    getAssessmentReportResponse_httpStatus,
    getAssessmentReportResponse_status,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAssessmentReport' smart constructor.
data GetAssessmentReport = GetAssessmentReport'
  { -- | The ARN that specifies the assessment run for which you want to generate
    -- a report.
    assessmentRunArn :: Prelude.Text,
    -- | Specifies the file format (html or pdf) of the assessment report that
    -- you want to generate.
    reportFileFormat :: ReportFileFormat,
    -- | Specifies the type of the assessment report that you want to generate.
    -- There are two types of assessment reports: a finding report and a full
    -- report. For more information, see
    -- <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_reports.html Assessment Reports>.
    reportType :: ReportType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssessmentReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentRunArn', 'getAssessmentReport_assessmentRunArn' - The ARN that specifies the assessment run for which you want to generate
-- a report.
--
-- 'reportFileFormat', 'getAssessmentReport_reportFileFormat' - Specifies the file format (html or pdf) of the assessment report that
-- you want to generate.
--
-- 'reportType', 'getAssessmentReport_reportType' - Specifies the type of the assessment report that you want to generate.
-- There are two types of assessment reports: a finding report and a full
-- report. For more information, see
-- <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_reports.html Assessment Reports>.
newGetAssessmentReport ::
  -- | 'assessmentRunArn'
  Prelude.Text ->
  -- | 'reportFileFormat'
  ReportFileFormat ->
  -- | 'reportType'
  ReportType ->
  GetAssessmentReport
newGetAssessmentReport
  pAssessmentRunArn_
  pReportFileFormat_
  pReportType_ =
    GetAssessmentReport'
      { assessmentRunArn =
          pAssessmentRunArn_,
        reportFileFormat = pReportFileFormat_,
        reportType = pReportType_
      }

-- | The ARN that specifies the assessment run for which you want to generate
-- a report.
getAssessmentReport_assessmentRunArn :: Lens.Lens' GetAssessmentReport Prelude.Text
getAssessmentReport_assessmentRunArn = Lens.lens (\GetAssessmentReport' {assessmentRunArn} -> assessmentRunArn) (\s@GetAssessmentReport' {} a -> s {assessmentRunArn = a} :: GetAssessmentReport)

-- | Specifies the file format (html or pdf) of the assessment report that
-- you want to generate.
getAssessmentReport_reportFileFormat :: Lens.Lens' GetAssessmentReport ReportFileFormat
getAssessmentReport_reportFileFormat = Lens.lens (\GetAssessmentReport' {reportFileFormat} -> reportFileFormat) (\s@GetAssessmentReport' {} a -> s {reportFileFormat = a} :: GetAssessmentReport)

-- | Specifies the type of the assessment report that you want to generate.
-- There are two types of assessment reports: a finding report and a full
-- report. For more information, see
-- <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_reports.html Assessment Reports>.
getAssessmentReport_reportType :: Lens.Lens' GetAssessmentReport ReportType
getAssessmentReport_reportType = Lens.lens (\GetAssessmentReport' {reportType} -> reportType) (\s@GetAssessmentReport' {} a -> s {reportType = a} :: GetAssessmentReport)

instance Core.AWSRequest GetAssessmentReport where
  type
    AWSResponse GetAssessmentReport =
      GetAssessmentReportResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssessmentReportResponse'
            Prelude.<$> (x Core..?> "url")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "status")
      )

instance Prelude.Hashable GetAssessmentReport

instance Prelude.NFData GetAssessmentReport

instance Core.ToHeaders GetAssessmentReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.GetAssessmentReport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetAssessmentReport where
  toJSON GetAssessmentReport' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("assessmentRunArn" Core..= assessmentRunArn),
            Prelude.Just
              ("reportFileFormat" Core..= reportFileFormat),
            Prelude.Just ("reportType" Core..= reportType)
          ]
      )

instance Core.ToPath GetAssessmentReport where
  toPath = Prelude.const "/"

instance Core.ToQuery GetAssessmentReport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAssessmentReportResponse' smart constructor.
data GetAssessmentReportResponse = GetAssessmentReportResponse'
  { -- | Specifies the URL where you can find the generated assessment report.
    -- This parameter is only returned if the report is successfully generated.
    url :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Specifies the status of the request to generate an assessment report.
    status :: ReportStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssessmentReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'getAssessmentReportResponse_url' - Specifies the URL where you can find the generated assessment report.
-- This parameter is only returned if the report is successfully generated.
--
-- 'httpStatus', 'getAssessmentReportResponse_httpStatus' - The response's http status code.
--
-- 'status', 'getAssessmentReportResponse_status' - Specifies the status of the request to generate an assessment report.
newGetAssessmentReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  ReportStatus ->
  GetAssessmentReportResponse
newGetAssessmentReportResponse pHttpStatus_ pStatus_ =
  GetAssessmentReportResponse'
    { url = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      status = pStatus_
    }

-- | Specifies the URL where you can find the generated assessment report.
-- This parameter is only returned if the report is successfully generated.
getAssessmentReportResponse_url :: Lens.Lens' GetAssessmentReportResponse (Prelude.Maybe Prelude.Text)
getAssessmentReportResponse_url = Lens.lens (\GetAssessmentReportResponse' {url} -> url) (\s@GetAssessmentReportResponse' {} a -> s {url = a} :: GetAssessmentReportResponse)

-- | The response's http status code.
getAssessmentReportResponse_httpStatus :: Lens.Lens' GetAssessmentReportResponse Prelude.Int
getAssessmentReportResponse_httpStatus = Lens.lens (\GetAssessmentReportResponse' {httpStatus} -> httpStatus) (\s@GetAssessmentReportResponse' {} a -> s {httpStatus = a} :: GetAssessmentReportResponse)

-- | Specifies the status of the request to generate an assessment report.
getAssessmentReportResponse_status :: Lens.Lens' GetAssessmentReportResponse ReportStatus
getAssessmentReportResponse_status = Lens.lens (\GetAssessmentReportResponse' {status} -> status) (\s@GetAssessmentReportResponse' {} a -> s {status = a} :: GetAssessmentReportResponse)

instance Prelude.NFData GetAssessmentReportResponse
