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
-- Module      : Amazonka.Inspector.GetAssessmentReport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces an assessment report that includes detailed and comprehensive
-- results of a specified assessment run.
module Amazonka.Inspector.GetAssessmentReport
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssessmentReportResponse'
            Prelude.<$> (x Data..?> "url")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable GetAssessmentReport where
  hashWithSalt _salt GetAssessmentReport' {..} =
    _salt `Prelude.hashWithSalt` assessmentRunArn
      `Prelude.hashWithSalt` reportFileFormat
      `Prelude.hashWithSalt` reportType

instance Prelude.NFData GetAssessmentReport where
  rnf GetAssessmentReport' {..} =
    Prelude.rnf assessmentRunArn
      `Prelude.seq` Prelude.rnf reportFileFormat
      `Prelude.seq` Prelude.rnf reportType

instance Data.ToHeaders GetAssessmentReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.GetAssessmentReport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAssessmentReport where
  toJSON GetAssessmentReport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("assessmentRunArn" Data..= assessmentRunArn),
            Prelude.Just
              ("reportFileFormat" Data..= reportFileFormat),
            Prelude.Just ("reportType" Data..= reportType)
          ]
      )

instance Data.ToPath GetAssessmentReport where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAssessmentReport where
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

instance Prelude.NFData GetAssessmentReportResponse where
  rnf GetAssessmentReportResponse' {..} =
    Prelude.rnf url
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf status
