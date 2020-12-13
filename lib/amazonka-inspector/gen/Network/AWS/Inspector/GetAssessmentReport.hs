{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.GetAssessmentReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces an assessment report that includes detailed and comprehensive results of a specified assessment run.
module Network.AWS.Inspector.GetAssessmentReport
  ( -- * Creating a request
    GetAssessmentReport (..),
    mkGetAssessmentReport,

    -- ** Request lenses
    garAssessmentRunARN,
    garReportFileFormat,
    garReportType,

    -- * Destructuring the response
    GetAssessmentReportResponse (..),
    mkGetAssessmentReportResponse,

    -- ** Response lenses
    garrsStatus,
    garrsUrl,
    garrsResponseStatus,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAssessmentReport' smart constructor.
data GetAssessmentReport = GetAssessmentReport'
  { -- | The ARN that specifies the assessment run for which you want to generate a report.
    assessmentRunARN :: Lude.Text,
    -- | Specifies the file format (html or pdf) of the assessment report that you want to generate.
    reportFileFormat :: ReportFileFormat,
    -- | Specifies the type of the assessment report that you want to generate. There are two types of assessment reports: a finding report and a full report. For more information, see <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_reports.html Assessment Reports> .
    reportType :: ReportType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAssessmentReport' with the minimum fields required to make a request.
--
-- * 'assessmentRunARN' - The ARN that specifies the assessment run for which you want to generate a report.
-- * 'reportFileFormat' - Specifies the file format (html or pdf) of the assessment report that you want to generate.
-- * 'reportType' - Specifies the type of the assessment report that you want to generate. There are two types of assessment reports: a finding report and a full report. For more information, see <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_reports.html Assessment Reports> .
mkGetAssessmentReport ::
  -- | 'assessmentRunARN'
  Lude.Text ->
  -- | 'reportFileFormat'
  ReportFileFormat ->
  -- | 'reportType'
  ReportType ->
  GetAssessmentReport
mkGetAssessmentReport
  pAssessmentRunARN_
  pReportFileFormat_
  pReportType_ =
    GetAssessmentReport'
      { assessmentRunARN = pAssessmentRunARN_,
        reportFileFormat = pReportFileFormat_,
        reportType = pReportType_
      }

-- | The ARN that specifies the assessment run for which you want to generate a report.
--
-- /Note:/ Consider using 'assessmentRunARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garAssessmentRunARN :: Lens.Lens' GetAssessmentReport Lude.Text
garAssessmentRunARN = Lens.lens (assessmentRunARN :: GetAssessmentReport -> Lude.Text) (\s a -> s {assessmentRunARN = a} :: GetAssessmentReport)
{-# DEPRECATED garAssessmentRunARN "Use generic-lens or generic-optics with 'assessmentRunARN' instead." #-}

-- | Specifies the file format (html or pdf) of the assessment report that you want to generate.
--
-- /Note:/ Consider using 'reportFileFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garReportFileFormat :: Lens.Lens' GetAssessmentReport ReportFileFormat
garReportFileFormat = Lens.lens (reportFileFormat :: GetAssessmentReport -> ReportFileFormat) (\s a -> s {reportFileFormat = a} :: GetAssessmentReport)
{-# DEPRECATED garReportFileFormat "Use generic-lens or generic-optics with 'reportFileFormat' instead." #-}

-- | Specifies the type of the assessment report that you want to generate. There are two types of assessment reports: a finding report and a full report. For more information, see <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_reports.html Assessment Reports> .
--
-- /Note:/ Consider using 'reportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garReportType :: Lens.Lens' GetAssessmentReport ReportType
garReportType = Lens.lens (reportType :: GetAssessmentReport -> ReportType) (\s a -> s {reportType = a} :: GetAssessmentReport)
{-# DEPRECATED garReportType "Use generic-lens or generic-optics with 'reportType' instead." #-}

instance Lude.AWSRequest GetAssessmentReport where
  type Rs GetAssessmentReport = GetAssessmentReportResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAssessmentReportResponse'
            Lude.<$> (x Lude..:> "status")
            Lude.<*> (x Lude..?> "url")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAssessmentReport where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.GetAssessmentReport" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAssessmentReport where
  toJSON GetAssessmentReport' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("assessmentRunArn" Lude..= assessmentRunARN),
            Lude.Just ("reportFileFormat" Lude..= reportFileFormat),
            Lude.Just ("reportType" Lude..= reportType)
          ]
      )

instance Lude.ToPath GetAssessmentReport where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAssessmentReport where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAssessmentReportResponse' smart constructor.
data GetAssessmentReportResponse = GetAssessmentReportResponse'
  { -- | Specifies the status of the request to generate an assessment report.
    status :: ReportStatus,
    -- | Specifies the URL where you can find the generated assessment report. This parameter is only returned if the report is successfully generated.
    url :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAssessmentReportResponse' with the minimum fields required to make a request.
--
-- * 'status' - Specifies the status of the request to generate an assessment report.
-- * 'url' - Specifies the URL where you can find the generated assessment report. This parameter is only returned if the report is successfully generated.
-- * 'responseStatus' - The response status code.
mkGetAssessmentReportResponse ::
  -- | 'status'
  ReportStatus ->
  -- | 'responseStatus'
  Lude.Int ->
  GetAssessmentReportResponse
mkGetAssessmentReportResponse pStatus_ pResponseStatus_ =
  GetAssessmentReportResponse'
    { status = pStatus_,
      url = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Specifies the status of the request to generate an assessment report.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsStatus :: Lens.Lens' GetAssessmentReportResponse ReportStatus
garrsStatus = Lens.lens (status :: GetAssessmentReportResponse -> ReportStatus) (\s a -> s {status = a} :: GetAssessmentReportResponse)
{-# DEPRECATED garrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the URL where you can find the generated assessment report. This parameter is only returned if the report is successfully generated.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsUrl :: Lens.Lens' GetAssessmentReportResponse (Lude.Maybe Lude.Text)
garrsUrl = Lens.lens (url :: GetAssessmentReportResponse -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: GetAssessmentReportResponse)
{-# DEPRECATED garrsUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetAssessmentReportResponse Lude.Int
garrsResponseStatus = Lens.lens (responseStatus :: GetAssessmentReportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAssessmentReportResponse)
{-# DEPRECATED garrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
