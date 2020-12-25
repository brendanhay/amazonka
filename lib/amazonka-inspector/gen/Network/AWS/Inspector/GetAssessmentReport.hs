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
    garAssessmentRunArn,
    garReportFileFormat,
    garReportType,

    -- * Destructuring the response
    GetAssessmentReportResponse (..),
    mkGetAssessmentReportResponse,

    -- ** Response lenses
    garrrsStatus,
    garrrsUrl,
    garrrsResponseStatus,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAssessmentReport' smart constructor.
data GetAssessmentReport = GetAssessmentReport'
  { -- | The ARN that specifies the assessment run for which you want to generate a report.
    assessmentRunArn :: Types.Arn,
    -- | Specifies the file format (html or pdf) of the assessment report that you want to generate.
    reportFileFormat :: Types.ReportFileFormat,
    -- | Specifies the type of the assessment report that you want to generate. There are two types of assessment reports: a finding report and a full report. For more information, see <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_reports.html Assessment Reports> .
    reportType :: Types.ReportType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAssessmentReport' value with any optional fields omitted.
mkGetAssessmentReport ::
  -- | 'assessmentRunArn'
  Types.Arn ->
  -- | 'reportFileFormat'
  Types.ReportFileFormat ->
  -- | 'reportType'
  Types.ReportType ->
  GetAssessmentReport
mkGetAssessmentReport assessmentRunArn reportFileFormat reportType =
  GetAssessmentReport'
    { assessmentRunArn,
      reportFileFormat,
      reportType
    }

-- | The ARN that specifies the assessment run for which you want to generate a report.
--
-- /Note:/ Consider using 'assessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garAssessmentRunArn :: Lens.Lens' GetAssessmentReport Types.Arn
garAssessmentRunArn = Lens.field @"assessmentRunArn"
{-# DEPRECATED garAssessmentRunArn "Use generic-lens or generic-optics with 'assessmentRunArn' instead." #-}

-- | Specifies the file format (html or pdf) of the assessment report that you want to generate.
--
-- /Note:/ Consider using 'reportFileFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garReportFileFormat :: Lens.Lens' GetAssessmentReport Types.ReportFileFormat
garReportFileFormat = Lens.field @"reportFileFormat"
{-# DEPRECATED garReportFileFormat "Use generic-lens or generic-optics with 'reportFileFormat' instead." #-}

-- | Specifies the type of the assessment report that you want to generate. There are two types of assessment reports: a finding report and a full report. For more information, see <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_reports.html Assessment Reports> .
--
-- /Note:/ Consider using 'reportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garReportType :: Lens.Lens' GetAssessmentReport Types.ReportType
garReportType = Lens.field @"reportType"
{-# DEPRECATED garReportType "Use generic-lens or generic-optics with 'reportType' instead." #-}

instance Core.FromJSON GetAssessmentReport where
  toJSON GetAssessmentReport {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("assessmentRunArn" Core..= assessmentRunArn),
            Core.Just ("reportFileFormat" Core..= reportFileFormat),
            Core.Just ("reportType" Core..= reportType)
          ]
      )

instance Core.AWSRequest GetAssessmentReport where
  type Rs GetAssessmentReport = GetAssessmentReportResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "InspectorService.GetAssessmentReport")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssessmentReportResponse'
            Core.<$> (x Core..: "status")
            Core.<*> (x Core..:? "url")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAssessmentReportResponse' smart constructor.
data GetAssessmentReportResponse = GetAssessmentReportResponse'
  { -- | Specifies the status of the request to generate an assessment report.
    status :: Types.ReportStatus,
    -- | Specifies the URL where you can find the generated assessment report. This parameter is only returned if the report is successfully generated.
    url :: Core.Maybe Types.Url,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAssessmentReportResponse' value with any optional fields omitted.
mkGetAssessmentReportResponse ::
  -- | 'status'
  Types.ReportStatus ->
  -- | 'responseStatus'
  Core.Int ->
  GetAssessmentReportResponse
mkGetAssessmentReportResponse status responseStatus =
  GetAssessmentReportResponse'
    { status,
      url = Core.Nothing,
      responseStatus
    }

-- | Specifies the status of the request to generate an assessment report.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrrsStatus :: Lens.Lens' GetAssessmentReportResponse Types.ReportStatus
garrrsStatus = Lens.field @"status"
{-# DEPRECATED garrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the URL where you can find the generated assessment report. This parameter is only returned if the report is successfully generated.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrrsUrl :: Lens.Lens' GetAssessmentReportResponse (Core.Maybe Types.Url)
garrrsUrl = Lens.field @"url"
{-# DEPRECATED garrrsUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrrsResponseStatus :: Lens.Lens' GetAssessmentReportResponse Core.Int
garrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED garrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
