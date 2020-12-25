{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetCredentialReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a credential report for the AWS account. For more information about the credential report, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting Credential Reports> in the /IAM User Guide/ .
module Network.AWS.IAM.GetCredentialReport
  ( -- * Creating a request
    GetCredentialReport (..),
    mkGetCredentialReport,

    -- * Destructuring the response
    GetCredentialReportResponse (..),
    mkGetCredentialReportResponse,

    -- ** Response lenses
    grsContent,
    grsGeneratedTime,
    grsReportFormat,
    grsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCredentialReport' smart constructor.
data GetCredentialReport = GetCredentialReport'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCredentialReport' value with any optional fields omitted.
mkGetCredentialReport ::
  GetCredentialReport
mkGetCredentialReport = GetCredentialReport'

instance Core.AWSRequest GetCredentialReport where
  type Rs GetCredentialReport = GetCredentialReportResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetCredentialReport")
                Core.<> (Core.pure ("Version", "2010-05-08"))
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetCredentialReportResult"
      ( \s h x ->
          GetCredentialReportResponse'
            Core.<$> (x Core..@? "Content")
            Core.<*> (x Core..@? "GeneratedTime")
            Core.<*> (x Core..@? "ReportFormat")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'GetCredentialReport' request.
--
-- /See:/ 'mkGetCredentialReportResponse' smart constructor.
data GetCredentialReportResponse = GetCredentialReportResponse'
  { -- | Contains the credential report. The report is Base64-encoded.
    content :: Core.Maybe Core.Base64,
    -- | The date and time when the credential report was created, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> .
    generatedTime :: Core.Maybe Core.UTCTime,
    -- | The format (MIME type) of the credential report.
    reportFormat :: Core.Maybe Types.ReportFormatType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetCredentialReportResponse' value with any optional fields omitted.
mkGetCredentialReportResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCredentialReportResponse
mkGetCredentialReportResponse responseStatus =
  GetCredentialReportResponse'
    { content = Core.Nothing,
      generatedTime = Core.Nothing,
      reportFormat = Core.Nothing,
      responseStatus
    }

-- | Contains the credential report. The report is Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsContent :: Lens.Lens' GetCredentialReportResponse (Core.Maybe Core.Base64)
grsContent = Lens.field @"content"
{-# DEPRECATED grsContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The date and time when the credential report was created, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> .
--
-- /Note:/ Consider using 'generatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsGeneratedTime :: Lens.Lens' GetCredentialReportResponse (Core.Maybe Core.UTCTime)
grsGeneratedTime = Lens.field @"generatedTime"
{-# DEPRECATED grsGeneratedTime "Use generic-lens or generic-optics with 'generatedTime' instead." #-}

-- | The format (MIME type) of the credential report.
--
-- /Note:/ Consider using 'reportFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsReportFormat :: Lens.Lens' GetCredentialReportResponse (Core.Maybe Types.ReportFormatType)
grsReportFormat = Lens.field @"reportFormat"
{-# DEPRECATED grsReportFormat "Use generic-lens or generic-optics with 'reportFormat' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetCredentialReportResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
