{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCredentialReport' smart constructor.
data GetCredentialReport = GetCredentialReport'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCredentialReport' with the minimum fields required to make a request.
mkGetCredentialReport ::
  GetCredentialReport
mkGetCredentialReport = GetCredentialReport'

instance Lude.AWSRequest GetCredentialReport where
  type Rs GetCredentialReport = GetCredentialReportResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetCredentialReportResult"
      ( \s h x ->
          GetCredentialReportResponse'
            Lude.<$> (x Lude..@? "Content")
            Lude.<*> (x Lude..@? "GeneratedTime")
            Lude.<*> (x Lude..@? "ReportFormat")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCredentialReport where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetCredentialReport where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCredentialReport where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("GetCredentialReport" :: Lude.ByteString),
            "Version" Lude.=: ("2010-05-08" :: Lude.ByteString)
          ]
      )

-- | Contains the response to a successful 'GetCredentialReport' request.
--
-- /See:/ 'mkGetCredentialReportResponse' smart constructor.
data GetCredentialReportResponse = GetCredentialReportResponse'
  { content ::
      Lude.Maybe Lude.Base64,
    generatedTime ::
      Lude.Maybe Lude.ISO8601,
    reportFormat ::
      Lude.Maybe ReportFormatType,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCredentialReportResponse' with the minimum fields required to make a request.
--
-- * 'content' - Contains the credential report. The report is Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'generatedTime' - The date and time when the credential report was created, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> .
-- * 'reportFormat' - The format (MIME type) of the credential report.
-- * 'responseStatus' - The response status code.
mkGetCredentialReportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCredentialReportResponse
mkGetCredentialReportResponse pResponseStatus_ =
  GetCredentialReportResponse'
    { content = Lude.Nothing,
      generatedTime = Lude.Nothing,
      reportFormat = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the credential report. The report is Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsContent :: Lens.Lens' GetCredentialReportResponse (Lude.Maybe Lude.Base64)
grsContent = Lens.lens (content :: GetCredentialReportResponse -> Lude.Maybe Lude.Base64) (\s a -> s {content = a} :: GetCredentialReportResponse)
{-# DEPRECATED grsContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The date and time when the credential report was created, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> .
--
-- /Note:/ Consider using 'generatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsGeneratedTime :: Lens.Lens' GetCredentialReportResponse (Lude.Maybe Lude.ISO8601)
grsGeneratedTime = Lens.lens (generatedTime :: GetCredentialReportResponse -> Lude.Maybe Lude.ISO8601) (\s a -> s {generatedTime = a} :: GetCredentialReportResponse)
{-# DEPRECATED grsGeneratedTime "Use generic-lens or generic-optics with 'generatedTime' instead." #-}

-- | The format (MIME type) of the credential report.
--
-- /Note:/ Consider using 'reportFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsReportFormat :: Lens.Lens' GetCredentialReportResponse (Lude.Maybe ReportFormatType)
grsReportFormat = Lens.lens (reportFormat :: GetCredentialReportResponse -> Lude.Maybe ReportFormatType) (\s a -> s {reportFormat = a} :: GetCredentialReportResponse)
{-# DEPRECATED grsReportFormat "Use generic-lens or generic-optics with 'reportFormat' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetCredentialReportResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetCredentialReportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCredentialReportResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
