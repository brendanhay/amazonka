{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about a specific audit report created by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport> action. Audit information is created every time the certificate authority (CA) private key is used. The private key is used when you call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_IssueCertificate.html IssueCertificate> action or the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_RevokeCertificate.html RevokeCertificate> action.
module Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
  ( -- * Creating a request
    DescribeCertificateAuthorityAuditReport (..),
    mkDescribeCertificateAuthorityAuditReport,

    -- ** Request lenses
    dcaarCertificateAuthorityArn,
    dcaarAuditReportId,

    -- * Destructuring the response
    DescribeCertificateAuthorityAuditReportResponse (..),
    mkDescribeCertificateAuthorityAuditReportResponse,

    -- ** Response lenses
    dcaarrrsAuditReportStatus,
    dcaarrrsCreatedAt,
    dcaarrrsS3BucketName,
    dcaarrrsS3Key,
    dcaarrrsResponseStatus,
  )
where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCertificateAuthorityAuditReport' smart constructor.
data DescribeCertificateAuthorityAuditReport = DescribeCertificateAuthorityAuditReport'
  { -- | The Amazon Resource Name (ARN) of the private CA. This must be of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
    certificateAuthorityArn :: Types.Arn,
    -- | The report ID returned by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport> action.
    auditReportId :: Types.AuditReportId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCertificateAuthorityAuditReport' value with any optional fields omitted.
mkDescribeCertificateAuthorityAuditReport ::
  -- | 'certificateAuthorityArn'
  Types.Arn ->
  -- | 'auditReportId'
  Types.AuditReportId ->
  DescribeCertificateAuthorityAuditReport
mkDescribeCertificateAuthorityAuditReport
  certificateAuthorityArn
  auditReportId =
    DescribeCertificateAuthorityAuditReport'
      { certificateAuthorityArn,
        auditReportId
      }

-- | The Amazon Resource Name (ARN) of the private CA. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarCertificateAuthorityArn :: Lens.Lens' DescribeCertificateAuthorityAuditReport Types.Arn
dcaarCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# DEPRECATED dcaarCertificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead." #-}

-- | The report ID returned by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport> action.
--
-- /Note:/ Consider using 'auditReportId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarAuditReportId :: Lens.Lens' DescribeCertificateAuthorityAuditReport Types.AuditReportId
dcaarAuditReportId = Lens.field @"auditReportId"
{-# DEPRECATED dcaarAuditReportId "Use generic-lens or generic-optics with 'auditReportId' instead." #-}

instance Core.FromJSON DescribeCertificateAuthorityAuditReport where
  toJSON DescribeCertificateAuthorityAuditReport {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CertificateAuthorityArn" Core..= certificateAuthorityArn),
            Core.Just ("AuditReportId" Core..= auditReportId)
          ]
      )

instance Core.AWSRequest DescribeCertificateAuthorityAuditReport where
  type
    Rs DescribeCertificateAuthorityAuditReport =
      DescribeCertificateAuthorityAuditReportResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "ACMPrivateCA.DescribeCertificateAuthorityAuditReport"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCertificateAuthorityAuditReportResponse'
            Core.<$> (x Core..:? "AuditReportStatus")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "S3BucketName")
            Core.<*> (x Core..:? "S3Key")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeCertificateAuthorityAuditReportResponse' smart constructor.
data DescribeCertificateAuthorityAuditReportResponse = DescribeCertificateAuthorityAuditReportResponse'
  { -- | Specifies whether report creation is in progress, has succeeded, or has failed.
    auditReportStatus :: Core.Maybe Types.AuditReportStatus,
    -- | The date and time at which the report was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | Name of the S3 bucket that contains the report.
    s3BucketName :: Core.Maybe Types.S3BucketName,
    -- | S3 __key__ that uniquely identifies the report file in your S3 bucket.
    s3Key :: Core.Maybe Types.S3Key,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeCertificateAuthorityAuditReportResponse' value with any optional fields omitted.
mkDescribeCertificateAuthorityAuditReportResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCertificateAuthorityAuditReportResponse
mkDescribeCertificateAuthorityAuditReportResponse responseStatus =
  DescribeCertificateAuthorityAuditReportResponse'
    { auditReportStatus =
        Core.Nothing,
      createdAt = Core.Nothing,
      s3BucketName = Core.Nothing,
      s3Key = Core.Nothing,
      responseStatus
    }

-- | Specifies whether report creation is in progress, has succeeded, or has failed.
--
-- /Note:/ Consider using 'auditReportStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrrsAuditReportStatus :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse (Core.Maybe Types.AuditReportStatus)
dcaarrrsAuditReportStatus = Lens.field @"auditReportStatus"
{-# DEPRECATED dcaarrrsAuditReportStatus "Use generic-lens or generic-optics with 'auditReportStatus' instead." #-}

-- | The date and time at which the report was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrrsCreatedAt :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse (Core.Maybe Core.NominalDiffTime)
dcaarrrsCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED dcaarrrsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Name of the S3 bucket that contains the report.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrrsS3BucketName :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse (Core.Maybe Types.S3BucketName)
dcaarrrsS3BucketName = Lens.field @"s3BucketName"
{-# DEPRECATED dcaarrrsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | S3 __key__ that uniquely identifies the report file in your S3 bucket.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrrsS3Key :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse (Core.Maybe Types.S3Key)
dcaarrrsS3Key = Lens.field @"s3Key"
{-# DEPRECATED dcaarrrsS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrrsResponseStatus :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse Core.Int
dcaarrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcaarrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
