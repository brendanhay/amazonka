{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.CreateCertificateAuthorityAuditReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an audit report that lists every time that your CA private key is used. The report is saved in the Amazon S3 bucket that you specify on input. The <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_IssueCertificate.html IssueCertificate> and <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_RevokeCertificate.html RevokeCertificate> actions use the private key. 
--
-- ACM Private CAA assets that are stored in Amazon S3 can be protected with encryption. For more information, see <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaAuditReport.html#audit-report-encryption Encrypting Your Audit Reports> .
module Network.AWS.CertificateManagerPCA.CreateCertificateAuthorityAuditReport
    (
    -- * Creating a request
      CreateCertificateAuthorityAuditReport (..)
    , mkCreateCertificateAuthorityAuditReport
    -- ** Request lenses
    , ccaarCertificateAuthorityArn
    , ccaarS3BucketName
    , ccaarAuditReportResponseFormat

    -- * Destructuring the response
    , CreateCertificateAuthorityAuditReportResponse (..)
    , mkCreateCertificateAuthorityAuditReportResponse
    -- ** Response lenses
    , ccaarrrsAuditReportId
    , ccaarrrsS3Key
    , ccaarrrsResponseStatus
    ) where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCertificateAuthorityAuditReport' smart constructor.
data CreateCertificateAuthorityAuditReport = CreateCertificateAuthorityAuditReport'
  { certificateAuthorityArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the CA to be audited. This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
  , s3BucketName :: Types.S3BucketName
    -- ^ The name of the S3 bucket that will contain the audit report.
  , auditReportResponseFormat :: Types.AuditReportResponseFormat
    -- ^ The format in which to create the report. This can be either __JSON__ or __CSV__ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCertificateAuthorityAuditReport' value with any optional fields omitted.
mkCreateCertificateAuthorityAuditReport
    :: Types.Arn -- ^ 'certificateAuthorityArn'
    -> Types.S3BucketName -- ^ 's3BucketName'
    -> Types.AuditReportResponseFormat -- ^ 'auditReportResponseFormat'
    -> CreateCertificateAuthorityAuditReport
mkCreateCertificateAuthorityAuditReport certificateAuthorityArn
  s3BucketName auditReportResponseFormat
  = CreateCertificateAuthorityAuditReport'{certificateAuthorityArn,
                                           s3BucketName, auditReportResponseFormat}

-- | The Amazon Resource Name (ARN) of the CA to be audited. This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaarCertificateAuthorityArn :: Lens.Lens' CreateCertificateAuthorityAuditReport Types.Arn
ccaarCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# INLINEABLE ccaarCertificateAuthorityArn #-}
{-# DEPRECATED certificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead"  #-}

-- | The name of the S3 bucket that will contain the audit report.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaarS3BucketName :: Lens.Lens' CreateCertificateAuthorityAuditReport Types.S3BucketName
ccaarS3BucketName = Lens.field @"s3BucketName"
{-# INLINEABLE ccaarS3BucketName #-}
{-# DEPRECATED s3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead"  #-}

-- | The format in which to create the report. This can be either __JSON__ or __CSV__ .
--
-- /Note:/ Consider using 'auditReportResponseFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaarAuditReportResponseFormat :: Lens.Lens' CreateCertificateAuthorityAuditReport Types.AuditReportResponseFormat
ccaarAuditReportResponseFormat = Lens.field @"auditReportResponseFormat"
{-# INLINEABLE ccaarAuditReportResponseFormat #-}
{-# DEPRECATED auditReportResponseFormat "Use generic-lens or generic-optics with 'auditReportResponseFormat' instead"  #-}

instance Core.ToQuery CreateCertificateAuthorityAuditReport where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCertificateAuthorityAuditReport where
        toHeaders CreateCertificateAuthorityAuditReport{..}
          = Core.pure
              ("X-Amz-Target",
               "ACMPrivateCA.CreateCertificateAuthorityAuditReport")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCertificateAuthorityAuditReport where
        toJSON CreateCertificateAuthorityAuditReport{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("CertificateAuthorityArn" Core..= certificateAuthorityArn),
                  Core.Just ("S3BucketName" Core..= s3BucketName),
                  Core.Just
                    ("AuditReportResponseFormat" Core..= auditReportResponseFormat)])

instance Core.AWSRequest CreateCertificateAuthorityAuditReport
         where
        type Rs CreateCertificateAuthorityAuditReport =
             CreateCertificateAuthorityAuditReportResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCertificateAuthorityAuditReportResponse' Core.<$>
                   (x Core..:? "AuditReportId") Core.<*> x Core..:? "S3Key" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCertificateAuthorityAuditReportResponse' smart constructor.
data CreateCertificateAuthorityAuditReportResponse = CreateCertificateAuthorityAuditReportResponse'
  { auditReportId :: Core.Maybe Types.AuditReportId
    -- ^ An alphanumeric string that contains a report identifier.
  , s3Key :: Core.Maybe Types.S3Key
    -- ^ The __key__ that uniquely identifies the report file in your S3 bucket.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCertificateAuthorityAuditReportResponse' value with any optional fields omitted.
mkCreateCertificateAuthorityAuditReportResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCertificateAuthorityAuditReportResponse
mkCreateCertificateAuthorityAuditReportResponse responseStatus
  = CreateCertificateAuthorityAuditReportResponse'{auditReportId =
                                                     Core.Nothing,
                                                   s3Key = Core.Nothing, responseStatus}

-- | An alphanumeric string that contains a report identifier.
--
-- /Note:/ Consider using 'auditReportId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaarrrsAuditReportId :: Lens.Lens' CreateCertificateAuthorityAuditReportResponse (Core.Maybe Types.AuditReportId)
ccaarrrsAuditReportId = Lens.field @"auditReportId"
{-# INLINEABLE ccaarrrsAuditReportId #-}
{-# DEPRECATED auditReportId "Use generic-lens or generic-optics with 'auditReportId' instead"  #-}

-- | The __key__ that uniquely identifies the report file in your S3 bucket.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaarrrsS3Key :: Lens.Lens' CreateCertificateAuthorityAuditReportResponse (Core.Maybe Types.S3Key)
ccaarrrsS3Key = Lens.field @"s3Key"
{-# INLINEABLE ccaarrrsS3Key #-}
{-# DEPRECATED s3Key "Use generic-lens or generic-optics with 's3Key' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaarrrsResponseStatus :: Lens.Lens' CreateCertificateAuthorityAuditReportResponse Core.Int
ccaarrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccaarrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
