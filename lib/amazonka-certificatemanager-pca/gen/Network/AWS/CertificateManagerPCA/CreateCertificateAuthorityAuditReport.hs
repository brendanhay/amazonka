{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateCertificateAuthorityAuditReport (..),
    mkCreateCertificateAuthorityAuditReport,

    -- ** Request lenses
    ccaarCertificateAuthorityARN,
    ccaarS3BucketName,
    ccaarAuditReportResponseFormat,

    -- * Destructuring the response
    CreateCertificateAuthorityAuditReportResponse (..),
    mkCreateCertificateAuthorityAuditReportResponse,

    -- ** Response lenses
    ccaarrsS3Key,
    ccaarrsAuditReportId,
    ccaarrsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCertificateAuthorityAuditReport' smart constructor.
data CreateCertificateAuthorityAuditReport = CreateCertificateAuthorityAuditReport'
  { certificateAuthorityARN ::
      Lude.Text,
    s3BucketName ::
      Lude.Text,
    auditReportResponseFormat ::
      AuditReportResponseFormat
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCertificateAuthorityAuditReport' with the minimum fields required to make a request.
--
-- * 'auditReportResponseFormat' - The format in which to create the report. This can be either __JSON__ or __CSV__ .
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) of the CA to be audited. This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
-- * 's3BucketName' - The name of the S3 bucket that will contain the audit report.
mkCreateCertificateAuthorityAuditReport ::
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  -- | 's3BucketName'
  Lude.Text ->
  -- | 'auditReportResponseFormat'
  AuditReportResponseFormat ->
  CreateCertificateAuthorityAuditReport
mkCreateCertificateAuthorityAuditReport
  pCertificateAuthorityARN_
  pS3BucketName_
  pAuditReportResponseFormat_ =
    CreateCertificateAuthorityAuditReport'
      { certificateAuthorityARN =
          pCertificateAuthorityARN_,
        s3BucketName = pS3BucketName_,
        auditReportResponseFormat = pAuditReportResponseFormat_
      }

-- | The Amazon Resource Name (ARN) of the CA to be audited. This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaarCertificateAuthorityARN :: Lens.Lens' CreateCertificateAuthorityAuditReport Lude.Text
ccaarCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: CreateCertificateAuthorityAuditReport -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: CreateCertificateAuthorityAuditReport)
{-# DEPRECATED ccaarCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

-- | The name of the S3 bucket that will contain the audit report.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaarS3BucketName :: Lens.Lens' CreateCertificateAuthorityAuditReport Lude.Text
ccaarS3BucketName = Lens.lens (s3BucketName :: CreateCertificateAuthorityAuditReport -> Lude.Text) (\s a -> s {s3BucketName = a} :: CreateCertificateAuthorityAuditReport)
{-# DEPRECATED ccaarS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The format in which to create the report. This can be either __JSON__ or __CSV__ .
--
-- /Note:/ Consider using 'auditReportResponseFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaarAuditReportResponseFormat :: Lens.Lens' CreateCertificateAuthorityAuditReport AuditReportResponseFormat
ccaarAuditReportResponseFormat = Lens.lens (auditReportResponseFormat :: CreateCertificateAuthorityAuditReport -> AuditReportResponseFormat) (\s a -> s {auditReportResponseFormat = a} :: CreateCertificateAuthorityAuditReport)
{-# DEPRECATED ccaarAuditReportResponseFormat "Use generic-lens or generic-optics with 'auditReportResponseFormat' instead." #-}

instance Lude.AWSRequest CreateCertificateAuthorityAuditReport where
  type
    Rs CreateCertificateAuthorityAuditReport =
      CreateCertificateAuthorityAuditReportResponse
  request = Req.postJSON certificateManagerPCAService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCertificateAuthorityAuditReportResponse'
            Lude.<$> (x Lude..?> "S3Key")
            Lude.<*> (x Lude..?> "AuditReportId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCertificateAuthorityAuditReport where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ACMPrivateCA.CreateCertificateAuthorityAuditReport" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCertificateAuthorityAuditReport where
  toJSON CreateCertificateAuthorityAuditReport' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN),
            Lude.Just ("S3BucketName" Lude..= s3BucketName),
            Lude.Just
              ("AuditReportResponseFormat" Lude..= auditReportResponseFormat)
          ]
      )

instance Lude.ToPath CreateCertificateAuthorityAuditReport where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCertificateAuthorityAuditReport where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCertificateAuthorityAuditReportResponse' smart constructor.
data CreateCertificateAuthorityAuditReportResponse = CreateCertificateAuthorityAuditReportResponse'
  { s3Key ::
      Lude.Maybe
        Lude.Text,
    auditReportId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'CreateCertificateAuthorityAuditReportResponse' with the minimum fields required to make a request.
--
-- * 'auditReportId' - An alphanumeric string that contains a report identifier.
-- * 'responseStatus' - The response status code.
-- * 's3Key' - The __key__ that uniquely identifies the report file in your S3 bucket.
mkCreateCertificateAuthorityAuditReportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCertificateAuthorityAuditReportResponse
mkCreateCertificateAuthorityAuditReportResponse pResponseStatus_ =
  CreateCertificateAuthorityAuditReportResponse'
    { s3Key =
        Lude.Nothing,
      auditReportId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The __key__ that uniquely identifies the report file in your S3 bucket.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaarrsS3Key :: Lens.Lens' CreateCertificateAuthorityAuditReportResponse (Lude.Maybe Lude.Text)
ccaarrsS3Key = Lens.lens (s3Key :: CreateCertificateAuthorityAuditReportResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3Key = a} :: CreateCertificateAuthorityAuditReportResponse)
{-# DEPRECATED ccaarrsS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

-- | An alphanumeric string that contains a report identifier.
--
-- /Note:/ Consider using 'auditReportId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaarrsAuditReportId :: Lens.Lens' CreateCertificateAuthorityAuditReportResponse (Lude.Maybe Lude.Text)
ccaarrsAuditReportId = Lens.lens (auditReportId :: CreateCertificateAuthorityAuditReportResponse -> Lude.Maybe Lude.Text) (\s a -> s {auditReportId = a} :: CreateCertificateAuthorityAuditReportResponse)
{-# DEPRECATED ccaarrsAuditReportId "Use generic-lens or generic-optics with 'auditReportId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaarrsResponseStatus :: Lens.Lens' CreateCertificateAuthorityAuditReportResponse Lude.Int
ccaarrsResponseStatus = Lens.lens (responseStatus :: CreateCertificateAuthorityAuditReportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCertificateAuthorityAuditReportResponse)
{-# DEPRECATED ccaarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
