{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dcaarCertificateAuthorityARN,
    dcaarAuditReportId,

    -- * Destructuring the response
    DescribeCertificateAuthorityAuditReportResponse (..),
    mkDescribeCertificateAuthorityAuditReportResponse,

    -- ** Response lenses
    dcaarrsS3Key,
    dcaarrsCreatedAt,
    dcaarrsAuditReportStatus,
    dcaarrsS3BucketName,
    dcaarrsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCertificateAuthorityAuditReport' smart constructor.
data DescribeCertificateAuthorityAuditReport = DescribeCertificateAuthorityAuditReport'
  { certificateAuthorityARN ::
      Lude.Text,
    auditReportId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCertificateAuthorityAuditReport' with the minimum fields required to make a request.
--
-- * 'auditReportId' - The report ID returned by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport> action.
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) of the private CA. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
mkDescribeCertificateAuthorityAuditReport ::
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  -- | 'auditReportId'
  Lude.Text ->
  DescribeCertificateAuthorityAuditReport
mkDescribeCertificateAuthorityAuditReport
  pCertificateAuthorityARN_
  pAuditReportId_ =
    DescribeCertificateAuthorityAuditReport'
      { certificateAuthorityARN =
          pCertificateAuthorityARN_,
        auditReportId = pAuditReportId_
      }

-- | The Amazon Resource Name (ARN) of the private CA. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarCertificateAuthorityARN :: Lens.Lens' DescribeCertificateAuthorityAuditReport Lude.Text
dcaarCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: DescribeCertificateAuthorityAuditReport -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: DescribeCertificateAuthorityAuditReport)
{-# DEPRECATED dcaarCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

-- | The report ID returned by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport> action.
--
-- /Note:/ Consider using 'auditReportId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarAuditReportId :: Lens.Lens' DescribeCertificateAuthorityAuditReport Lude.Text
dcaarAuditReportId = Lens.lens (auditReportId :: DescribeCertificateAuthorityAuditReport -> Lude.Text) (\s a -> s {auditReportId = a} :: DescribeCertificateAuthorityAuditReport)
{-# DEPRECATED dcaarAuditReportId "Use generic-lens or generic-optics with 'auditReportId' instead." #-}

instance Lude.AWSRequest DescribeCertificateAuthorityAuditReport where
  type
    Rs DescribeCertificateAuthorityAuditReport =
      DescribeCertificateAuthorityAuditReportResponse
  request = Req.postJSON certificateManagerPCAService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCertificateAuthorityAuditReportResponse'
            Lude.<$> (x Lude..?> "S3Key")
            Lude.<*> (x Lude..?> "CreatedAt")
            Lude.<*> (x Lude..?> "AuditReportStatus")
            Lude.<*> (x Lude..?> "S3BucketName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCertificateAuthorityAuditReport where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ACMPrivateCA.DescribeCertificateAuthorityAuditReport" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCertificateAuthorityAuditReport where
  toJSON DescribeCertificateAuthorityAuditReport' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN),
            Lude.Just ("AuditReportId" Lude..= auditReportId)
          ]
      )

instance Lude.ToPath DescribeCertificateAuthorityAuditReport where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCertificateAuthorityAuditReport where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCertificateAuthorityAuditReportResponse' smart constructor.
data DescribeCertificateAuthorityAuditReportResponse = DescribeCertificateAuthorityAuditReportResponse'
  { s3Key ::
      Lude.Maybe
        Lude.Text,
    createdAt ::
      Lude.Maybe
        Lude.Timestamp,
    auditReportStatus ::
      Lude.Maybe
        AuditReportStatus,
    s3BucketName ::
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

-- | Creates a value of 'DescribeCertificateAuthorityAuditReportResponse' with the minimum fields required to make a request.
--
-- * 'auditReportStatus' - Specifies whether report creation is in progress, has succeeded, or has failed.
-- * 'createdAt' - The date and time at which the report was created.
-- * 'responseStatus' - The response status code.
-- * 's3BucketName' - Name of the S3 bucket that contains the report.
-- * 's3Key' - S3 __key__ that uniquely identifies the report file in your S3 bucket.
mkDescribeCertificateAuthorityAuditReportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCertificateAuthorityAuditReportResponse
mkDescribeCertificateAuthorityAuditReportResponse pResponseStatus_ =
  DescribeCertificateAuthorityAuditReportResponse'
    { s3Key =
        Lude.Nothing,
      createdAt = Lude.Nothing,
      auditReportStatus = Lude.Nothing,
      s3BucketName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | S3 __key__ that uniquely identifies the report file in your S3 bucket.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrsS3Key :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse (Lude.Maybe Lude.Text)
dcaarrsS3Key = Lens.lens (s3Key :: DescribeCertificateAuthorityAuditReportResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3Key = a} :: DescribeCertificateAuthorityAuditReportResponse)
{-# DEPRECATED dcaarrsS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

-- | The date and time at which the report was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrsCreatedAt :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse (Lude.Maybe Lude.Timestamp)
dcaarrsCreatedAt = Lens.lens (createdAt :: DescribeCertificateAuthorityAuditReportResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: DescribeCertificateAuthorityAuditReportResponse)
{-# DEPRECATED dcaarrsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Specifies whether report creation is in progress, has succeeded, or has failed.
--
-- /Note:/ Consider using 'auditReportStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrsAuditReportStatus :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse (Lude.Maybe AuditReportStatus)
dcaarrsAuditReportStatus = Lens.lens (auditReportStatus :: DescribeCertificateAuthorityAuditReportResponse -> Lude.Maybe AuditReportStatus) (\s a -> s {auditReportStatus = a} :: DescribeCertificateAuthorityAuditReportResponse)
{-# DEPRECATED dcaarrsAuditReportStatus "Use generic-lens or generic-optics with 'auditReportStatus' instead." #-}

-- | Name of the S3 bucket that contains the report.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrsS3BucketName :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse (Lude.Maybe Lude.Text)
dcaarrsS3BucketName = Lens.lens (s3BucketName :: DescribeCertificateAuthorityAuditReportResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: DescribeCertificateAuthorityAuditReportResponse)
{-# DEPRECATED dcaarrsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaarrsResponseStatus :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse Lude.Int
dcaarrsResponseStatus = Lens.lens (responseStatus :: DescribeCertificateAuthorityAuditReportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCertificateAuthorityAuditReportResponse)
{-# DEPRECATED dcaarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
