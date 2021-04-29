{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about a specific audit report created by calling the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport>
-- action. Audit information is created every time the certificate
-- authority (CA) private key is used. The private key is used when you
-- call the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_IssueCertificate.html IssueCertificate>
-- action or the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_RevokeCertificate.html RevokeCertificate>
-- action.
module Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
  ( -- * Creating a Request
    DescribeCertificateAuthorityAuditReport (..),
    newDescribeCertificateAuthorityAuditReport,

    -- * Request Lenses
    describeCertificateAuthorityAuditReport_certificateAuthorityArn,
    describeCertificateAuthorityAuditReport_auditReportId,

    -- * Destructuring the Response
    DescribeCertificateAuthorityAuditReportResponse (..),
    newDescribeCertificateAuthorityAuditReportResponse,

    -- * Response Lenses
    describeCertificateAuthorityAuditReportResponse_auditReportStatus,
    describeCertificateAuthorityAuditReportResponse_createdAt,
    describeCertificateAuthorityAuditReportResponse_s3Key,
    describeCertificateAuthorityAuditReportResponse_s3BucketName,
    describeCertificateAuthorityAuditReportResponse_httpStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCertificateAuthorityAuditReport' smart constructor.
data DescribeCertificateAuthorityAuditReport = DescribeCertificateAuthorityAuditReport'
  { -- | The Amazon Resource Name (ARN) of the private CA. This must be of the
    -- form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
    certificateAuthorityArn :: Prelude.Text,
    -- | The report ID returned by calling the
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport>
    -- action.
    auditReportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeCertificateAuthorityAuditReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'describeCertificateAuthorityAuditReport_certificateAuthorityArn' - The Amazon Resource Name (ARN) of the private CA. This must be of the
-- form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
--
-- 'auditReportId', 'describeCertificateAuthorityAuditReport_auditReportId' - The report ID returned by calling the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport>
-- action.
newDescribeCertificateAuthorityAuditReport ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  -- | 'auditReportId'
  Prelude.Text ->
  DescribeCertificateAuthorityAuditReport
newDescribeCertificateAuthorityAuditReport
  pCertificateAuthorityArn_
  pAuditReportId_ =
    DescribeCertificateAuthorityAuditReport'
      { certificateAuthorityArn =
          pCertificateAuthorityArn_,
        auditReportId = pAuditReportId_
      }

-- | The Amazon Resource Name (ARN) of the private CA. This must be of the
-- form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
describeCertificateAuthorityAuditReport_certificateAuthorityArn :: Lens.Lens' DescribeCertificateAuthorityAuditReport Prelude.Text
describeCertificateAuthorityAuditReport_certificateAuthorityArn = Lens.lens (\DescribeCertificateAuthorityAuditReport' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@DescribeCertificateAuthorityAuditReport' {} a -> s {certificateAuthorityArn = a} :: DescribeCertificateAuthorityAuditReport)

-- | The report ID returned by calling the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport>
-- action.
describeCertificateAuthorityAuditReport_auditReportId :: Lens.Lens' DescribeCertificateAuthorityAuditReport Prelude.Text
describeCertificateAuthorityAuditReport_auditReportId = Lens.lens (\DescribeCertificateAuthorityAuditReport' {auditReportId} -> auditReportId) (\s@DescribeCertificateAuthorityAuditReport' {} a -> s {auditReportId = a} :: DescribeCertificateAuthorityAuditReport)

instance
  Prelude.AWSRequest
    DescribeCertificateAuthorityAuditReport
  where
  type
    Rs DescribeCertificateAuthorityAuditReport =
      DescribeCertificateAuthorityAuditReportResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCertificateAuthorityAuditReportResponse'
            Prelude.<$> (x Prelude..?> "AuditReportStatus")
              Prelude.<*> (x Prelude..?> "CreatedAt")
              Prelude.<*> (x Prelude..?> "S3Key")
              Prelude.<*> (x Prelude..?> "S3BucketName")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeCertificateAuthorityAuditReport

instance
  Prelude.NFData
    DescribeCertificateAuthorityAuditReport

instance
  Prelude.ToHeaders
    DescribeCertificateAuthorityAuditReport
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ACMPrivateCA.DescribeCertificateAuthorityAuditReport" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DescribeCertificateAuthorityAuditReport
  where
  toJSON DescribeCertificateAuthorityAuditReport' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CertificateAuthorityArn"
                  Prelude..= certificateAuthorityArn
              ),
            Prelude.Just
              ("AuditReportId" Prelude..= auditReportId)
          ]
      )

instance
  Prelude.ToPath
    DescribeCertificateAuthorityAuditReport
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeCertificateAuthorityAuditReport
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCertificateAuthorityAuditReportResponse' smart constructor.
data DescribeCertificateAuthorityAuditReportResponse = DescribeCertificateAuthorityAuditReportResponse'
  { -- | Specifies whether report creation is in progress, has succeeded, or has
    -- failed.
    auditReportStatus :: Prelude.Maybe AuditReportStatus,
    -- | The date and time at which the report was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | S3 __key__ that uniquely identifies the report file in your S3 bucket.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | Name of the S3 bucket that contains the report.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeCertificateAuthorityAuditReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditReportStatus', 'describeCertificateAuthorityAuditReportResponse_auditReportStatus' - Specifies whether report creation is in progress, has succeeded, or has
-- failed.
--
-- 'createdAt', 'describeCertificateAuthorityAuditReportResponse_createdAt' - The date and time at which the report was created.
--
-- 's3Key', 'describeCertificateAuthorityAuditReportResponse_s3Key' - S3 __key__ that uniquely identifies the report file in your S3 bucket.
--
-- 's3BucketName', 'describeCertificateAuthorityAuditReportResponse_s3BucketName' - Name of the S3 bucket that contains the report.
--
-- 'httpStatus', 'describeCertificateAuthorityAuditReportResponse_httpStatus' - The response's http status code.
newDescribeCertificateAuthorityAuditReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCertificateAuthorityAuditReportResponse
newDescribeCertificateAuthorityAuditReportResponse
  pHttpStatus_ =
    DescribeCertificateAuthorityAuditReportResponse'
      { auditReportStatus =
          Prelude.Nothing,
        createdAt =
          Prelude.Nothing,
        s3Key = Prelude.Nothing,
        s3BucketName =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Specifies whether report creation is in progress, has succeeded, or has
-- failed.
describeCertificateAuthorityAuditReportResponse_auditReportStatus :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse (Prelude.Maybe AuditReportStatus)
describeCertificateAuthorityAuditReportResponse_auditReportStatus = Lens.lens (\DescribeCertificateAuthorityAuditReportResponse' {auditReportStatus} -> auditReportStatus) (\s@DescribeCertificateAuthorityAuditReportResponse' {} a -> s {auditReportStatus = a} :: DescribeCertificateAuthorityAuditReportResponse)

-- | The date and time at which the report was created.
describeCertificateAuthorityAuditReportResponse_createdAt :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse (Prelude.Maybe Prelude.UTCTime)
describeCertificateAuthorityAuditReportResponse_createdAt = Lens.lens (\DescribeCertificateAuthorityAuditReportResponse' {createdAt} -> createdAt) (\s@DescribeCertificateAuthorityAuditReportResponse' {} a -> s {createdAt = a} :: DescribeCertificateAuthorityAuditReportResponse) Prelude.. Lens.mapping Prelude._Time

-- | S3 __key__ that uniquely identifies the report file in your S3 bucket.
describeCertificateAuthorityAuditReportResponse_s3Key :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse (Prelude.Maybe Prelude.Text)
describeCertificateAuthorityAuditReportResponse_s3Key = Lens.lens (\DescribeCertificateAuthorityAuditReportResponse' {s3Key} -> s3Key) (\s@DescribeCertificateAuthorityAuditReportResponse' {} a -> s {s3Key = a} :: DescribeCertificateAuthorityAuditReportResponse)

-- | Name of the S3 bucket that contains the report.
describeCertificateAuthorityAuditReportResponse_s3BucketName :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse (Prelude.Maybe Prelude.Text)
describeCertificateAuthorityAuditReportResponse_s3BucketName = Lens.lens (\DescribeCertificateAuthorityAuditReportResponse' {s3BucketName} -> s3BucketName) (\s@DescribeCertificateAuthorityAuditReportResponse' {} a -> s {s3BucketName = a} :: DescribeCertificateAuthorityAuditReportResponse)

-- | The response's http status code.
describeCertificateAuthorityAuditReportResponse_httpStatus :: Lens.Lens' DescribeCertificateAuthorityAuditReportResponse Prelude.Int
describeCertificateAuthorityAuditReportResponse_httpStatus = Lens.lens (\DescribeCertificateAuthorityAuditReportResponse' {httpStatus} -> httpStatus) (\s@DescribeCertificateAuthorityAuditReportResponse' {} a -> s {httpStatus = a} :: DescribeCertificateAuthorityAuditReportResponse)

instance
  Prelude.NFData
    DescribeCertificateAuthorityAuditReportResponse
