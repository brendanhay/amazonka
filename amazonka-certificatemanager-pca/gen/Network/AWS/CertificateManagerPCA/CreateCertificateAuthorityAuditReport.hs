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
-- Module      : Network.AWS.CertificateManagerPCA.CreateCertificateAuthorityAuditReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an audit report that lists every time that your CA private key
-- is used. The report is saved in the Amazon S3 bucket that you specify on
-- input. The
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_IssueCertificate.html IssueCertificate>
-- and
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_RevokeCertificate.html RevokeCertificate>
-- actions use the private key.
--
-- Both PCA and the IAM principal must have permission to write to the S3
-- bucket that you specify. If the IAM principal making the call does not
-- have permission to write to the bucket, then an exception is thrown. For
-- more information, see
-- <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaAuthAccess.html Configure Access to ACM Private CA>.
--
-- ACM Private CAA assets that are stored in Amazon S3 can be protected
-- with encryption. For more information, see
-- <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaAuditReport.html#audit-report-encryption Encrypting Your Audit Reports>.
module Network.AWS.CertificateManagerPCA.CreateCertificateAuthorityAuditReport
  ( -- * Creating a Request
    CreateCertificateAuthorityAuditReport (..),
    newCreateCertificateAuthorityAuditReport,

    -- * Request Lenses
    createCertificateAuthorityAuditReport_certificateAuthorityArn,
    createCertificateAuthorityAuditReport_s3BucketName,
    createCertificateAuthorityAuditReport_auditReportResponseFormat,

    -- * Destructuring the Response
    CreateCertificateAuthorityAuditReportResponse (..),
    newCreateCertificateAuthorityAuditReportResponse,

    -- * Response Lenses
    createCertificateAuthorityAuditReportResponse_s3Key,
    createCertificateAuthorityAuditReportResponse_auditReportId,
    createCertificateAuthorityAuditReportResponse_httpStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCertificateAuthorityAuditReport' smart constructor.
data CreateCertificateAuthorityAuditReport = CreateCertificateAuthorityAuditReport'
  { -- | The Amazon Resource Name (ARN) of the CA to be audited. This is of the
    -- form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
    certificateAuthorityArn :: Prelude.Text,
    -- | The name of the S3 bucket that will contain the audit report.
    s3BucketName :: Prelude.Text,
    -- | The format in which to create the report. This can be either __JSON__ or
    -- __CSV__.
    auditReportResponseFormat :: AuditReportResponseFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateCertificateAuthorityAuditReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'createCertificateAuthorityAuditReport_certificateAuthorityArn' - The Amazon Resource Name (ARN) of the CA to be audited. This is of the
-- form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
--
-- 's3BucketName', 'createCertificateAuthorityAuditReport_s3BucketName' - The name of the S3 bucket that will contain the audit report.
--
-- 'auditReportResponseFormat', 'createCertificateAuthorityAuditReport_auditReportResponseFormat' - The format in which to create the report. This can be either __JSON__ or
-- __CSV__.
newCreateCertificateAuthorityAuditReport ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  -- | 's3BucketName'
  Prelude.Text ->
  -- | 'auditReportResponseFormat'
  AuditReportResponseFormat ->
  CreateCertificateAuthorityAuditReport
newCreateCertificateAuthorityAuditReport
  pCertificateAuthorityArn_
  pS3BucketName_
  pAuditReportResponseFormat_ =
    CreateCertificateAuthorityAuditReport'
      { certificateAuthorityArn =
          pCertificateAuthorityArn_,
        s3BucketName = pS3BucketName_,
        auditReportResponseFormat =
          pAuditReportResponseFormat_
      }

-- | The Amazon Resource Name (ARN) of the CA to be audited. This is of the
-- form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
createCertificateAuthorityAuditReport_certificateAuthorityArn :: Lens.Lens' CreateCertificateAuthorityAuditReport Prelude.Text
createCertificateAuthorityAuditReport_certificateAuthorityArn = Lens.lens (\CreateCertificateAuthorityAuditReport' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@CreateCertificateAuthorityAuditReport' {} a -> s {certificateAuthorityArn = a} :: CreateCertificateAuthorityAuditReport)

-- | The name of the S3 bucket that will contain the audit report.
createCertificateAuthorityAuditReport_s3BucketName :: Lens.Lens' CreateCertificateAuthorityAuditReport Prelude.Text
createCertificateAuthorityAuditReport_s3BucketName = Lens.lens (\CreateCertificateAuthorityAuditReport' {s3BucketName} -> s3BucketName) (\s@CreateCertificateAuthorityAuditReport' {} a -> s {s3BucketName = a} :: CreateCertificateAuthorityAuditReport)

-- | The format in which to create the report. This can be either __JSON__ or
-- __CSV__.
createCertificateAuthorityAuditReport_auditReportResponseFormat :: Lens.Lens' CreateCertificateAuthorityAuditReport AuditReportResponseFormat
createCertificateAuthorityAuditReport_auditReportResponseFormat = Lens.lens (\CreateCertificateAuthorityAuditReport' {auditReportResponseFormat} -> auditReportResponseFormat) (\s@CreateCertificateAuthorityAuditReport' {} a -> s {auditReportResponseFormat = a} :: CreateCertificateAuthorityAuditReport)

instance
  Prelude.AWSRequest
    CreateCertificateAuthorityAuditReport
  where
  type
    Rs CreateCertificateAuthorityAuditReport =
      CreateCertificateAuthorityAuditReportResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCertificateAuthorityAuditReportResponse'
            Prelude.<$> (x Prelude..?> "S3Key")
              Prelude.<*> (x Prelude..?> "AuditReportId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateCertificateAuthorityAuditReport

instance
  Prelude.NFData
    CreateCertificateAuthorityAuditReport

instance
  Prelude.ToHeaders
    CreateCertificateAuthorityAuditReport
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ACMPrivateCA.CreateCertificateAuthorityAuditReport" ::
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
    CreateCertificateAuthorityAuditReport
  where
  toJSON CreateCertificateAuthorityAuditReport' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CertificateAuthorityArn"
                  Prelude..= certificateAuthorityArn
              ),
            Prelude.Just
              ("S3BucketName" Prelude..= s3BucketName),
            Prelude.Just
              ( "AuditReportResponseFormat"
                  Prelude..= auditReportResponseFormat
              )
          ]
      )

instance
  Prelude.ToPath
    CreateCertificateAuthorityAuditReport
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    CreateCertificateAuthorityAuditReport
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCertificateAuthorityAuditReportResponse' smart constructor.
data CreateCertificateAuthorityAuditReportResponse = CreateCertificateAuthorityAuditReportResponse'
  { -- | The __key__ that uniquely identifies the report file in your S3 bucket.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | An alphanumeric string that contains a report identifier.
    auditReportId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateCertificateAuthorityAuditReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Key', 'createCertificateAuthorityAuditReportResponse_s3Key' - The __key__ that uniquely identifies the report file in your S3 bucket.
--
-- 'auditReportId', 'createCertificateAuthorityAuditReportResponse_auditReportId' - An alphanumeric string that contains a report identifier.
--
-- 'httpStatus', 'createCertificateAuthorityAuditReportResponse_httpStatus' - The response's http status code.
newCreateCertificateAuthorityAuditReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCertificateAuthorityAuditReportResponse
newCreateCertificateAuthorityAuditReportResponse
  pHttpStatus_ =
    CreateCertificateAuthorityAuditReportResponse'
      { s3Key =
          Prelude.Nothing,
        auditReportId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The __key__ that uniquely identifies the report file in your S3 bucket.
createCertificateAuthorityAuditReportResponse_s3Key :: Lens.Lens' CreateCertificateAuthorityAuditReportResponse (Prelude.Maybe Prelude.Text)
createCertificateAuthorityAuditReportResponse_s3Key = Lens.lens (\CreateCertificateAuthorityAuditReportResponse' {s3Key} -> s3Key) (\s@CreateCertificateAuthorityAuditReportResponse' {} a -> s {s3Key = a} :: CreateCertificateAuthorityAuditReportResponse)

-- | An alphanumeric string that contains a report identifier.
createCertificateAuthorityAuditReportResponse_auditReportId :: Lens.Lens' CreateCertificateAuthorityAuditReportResponse (Prelude.Maybe Prelude.Text)
createCertificateAuthorityAuditReportResponse_auditReportId = Lens.lens (\CreateCertificateAuthorityAuditReportResponse' {auditReportId} -> auditReportId) (\s@CreateCertificateAuthorityAuditReportResponse' {} a -> s {auditReportId = a} :: CreateCertificateAuthorityAuditReportResponse)

-- | The response's http status code.
createCertificateAuthorityAuditReportResponse_httpStatus :: Lens.Lens' CreateCertificateAuthorityAuditReportResponse Prelude.Int
createCertificateAuthorityAuditReportResponse_httpStatus = Lens.lens (\CreateCertificateAuthorityAuditReportResponse' {httpStatus} -> httpStatus) (\s@CreateCertificateAuthorityAuditReportResponse' {} a -> s {httpStatus = a} :: CreateCertificateAuthorityAuditReportResponse)

instance
  Prelude.NFData
    CreateCertificateAuthorityAuditReportResponse
