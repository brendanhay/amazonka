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
-- Module      : Amazonka.CertificateManagerPCA.CreateCertificateAuthorityAuditReport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an audit report that lists every time that your CA private key
-- is used. The report is saved in the Amazon S3 bucket that you specify on
-- input. The
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_IssueCertificate.html IssueCertificate>
-- and
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_RevokeCertificate.html RevokeCertificate>
-- actions use the private key.
--
-- Both Amazon Web Services Private CA and the IAM principal must have
-- permission to write to the S3 bucket that you specify. If the IAM
-- principal making the call does not have permission to write to the
-- bucket, then an exception is thrown. For more information, see
-- <https://docs.aws.amazon.com/privateca/latest/userguide/crl-planning.html#s3-policies Access policies for CRLs in Amazon S3>.
--
-- Amazon Web Services Private CA assets that are stored in Amazon S3 can
-- be protected with encryption. For more information, see
-- <https://docs.aws.amazon.com/privateca/latest/userguide/PcaAuditReport.html#audit-report-encryption Encrypting Your Audit Reports>.
--
-- You can generate a maximum of one report every 30 minutes.
module Amazonka.CertificateManagerPCA.CreateCertificateAuthorityAuditReport
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
    createCertificateAuthorityAuditReportResponse_auditReportId,
    createCertificateAuthorityAuditReportResponse_s3Key,
    createCertificateAuthorityAuditReportResponse_httpStatus,
  )
where

import Amazonka.CertificateManagerPCA.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    CreateCertificateAuthorityAuditReport
  where
  type
    AWSResponse
      CreateCertificateAuthorityAuditReport =
      CreateCertificateAuthorityAuditReportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCertificateAuthorityAuditReportResponse'
            Prelude.<$> (x Data..?> "AuditReportId")
              Prelude.<*> (x Data..?> "S3Key")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateCertificateAuthorityAuditReport
  where
  hashWithSalt
    _salt
    CreateCertificateAuthorityAuditReport' {..} =
      _salt
        `Prelude.hashWithSalt` certificateAuthorityArn
        `Prelude.hashWithSalt` s3BucketName
        `Prelude.hashWithSalt` auditReportResponseFormat

instance
  Prelude.NFData
    CreateCertificateAuthorityAuditReport
  where
  rnf CreateCertificateAuthorityAuditReport' {..} =
    Prelude.rnf certificateAuthorityArn
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf auditReportResponseFormat

instance
  Data.ToHeaders
    CreateCertificateAuthorityAuditReport
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ACMPrivateCA.CreateCertificateAuthorityAuditReport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    CreateCertificateAuthorityAuditReport
  where
  toJSON CreateCertificateAuthorityAuditReport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CertificateAuthorityArn"
                  Data..= certificateAuthorityArn
              ),
            Prelude.Just ("S3BucketName" Data..= s3BucketName),
            Prelude.Just
              ( "AuditReportResponseFormat"
                  Data..= auditReportResponseFormat
              )
          ]
      )

instance
  Data.ToPath
    CreateCertificateAuthorityAuditReport
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateCertificateAuthorityAuditReport
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCertificateAuthorityAuditReportResponse' smart constructor.
data CreateCertificateAuthorityAuditReportResponse = CreateCertificateAuthorityAuditReportResponse'
  { -- | An alphanumeric string that contains a report identifier.
    auditReportId :: Prelude.Maybe Prelude.Text,
    -- | The __key__ that uniquely identifies the report file in your S3 bucket.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCertificateAuthorityAuditReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditReportId', 'createCertificateAuthorityAuditReportResponse_auditReportId' - An alphanumeric string that contains a report identifier.
--
-- 's3Key', 'createCertificateAuthorityAuditReportResponse_s3Key' - The __key__ that uniquely identifies the report file in your S3 bucket.
--
-- 'httpStatus', 'createCertificateAuthorityAuditReportResponse_httpStatus' - The response's http status code.
newCreateCertificateAuthorityAuditReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCertificateAuthorityAuditReportResponse
newCreateCertificateAuthorityAuditReportResponse
  pHttpStatus_ =
    CreateCertificateAuthorityAuditReportResponse'
      { auditReportId =
          Prelude.Nothing,
        s3Key = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An alphanumeric string that contains a report identifier.
createCertificateAuthorityAuditReportResponse_auditReportId :: Lens.Lens' CreateCertificateAuthorityAuditReportResponse (Prelude.Maybe Prelude.Text)
createCertificateAuthorityAuditReportResponse_auditReportId = Lens.lens (\CreateCertificateAuthorityAuditReportResponse' {auditReportId} -> auditReportId) (\s@CreateCertificateAuthorityAuditReportResponse' {} a -> s {auditReportId = a} :: CreateCertificateAuthorityAuditReportResponse)

-- | The __key__ that uniquely identifies the report file in your S3 bucket.
createCertificateAuthorityAuditReportResponse_s3Key :: Lens.Lens' CreateCertificateAuthorityAuditReportResponse (Prelude.Maybe Prelude.Text)
createCertificateAuthorityAuditReportResponse_s3Key = Lens.lens (\CreateCertificateAuthorityAuditReportResponse' {s3Key} -> s3Key) (\s@CreateCertificateAuthorityAuditReportResponse' {} a -> s {s3Key = a} :: CreateCertificateAuthorityAuditReportResponse)

-- | The response's http status code.
createCertificateAuthorityAuditReportResponse_httpStatus :: Lens.Lens' CreateCertificateAuthorityAuditReportResponse Prelude.Int
createCertificateAuthorityAuditReportResponse_httpStatus = Lens.lens (\CreateCertificateAuthorityAuditReportResponse' {httpStatus} -> httpStatus) (\s@CreateCertificateAuthorityAuditReportResponse' {} a -> s {httpStatus = a} :: CreateCertificateAuthorityAuditReportResponse)

instance
  Prelude.NFData
    CreateCertificateAuthorityAuditReportResponse
  where
  rnf
    CreateCertificateAuthorityAuditReportResponse' {..} =
      Prelude.rnf auditReportId
        `Prelude.seq` Prelude.rnf s3Key
        `Prelude.seq` Prelude.rnf httpStatus
