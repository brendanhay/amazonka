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
-- Module      : Amazonka.EC2.AssociateEnclaveCertificateIamRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an Identity and Access Management (IAM) role with an
-- Certificate Manager (ACM) certificate. This enables the certificate to
-- be used by the ACM for Nitro Enclaves application inside an enclave. For
-- more information, see
-- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave-refapp.html Certificate Manager for Nitro Enclaves>
-- in the /Amazon Web Services Nitro Enclaves User Guide/.
--
-- When the IAM role is associated with the ACM certificate, the
-- certificate, certificate chain, and encrypted private key are placed in
-- an Amazon S3 location that only the associated IAM role can access. The
-- private key of the certificate is encrypted with an Amazon Web Services
-- managed key that has an attached attestation-based key policy.
--
-- To enable the IAM role to access the Amazon S3 object, you must grant it
-- permission to call @s3:GetObject@ on the Amazon S3 bucket returned by
-- the command. To enable the IAM role to access the KMS key, you must
-- grant it permission to call @kms:Decrypt@ on the KMS key returned by the
-- command. For more information, see
-- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave-refapp.html#add-policy Grant the role permission to access the certificate and encryption key>
-- in the /Amazon Web Services Nitro Enclaves User Guide/.
module Amazonka.EC2.AssociateEnclaveCertificateIamRole
  ( -- * Creating a Request
    AssociateEnclaveCertificateIamRole (..),
    newAssociateEnclaveCertificateIamRole,

    -- * Request Lenses
    associateEnclaveCertificateIamRole_certificateArn,
    associateEnclaveCertificateIamRole_dryRun,
    associateEnclaveCertificateIamRole_roleArn,

    -- * Destructuring the Response
    AssociateEnclaveCertificateIamRoleResponse (..),
    newAssociateEnclaveCertificateIamRoleResponse,

    -- * Response Lenses
    associateEnclaveCertificateIamRoleResponse_certificateS3BucketName,
    associateEnclaveCertificateIamRoleResponse_certificateS3ObjectKey,
    associateEnclaveCertificateIamRoleResponse_encryptionKmsKeyId,
    associateEnclaveCertificateIamRoleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateEnclaveCertificateIamRole' smart constructor.
data AssociateEnclaveCertificateIamRole = AssociateEnclaveCertificateIamRole'
  { -- | The ARN of the ACM certificate with which to associate the IAM role.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the IAM role to associate with the ACM certificate. You can
    -- associate up to 16 IAM roles with an ACM certificate.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateEnclaveCertificateIamRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'associateEnclaveCertificateIamRole_certificateArn' - The ARN of the ACM certificate with which to associate the IAM role.
--
-- 'dryRun', 'associateEnclaveCertificateIamRole_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'roleArn', 'associateEnclaveCertificateIamRole_roleArn' - The ARN of the IAM role to associate with the ACM certificate. You can
-- associate up to 16 IAM roles with an ACM certificate.
newAssociateEnclaveCertificateIamRole ::
  AssociateEnclaveCertificateIamRole
newAssociateEnclaveCertificateIamRole =
  AssociateEnclaveCertificateIamRole'
    { certificateArn =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The ARN of the ACM certificate with which to associate the IAM role.
associateEnclaveCertificateIamRole_certificateArn :: Lens.Lens' AssociateEnclaveCertificateIamRole (Prelude.Maybe Prelude.Text)
associateEnclaveCertificateIamRole_certificateArn = Lens.lens (\AssociateEnclaveCertificateIamRole' {certificateArn} -> certificateArn) (\s@AssociateEnclaveCertificateIamRole' {} a -> s {certificateArn = a} :: AssociateEnclaveCertificateIamRole)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
associateEnclaveCertificateIamRole_dryRun :: Lens.Lens' AssociateEnclaveCertificateIamRole (Prelude.Maybe Prelude.Bool)
associateEnclaveCertificateIamRole_dryRun = Lens.lens (\AssociateEnclaveCertificateIamRole' {dryRun} -> dryRun) (\s@AssociateEnclaveCertificateIamRole' {} a -> s {dryRun = a} :: AssociateEnclaveCertificateIamRole)

-- | The ARN of the IAM role to associate with the ACM certificate. You can
-- associate up to 16 IAM roles with an ACM certificate.
associateEnclaveCertificateIamRole_roleArn :: Lens.Lens' AssociateEnclaveCertificateIamRole (Prelude.Maybe Prelude.Text)
associateEnclaveCertificateIamRole_roleArn = Lens.lens (\AssociateEnclaveCertificateIamRole' {roleArn} -> roleArn) (\s@AssociateEnclaveCertificateIamRole' {} a -> s {roleArn = a} :: AssociateEnclaveCertificateIamRole)

instance
  Core.AWSRequest
    AssociateEnclaveCertificateIamRole
  where
  type
    AWSResponse AssociateEnclaveCertificateIamRole =
      AssociateEnclaveCertificateIamRoleResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateEnclaveCertificateIamRoleResponse'
            Prelude.<$> (x Data..@? "certificateS3BucketName")
              Prelude.<*> (x Data..@? "certificateS3ObjectKey")
              Prelude.<*> (x Data..@? "encryptionKmsKeyId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateEnclaveCertificateIamRole
  where
  hashWithSalt
    _salt
    AssociateEnclaveCertificateIamRole' {..} =
      _salt `Prelude.hashWithSalt` certificateArn
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` roleArn

instance
  Prelude.NFData
    AssociateEnclaveCertificateIamRole
  where
  rnf AssociateEnclaveCertificateIamRole' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf roleArn

instance
  Data.ToHeaders
    AssociateEnclaveCertificateIamRole
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    AssociateEnclaveCertificateIamRole
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AssociateEnclaveCertificateIamRole
  where
  toQuery AssociateEnclaveCertificateIamRole' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "AssociateEnclaveCertificateIamRole" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "CertificateArn" Data.=: certificateArn,
        "DryRun" Data.=: dryRun,
        "RoleArn" Data.=: roleArn
      ]

-- | /See:/ 'newAssociateEnclaveCertificateIamRoleResponse' smart constructor.
data AssociateEnclaveCertificateIamRoleResponse = AssociateEnclaveCertificateIamRoleResponse'
  { -- | The name of the Amazon S3 bucket to which the certificate was uploaded.
    certificateS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 object key where the certificate, certificate chain, and
    -- encrypted private key bundle are stored. The object key is formatted as
    -- follows: @role_arn@\/@certificate_arn@.
    certificateS3ObjectKey :: Prelude.Maybe Prelude.Text,
    -- | The ID of the KMS key used to encrypt the private key of the
    -- certificate.
    encryptionKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateEnclaveCertificateIamRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateS3BucketName', 'associateEnclaveCertificateIamRoleResponse_certificateS3BucketName' - The name of the Amazon S3 bucket to which the certificate was uploaded.
--
-- 'certificateS3ObjectKey', 'associateEnclaveCertificateIamRoleResponse_certificateS3ObjectKey' - The Amazon S3 object key where the certificate, certificate chain, and
-- encrypted private key bundle are stored. The object key is formatted as
-- follows: @role_arn@\/@certificate_arn@.
--
-- 'encryptionKmsKeyId', 'associateEnclaveCertificateIamRoleResponse_encryptionKmsKeyId' - The ID of the KMS key used to encrypt the private key of the
-- certificate.
--
-- 'httpStatus', 'associateEnclaveCertificateIamRoleResponse_httpStatus' - The response's http status code.
newAssociateEnclaveCertificateIamRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateEnclaveCertificateIamRoleResponse
newAssociateEnclaveCertificateIamRoleResponse
  pHttpStatus_ =
    AssociateEnclaveCertificateIamRoleResponse'
      { certificateS3BucketName =
          Prelude.Nothing,
        certificateS3ObjectKey =
          Prelude.Nothing,
        encryptionKmsKeyId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The name of the Amazon S3 bucket to which the certificate was uploaded.
associateEnclaveCertificateIamRoleResponse_certificateS3BucketName :: Lens.Lens' AssociateEnclaveCertificateIamRoleResponse (Prelude.Maybe Prelude.Text)
associateEnclaveCertificateIamRoleResponse_certificateS3BucketName = Lens.lens (\AssociateEnclaveCertificateIamRoleResponse' {certificateS3BucketName} -> certificateS3BucketName) (\s@AssociateEnclaveCertificateIamRoleResponse' {} a -> s {certificateS3BucketName = a} :: AssociateEnclaveCertificateIamRoleResponse)

-- | The Amazon S3 object key where the certificate, certificate chain, and
-- encrypted private key bundle are stored. The object key is formatted as
-- follows: @role_arn@\/@certificate_arn@.
associateEnclaveCertificateIamRoleResponse_certificateS3ObjectKey :: Lens.Lens' AssociateEnclaveCertificateIamRoleResponse (Prelude.Maybe Prelude.Text)
associateEnclaveCertificateIamRoleResponse_certificateS3ObjectKey = Lens.lens (\AssociateEnclaveCertificateIamRoleResponse' {certificateS3ObjectKey} -> certificateS3ObjectKey) (\s@AssociateEnclaveCertificateIamRoleResponse' {} a -> s {certificateS3ObjectKey = a} :: AssociateEnclaveCertificateIamRoleResponse)

-- | The ID of the KMS key used to encrypt the private key of the
-- certificate.
associateEnclaveCertificateIamRoleResponse_encryptionKmsKeyId :: Lens.Lens' AssociateEnclaveCertificateIamRoleResponse (Prelude.Maybe Prelude.Text)
associateEnclaveCertificateIamRoleResponse_encryptionKmsKeyId = Lens.lens (\AssociateEnclaveCertificateIamRoleResponse' {encryptionKmsKeyId} -> encryptionKmsKeyId) (\s@AssociateEnclaveCertificateIamRoleResponse' {} a -> s {encryptionKmsKeyId = a} :: AssociateEnclaveCertificateIamRoleResponse)

-- | The response's http status code.
associateEnclaveCertificateIamRoleResponse_httpStatus :: Lens.Lens' AssociateEnclaveCertificateIamRoleResponse Prelude.Int
associateEnclaveCertificateIamRoleResponse_httpStatus = Lens.lens (\AssociateEnclaveCertificateIamRoleResponse' {httpStatus} -> httpStatus) (\s@AssociateEnclaveCertificateIamRoleResponse' {} a -> s {httpStatus = a} :: AssociateEnclaveCertificateIamRoleResponse)

instance
  Prelude.NFData
    AssociateEnclaveCertificateIamRoleResponse
  where
  rnf AssociateEnclaveCertificateIamRoleResponse' {..} =
    Prelude.rnf certificateS3BucketName
      `Prelude.seq` Prelude.rnf certificateS3ObjectKey
      `Prelude.seq` Prelude.rnf encryptionKmsKeyId
      `Prelude.seq` Prelude.rnf httpStatus
