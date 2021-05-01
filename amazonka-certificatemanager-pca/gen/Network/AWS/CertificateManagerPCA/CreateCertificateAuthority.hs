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
-- Module      : Network.AWS.CertificateManagerPCA.CreateCertificateAuthority
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a root or subordinate private certificate authority (CA). You
-- must specify the CA configuration, the certificate revocation list (CRL)
-- configuration, the CA type, and an optional idempotency token to avoid
-- accidental creation of multiple CAs. The CA configuration specifies the
-- name of the algorithm and key size to be used to create the CA private
-- key, the type of signing algorithm that the CA uses, and X.500 subject
-- information. The CRL configuration specifies the CRL expiration period
-- in days (the validity period of the CRL), the Amazon S3 bucket that will
-- contain the CRL, and a CNAME alias for the S3 bucket that is included in
-- certificates issued by the CA. If successful, this action returns the
-- Amazon Resource Name (ARN) of the CA.
--
-- ACM Private CAA assets that are stored in Amazon S3 can be protected
-- with encryption. For more information, see
-- <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaCreateCa.html#crl-encryption Encrypting Your CRLs>.
--
-- Both PCA and the IAM principal must have permission to write to the S3
-- bucket that you specify. If the IAM principal making the call does not
-- have permission to write to the bucket, then an exception is thrown. For
-- more information, see
-- <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaAuthAccess.html Configure Access to ACM Private CA>.
module Network.AWS.CertificateManagerPCA.CreateCertificateAuthority
  ( -- * Creating a Request
    CreateCertificateAuthority (..),
    newCreateCertificateAuthority,

    -- * Request Lenses
    createCertificateAuthority_idempotencyToken,
    createCertificateAuthority_revocationConfiguration,
    createCertificateAuthority_tags,
    createCertificateAuthority_certificateAuthorityConfiguration,
    createCertificateAuthority_certificateAuthorityType,

    -- * Destructuring the Response
    CreateCertificateAuthorityResponse (..),
    newCreateCertificateAuthorityResponse,

    -- * Response Lenses
    createCertificateAuthorityResponse_certificateAuthorityArn,
    createCertificateAuthorityResponse_httpStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCertificateAuthority' smart constructor.
data CreateCertificateAuthority = CreateCertificateAuthority'
  { -- | Custom string that can be used to distinguish between calls to the
    -- __CreateCertificateAuthority__ action. Idempotency tokens for
    -- __CreateCertificateAuthority__ time out after five minutes. Therefore,
    -- if you call __CreateCertificateAuthority__ multiple times with the same
    -- idempotency token within five minutes, ACM Private CA recognizes that
    -- you are requesting only certificate authority and will issue only one.
    -- If you change the idempotency token for each call, PCA recognizes that
    -- you are requesting multiple certificate authorities.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | Contains a Boolean value that you can use to enable a certification
    -- revocation list (CRL) for the CA, the name of the S3 bucket to which ACM
    -- Private CA will write the CRL, and an optional CNAME alias that you can
    -- use to hide the name of your bucket in the __CRL Distribution Points__
    -- extension of your CA certificate. For more information, see the
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration>
    -- structure.
    revocationConfiguration :: Prelude.Maybe RevocationConfiguration,
    -- | Key-value pairs that will be attached to the new private CA. You can
    -- associate up to 50 tags with a private CA. For information using tags
    -- with IAM to manage permissions, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags>.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Name and bit size of the private key algorithm, the name of the signing
    -- algorithm, and X.500 certificate subject information.
    certificateAuthorityConfiguration :: CertificateAuthorityConfiguration,
    -- | The type of the certificate authority.
    certificateAuthorityType :: CertificateAuthorityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateCertificateAuthority' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idempotencyToken', 'createCertificateAuthority_idempotencyToken' - Custom string that can be used to distinguish between calls to the
-- __CreateCertificateAuthority__ action. Idempotency tokens for
-- __CreateCertificateAuthority__ time out after five minutes. Therefore,
-- if you call __CreateCertificateAuthority__ multiple times with the same
-- idempotency token within five minutes, ACM Private CA recognizes that
-- you are requesting only certificate authority and will issue only one.
-- If you change the idempotency token for each call, PCA recognizes that
-- you are requesting multiple certificate authorities.
--
-- 'revocationConfiguration', 'createCertificateAuthority_revocationConfiguration' - Contains a Boolean value that you can use to enable a certification
-- revocation list (CRL) for the CA, the name of the S3 bucket to which ACM
-- Private CA will write the CRL, and an optional CNAME alias that you can
-- use to hide the name of your bucket in the __CRL Distribution Points__
-- extension of your CA certificate. For more information, see the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration>
-- structure.
--
-- 'tags', 'createCertificateAuthority_tags' - Key-value pairs that will be attached to the new private CA. You can
-- associate up to 50 tags with a private CA. For information using tags
-- with IAM to manage permissions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags>.
--
-- 'certificateAuthorityConfiguration', 'createCertificateAuthority_certificateAuthorityConfiguration' - Name and bit size of the private key algorithm, the name of the signing
-- algorithm, and X.500 certificate subject information.
--
-- 'certificateAuthorityType', 'createCertificateAuthority_certificateAuthorityType' - The type of the certificate authority.
newCreateCertificateAuthority ::
  -- | 'certificateAuthorityConfiguration'
  CertificateAuthorityConfiguration ->
  -- | 'certificateAuthorityType'
  CertificateAuthorityType ->
  CreateCertificateAuthority
newCreateCertificateAuthority
  pCertificateAuthorityConfiguration_
  pCertificateAuthorityType_ =
    CreateCertificateAuthority'
      { idempotencyToken =
          Prelude.Nothing,
        revocationConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        certificateAuthorityConfiguration =
          pCertificateAuthorityConfiguration_,
        certificateAuthorityType =
          pCertificateAuthorityType_
      }

-- | Custom string that can be used to distinguish between calls to the
-- __CreateCertificateAuthority__ action. Idempotency tokens for
-- __CreateCertificateAuthority__ time out after five minutes. Therefore,
-- if you call __CreateCertificateAuthority__ multiple times with the same
-- idempotency token within five minutes, ACM Private CA recognizes that
-- you are requesting only certificate authority and will issue only one.
-- If you change the idempotency token for each call, PCA recognizes that
-- you are requesting multiple certificate authorities.
createCertificateAuthority_idempotencyToken :: Lens.Lens' CreateCertificateAuthority (Prelude.Maybe Prelude.Text)
createCertificateAuthority_idempotencyToken = Lens.lens (\CreateCertificateAuthority' {idempotencyToken} -> idempotencyToken) (\s@CreateCertificateAuthority' {} a -> s {idempotencyToken = a} :: CreateCertificateAuthority)

-- | Contains a Boolean value that you can use to enable a certification
-- revocation list (CRL) for the CA, the name of the S3 bucket to which ACM
-- Private CA will write the CRL, and an optional CNAME alias that you can
-- use to hide the name of your bucket in the __CRL Distribution Points__
-- extension of your CA certificate. For more information, see the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration>
-- structure.
createCertificateAuthority_revocationConfiguration :: Lens.Lens' CreateCertificateAuthority (Prelude.Maybe RevocationConfiguration)
createCertificateAuthority_revocationConfiguration = Lens.lens (\CreateCertificateAuthority' {revocationConfiguration} -> revocationConfiguration) (\s@CreateCertificateAuthority' {} a -> s {revocationConfiguration = a} :: CreateCertificateAuthority)

-- | Key-value pairs that will be attached to the new private CA. You can
-- associate up to 50 tags with a private CA. For information using tags
-- with IAM to manage permissions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags>.
createCertificateAuthority_tags :: Lens.Lens' CreateCertificateAuthority (Prelude.Maybe (Prelude.NonEmpty Tag))
createCertificateAuthority_tags = Lens.lens (\CreateCertificateAuthority' {tags} -> tags) (\s@CreateCertificateAuthority' {} a -> s {tags = a} :: CreateCertificateAuthority) Prelude.. Lens.mapping Prelude._Coerce

-- | Name and bit size of the private key algorithm, the name of the signing
-- algorithm, and X.500 certificate subject information.
createCertificateAuthority_certificateAuthorityConfiguration :: Lens.Lens' CreateCertificateAuthority CertificateAuthorityConfiguration
createCertificateAuthority_certificateAuthorityConfiguration = Lens.lens (\CreateCertificateAuthority' {certificateAuthorityConfiguration} -> certificateAuthorityConfiguration) (\s@CreateCertificateAuthority' {} a -> s {certificateAuthorityConfiguration = a} :: CreateCertificateAuthority)

-- | The type of the certificate authority.
createCertificateAuthority_certificateAuthorityType :: Lens.Lens' CreateCertificateAuthority CertificateAuthorityType
createCertificateAuthority_certificateAuthorityType = Lens.lens (\CreateCertificateAuthority' {certificateAuthorityType} -> certificateAuthorityType) (\s@CreateCertificateAuthority' {} a -> s {certificateAuthorityType = a} :: CreateCertificateAuthority)

instance
  Prelude.AWSRequest
    CreateCertificateAuthority
  where
  type
    Rs CreateCertificateAuthority =
      CreateCertificateAuthorityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCertificateAuthorityResponse'
            Prelude.<$> (x Prelude..?> "CertificateAuthorityArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCertificateAuthority

instance Prelude.NFData CreateCertificateAuthority

instance Prelude.ToHeaders CreateCertificateAuthority where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ACMPrivateCA.CreateCertificateAuthority" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateCertificateAuthority where
  toJSON CreateCertificateAuthority' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IdempotencyToken" Prelude..=)
              Prelude.<$> idempotencyToken,
            ("RevocationConfiguration" Prelude..=)
              Prelude.<$> revocationConfiguration,
            ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just
              ( "CertificateAuthorityConfiguration"
                  Prelude..= certificateAuthorityConfiguration
              ),
            Prelude.Just
              ( "CertificateAuthorityType"
                  Prelude..= certificateAuthorityType
              )
          ]
      )

instance Prelude.ToPath CreateCertificateAuthority where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateCertificateAuthority where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCertificateAuthorityResponse' smart constructor.
data CreateCertificateAuthorityResponse = CreateCertificateAuthorityResponse'
  { -- | If successful, the Amazon Resource Name (ARN) of the certificate
    -- authority (CA). This is of the form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateCertificateAuthorityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'createCertificateAuthorityResponse_certificateAuthorityArn' - If successful, the Amazon Resource Name (ARN) of the certificate
-- authority (CA). This is of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
--
-- 'httpStatus', 'createCertificateAuthorityResponse_httpStatus' - The response's http status code.
newCreateCertificateAuthorityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCertificateAuthorityResponse
newCreateCertificateAuthorityResponse pHttpStatus_ =
  CreateCertificateAuthorityResponse'
    { certificateAuthorityArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If successful, the Amazon Resource Name (ARN) of the certificate
-- authority (CA). This is of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
createCertificateAuthorityResponse_certificateAuthorityArn :: Lens.Lens' CreateCertificateAuthorityResponse (Prelude.Maybe Prelude.Text)
createCertificateAuthorityResponse_certificateAuthorityArn = Lens.lens (\CreateCertificateAuthorityResponse' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@CreateCertificateAuthorityResponse' {} a -> s {certificateAuthorityArn = a} :: CreateCertificateAuthorityResponse)

-- | The response's http status code.
createCertificateAuthorityResponse_httpStatus :: Lens.Lens' CreateCertificateAuthorityResponse Prelude.Int
createCertificateAuthorityResponse_httpStatus = Lens.lens (\CreateCertificateAuthorityResponse' {httpStatus} -> httpStatus) (\s@CreateCertificateAuthorityResponse' {} a -> s {httpStatus = a} :: CreateCertificateAuthorityResponse)

instance
  Prelude.NFData
    CreateCertificateAuthorityResponse
