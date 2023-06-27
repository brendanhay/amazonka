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
-- Module      : Amazonka.CertificateManagerPCA.CreateCertificateAuthority
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a root or subordinate private certificate authority (CA). You
-- must specify the CA configuration, an optional configuration for Online
-- Certificate Status Protocol (OCSP) and\/or a certificate revocation list
-- (CRL), the CA type, and an optional idempotency token to avoid
-- accidental creation of multiple CAs. The CA configuration specifies the
-- name of the algorithm and key size to be used to create the CA private
-- key, the type of signing algorithm that the CA uses, and X.500 subject
-- information. The OCSP configuration can optionally specify a custom URL
-- for the OCSP responder. The CRL configuration specifies the CRL
-- expiration period in days (the validity period of the CRL), the Amazon
-- S3 bucket that will contain the CRL, and a CNAME alias for the S3 bucket
-- that is included in certificates issued by the CA. If successful, this
-- action returns the Amazon Resource Name (ARN) of the CA.
--
-- Both Amazon Web Services Private CA and the IAM principal must have
-- permission to write to the S3 bucket that you specify. If the IAM
-- principal making the call does not have permission to write to the
-- bucket, then an exception is thrown. For more information, see
-- <https://docs.aws.amazon.com/privateca/latest/userguide/crl-planning.html#s3-policies Access policies for CRLs in Amazon S3>.
--
-- Amazon Web Services Private CA assets that are stored in Amazon S3 can
-- be protected with encryption. For more information, see
-- <https://docs.aws.amazon.com/privateca/latest/userguide/PcaCreateCa.html#crl-encryption Encrypting Your CRLs>.
module Amazonka.CertificateManagerPCA.CreateCertificateAuthority
  ( -- * Creating a Request
    CreateCertificateAuthority (..),
    newCreateCertificateAuthority,

    -- * Request Lenses
    createCertificateAuthority_idempotencyToken,
    createCertificateAuthority_keyStorageSecurityStandard,
    createCertificateAuthority_revocationConfiguration,
    createCertificateAuthority_tags,
    createCertificateAuthority_usageMode,
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

import Amazonka.CertificateManagerPCA.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCertificateAuthority' smart constructor.
data CreateCertificateAuthority = CreateCertificateAuthority'
  { -- | Custom string that can be used to distinguish between calls to the
    -- __CreateCertificateAuthority__ action. Idempotency tokens for
    -- __CreateCertificateAuthority__ time out after five minutes. Therefore,
    -- if you call __CreateCertificateAuthority__ multiple times with the same
    -- idempotency token within five minutes, Amazon Web Services Private CA
    -- recognizes that you are requesting only certificate authority and will
    -- issue only one. If you change the idempotency token for each call,
    -- Amazon Web Services Private CA recognizes that you are requesting
    -- multiple certificate authorities.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies a cryptographic key management compliance standard used for
    -- handling CA keys.
    --
    -- Default: FIPS_140_2_LEVEL_3_OR_HIGHER
    --
    -- Some Amazon Web Services Regions do not support the default. When
    -- creating a CA in these Regions, you must provide
    -- @FIPS_140_2_LEVEL_2_OR_HIGHER@ as the argument for
    -- @KeyStorageSecurityStandard@. Failure to do this results in an
    -- @InvalidArgsException@ with the message, \"A certificate authority
    -- cannot be created in this region with the specified security standard.\"
    --
    -- For information about security standard support in various Regions, see
    -- <https://docs.aws.amazon.com/privateca/latest/userguide/data-protection.html#private-keys Storage and security compliance of Amazon Web Services Private CA private keys>.
    keyStorageSecurityStandard :: Prelude.Maybe KeyStorageSecurityStandard,
    -- | Contains information to enable Online Certificate Status Protocol (OCSP)
    -- support, to enable a certificate revocation list (CRL), to enable both,
    -- or to enable neither. The default is for both certificate validation
    -- mechanisms to be disabled.
    --
    -- The following requirements apply to revocation configurations.
    --
    -- -   A configuration disabling CRLs or OCSP must contain only the
    --     @Enabled=False@ parameter, and will fail if other parameters such as
    --     @CustomCname@ or @ExpirationInDays@ are included.
    --
    -- -   In a CRL configuration, the @S3BucketName@ parameter must conform to
    --     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucketnamingrules.html Amazon S3 bucket naming rules>.
    --
    -- -   A configuration containing a custom Canonical Name (CNAME) parameter
    --     for CRLs or OCSP must conform to
    --     <https://www.ietf.org/rfc/rfc2396.txt RFC2396> restrictions on the
    --     use of special characters in a CNAME.
    --
    -- -   In a CRL or OCSP configuration, the value of a CNAME parameter must
    --     not include a protocol prefix such as \"http:\/\/\" or
    --     \"https:\/\/\".
    --
    -- For more information, see the
    -- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_OcspConfiguration.html OcspConfiguration>
    -- and
    -- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration>
    -- types.
    revocationConfiguration :: Prelude.Maybe RevocationConfiguration,
    -- | Key-value pairs that will be attached to the new private CA. You can
    -- associate up to 50 tags with a private CA. For information using tags
    -- with IAM to manage permissions, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags>.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Specifies whether the CA issues general-purpose certificates that
    -- typically require a revocation mechanism, or short-lived certificates
    -- that may optionally omit revocation because they expire quickly.
    -- Short-lived certificate validity is limited to seven days.
    --
    -- The default value is GENERAL_PURPOSE.
    usageMode :: Prelude.Maybe CertificateAuthorityUsageMode,
    -- | Name and bit size of the private key algorithm, the name of the signing
    -- algorithm, and X.500 certificate subject information.
    certificateAuthorityConfiguration :: CertificateAuthorityConfiguration,
    -- | The type of the certificate authority.
    certificateAuthorityType :: CertificateAuthorityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- idempotency token within five minutes, Amazon Web Services Private CA
-- recognizes that you are requesting only certificate authority and will
-- issue only one. If you change the idempotency token for each call,
-- Amazon Web Services Private CA recognizes that you are requesting
-- multiple certificate authorities.
--
-- 'keyStorageSecurityStandard', 'createCertificateAuthority_keyStorageSecurityStandard' - Specifies a cryptographic key management compliance standard used for
-- handling CA keys.
--
-- Default: FIPS_140_2_LEVEL_3_OR_HIGHER
--
-- Some Amazon Web Services Regions do not support the default. When
-- creating a CA in these Regions, you must provide
-- @FIPS_140_2_LEVEL_2_OR_HIGHER@ as the argument for
-- @KeyStorageSecurityStandard@. Failure to do this results in an
-- @InvalidArgsException@ with the message, \"A certificate authority
-- cannot be created in this region with the specified security standard.\"
--
-- For information about security standard support in various Regions, see
-- <https://docs.aws.amazon.com/privateca/latest/userguide/data-protection.html#private-keys Storage and security compliance of Amazon Web Services Private CA private keys>.
--
-- 'revocationConfiguration', 'createCertificateAuthority_revocationConfiguration' - Contains information to enable Online Certificate Status Protocol (OCSP)
-- support, to enable a certificate revocation list (CRL), to enable both,
-- or to enable neither. The default is for both certificate validation
-- mechanisms to be disabled.
--
-- The following requirements apply to revocation configurations.
--
-- -   A configuration disabling CRLs or OCSP must contain only the
--     @Enabled=False@ parameter, and will fail if other parameters such as
--     @CustomCname@ or @ExpirationInDays@ are included.
--
-- -   In a CRL configuration, the @S3BucketName@ parameter must conform to
--     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucketnamingrules.html Amazon S3 bucket naming rules>.
--
-- -   A configuration containing a custom Canonical Name (CNAME) parameter
--     for CRLs or OCSP must conform to
--     <https://www.ietf.org/rfc/rfc2396.txt RFC2396> restrictions on the
--     use of special characters in a CNAME.
--
-- -   In a CRL or OCSP configuration, the value of a CNAME parameter must
--     not include a protocol prefix such as \"http:\/\/\" or
--     \"https:\/\/\".
--
-- For more information, see the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_OcspConfiguration.html OcspConfiguration>
-- and
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration>
-- types.
--
-- 'tags', 'createCertificateAuthority_tags' - Key-value pairs that will be attached to the new private CA. You can
-- associate up to 50 tags with a private CA. For information using tags
-- with IAM to manage permissions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags>.
--
-- 'usageMode', 'createCertificateAuthority_usageMode' - Specifies whether the CA issues general-purpose certificates that
-- typically require a revocation mechanism, or short-lived certificates
-- that may optionally omit revocation because they expire quickly.
-- Short-lived certificate validity is limited to seven days.
--
-- The default value is GENERAL_PURPOSE.
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
        keyStorageSecurityStandard = Prelude.Nothing,
        revocationConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        usageMode = Prelude.Nothing,
        certificateAuthorityConfiguration =
          pCertificateAuthorityConfiguration_,
        certificateAuthorityType =
          pCertificateAuthorityType_
      }

-- | Custom string that can be used to distinguish between calls to the
-- __CreateCertificateAuthority__ action. Idempotency tokens for
-- __CreateCertificateAuthority__ time out after five minutes. Therefore,
-- if you call __CreateCertificateAuthority__ multiple times with the same
-- idempotency token within five minutes, Amazon Web Services Private CA
-- recognizes that you are requesting only certificate authority and will
-- issue only one. If you change the idempotency token for each call,
-- Amazon Web Services Private CA recognizes that you are requesting
-- multiple certificate authorities.
createCertificateAuthority_idempotencyToken :: Lens.Lens' CreateCertificateAuthority (Prelude.Maybe Prelude.Text)
createCertificateAuthority_idempotencyToken = Lens.lens (\CreateCertificateAuthority' {idempotencyToken} -> idempotencyToken) (\s@CreateCertificateAuthority' {} a -> s {idempotencyToken = a} :: CreateCertificateAuthority)

-- | Specifies a cryptographic key management compliance standard used for
-- handling CA keys.
--
-- Default: FIPS_140_2_LEVEL_3_OR_HIGHER
--
-- Some Amazon Web Services Regions do not support the default. When
-- creating a CA in these Regions, you must provide
-- @FIPS_140_2_LEVEL_2_OR_HIGHER@ as the argument for
-- @KeyStorageSecurityStandard@. Failure to do this results in an
-- @InvalidArgsException@ with the message, \"A certificate authority
-- cannot be created in this region with the specified security standard.\"
--
-- For information about security standard support in various Regions, see
-- <https://docs.aws.amazon.com/privateca/latest/userguide/data-protection.html#private-keys Storage and security compliance of Amazon Web Services Private CA private keys>.
createCertificateAuthority_keyStorageSecurityStandard :: Lens.Lens' CreateCertificateAuthority (Prelude.Maybe KeyStorageSecurityStandard)
createCertificateAuthority_keyStorageSecurityStandard = Lens.lens (\CreateCertificateAuthority' {keyStorageSecurityStandard} -> keyStorageSecurityStandard) (\s@CreateCertificateAuthority' {} a -> s {keyStorageSecurityStandard = a} :: CreateCertificateAuthority)

-- | Contains information to enable Online Certificate Status Protocol (OCSP)
-- support, to enable a certificate revocation list (CRL), to enable both,
-- or to enable neither. The default is for both certificate validation
-- mechanisms to be disabled.
--
-- The following requirements apply to revocation configurations.
--
-- -   A configuration disabling CRLs or OCSP must contain only the
--     @Enabled=False@ parameter, and will fail if other parameters such as
--     @CustomCname@ or @ExpirationInDays@ are included.
--
-- -   In a CRL configuration, the @S3BucketName@ parameter must conform to
--     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucketnamingrules.html Amazon S3 bucket naming rules>.
--
-- -   A configuration containing a custom Canonical Name (CNAME) parameter
--     for CRLs or OCSP must conform to
--     <https://www.ietf.org/rfc/rfc2396.txt RFC2396> restrictions on the
--     use of special characters in a CNAME.
--
-- -   In a CRL or OCSP configuration, the value of a CNAME parameter must
--     not include a protocol prefix such as \"http:\/\/\" or
--     \"https:\/\/\".
--
-- For more information, see the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_OcspConfiguration.html OcspConfiguration>
-- and
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration>
-- types.
createCertificateAuthority_revocationConfiguration :: Lens.Lens' CreateCertificateAuthority (Prelude.Maybe RevocationConfiguration)
createCertificateAuthority_revocationConfiguration = Lens.lens (\CreateCertificateAuthority' {revocationConfiguration} -> revocationConfiguration) (\s@CreateCertificateAuthority' {} a -> s {revocationConfiguration = a} :: CreateCertificateAuthority)

-- | Key-value pairs that will be attached to the new private CA. You can
-- associate up to 50 tags with a private CA. For information using tags
-- with IAM to manage permissions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags>.
createCertificateAuthority_tags :: Lens.Lens' CreateCertificateAuthority (Prelude.Maybe (Prelude.NonEmpty Tag))
createCertificateAuthority_tags = Lens.lens (\CreateCertificateAuthority' {tags} -> tags) (\s@CreateCertificateAuthority' {} a -> s {tags = a} :: CreateCertificateAuthority) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the CA issues general-purpose certificates that
-- typically require a revocation mechanism, or short-lived certificates
-- that may optionally omit revocation because they expire quickly.
-- Short-lived certificate validity is limited to seven days.
--
-- The default value is GENERAL_PURPOSE.
createCertificateAuthority_usageMode :: Lens.Lens' CreateCertificateAuthority (Prelude.Maybe CertificateAuthorityUsageMode)
createCertificateAuthority_usageMode = Lens.lens (\CreateCertificateAuthority' {usageMode} -> usageMode) (\s@CreateCertificateAuthority' {} a -> s {usageMode = a} :: CreateCertificateAuthority)

-- | Name and bit size of the private key algorithm, the name of the signing
-- algorithm, and X.500 certificate subject information.
createCertificateAuthority_certificateAuthorityConfiguration :: Lens.Lens' CreateCertificateAuthority CertificateAuthorityConfiguration
createCertificateAuthority_certificateAuthorityConfiguration = Lens.lens (\CreateCertificateAuthority' {certificateAuthorityConfiguration} -> certificateAuthorityConfiguration) (\s@CreateCertificateAuthority' {} a -> s {certificateAuthorityConfiguration = a} :: CreateCertificateAuthority)

-- | The type of the certificate authority.
createCertificateAuthority_certificateAuthorityType :: Lens.Lens' CreateCertificateAuthority CertificateAuthorityType
createCertificateAuthority_certificateAuthorityType = Lens.lens (\CreateCertificateAuthority' {certificateAuthorityType} -> certificateAuthorityType) (\s@CreateCertificateAuthority' {} a -> s {certificateAuthorityType = a} :: CreateCertificateAuthority)

instance Core.AWSRequest CreateCertificateAuthority where
  type
    AWSResponse CreateCertificateAuthority =
      CreateCertificateAuthorityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCertificateAuthorityResponse'
            Prelude.<$> (x Data..?> "CertificateAuthorityArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCertificateAuthority where
  hashWithSalt _salt CreateCertificateAuthority' {..} =
    _salt
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` keyStorageSecurityStandard
      `Prelude.hashWithSalt` revocationConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` usageMode
      `Prelude.hashWithSalt` certificateAuthorityConfiguration
      `Prelude.hashWithSalt` certificateAuthorityType

instance Prelude.NFData CreateCertificateAuthority where
  rnf CreateCertificateAuthority' {..} =
    Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf keyStorageSecurityStandard
      `Prelude.seq` Prelude.rnf revocationConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf usageMode
      `Prelude.seq` Prelude.rnf certificateAuthorityConfiguration
      `Prelude.seq` Prelude.rnf certificateAuthorityType

instance Data.ToHeaders CreateCertificateAuthority where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ACMPrivateCA.CreateCertificateAuthority" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCertificateAuthority where
  toJSON CreateCertificateAuthority' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken,
            ("KeyStorageSecurityStandard" Data..=)
              Prelude.<$> keyStorageSecurityStandard,
            ("RevocationConfiguration" Data..=)
              Prelude.<$> revocationConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            ("UsageMode" Data..=) Prelude.<$> usageMode,
            Prelude.Just
              ( "CertificateAuthorityConfiguration"
                  Data..= certificateAuthorityConfiguration
              ),
            Prelude.Just
              ( "CertificateAuthorityType"
                  Data..= certificateAuthorityType
              )
          ]
      )

instance Data.ToPath CreateCertificateAuthority where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCertificateAuthority where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCertificateAuthorityResponse' smart constructor.
data CreateCertificateAuthorityResponse = CreateCertificateAuthorityResponse'
  { -- | If successful, the Amazon Resource Name (ARN) of the certificate
    -- authority (CA). This is of the form:
    --
    -- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @.
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @.
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
-- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @.
createCertificateAuthorityResponse_certificateAuthorityArn :: Lens.Lens' CreateCertificateAuthorityResponse (Prelude.Maybe Prelude.Text)
createCertificateAuthorityResponse_certificateAuthorityArn = Lens.lens (\CreateCertificateAuthorityResponse' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@CreateCertificateAuthorityResponse' {} a -> s {certificateAuthorityArn = a} :: CreateCertificateAuthorityResponse)

-- | The response's http status code.
createCertificateAuthorityResponse_httpStatus :: Lens.Lens' CreateCertificateAuthorityResponse Prelude.Int
createCertificateAuthorityResponse_httpStatus = Lens.lens (\CreateCertificateAuthorityResponse' {httpStatus} -> httpStatus) (\s@CreateCertificateAuthorityResponse' {} a -> s {httpStatus = a} :: CreateCertificateAuthorityResponse)

instance
  Prelude.NFData
    CreateCertificateAuthorityResponse
  where
  rnf CreateCertificateAuthorityResponse' {..} =
    Prelude.rnf certificateAuthorityArn
      `Prelude.seq` Prelude.rnf httpStatus
