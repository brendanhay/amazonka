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
-- Module      : Amazonka.CertificateManagerPCA.UpdateCertificateAuthority
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status or configuration of a private certificate authority
-- (CA). Your private CA must be in the @ACTIVE@ or @DISABLED@ state before
-- you can update it. You can disable a private CA that is in the @ACTIVE@
-- state or make a CA that is in the @DISABLED@ state active again.
--
-- Both Amazon Web Services Private CA and the IAM principal must have
-- permission to write to the S3 bucket that you specify. If the IAM
-- principal making the call does not have permission to write to the
-- bucket, then an exception is thrown. For more information, see
-- <https://docs.aws.amazon.com/privateca/latest/userguide/crl-planning.html#s3-policies Access policies for CRLs in Amazon S3>.
module Amazonka.CertificateManagerPCA.UpdateCertificateAuthority
  ( -- * Creating a Request
    UpdateCertificateAuthority (..),
    newUpdateCertificateAuthority,

    -- * Request Lenses
    updateCertificateAuthority_revocationConfiguration,
    updateCertificateAuthority_status,
    updateCertificateAuthority_certificateAuthorityArn,

    -- * Destructuring the Response
    UpdateCertificateAuthorityResponse (..),
    newUpdateCertificateAuthorityResponse,
  )
where

import Amazonka.CertificateManagerPCA.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCertificateAuthority' smart constructor.
data UpdateCertificateAuthority = UpdateCertificateAuthority'
  { -- | Contains information to enable Online Certificate Status Protocol (OCSP)
    -- support, to enable a certificate revocation list (CRL), to enable both,
    -- or to enable neither. If this parameter is not supplied, existing
    -- capibilites remain unchanged. For more information, see the
    -- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_OcspConfiguration.html OcspConfiguration>
    -- and
    -- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration>
    -- types.
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
    revocationConfiguration :: Prelude.Maybe RevocationConfiguration,
    -- | Status of your private CA.
    status :: Prelude.Maybe CertificateAuthorityStatus,
    -- | Amazon Resource Name (ARN) of the private CA that issued the certificate
    -- to be revoked. This must be of the form:
    --
    -- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @
    certificateAuthorityArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCertificateAuthority' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revocationConfiguration', 'updateCertificateAuthority_revocationConfiguration' - Contains information to enable Online Certificate Status Protocol (OCSP)
-- support, to enable a certificate revocation list (CRL), to enable both,
-- or to enable neither. If this parameter is not supplied, existing
-- capibilites remain unchanged. For more information, see the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_OcspConfiguration.html OcspConfiguration>
-- and
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration>
-- types.
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
-- 'status', 'updateCertificateAuthority_status' - Status of your private CA.
--
-- 'certificateAuthorityArn', 'updateCertificateAuthority_certificateAuthorityArn' - Amazon Resource Name (ARN) of the private CA that issued the certificate
-- to be revoked. This must be of the form:
--
-- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @
newUpdateCertificateAuthority ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  UpdateCertificateAuthority
newUpdateCertificateAuthority
  pCertificateAuthorityArn_ =
    UpdateCertificateAuthority'
      { revocationConfiguration =
          Prelude.Nothing,
        status = Prelude.Nothing,
        certificateAuthorityArn =
          pCertificateAuthorityArn_
      }

-- | Contains information to enable Online Certificate Status Protocol (OCSP)
-- support, to enable a certificate revocation list (CRL), to enable both,
-- or to enable neither. If this parameter is not supplied, existing
-- capibilites remain unchanged. For more information, see the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_OcspConfiguration.html OcspConfiguration>
-- and
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration>
-- types.
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
updateCertificateAuthority_revocationConfiguration :: Lens.Lens' UpdateCertificateAuthority (Prelude.Maybe RevocationConfiguration)
updateCertificateAuthority_revocationConfiguration = Lens.lens (\UpdateCertificateAuthority' {revocationConfiguration} -> revocationConfiguration) (\s@UpdateCertificateAuthority' {} a -> s {revocationConfiguration = a} :: UpdateCertificateAuthority)

-- | Status of your private CA.
updateCertificateAuthority_status :: Lens.Lens' UpdateCertificateAuthority (Prelude.Maybe CertificateAuthorityStatus)
updateCertificateAuthority_status = Lens.lens (\UpdateCertificateAuthority' {status} -> status) (\s@UpdateCertificateAuthority' {} a -> s {status = a} :: UpdateCertificateAuthority)

-- | Amazon Resource Name (ARN) of the private CA that issued the certificate
-- to be revoked. This must be of the form:
--
-- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @
updateCertificateAuthority_certificateAuthorityArn :: Lens.Lens' UpdateCertificateAuthority Prelude.Text
updateCertificateAuthority_certificateAuthorityArn = Lens.lens (\UpdateCertificateAuthority' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@UpdateCertificateAuthority' {} a -> s {certificateAuthorityArn = a} :: UpdateCertificateAuthority)

instance Core.AWSRequest UpdateCertificateAuthority where
  type
    AWSResponse UpdateCertificateAuthority =
      UpdateCertificateAuthorityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateCertificateAuthorityResponse'

instance Prelude.Hashable UpdateCertificateAuthority where
  hashWithSalt _salt UpdateCertificateAuthority' {..} =
    _salt
      `Prelude.hashWithSalt` revocationConfiguration
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` certificateAuthorityArn

instance Prelude.NFData UpdateCertificateAuthority where
  rnf UpdateCertificateAuthority' {..} =
    Prelude.rnf revocationConfiguration
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf certificateAuthorityArn

instance Data.ToHeaders UpdateCertificateAuthority where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ACMPrivateCA.UpdateCertificateAuthority" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCertificateAuthority where
  toJSON UpdateCertificateAuthority' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RevocationConfiguration" Data..=)
              Prelude.<$> revocationConfiguration,
            ("Status" Data..=) Prelude.<$> status,
            Prelude.Just
              ( "CertificateAuthorityArn"
                  Data..= certificateAuthorityArn
              )
          ]
      )

instance Data.ToPath UpdateCertificateAuthority where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateCertificateAuthority where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCertificateAuthorityResponse' smart constructor.
data UpdateCertificateAuthorityResponse = UpdateCertificateAuthorityResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCertificateAuthorityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateCertificateAuthorityResponse ::
  UpdateCertificateAuthorityResponse
newUpdateCertificateAuthorityResponse =
  UpdateCertificateAuthorityResponse'

instance
  Prelude.NFData
    UpdateCertificateAuthorityResponse
  where
  rnf _ = ()
