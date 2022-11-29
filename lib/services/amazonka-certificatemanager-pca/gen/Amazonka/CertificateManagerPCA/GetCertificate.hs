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
-- Module      : Amazonka.CertificateManagerPCA.GetCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a certificate from your private CA or one that has been shared
-- with you. The ARN of the certificate is returned when you call the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_IssueCertificate.html IssueCertificate>
-- action. You must specify both the ARN of your private CA and the ARN of
-- the issued certificate when calling the __GetCertificate__ action. You
-- can retrieve the certificate if it is in the __ISSUED__ state. You can
-- call the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport>
-- action to create a report that contains information about all of the
-- certificates issued and revoked by your private CA.
module Amazonka.CertificateManagerPCA.GetCertificate
  ( -- * Creating a Request
    GetCertificate (..),
    newGetCertificate,

    -- * Request Lenses
    getCertificate_certificateAuthorityArn,
    getCertificate_certificateArn,

    -- * Destructuring the Response
    GetCertificateResponse (..),
    newGetCertificateResponse,

    -- * Response Lenses
    getCertificateResponse_certificate,
    getCertificateResponse_certificateChain,
    getCertificateResponse_httpStatus,
  )
where

import Amazonka.CertificateManagerPCA.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCertificate' smart constructor.
data GetCertificate = GetCertificate'
  { -- | The Amazon Resource Name (ARN) that was returned when you called
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
    -- This must be of the form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
    certificateAuthorityArn :: Prelude.Text,
    -- | The ARN of the issued certificate. The ARN contains the certificate
    -- serial number and must be in the following form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012\/certificate\/286535153982981100925020015808220737245 @
    certificateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'getCertificate_certificateAuthorityArn' - The Amazon Resource Name (ARN) that was returned when you called
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
-- This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
--
-- 'certificateArn', 'getCertificate_certificateArn' - The ARN of the issued certificate. The ARN contains the certificate
-- serial number and must be in the following form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012\/certificate\/286535153982981100925020015808220737245 @
newGetCertificate ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  -- | 'certificateArn'
  Prelude.Text ->
  GetCertificate
newGetCertificate
  pCertificateAuthorityArn_
  pCertificateArn_ =
    GetCertificate'
      { certificateAuthorityArn =
          pCertificateAuthorityArn_,
        certificateArn = pCertificateArn_
      }

-- | The Amazon Resource Name (ARN) that was returned when you called
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
-- This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @.
getCertificate_certificateAuthorityArn :: Lens.Lens' GetCertificate Prelude.Text
getCertificate_certificateAuthorityArn = Lens.lens (\GetCertificate' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@GetCertificate' {} a -> s {certificateAuthorityArn = a} :: GetCertificate)

-- | The ARN of the issued certificate. The ARN contains the certificate
-- serial number and must be in the following form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012\/certificate\/286535153982981100925020015808220737245 @
getCertificate_certificateArn :: Lens.Lens' GetCertificate Prelude.Text
getCertificate_certificateArn = Lens.lens (\GetCertificate' {certificateArn} -> certificateArn) (\s@GetCertificate' {} a -> s {certificateArn = a} :: GetCertificate)

instance Core.AWSRequest GetCertificate where
  type
    AWSResponse GetCertificate =
      GetCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCertificateResponse'
            Prelude.<$> (x Core..?> "Certificate")
            Prelude.<*> (x Core..?> "CertificateChain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCertificate where
  hashWithSalt _salt GetCertificate' {..} =
    _salt
      `Prelude.hashWithSalt` certificateAuthorityArn
      `Prelude.hashWithSalt` certificateArn

instance Prelude.NFData GetCertificate where
  rnf GetCertificate' {..} =
    Prelude.rnf certificateAuthorityArn
      `Prelude.seq` Prelude.rnf certificateArn

instance Core.ToHeaders GetCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ACMPrivateCA.GetCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetCertificate where
  toJSON GetCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CertificateAuthorityArn"
                  Core..= certificateAuthorityArn
              ),
            Prelude.Just
              ("CertificateArn" Core..= certificateArn)
          ]
      )

instance Core.ToPath GetCertificate where
  toPath = Prelude.const "/"

instance Core.ToQuery GetCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCertificateResponse' smart constructor.
data GetCertificateResponse = GetCertificateResponse'
  { -- | The base64 PEM-encoded certificate specified by the @CertificateArn@
    -- parameter.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | The base64 PEM-encoded certificate chain that chains up to the root CA
    -- certificate that you used to sign your private CA certificate.
    certificateChain :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificate', 'getCertificateResponse_certificate' - The base64 PEM-encoded certificate specified by the @CertificateArn@
-- parameter.
--
-- 'certificateChain', 'getCertificateResponse_certificateChain' - The base64 PEM-encoded certificate chain that chains up to the root CA
-- certificate that you used to sign your private CA certificate.
--
-- 'httpStatus', 'getCertificateResponse_httpStatus' - The response's http status code.
newGetCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCertificateResponse
newGetCertificateResponse pHttpStatus_ =
  GetCertificateResponse'
    { certificate =
        Prelude.Nothing,
      certificateChain = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The base64 PEM-encoded certificate specified by the @CertificateArn@
-- parameter.
getCertificateResponse_certificate :: Lens.Lens' GetCertificateResponse (Prelude.Maybe Prelude.Text)
getCertificateResponse_certificate = Lens.lens (\GetCertificateResponse' {certificate} -> certificate) (\s@GetCertificateResponse' {} a -> s {certificate = a} :: GetCertificateResponse)

-- | The base64 PEM-encoded certificate chain that chains up to the root CA
-- certificate that you used to sign your private CA certificate.
getCertificateResponse_certificateChain :: Lens.Lens' GetCertificateResponse (Prelude.Maybe Prelude.Text)
getCertificateResponse_certificateChain = Lens.lens (\GetCertificateResponse' {certificateChain} -> certificateChain) (\s@GetCertificateResponse' {} a -> s {certificateChain = a} :: GetCertificateResponse)

-- | The response's http status code.
getCertificateResponse_httpStatus :: Lens.Lens' GetCertificateResponse Prelude.Int
getCertificateResponse_httpStatus = Lens.lens (\GetCertificateResponse' {httpStatus} -> httpStatus) (\s@GetCertificateResponse' {} a -> s {httpStatus = a} :: GetCertificateResponse)

instance Prelude.NFData GetCertificateResponse where
  rnf GetCertificateResponse' {..} =
    Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf certificateChain
      `Prelude.seq` Prelude.rnf httpStatus
