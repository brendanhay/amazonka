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
-- Module      : Network.AWS.CertificateManagerPCA.GetCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CertificateManagerPCA.GetCertificate
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
    getCertificateResponse_certificateChain,
    getCertificateResponse_certificate,
    getCertificateResponse_httpStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetCertificate where
  type Rs GetCertificate = GetCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCertificateResponse'
            Prelude.<$> (x Prelude..?> "CertificateChain")
            Prelude.<*> (x Prelude..?> "Certificate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCertificate

instance Prelude.NFData GetCertificate

instance Prelude.ToHeaders GetCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ACMPrivateCA.GetCertificate" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetCertificate where
  toJSON GetCertificate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CertificateAuthorityArn"
                  Prelude..= certificateAuthorityArn
              ),
            Prelude.Just
              ("CertificateArn" Prelude..= certificateArn)
          ]
      )

instance Prelude.ToPath GetCertificate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCertificateResponse' smart constructor.
data GetCertificateResponse = GetCertificateResponse'
  { -- | The base64 PEM-encoded certificate chain that chains up to the root CA
    -- certificate that you used to sign your private CA certificate.
    certificateChain :: Prelude.Maybe Prelude.Text,
    -- | The base64 PEM-encoded certificate specified by the @CertificateArn@
    -- parameter.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateChain', 'getCertificateResponse_certificateChain' - The base64 PEM-encoded certificate chain that chains up to the root CA
-- certificate that you used to sign your private CA certificate.
--
-- 'certificate', 'getCertificateResponse_certificate' - The base64 PEM-encoded certificate specified by the @CertificateArn@
-- parameter.
--
-- 'httpStatus', 'getCertificateResponse_httpStatus' - The response's http status code.
newGetCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCertificateResponse
newGetCertificateResponse pHttpStatus_ =
  GetCertificateResponse'
    { certificateChain =
        Prelude.Nothing,
      certificate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The base64 PEM-encoded certificate chain that chains up to the root CA
-- certificate that you used to sign your private CA certificate.
getCertificateResponse_certificateChain :: Lens.Lens' GetCertificateResponse (Prelude.Maybe Prelude.Text)
getCertificateResponse_certificateChain = Lens.lens (\GetCertificateResponse' {certificateChain} -> certificateChain) (\s@GetCertificateResponse' {} a -> s {certificateChain = a} :: GetCertificateResponse)

-- | The base64 PEM-encoded certificate specified by the @CertificateArn@
-- parameter.
getCertificateResponse_certificate :: Lens.Lens' GetCertificateResponse (Prelude.Maybe Prelude.Text)
getCertificateResponse_certificate = Lens.lens (\GetCertificateResponse' {certificate} -> certificate) (\s@GetCertificateResponse' {} a -> s {certificate = a} :: GetCertificateResponse)

-- | The response's http status code.
getCertificateResponse_httpStatus :: Lens.Lens' GetCertificateResponse Prelude.Int
getCertificateResponse_httpStatus = Lens.lens (\GetCertificateResponse' {httpStatus} -> httpStatus) (\s@GetCertificateResponse' {} a -> s {httpStatus = a} :: GetCertificateResponse)

instance Prelude.NFData GetCertificateResponse
