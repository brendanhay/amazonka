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
-- Module      : Amazonka.CertificateManagerPCA.GetCertificateAuthorityCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the certificate and certificate chain for your private
-- certificate authority (CA) or one that has been shared with you. Both
-- the certificate and the chain are base64 PEM-encoded. The chain does not
-- include the CA certificate. Each certificate in the chain signs the one
-- before it.
module Amazonka.CertificateManagerPCA.GetCertificateAuthorityCertificate
  ( -- * Creating a Request
    GetCertificateAuthorityCertificate (..),
    newGetCertificateAuthorityCertificate,

    -- * Request Lenses
    getCertificateAuthorityCertificate_certificateAuthorityArn,

    -- * Destructuring the Response
    GetCertificateAuthorityCertificateResponse (..),
    newGetCertificateAuthorityCertificateResponse,

    -- * Response Lenses
    getCertificateAuthorityCertificateResponse_certificate,
    getCertificateAuthorityCertificateResponse_certificateChain,
    getCertificateAuthorityCertificateResponse_httpStatus,
  )
where

import Amazonka.CertificateManagerPCA.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCertificateAuthorityCertificate' smart constructor.
data GetCertificateAuthorityCertificate = GetCertificateAuthorityCertificate'
  { -- | The Amazon Resource Name (ARN) of your private CA. This is of the form:
    --
    -- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @.
    certificateAuthorityArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCertificateAuthorityCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'getCertificateAuthorityCertificate_certificateAuthorityArn' - The Amazon Resource Name (ARN) of your private CA. This is of the form:
--
-- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @.
newGetCertificateAuthorityCertificate ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  GetCertificateAuthorityCertificate
newGetCertificateAuthorityCertificate
  pCertificateAuthorityArn_ =
    GetCertificateAuthorityCertificate'
      { certificateAuthorityArn =
          pCertificateAuthorityArn_
      }

-- | The Amazon Resource Name (ARN) of your private CA. This is of the form:
--
-- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @.
getCertificateAuthorityCertificate_certificateAuthorityArn :: Lens.Lens' GetCertificateAuthorityCertificate Prelude.Text
getCertificateAuthorityCertificate_certificateAuthorityArn = Lens.lens (\GetCertificateAuthorityCertificate' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@GetCertificateAuthorityCertificate' {} a -> s {certificateAuthorityArn = a} :: GetCertificateAuthorityCertificate)

instance
  Core.AWSRequest
    GetCertificateAuthorityCertificate
  where
  type
    AWSResponse GetCertificateAuthorityCertificate =
      GetCertificateAuthorityCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCertificateAuthorityCertificateResponse'
            Prelude.<$> (x Data..?> "Certificate")
            Prelude.<*> (x Data..?> "CertificateChain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetCertificateAuthorityCertificate
  where
  hashWithSalt
    _salt
    GetCertificateAuthorityCertificate' {..} =
      _salt
        `Prelude.hashWithSalt` certificateAuthorityArn

instance
  Prelude.NFData
    GetCertificateAuthorityCertificate
  where
  rnf GetCertificateAuthorityCertificate' {..} =
    Prelude.rnf certificateAuthorityArn

instance
  Data.ToHeaders
    GetCertificateAuthorityCertificate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ACMPrivateCA.GetCertificateAuthorityCertificate" ::
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
    GetCertificateAuthorityCertificate
  where
  toJSON GetCertificateAuthorityCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CertificateAuthorityArn"
                  Data..= certificateAuthorityArn
              )
          ]
      )

instance
  Data.ToPath
    GetCertificateAuthorityCertificate
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetCertificateAuthorityCertificate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCertificateAuthorityCertificateResponse' smart constructor.
data GetCertificateAuthorityCertificateResponse = GetCertificateAuthorityCertificateResponse'
  { -- | Base64-encoded certificate authority (CA) certificate.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | Base64-encoded certificate chain that includes any intermediate
    -- certificates and chains up to root certificate that you used to sign
    -- your private CA certificate. The chain does not include your private CA
    -- certificate. If this is a root CA, the value will be null.
    certificateChain :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCertificateAuthorityCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificate', 'getCertificateAuthorityCertificateResponse_certificate' - Base64-encoded certificate authority (CA) certificate.
--
-- 'certificateChain', 'getCertificateAuthorityCertificateResponse_certificateChain' - Base64-encoded certificate chain that includes any intermediate
-- certificates and chains up to root certificate that you used to sign
-- your private CA certificate. The chain does not include your private CA
-- certificate. If this is a root CA, the value will be null.
--
-- 'httpStatus', 'getCertificateAuthorityCertificateResponse_httpStatus' - The response's http status code.
newGetCertificateAuthorityCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCertificateAuthorityCertificateResponse
newGetCertificateAuthorityCertificateResponse
  pHttpStatus_ =
    GetCertificateAuthorityCertificateResponse'
      { certificate =
          Prelude.Nothing,
        certificateChain =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Base64-encoded certificate authority (CA) certificate.
getCertificateAuthorityCertificateResponse_certificate :: Lens.Lens' GetCertificateAuthorityCertificateResponse (Prelude.Maybe Prelude.Text)
getCertificateAuthorityCertificateResponse_certificate = Lens.lens (\GetCertificateAuthorityCertificateResponse' {certificate} -> certificate) (\s@GetCertificateAuthorityCertificateResponse' {} a -> s {certificate = a} :: GetCertificateAuthorityCertificateResponse)

-- | Base64-encoded certificate chain that includes any intermediate
-- certificates and chains up to root certificate that you used to sign
-- your private CA certificate. The chain does not include your private CA
-- certificate. If this is a root CA, the value will be null.
getCertificateAuthorityCertificateResponse_certificateChain :: Lens.Lens' GetCertificateAuthorityCertificateResponse (Prelude.Maybe Prelude.Text)
getCertificateAuthorityCertificateResponse_certificateChain = Lens.lens (\GetCertificateAuthorityCertificateResponse' {certificateChain} -> certificateChain) (\s@GetCertificateAuthorityCertificateResponse' {} a -> s {certificateChain = a} :: GetCertificateAuthorityCertificateResponse)

-- | The response's http status code.
getCertificateAuthorityCertificateResponse_httpStatus :: Lens.Lens' GetCertificateAuthorityCertificateResponse Prelude.Int
getCertificateAuthorityCertificateResponse_httpStatus = Lens.lens (\GetCertificateAuthorityCertificateResponse' {httpStatus} -> httpStatus) (\s@GetCertificateAuthorityCertificateResponse' {} a -> s {httpStatus = a} :: GetCertificateAuthorityCertificateResponse)

instance
  Prelude.NFData
    GetCertificateAuthorityCertificateResponse
  where
  rnf GetCertificateAuthorityCertificateResponse' {..} =
    Prelude.rnf certificate `Prelude.seq`
      Prelude.rnf certificateChain `Prelude.seq`
        Prelude.rnf httpStatus
