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
-- Module      : Amazonka.CertificateManagerPCA.GetCertificateAuthorityCsr
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the certificate signing request (CSR) for your private
-- certificate authority (CA). The CSR is created when you call the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
-- action. Sign the CSR with your Amazon Web Services Private CA-hosted or
-- on-premises root or subordinate CA. Then import the signed certificate
-- back into Amazon Web Services Private CA by calling the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_ImportCertificateAuthorityCertificate.html ImportCertificateAuthorityCertificate>
-- action. The CSR is returned as a base64 PEM-encoded string.
module Amazonka.CertificateManagerPCA.GetCertificateAuthorityCsr
  ( -- * Creating a Request
    GetCertificateAuthorityCsr (..),
    newGetCertificateAuthorityCsr,

    -- * Request Lenses
    getCertificateAuthorityCsr_certificateAuthorityArn,

    -- * Destructuring the Response
    GetCertificateAuthorityCsrResponse (..),
    newGetCertificateAuthorityCsrResponse,

    -- * Response Lenses
    getCertificateAuthorityCsrResponse_csr,
    getCertificateAuthorityCsrResponse_httpStatus,
  )
where

import Amazonka.CertificateManagerPCA.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCertificateAuthorityCsr' smart constructor.
data GetCertificateAuthorityCsr = GetCertificateAuthorityCsr'
  { -- | The Amazon Resource Name (ARN) that was returned when you called the
    -- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
    -- action. This must be of the form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
    certificateAuthorityArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCertificateAuthorityCsr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'getCertificateAuthorityCsr_certificateAuthorityArn' - The Amazon Resource Name (ARN) that was returned when you called the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
-- action. This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
newGetCertificateAuthorityCsr ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  GetCertificateAuthorityCsr
newGetCertificateAuthorityCsr
  pCertificateAuthorityArn_ =
    GetCertificateAuthorityCsr'
      { certificateAuthorityArn =
          pCertificateAuthorityArn_
      }

-- | The Amazon Resource Name (ARN) that was returned when you called the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
-- action. This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
getCertificateAuthorityCsr_certificateAuthorityArn :: Lens.Lens' GetCertificateAuthorityCsr Prelude.Text
getCertificateAuthorityCsr_certificateAuthorityArn = Lens.lens (\GetCertificateAuthorityCsr' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@GetCertificateAuthorityCsr' {} a -> s {certificateAuthorityArn = a} :: GetCertificateAuthorityCsr)

instance Core.AWSRequest GetCertificateAuthorityCsr where
  type
    AWSResponse GetCertificateAuthorityCsr =
      GetCertificateAuthorityCsrResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCertificateAuthorityCsrResponse'
            Prelude.<$> (x Data..?> "Csr")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCertificateAuthorityCsr where
  hashWithSalt _salt GetCertificateAuthorityCsr' {..} =
    _salt
      `Prelude.hashWithSalt` certificateAuthorityArn

instance Prelude.NFData GetCertificateAuthorityCsr where
  rnf GetCertificateAuthorityCsr' {..} =
    Prelude.rnf certificateAuthorityArn

instance Data.ToHeaders GetCertificateAuthorityCsr where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ACMPrivateCA.GetCertificateAuthorityCsr" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCertificateAuthorityCsr where
  toJSON GetCertificateAuthorityCsr' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CertificateAuthorityArn"
                  Data..= certificateAuthorityArn
              )
          ]
      )

instance Data.ToPath GetCertificateAuthorityCsr where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCertificateAuthorityCsr where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCertificateAuthorityCsrResponse' smart constructor.
data GetCertificateAuthorityCsrResponse = GetCertificateAuthorityCsrResponse'
  { -- | The base64 PEM-encoded certificate signing request (CSR) for your
    -- private CA certificate.
    csr :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCertificateAuthorityCsrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csr', 'getCertificateAuthorityCsrResponse_csr' - The base64 PEM-encoded certificate signing request (CSR) for your
-- private CA certificate.
--
-- 'httpStatus', 'getCertificateAuthorityCsrResponse_httpStatus' - The response's http status code.
newGetCertificateAuthorityCsrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCertificateAuthorityCsrResponse
newGetCertificateAuthorityCsrResponse pHttpStatus_ =
  GetCertificateAuthorityCsrResponse'
    { csr =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The base64 PEM-encoded certificate signing request (CSR) for your
-- private CA certificate.
getCertificateAuthorityCsrResponse_csr :: Lens.Lens' GetCertificateAuthorityCsrResponse (Prelude.Maybe Prelude.Text)
getCertificateAuthorityCsrResponse_csr = Lens.lens (\GetCertificateAuthorityCsrResponse' {csr} -> csr) (\s@GetCertificateAuthorityCsrResponse' {} a -> s {csr = a} :: GetCertificateAuthorityCsrResponse)

-- | The response's http status code.
getCertificateAuthorityCsrResponse_httpStatus :: Lens.Lens' GetCertificateAuthorityCsrResponse Prelude.Int
getCertificateAuthorityCsrResponse_httpStatus = Lens.lens (\GetCertificateAuthorityCsrResponse' {httpStatus} -> httpStatus) (\s@GetCertificateAuthorityCsrResponse' {} a -> s {httpStatus = a} :: GetCertificateAuthorityCsrResponse)

instance
  Prelude.NFData
    GetCertificateAuthorityCsrResponse
  where
  rnf GetCertificateAuthorityCsrResponse' {..} =
    Prelude.rnf csr
      `Prelude.seq` Prelude.rnf httpStatus
