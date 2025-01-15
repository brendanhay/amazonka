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
-- Module      : Amazonka.CertificateManager.GetCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an Amazon-issued certificate and its certificate chain. The
-- chain consists of the certificate of the issuing CA and the intermediate
-- certificates of any other subordinate CAs. All of the certificates are
-- base64 encoded. You can use
-- <https://wiki.openssl.org/index.php/Command_Line_Utilities OpenSSL> to
-- decode the certificates and inspect individual fields.
module Amazonka.CertificateManager.GetCertificate
  ( -- * Creating a Request
    GetCertificate (..),
    newGetCertificate,

    -- * Request Lenses
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

import Amazonka.CertificateManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCertificate' smart constructor.
data GetCertificate = GetCertificate'
  { -- | String that contains a certificate ARN in the following format:
    --
    -- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
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
-- 'certificateArn', 'getCertificate_certificateArn' - String that contains a certificate ARN in the following format:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
newGetCertificate ::
  -- | 'certificateArn'
  Prelude.Text ->
  GetCertificate
newGetCertificate pCertificateArn_ =
  GetCertificate' {certificateArn = pCertificateArn_}

-- | String that contains a certificate ARN in the following format:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
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
            Prelude.<$> (x Data..?> "Certificate")
            Prelude.<*> (x Data..?> "CertificateChain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCertificate where
  hashWithSalt _salt GetCertificate' {..} =
    _salt `Prelude.hashWithSalt` certificateArn

instance Prelude.NFData GetCertificate where
  rnf GetCertificate' {..} = Prelude.rnf certificateArn

instance Data.ToHeaders GetCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CertificateManager.GetCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCertificate where
  toJSON GetCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CertificateArn" Data..= certificateArn)
          ]
      )

instance Data.ToPath GetCertificate where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCertificateResponse' smart constructor.
data GetCertificateResponse = GetCertificateResponse'
  { -- | The ACM-issued certificate corresponding to the ARN specified as input.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | Certificates forming the requested certificate\'s chain of trust. The
    -- chain consists of the certificate of the issuing CA and the intermediate
    -- certificates of any other subordinate CAs.
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
-- 'certificate', 'getCertificateResponse_certificate' - The ACM-issued certificate corresponding to the ARN specified as input.
--
-- 'certificateChain', 'getCertificateResponse_certificateChain' - Certificates forming the requested certificate\'s chain of trust. The
-- chain consists of the certificate of the issuing CA and the intermediate
-- certificates of any other subordinate CAs.
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

-- | The ACM-issued certificate corresponding to the ARN specified as input.
getCertificateResponse_certificate :: Lens.Lens' GetCertificateResponse (Prelude.Maybe Prelude.Text)
getCertificateResponse_certificate = Lens.lens (\GetCertificateResponse' {certificate} -> certificate) (\s@GetCertificateResponse' {} a -> s {certificate = a} :: GetCertificateResponse)

-- | Certificates forming the requested certificate\'s chain of trust. The
-- chain consists of the certificate of the issuing CA and the intermediate
-- certificates of any other subordinate CAs.
getCertificateResponse_certificateChain :: Lens.Lens' GetCertificateResponse (Prelude.Maybe Prelude.Text)
getCertificateResponse_certificateChain = Lens.lens (\GetCertificateResponse' {certificateChain} -> certificateChain) (\s@GetCertificateResponse' {} a -> s {certificateChain = a} :: GetCertificateResponse)

-- | The response's http status code.
getCertificateResponse_httpStatus :: Lens.Lens' GetCertificateResponse Prelude.Int
getCertificateResponse_httpStatus = Lens.lens (\GetCertificateResponse' {httpStatus} -> httpStatus) (\s@GetCertificateResponse' {} a -> s {httpStatus = a} :: GetCertificateResponse)

instance Prelude.NFData GetCertificateResponse where
  rnf GetCertificateResponse' {..} =
    Prelude.rnf certificate `Prelude.seq`
      Prelude.rnf certificateChain `Prelude.seq`
        Prelude.rnf httpStatus
