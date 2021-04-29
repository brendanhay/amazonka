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
-- Module      : Network.AWS.CertificateManager.GetCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an Amazon-issued certificate and its certificate chain. The
-- chain consists of the certificate of the issuing CA and the intermediate
-- certificates of any other subordinate CAs. All of the certificates are
-- base64 encoded. You can use
-- <https://wiki.openssl.org/index.php/Command_Line_Utilities OpenSSL> to
-- decode the certificates and inspect individual fields.
module Network.AWS.CertificateManager.GetCertificate
  ( -- * Creating a Request
    GetCertificate (..),
    newGetCertificate,

    -- * Request Lenses
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

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
              Prelude.=# ( "CertificateManager.GetCertificate" ::
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
              ("CertificateArn" Prelude..= certificateArn)
          ]
      )

instance Prelude.ToPath GetCertificate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCertificateResponse' smart constructor.
data GetCertificateResponse = GetCertificateResponse'
  { -- | Certificates forming the requested certificate\'s chain of trust. The
    -- chain consists of the certificate of the issuing CA and the intermediate
    -- certificates of any other subordinate CAs.
    certificateChain :: Prelude.Maybe Prelude.Text,
    -- | The ACM-issued certificate corresponding to the ARN specified as input.
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
-- 'certificateChain', 'getCertificateResponse_certificateChain' - Certificates forming the requested certificate\'s chain of trust. The
-- chain consists of the certificate of the issuing CA and the intermediate
-- certificates of any other subordinate CAs.
--
-- 'certificate', 'getCertificateResponse_certificate' - The ACM-issued certificate corresponding to the ARN specified as input.
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

-- | Certificates forming the requested certificate\'s chain of trust. The
-- chain consists of the certificate of the issuing CA and the intermediate
-- certificates of any other subordinate CAs.
getCertificateResponse_certificateChain :: Lens.Lens' GetCertificateResponse (Prelude.Maybe Prelude.Text)
getCertificateResponse_certificateChain = Lens.lens (\GetCertificateResponse' {certificateChain} -> certificateChain) (\s@GetCertificateResponse' {} a -> s {certificateChain = a} :: GetCertificateResponse)

-- | The ACM-issued certificate corresponding to the ARN specified as input.
getCertificateResponse_certificate :: Lens.Lens' GetCertificateResponse (Prelude.Maybe Prelude.Text)
getCertificateResponse_certificate = Lens.lens (\GetCertificateResponse' {certificate} -> certificate) (\s@GetCertificateResponse' {} a -> s {certificate = a} :: GetCertificateResponse)

-- | The response's http status code.
getCertificateResponse_httpStatus :: Lens.Lens' GetCertificateResponse Prelude.Int
getCertificateResponse_httpStatus = Lens.lens (\GetCertificateResponse' {httpStatus} -> httpStatus) (\s@GetCertificateResponse' {} a -> s {httpStatus = a} :: GetCertificateResponse)

instance Prelude.NFData GetCertificateResponse
