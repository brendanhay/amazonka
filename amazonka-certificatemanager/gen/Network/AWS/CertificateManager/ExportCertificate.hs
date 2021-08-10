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
-- Module      : Network.AWS.CertificateManager.ExportCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a private certificate issued by a private certificate authority
-- (CA) for use anywhere. The exported file contains the certificate, the
-- certificate chain, and the encrypted private 2048-bit RSA key associated
-- with the public key that is embedded in the certificate. For security,
-- you must assign a passphrase for the private key when exporting it.
--
-- For information about exporting and formatting a certificate using the
-- ACM console or CLI, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-export-private.html Export a Private Certificate>.
module Network.AWS.CertificateManager.ExportCertificate
  ( -- * Creating a Request
    ExportCertificate (..),
    newExportCertificate,

    -- * Request Lenses
    exportCertificate_certificateArn,
    exportCertificate_passphrase,

    -- * Destructuring the Response
    ExportCertificateResponse (..),
    newExportCertificateResponse,

    -- * Response Lenses
    exportCertificateResponse_privateKey,
    exportCertificateResponse_certificateChain,
    exportCertificateResponse_certificate,
    exportCertificateResponse_httpStatus,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newExportCertificate' smart constructor.
data ExportCertificate = ExportCertificate'
  { -- | An Amazon Resource Name (ARN) of the issued certificate. This must be of
    -- the form:
    --
    -- @arn:aws:acm:region:account:certificate\/12345678-1234-1234-1234-123456789012@
    certificateArn :: Prelude.Text,
    -- | Passphrase to associate with the encrypted exported private key. If you
    -- want to later decrypt the private key, you must have the passphrase. You
    -- can use the following OpenSSL command to decrypt a private key:
    --
    -- @openssl rsa -in encrypted_key.pem -out decrypted_key.pem@
    passphrase :: Core.Sensitive Core.Base64
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'exportCertificate_certificateArn' - An Amazon Resource Name (ARN) of the issued certificate. This must be of
-- the form:
--
-- @arn:aws:acm:region:account:certificate\/12345678-1234-1234-1234-123456789012@
--
-- 'passphrase', 'exportCertificate_passphrase' - Passphrase to associate with the encrypted exported private key. If you
-- want to later decrypt the private key, you must have the passphrase. You
-- can use the following OpenSSL command to decrypt a private key:
--
-- @openssl rsa -in encrypted_key.pem -out decrypted_key.pem@--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newExportCertificate ::
  -- | 'certificateArn'
  Prelude.Text ->
  -- | 'passphrase'
  Prelude.ByteString ->
  ExportCertificate
newExportCertificate pCertificateArn_ pPassphrase_ =
  ExportCertificate'
    { certificateArn =
        pCertificateArn_,
      passphrase =
        Core._Sensitive Prelude.. Core._Base64
          Lens.# pPassphrase_
    }

-- | An Amazon Resource Name (ARN) of the issued certificate. This must be of
-- the form:
--
-- @arn:aws:acm:region:account:certificate\/12345678-1234-1234-1234-123456789012@
exportCertificate_certificateArn :: Lens.Lens' ExportCertificate Prelude.Text
exportCertificate_certificateArn = Lens.lens (\ExportCertificate' {certificateArn} -> certificateArn) (\s@ExportCertificate' {} a -> s {certificateArn = a} :: ExportCertificate)

-- | Passphrase to associate with the encrypted exported private key. If you
-- want to later decrypt the private key, you must have the passphrase. You
-- can use the following OpenSSL command to decrypt a private key:
--
-- @openssl rsa -in encrypted_key.pem -out decrypted_key.pem@--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
exportCertificate_passphrase :: Lens.Lens' ExportCertificate Prelude.ByteString
exportCertificate_passphrase = Lens.lens (\ExportCertificate' {passphrase} -> passphrase) (\s@ExportCertificate' {} a -> s {passphrase = a} :: ExportCertificate) Prelude.. Core._Sensitive Prelude.. Core._Base64

instance Core.AWSRequest ExportCertificate where
  type
    AWSResponse ExportCertificate =
      ExportCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportCertificateResponse'
            Prelude.<$> (x Core..?> "PrivateKey")
            Prelude.<*> (x Core..?> "CertificateChain")
            Prelude.<*> (x Core..?> "Certificate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExportCertificate

instance Prelude.NFData ExportCertificate

instance Core.ToHeaders ExportCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CertificateManager.ExportCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ExportCertificate where
  toJSON ExportCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CertificateArn" Core..= certificateArn),
            Prelude.Just ("Passphrase" Core..= passphrase)
          ]
      )

instance Core.ToPath ExportCertificate where
  toPath = Prelude.const "/"

instance Core.ToQuery ExportCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportCertificateResponse' smart constructor.
data ExportCertificateResponse = ExportCertificateResponse'
  { -- | The encrypted private key associated with the public key in the
    -- certificate. The key is output in PKCS #8 format and is base64
    -- PEM-encoded.
    privateKey :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The base64 PEM-encoded certificate chain. This does not include the
    -- certificate that you are exporting.
    certificateChain :: Prelude.Maybe Prelude.Text,
    -- | The base64 PEM-encoded certificate.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateKey', 'exportCertificateResponse_privateKey' - The encrypted private key associated with the public key in the
-- certificate. The key is output in PKCS #8 format and is base64
-- PEM-encoded.
--
-- 'certificateChain', 'exportCertificateResponse_certificateChain' - The base64 PEM-encoded certificate chain. This does not include the
-- certificate that you are exporting.
--
-- 'certificate', 'exportCertificateResponse_certificate' - The base64 PEM-encoded certificate.
--
-- 'httpStatus', 'exportCertificateResponse_httpStatus' - The response's http status code.
newExportCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportCertificateResponse
newExportCertificateResponse pHttpStatus_ =
  ExportCertificateResponse'
    { privateKey =
        Prelude.Nothing,
      certificateChain = Prelude.Nothing,
      certificate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The encrypted private key associated with the public key in the
-- certificate. The key is output in PKCS #8 format and is base64
-- PEM-encoded.
exportCertificateResponse_privateKey :: Lens.Lens' ExportCertificateResponse (Prelude.Maybe Prelude.Text)
exportCertificateResponse_privateKey = Lens.lens (\ExportCertificateResponse' {privateKey} -> privateKey) (\s@ExportCertificateResponse' {} a -> s {privateKey = a} :: ExportCertificateResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The base64 PEM-encoded certificate chain. This does not include the
-- certificate that you are exporting.
exportCertificateResponse_certificateChain :: Lens.Lens' ExportCertificateResponse (Prelude.Maybe Prelude.Text)
exportCertificateResponse_certificateChain = Lens.lens (\ExportCertificateResponse' {certificateChain} -> certificateChain) (\s@ExportCertificateResponse' {} a -> s {certificateChain = a} :: ExportCertificateResponse)

-- | The base64 PEM-encoded certificate.
exportCertificateResponse_certificate :: Lens.Lens' ExportCertificateResponse (Prelude.Maybe Prelude.Text)
exportCertificateResponse_certificate = Lens.lens (\ExportCertificateResponse' {certificate} -> certificate) (\s@ExportCertificateResponse' {} a -> s {certificate = a} :: ExportCertificateResponse)

-- | The response's http status code.
exportCertificateResponse_httpStatus :: Lens.Lens' ExportCertificateResponse Prelude.Int
exportCertificateResponse_httpStatus = Lens.lens (\ExportCertificateResponse' {httpStatus} -> httpStatus) (\s@ExportCertificateResponse' {} a -> s {httpStatus = a} :: ExportCertificateResponse)

instance Prelude.NFData ExportCertificateResponse
