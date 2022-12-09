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
-- Module      : Amazonka.CertificateManager.ExportCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CertificateManager.ExportCertificate
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
    exportCertificateResponse_certificate,
    exportCertificateResponse_certificateChain,
    exportCertificateResponse_privateKey,
    exportCertificateResponse_httpStatus,
  )
where

import Amazonka.CertificateManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportCertificate' smart constructor.
data ExportCertificate = ExportCertificate'
  { -- | An Amazon Resource Name (ARN) of the issued certificate. This must be of
    -- the form:
    --
    -- @arn:aws:acm:region:account:certificate\/12345678-1234-1234-1234-123456789012@
    certificateArn :: Prelude.Text,
    -- | Passphrase to associate with the encrypted exported private key.
    --
    -- When creating your passphrase, you can use any ASCII character except #,
    -- \$, or %.
    --
    -- If you want to later decrypt the private key, you must have the
    -- passphrase. You can use the following OpenSSL command to decrypt a
    -- private key. After entering the command, you are prompted for the
    -- passphrase.
    --
    -- @openssl rsa -in encrypted_key.pem -out decrypted_key.pem@
    passphrase :: Data.Sensitive Data.Base64
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
-- 'passphrase', 'exportCertificate_passphrase' - Passphrase to associate with the encrypted exported private key.
--
-- When creating your passphrase, you can use any ASCII character except #,
-- \$, or %.
--
-- If you want to later decrypt the private key, you must have the
-- passphrase. You can use the following OpenSSL command to decrypt a
-- private key. After entering the command, you are prompted for the
-- passphrase.
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
        Data._Sensitive Prelude.. Data._Base64
          Lens.# pPassphrase_
    }

-- | An Amazon Resource Name (ARN) of the issued certificate. This must be of
-- the form:
--
-- @arn:aws:acm:region:account:certificate\/12345678-1234-1234-1234-123456789012@
exportCertificate_certificateArn :: Lens.Lens' ExportCertificate Prelude.Text
exportCertificate_certificateArn = Lens.lens (\ExportCertificate' {certificateArn} -> certificateArn) (\s@ExportCertificate' {} a -> s {certificateArn = a} :: ExportCertificate)

-- | Passphrase to associate with the encrypted exported private key.
--
-- When creating your passphrase, you can use any ASCII character except #,
-- \$, or %.
--
-- If you want to later decrypt the private key, you must have the
-- passphrase. You can use the following OpenSSL command to decrypt a
-- private key. After entering the command, you are prompted for the
-- passphrase.
--
-- @openssl rsa -in encrypted_key.pem -out decrypted_key.pem@--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
exportCertificate_passphrase :: Lens.Lens' ExportCertificate Prelude.ByteString
exportCertificate_passphrase = Lens.lens (\ExportCertificate' {passphrase} -> passphrase) (\s@ExportCertificate' {} a -> s {passphrase = a} :: ExportCertificate) Prelude.. Data._Sensitive Prelude.. Data._Base64

instance Core.AWSRequest ExportCertificate where
  type
    AWSResponse ExportCertificate =
      ExportCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportCertificateResponse'
            Prelude.<$> (x Data..?> "Certificate")
            Prelude.<*> (x Data..?> "CertificateChain")
            Prelude.<*> (x Data..?> "PrivateKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExportCertificate where
  hashWithSalt _salt ExportCertificate' {..} =
    _salt `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` passphrase

instance Prelude.NFData ExportCertificate where
  rnf ExportCertificate' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf passphrase

instance Data.ToHeaders ExportCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CertificateManager.ExportCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExportCertificate where
  toJSON ExportCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CertificateArn" Data..= certificateArn),
            Prelude.Just ("Passphrase" Data..= passphrase)
          ]
      )

instance Data.ToPath ExportCertificate where
  toPath = Prelude.const "/"

instance Data.ToQuery ExportCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportCertificateResponse' smart constructor.
data ExportCertificateResponse = ExportCertificateResponse'
  { -- | The base64 PEM-encoded certificate.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | The base64 PEM-encoded certificate chain. This does not include the
    -- certificate that you are exporting.
    certificateChain :: Prelude.Maybe Prelude.Text,
    -- | The encrypted private key associated with the public key in the
    -- certificate. The key is output in PKCS #8 format and is base64
    -- PEM-encoded.
    privateKey :: Prelude.Maybe (Data.Sensitive Prelude.Text),
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
-- 'certificate', 'exportCertificateResponse_certificate' - The base64 PEM-encoded certificate.
--
-- 'certificateChain', 'exportCertificateResponse_certificateChain' - The base64 PEM-encoded certificate chain. This does not include the
-- certificate that you are exporting.
--
-- 'privateKey', 'exportCertificateResponse_privateKey' - The encrypted private key associated with the public key in the
-- certificate. The key is output in PKCS #8 format and is base64
-- PEM-encoded.
--
-- 'httpStatus', 'exportCertificateResponse_httpStatus' - The response's http status code.
newExportCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportCertificateResponse
newExportCertificateResponse pHttpStatus_ =
  ExportCertificateResponse'
    { certificate =
        Prelude.Nothing,
      certificateChain = Prelude.Nothing,
      privateKey = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The base64 PEM-encoded certificate.
exportCertificateResponse_certificate :: Lens.Lens' ExportCertificateResponse (Prelude.Maybe Prelude.Text)
exportCertificateResponse_certificate = Lens.lens (\ExportCertificateResponse' {certificate} -> certificate) (\s@ExportCertificateResponse' {} a -> s {certificate = a} :: ExportCertificateResponse)

-- | The base64 PEM-encoded certificate chain. This does not include the
-- certificate that you are exporting.
exportCertificateResponse_certificateChain :: Lens.Lens' ExportCertificateResponse (Prelude.Maybe Prelude.Text)
exportCertificateResponse_certificateChain = Lens.lens (\ExportCertificateResponse' {certificateChain} -> certificateChain) (\s@ExportCertificateResponse' {} a -> s {certificateChain = a} :: ExportCertificateResponse)

-- | The encrypted private key associated with the public key in the
-- certificate. The key is output in PKCS #8 format and is base64
-- PEM-encoded.
exportCertificateResponse_privateKey :: Lens.Lens' ExportCertificateResponse (Prelude.Maybe Prelude.Text)
exportCertificateResponse_privateKey = Lens.lens (\ExportCertificateResponse' {privateKey} -> privateKey) (\s@ExportCertificateResponse' {} a -> s {privateKey = a} :: ExportCertificateResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
exportCertificateResponse_httpStatus :: Lens.Lens' ExportCertificateResponse Prelude.Int
exportCertificateResponse_httpStatus = Lens.lens (\ExportCertificateResponse' {httpStatus} -> httpStatus) (\s@ExportCertificateResponse' {} a -> s {httpStatus = a} :: ExportCertificateResponse)

instance Prelude.NFData ExportCertificateResponse where
  rnf ExportCertificateResponse' {..} =
    Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf certificateChain
      `Prelude.seq` Prelude.rnf privateKey
      `Prelude.seq` Prelude.rnf httpStatus
