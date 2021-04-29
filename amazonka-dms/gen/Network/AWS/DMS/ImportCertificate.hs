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
-- Module      : Network.AWS.DMS.ImportCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads the specified certificate.
module Network.AWS.DMS.ImportCertificate
  ( -- * Creating a Request
    ImportCertificate (..),
    newImportCertificate,

    -- * Request Lenses
    importCertificate_certificateWallet,
    importCertificate_tags,
    importCertificate_certificatePem,
    importCertificate_certificateIdentifier,

    -- * Destructuring the Response
    ImportCertificateResponse (..),
    newImportCertificateResponse,

    -- * Response Lenses
    importCertificateResponse_certificate,
    importCertificateResponse_httpStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportCertificate' smart constructor.
data ImportCertificate = ImportCertificate'
  { -- | The location of an imported Oracle Wallet certificate for use with SSL.
    certificateWallet :: Prelude.Maybe Prelude.Base64,
    -- | The tags associated with the certificate.
    tags :: Prelude.Maybe [Tag],
    -- | The contents of a @.pem@ file, which contains an X.509 certificate.
    certificatePem :: Prelude.Maybe Prelude.Text,
    -- | A customer-assigned name for the certificate. Identifiers must begin
    -- with a letter and must contain only ASCII letters, digits, and hyphens.
    -- They can\'t end with a hyphen or contain two consecutive hyphens.
    certificateIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateWallet', 'importCertificate_certificateWallet' - The location of an imported Oracle Wallet certificate for use with SSL.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'tags', 'importCertificate_tags' - The tags associated with the certificate.
--
-- 'certificatePem', 'importCertificate_certificatePem' - The contents of a @.pem@ file, which contains an X.509 certificate.
--
-- 'certificateIdentifier', 'importCertificate_certificateIdentifier' - A customer-assigned name for the certificate. Identifiers must begin
-- with a letter and must contain only ASCII letters, digits, and hyphens.
-- They can\'t end with a hyphen or contain two consecutive hyphens.
newImportCertificate ::
  -- | 'certificateIdentifier'
  Prelude.Text ->
  ImportCertificate
newImportCertificate pCertificateIdentifier_ =
  ImportCertificate'
    { certificateWallet =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      certificatePem = Prelude.Nothing,
      certificateIdentifier = pCertificateIdentifier_
    }

-- | The location of an imported Oracle Wallet certificate for use with SSL.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
importCertificate_certificateWallet :: Lens.Lens' ImportCertificate (Prelude.Maybe Prelude.ByteString)
importCertificate_certificateWallet = Lens.lens (\ImportCertificate' {certificateWallet} -> certificateWallet) (\s@ImportCertificate' {} a -> s {certificateWallet = a} :: ImportCertificate) Prelude.. Lens.mapping Prelude._Base64

-- | The tags associated with the certificate.
importCertificate_tags :: Lens.Lens' ImportCertificate (Prelude.Maybe [Tag])
importCertificate_tags = Lens.lens (\ImportCertificate' {tags} -> tags) (\s@ImportCertificate' {} a -> s {tags = a} :: ImportCertificate) Prelude.. Lens.mapping Prelude._Coerce

-- | The contents of a @.pem@ file, which contains an X.509 certificate.
importCertificate_certificatePem :: Lens.Lens' ImportCertificate (Prelude.Maybe Prelude.Text)
importCertificate_certificatePem = Lens.lens (\ImportCertificate' {certificatePem} -> certificatePem) (\s@ImportCertificate' {} a -> s {certificatePem = a} :: ImportCertificate)

-- | A customer-assigned name for the certificate. Identifiers must begin
-- with a letter and must contain only ASCII letters, digits, and hyphens.
-- They can\'t end with a hyphen or contain two consecutive hyphens.
importCertificate_certificateIdentifier :: Lens.Lens' ImportCertificate Prelude.Text
importCertificate_certificateIdentifier = Lens.lens (\ImportCertificate' {certificateIdentifier} -> certificateIdentifier) (\s@ImportCertificate' {} a -> s {certificateIdentifier = a} :: ImportCertificate)

instance Prelude.AWSRequest ImportCertificate where
  type Rs ImportCertificate = ImportCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportCertificateResponse'
            Prelude.<$> (x Prelude..?> "Certificate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportCertificate

instance Prelude.NFData ImportCertificate

instance Prelude.ToHeaders ImportCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonDMSv20160101.ImportCertificate" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ImportCertificate where
  toJSON ImportCertificate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CertificateWallet" Prelude..=)
              Prelude.<$> certificateWallet,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("CertificatePem" Prelude..=)
              Prelude.<$> certificatePem,
            Prelude.Just
              ( "CertificateIdentifier"
                  Prelude..= certificateIdentifier
              )
          ]
      )

instance Prelude.ToPath ImportCertificate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ImportCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportCertificateResponse' smart constructor.
data ImportCertificateResponse = ImportCertificateResponse'
  { -- | The certificate to be uploaded.
    certificate :: Prelude.Maybe Certificate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificate', 'importCertificateResponse_certificate' - The certificate to be uploaded.
--
-- 'httpStatus', 'importCertificateResponse_httpStatus' - The response's http status code.
newImportCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportCertificateResponse
newImportCertificateResponse pHttpStatus_ =
  ImportCertificateResponse'
    { certificate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The certificate to be uploaded.
importCertificateResponse_certificate :: Lens.Lens' ImportCertificateResponse (Prelude.Maybe Certificate)
importCertificateResponse_certificate = Lens.lens (\ImportCertificateResponse' {certificate} -> certificate) (\s@ImportCertificateResponse' {} a -> s {certificate = a} :: ImportCertificateResponse)

-- | The response's http status code.
importCertificateResponse_httpStatus :: Lens.Lens' ImportCertificateResponse Prelude.Int
importCertificateResponse_httpStatus = Lens.lens (\ImportCertificateResponse' {httpStatus} -> httpStatus) (\s@ImportCertificateResponse' {} a -> s {httpStatus = a} :: ImportCertificateResponse)

instance Prelude.NFData ImportCertificateResponse
