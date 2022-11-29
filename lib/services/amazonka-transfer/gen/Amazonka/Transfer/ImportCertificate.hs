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
-- Module      : Amazonka.Transfer.ImportCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the signing and encryption certificates that you need to create
-- local (AS2) profiles and partner profiles.
module Amazonka.Transfer.ImportCertificate
  ( -- * Creating a Request
    ImportCertificate (..),
    newImportCertificate,

    -- * Request Lenses
    importCertificate_tags,
    importCertificate_privateKey,
    importCertificate_description,
    importCertificate_activeDate,
    importCertificate_certificateChain,
    importCertificate_inactiveDate,
    importCertificate_usage,
    importCertificate_certificate,

    -- * Destructuring the Response
    ImportCertificateResponse (..),
    newImportCertificateResponse,

    -- * Response Lenses
    importCertificateResponse_httpStatus,
    importCertificateResponse_certificateId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newImportCertificate' smart constructor.
data ImportCertificate = ImportCertificate'
  { -- | Key-value pairs that can be used to group and search for certificates.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The file that contains the private key for the certificate that\'s being
    -- imported.
    privateKey :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A short description that helps identify the certificate.
    description :: Prelude.Maybe Prelude.Text,
    -- | An optional date that specifies when the certificate becomes active.
    activeDate :: Prelude.Maybe Core.POSIX,
    -- | An optional list of certificates that make up the chain for the
    -- certificate that\'s being imported.
    certificateChain :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | An optional date that specifies when the certificate becomes inactive.
    inactiveDate :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether this certificate is used for signing or encryption.
    usage :: CertificateUsageType,
    -- | The file that contains the certificate to import.
    certificate :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'importCertificate_tags' - Key-value pairs that can be used to group and search for certificates.
--
-- 'privateKey', 'importCertificate_privateKey' - The file that contains the private key for the certificate that\'s being
-- imported.
--
-- 'description', 'importCertificate_description' - A short description that helps identify the certificate.
--
-- 'activeDate', 'importCertificate_activeDate' - An optional date that specifies when the certificate becomes active.
--
-- 'certificateChain', 'importCertificate_certificateChain' - An optional list of certificates that make up the chain for the
-- certificate that\'s being imported.
--
-- 'inactiveDate', 'importCertificate_inactiveDate' - An optional date that specifies when the certificate becomes inactive.
--
-- 'usage', 'importCertificate_usage' - Specifies whether this certificate is used for signing or encryption.
--
-- 'certificate', 'importCertificate_certificate' - The file that contains the certificate to import.
newImportCertificate ::
  -- | 'usage'
  CertificateUsageType ->
  -- | 'certificate'
  Prelude.Text ->
  ImportCertificate
newImportCertificate pUsage_ pCertificate_ =
  ImportCertificate'
    { tags = Prelude.Nothing,
      privateKey = Prelude.Nothing,
      description = Prelude.Nothing,
      activeDate = Prelude.Nothing,
      certificateChain = Prelude.Nothing,
      inactiveDate = Prelude.Nothing,
      usage = pUsage_,
      certificate = Core._Sensitive Lens.# pCertificate_
    }

-- | Key-value pairs that can be used to group and search for certificates.
importCertificate_tags :: Lens.Lens' ImportCertificate (Prelude.Maybe (Prelude.NonEmpty Tag))
importCertificate_tags = Lens.lens (\ImportCertificate' {tags} -> tags) (\s@ImportCertificate' {} a -> s {tags = a} :: ImportCertificate) Prelude.. Lens.mapping Lens.coerced

-- | The file that contains the private key for the certificate that\'s being
-- imported.
importCertificate_privateKey :: Lens.Lens' ImportCertificate (Prelude.Maybe Prelude.Text)
importCertificate_privateKey = Lens.lens (\ImportCertificate' {privateKey} -> privateKey) (\s@ImportCertificate' {} a -> s {privateKey = a} :: ImportCertificate) Prelude.. Lens.mapping Core._Sensitive

-- | A short description that helps identify the certificate.
importCertificate_description :: Lens.Lens' ImportCertificate (Prelude.Maybe Prelude.Text)
importCertificate_description = Lens.lens (\ImportCertificate' {description} -> description) (\s@ImportCertificate' {} a -> s {description = a} :: ImportCertificate)

-- | An optional date that specifies when the certificate becomes active.
importCertificate_activeDate :: Lens.Lens' ImportCertificate (Prelude.Maybe Prelude.UTCTime)
importCertificate_activeDate = Lens.lens (\ImportCertificate' {activeDate} -> activeDate) (\s@ImportCertificate' {} a -> s {activeDate = a} :: ImportCertificate) Prelude.. Lens.mapping Core._Time

-- | An optional list of certificates that make up the chain for the
-- certificate that\'s being imported.
importCertificate_certificateChain :: Lens.Lens' ImportCertificate (Prelude.Maybe Prelude.Text)
importCertificate_certificateChain = Lens.lens (\ImportCertificate' {certificateChain} -> certificateChain) (\s@ImportCertificate' {} a -> s {certificateChain = a} :: ImportCertificate) Prelude.. Lens.mapping Core._Sensitive

-- | An optional date that specifies when the certificate becomes inactive.
importCertificate_inactiveDate :: Lens.Lens' ImportCertificate (Prelude.Maybe Prelude.UTCTime)
importCertificate_inactiveDate = Lens.lens (\ImportCertificate' {inactiveDate} -> inactiveDate) (\s@ImportCertificate' {} a -> s {inactiveDate = a} :: ImportCertificate) Prelude.. Lens.mapping Core._Time

-- | Specifies whether this certificate is used for signing or encryption.
importCertificate_usage :: Lens.Lens' ImportCertificate CertificateUsageType
importCertificate_usage = Lens.lens (\ImportCertificate' {usage} -> usage) (\s@ImportCertificate' {} a -> s {usage = a} :: ImportCertificate)

-- | The file that contains the certificate to import.
importCertificate_certificate :: Lens.Lens' ImportCertificate Prelude.Text
importCertificate_certificate = Lens.lens (\ImportCertificate' {certificate} -> certificate) (\s@ImportCertificate' {} a -> s {certificate = a} :: ImportCertificate) Prelude.. Core._Sensitive

instance Core.AWSRequest ImportCertificate where
  type
    AWSResponse ImportCertificate =
      ImportCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportCertificateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "CertificateId")
      )

instance Prelude.Hashable ImportCertificate where
  hashWithSalt _salt ImportCertificate' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` privateKey
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` activeDate
      `Prelude.hashWithSalt` certificateChain
      `Prelude.hashWithSalt` inactiveDate
      `Prelude.hashWithSalt` usage
      `Prelude.hashWithSalt` certificate

instance Prelude.NFData ImportCertificate where
  rnf ImportCertificate' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf privateKey
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf activeDate
      `Prelude.seq` Prelude.rnf certificateChain
      `Prelude.seq` Prelude.rnf inactiveDate
      `Prelude.seq` Prelude.rnf usage
      `Prelude.seq` Prelude.rnf certificate

instance Core.ToHeaders ImportCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TransferService.ImportCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ImportCertificate where
  toJSON ImportCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("PrivateKey" Core..=) Prelude.<$> privateKey,
            ("Description" Core..=) Prelude.<$> description,
            ("ActiveDate" Core..=) Prelude.<$> activeDate,
            ("CertificateChain" Core..=)
              Prelude.<$> certificateChain,
            ("InactiveDate" Core..=) Prelude.<$> inactiveDate,
            Prelude.Just ("Usage" Core..= usage),
            Prelude.Just ("Certificate" Core..= certificate)
          ]
      )

instance Core.ToPath ImportCertificate where
  toPath = Prelude.const "/"

instance Core.ToQuery ImportCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportCertificateResponse' smart constructor.
data ImportCertificateResponse = ImportCertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of identifiers for the imported certificates. You use this
    -- identifier for working with profiles and partner profiles.
    certificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'importCertificateResponse_httpStatus' - The response's http status code.
--
-- 'certificateId', 'importCertificateResponse_certificateId' - An array of identifiers for the imported certificates. You use this
-- identifier for working with profiles and partner profiles.
newImportCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'certificateId'
  Prelude.Text ->
  ImportCertificateResponse
newImportCertificateResponse
  pHttpStatus_
  pCertificateId_ =
    ImportCertificateResponse'
      { httpStatus =
          pHttpStatus_,
        certificateId = pCertificateId_
      }

-- | The response's http status code.
importCertificateResponse_httpStatus :: Lens.Lens' ImportCertificateResponse Prelude.Int
importCertificateResponse_httpStatus = Lens.lens (\ImportCertificateResponse' {httpStatus} -> httpStatus) (\s@ImportCertificateResponse' {} a -> s {httpStatus = a} :: ImportCertificateResponse)

-- | An array of identifiers for the imported certificates. You use this
-- identifier for working with profiles and partner profiles.
importCertificateResponse_certificateId :: Lens.Lens' ImportCertificateResponse Prelude.Text
importCertificateResponse_certificateId = Lens.lens (\ImportCertificateResponse' {certificateId} -> certificateId) (\s@ImportCertificateResponse' {} a -> s {certificateId = a} :: ImportCertificateResponse)

instance Prelude.NFData ImportCertificateResponse where
  rnf ImportCertificateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf certificateId
