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
-- Module      : Amazonka.PaymentCryptography.ExportKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a key from Amazon Web Services Payment Cryptography using either
-- ANSI X9 TR-34 or TR-31 key export standard.
--
-- Amazon Web Services Payment Cryptography simplifies main or root key
-- exchange process by eliminating the need of a paper-based key exchange
-- process. It takes a modern and secure approach based of the ANSI X9
-- TR-34 key exchange standard.
--
-- You can use @ExportKey@ to export main or root keys such as KEK (Key
-- Encryption Key), using asymmetric key exchange technique following ANSI
-- X9 TR-34 standard. The ANSI X9 TR-34 standard uses asymmetric keys to
-- establishes bi-directional trust between the two parties exchanging
-- keys. After which you can export working keys using the ANSI X9 TR-31
-- symmetric key exchange standard as mandated by PCI PIN. Using this
-- operation, you can share your Amazon Web Services Payment Cryptography
-- generated keys with other service partners to perform cryptographic
-- operations outside of Amazon Web Services Payment Cryptography
--
-- __TR-34 key export__
--
-- Amazon Web Services Payment Cryptography uses TR-34 asymmetric key
-- exchange standard to export main keys such as KEK. In TR-34 terminology,
-- the sending party of the key is called Key Distribution Host (KDH) and
-- the receiving party of the key is called Key Receiving Host (KRH). In
-- key export process, KDH is Amazon Web Services Payment Cryptography
-- which initiates key export. KRH is the user receiving the key. Before
-- you initiate TR-34 key export, you must obtain an export token by
-- calling GetParametersForExport. This operation also returns the signing
-- key certificate that KDH uses to sign the wrapped key to generate a
-- TR-34 wrapped key block. The export token expires after 7 days.
--
-- Set the following parameters:
--
-- [CertificateAuthorityPublicKeyIdentifier]
--     The @KeyARN@ of the certificate chain that will sign the wrapping
--     key certificate. This must exist within Amazon Web Services Payment
--     Cryptography before you initiate TR-34 key export. If it does not
--     exist, you can import it by calling ImportKey for
--     @RootCertificatePublicKey@.
--
-- [ExportToken]
--     Obtained from KDH by calling GetParametersForExport.
--
-- [WrappingKeyCertificate]
--     Amazon Web Services Payment Cryptography uses this to wrap the key
--     under export.
--
-- When this operation is successful, Amazon Web Services Payment
-- Cryptography returns the TR-34 wrapped key block.
--
-- __TR-31 key export__
--
-- Amazon Web Services Payment Cryptography uses TR-31 symmetric key
-- exchange standard to export working keys. In TR-31, you must use a main
-- key such as KEK to encrypt or wrap the key under export. To establish a
-- KEK, you can use CreateKey or ImportKey. When this operation is
-- successful, Amazon Web Services Payment Cryptography returns a TR-31
-- wrapped key block.
--
-- __Cross-account use:__ This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   GetParametersForExport
--
-- -   ImportKey
module Amazonka.PaymentCryptography.ExportKey
  ( -- * Creating a Request
    ExportKey (..),
    newExportKey,

    -- * Request Lenses
    exportKey_exportKeyIdentifier,
    exportKey_keyMaterial,

    -- * Destructuring the Response
    ExportKeyResponse (..),
    newExportKeyResponse,

    -- * Response Lenses
    exportKeyResponse_wrappedKey,
    exportKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportKey' smart constructor.
data ExportKey = ExportKey'
  { -- | The @KeyARN@ of the key under export from Amazon Web Services Payment
    -- Cryptography.
    exportKeyIdentifier :: Prelude.Text,
    -- | The key block format type, for example, TR-34 or TR-31, to use during
    -- key material export.
    keyMaterial :: ExportKeyMaterial
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportKeyIdentifier', 'exportKey_exportKeyIdentifier' - The @KeyARN@ of the key under export from Amazon Web Services Payment
-- Cryptography.
--
-- 'keyMaterial', 'exportKey_keyMaterial' - The key block format type, for example, TR-34 or TR-31, to use during
-- key material export.
newExportKey ::
  -- | 'exportKeyIdentifier'
  Prelude.Text ->
  -- | 'keyMaterial'
  ExportKeyMaterial ->
  ExportKey
newExportKey pExportKeyIdentifier_ pKeyMaterial_ =
  ExportKey'
    { exportKeyIdentifier =
        pExportKeyIdentifier_,
      keyMaterial = pKeyMaterial_
    }

-- | The @KeyARN@ of the key under export from Amazon Web Services Payment
-- Cryptography.
exportKey_exportKeyIdentifier :: Lens.Lens' ExportKey Prelude.Text
exportKey_exportKeyIdentifier = Lens.lens (\ExportKey' {exportKeyIdentifier} -> exportKeyIdentifier) (\s@ExportKey' {} a -> s {exportKeyIdentifier = a} :: ExportKey)

-- | The key block format type, for example, TR-34 or TR-31, to use during
-- key material export.
exportKey_keyMaterial :: Lens.Lens' ExportKey ExportKeyMaterial
exportKey_keyMaterial = Lens.lens (\ExportKey' {keyMaterial} -> keyMaterial) (\s@ExportKey' {} a -> s {keyMaterial = a} :: ExportKey)

instance Core.AWSRequest ExportKey where
  type AWSResponse ExportKey = ExportKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportKeyResponse'
            Prelude.<$> (x Data..?> "WrappedKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExportKey where
  hashWithSalt _salt ExportKey' {..} =
    _salt
      `Prelude.hashWithSalt` exportKeyIdentifier
      `Prelude.hashWithSalt` keyMaterial

instance Prelude.NFData ExportKey where
  rnf ExportKey' {..} =
    Prelude.rnf exportKeyIdentifier
      `Prelude.seq` Prelude.rnf keyMaterial

instance Data.ToHeaders ExportKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.ExportKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExportKey where
  toJSON ExportKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ExportKeyIdentifier" Data..= exportKeyIdentifier),
            Prelude.Just ("KeyMaterial" Data..= keyMaterial)
          ]
      )

instance Data.ToPath ExportKey where
  toPath = Prelude.const "/"

instance Data.ToQuery ExportKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportKeyResponse' smart constructor.
data ExportKeyResponse = ExportKeyResponse'
  { -- | The key material under export as a TR-34 or TR-31 wrapped key block.
    wrappedKey :: Prelude.Maybe WrappedKey,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'wrappedKey', 'exportKeyResponse_wrappedKey' - The key material under export as a TR-34 or TR-31 wrapped key block.
--
-- 'httpStatus', 'exportKeyResponse_httpStatus' - The response's http status code.
newExportKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportKeyResponse
newExportKeyResponse pHttpStatus_ =
  ExportKeyResponse'
    { wrappedKey = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The key material under export as a TR-34 or TR-31 wrapped key block.
exportKeyResponse_wrappedKey :: Lens.Lens' ExportKeyResponse (Prelude.Maybe WrappedKey)
exportKeyResponse_wrappedKey = Lens.lens (\ExportKeyResponse' {wrappedKey} -> wrappedKey) (\s@ExportKeyResponse' {} a -> s {wrappedKey = a} :: ExportKeyResponse)

-- | The response's http status code.
exportKeyResponse_httpStatus :: Lens.Lens' ExportKeyResponse Prelude.Int
exportKeyResponse_httpStatus = Lens.lens (\ExportKeyResponse' {httpStatus} -> httpStatus) (\s@ExportKeyResponse' {} a -> s {httpStatus = a} :: ExportKeyResponse)

instance Prelude.NFData ExportKeyResponse where
  rnf ExportKeyResponse' {..} =
    Prelude.rnf wrappedKey
      `Prelude.seq` Prelude.rnf httpStatus
