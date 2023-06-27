{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PaymentCryptography.Types.ExportTr34KeyBlock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.ExportTr34KeyBlock where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types.Tr34KeyBlockFormat
import qualified Amazonka.Prelude as Prelude

-- | Parameter information for key material export using TR-34 standard.
--
-- /See:/ 'newExportTr34KeyBlock' smart constructor.
data ExportTr34KeyBlock = ExportTr34KeyBlock'
  { -- | A random number value that is unique to the TR-34 key block generated
    -- using 2 pass. The operation will fail, if a random nonce value is not
    -- provided for a TR-34 key block generated using 2 pass.
    randomNonce :: Prelude.Maybe Prelude.Text,
    -- | The @KeyARN@ of the certificate chain that signs the wrapping key
    -- certificate during TR-34 key export.
    certificateAuthorityPublicKeyIdentifier :: Prelude.Text,
    -- | The export token to initiate key export from Amazon Web Services Payment
    -- Cryptography. It also contains the signing key certificate that will
    -- sign the wrapped key during TR-34 key block generation. Call
    -- GetParametersForExport to receive an export token. It expires after 7
    -- days. You can use the same export token to export multiple keys from the
    -- same service account.
    exportToken :: Prelude.Text,
    -- | The format of key block that Amazon Web Services Payment Cryptography
    -- will use during key export.
    keyBlockFormat :: Tr34KeyBlockFormat,
    -- | The @KeyARN@ of the wrapping key certificate. Amazon Web Services
    -- Payment Cryptography uses this certificate to wrap the key under export.
    wrappingKeyCertificate :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportTr34KeyBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'randomNonce', 'exportTr34KeyBlock_randomNonce' - A random number value that is unique to the TR-34 key block generated
-- using 2 pass. The operation will fail, if a random nonce value is not
-- provided for a TR-34 key block generated using 2 pass.
--
-- 'certificateAuthorityPublicKeyIdentifier', 'exportTr34KeyBlock_certificateAuthorityPublicKeyIdentifier' - The @KeyARN@ of the certificate chain that signs the wrapping key
-- certificate during TR-34 key export.
--
-- 'exportToken', 'exportTr34KeyBlock_exportToken' - The export token to initiate key export from Amazon Web Services Payment
-- Cryptography. It also contains the signing key certificate that will
-- sign the wrapped key during TR-34 key block generation. Call
-- GetParametersForExport to receive an export token. It expires after 7
-- days. You can use the same export token to export multiple keys from the
-- same service account.
--
-- 'keyBlockFormat', 'exportTr34KeyBlock_keyBlockFormat' - The format of key block that Amazon Web Services Payment Cryptography
-- will use during key export.
--
-- 'wrappingKeyCertificate', 'exportTr34KeyBlock_wrappingKeyCertificate' - The @KeyARN@ of the wrapping key certificate. Amazon Web Services
-- Payment Cryptography uses this certificate to wrap the key under export.
newExportTr34KeyBlock ::
  -- | 'certificateAuthorityPublicKeyIdentifier'
  Prelude.Text ->
  -- | 'exportToken'
  Prelude.Text ->
  -- | 'keyBlockFormat'
  Tr34KeyBlockFormat ->
  -- | 'wrappingKeyCertificate'
  Prelude.Text ->
  ExportTr34KeyBlock
newExportTr34KeyBlock
  pCertificateAuthorityPublicKeyIdentifier_
  pExportToken_
  pKeyBlockFormat_
  pWrappingKeyCertificate_ =
    ExportTr34KeyBlock'
      { randomNonce = Prelude.Nothing,
        certificateAuthorityPublicKeyIdentifier =
          pCertificateAuthorityPublicKeyIdentifier_,
        exportToken = pExportToken_,
        keyBlockFormat = pKeyBlockFormat_,
        wrappingKeyCertificate =
          Data._Sensitive Lens.# pWrappingKeyCertificate_
      }

-- | A random number value that is unique to the TR-34 key block generated
-- using 2 pass. The operation will fail, if a random nonce value is not
-- provided for a TR-34 key block generated using 2 pass.
exportTr34KeyBlock_randomNonce :: Lens.Lens' ExportTr34KeyBlock (Prelude.Maybe Prelude.Text)
exportTr34KeyBlock_randomNonce = Lens.lens (\ExportTr34KeyBlock' {randomNonce} -> randomNonce) (\s@ExportTr34KeyBlock' {} a -> s {randomNonce = a} :: ExportTr34KeyBlock)

-- | The @KeyARN@ of the certificate chain that signs the wrapping key
-- certificate during TR-34 key export.
exportTr34KeyBlock_certificateAuthorityPublicKeyIdentifier :: Lens.Lens' ExportTr34KeyBlock Prelude.Text
exportTr34KeyBlock_certificateAuthorityPublicKeyIdentifier = Lens.lens (\ExportTr34KeyBlock' {certificateAuthorityPublicKeyIdentifier} -> certificateAuthorityPublicKeyIdentifier) (\s@ExportTr34KeyBlock' {} a -> s {certificateAuthorityPublicKeyIdentifier = a} :: ExportTr34KeyBlock)

-- | The export token to initiate key export from Amazon Web Services Payment
-- Cryptography. It also contains the signing key certificate that will
-- sign the wrapped key during TR-34 key block generation. Call
-- GetParametersForExport to receive an export token. It expires after 7
-- days. You can use the same export token to export multiple keys from the
-- same service account.
exportTr34KeyBlock_exportToken :: Lens.Lens' ExportTr34KeyBlock Prelude.Text
exportTr34KeyBlock_exportToken = Lens.lens (\ExportTr34KeyBlock' {exportToken} -> exportToken) (\s@ExportTr34KeyBlock' {} a -> s {exportToken = a} :: ExportTr34KeyBlock)

-- | The format of key block that Amazon Web Services Payment Cryptography
-- will use during key export.
exportTr34KeyBlock_keyBlockFormat :: Lens.Lens' ExportTr34KeyBlock Tr34KeyBlockFormat
exportTr34KeyBlock_keyBlockFormat = Lens.lens (\ExportTr34KeyBlock' {keyBlockFormat} -> keyBlockFormat) (\s@ExportTr34KeyBlock' {} a -> s {keyBlockFormat = a} :: ExportTr34KeyBlock)

-- | The @KeyARN@ of the wrapping key certificate. Amazon Web Services
-- Payment Cryptography uses this certificate to wrap the key under export.
exportTr34KeyBlock_wrappingKeyCertificate :: Lens.Lens' ExportTr34KeyBlock Prelude.Text
exportTr34KeyBlock_wrappingKeyCertificate = Lens.lens (\ExportTr34KeyBlock' {wrappingKeyCertificate} -> wrappingKeyCertificate) (\s@ExportTr34KeyBlock' {} a -> s {wrappingKeyCertificate = a} :: ExportTr34KeyBlock) Prelude.. Data._Sensitive

instance Prelude.Hashable ExportTr34KeyBlock where
  hashWithSalt _salt ExportTr34KeyBlock' {..} =
    _salt
      `Prelude.hashWithSalt` randomNonce
      `Prelude.hashWithSalt` certificateAuthorityPublicKeyIdentifier
      `Prelude.hashWithSalt` exportToken
      `Prelude.hashWithSalt` keyBlockFormat
      `Prelude.hashWithSalt` wrappingKeyCertificate

instance Prelude.NFData ExportTr34KeyBlock where
  rnf ExportTr34KeyBlock' {..} =
    Prelude.rnf randomNonce
      `Prelude.seq` Prelude.rnf certificateAuthorityPublicKeyIdentifier
      `Prelude.seq` Prelude.rnf exportToken
      `Prelude.seq` Prelude.rnf keyBlockFormat
      `Prelude.seq` Prelude.rnf wrappingKeyCertificate

instance Data.ToJSON ExportTr34KeyBlock where
  toJSON ExportTr34KeyBlock' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RandomNonce" Data..=) Prelude.<$> randomNonce,
            Prelude.Just
              ( "CertificateAuthorityPublicKeyIdentifier"
                  Data..= certificateAuthorityPublicKeyIdentifier
              ),
            Prelude.Just ("ExportToken" Data..= exportToken),
            Prelude.Just
              ("KeyBlockFormat" Data..= keyBlockFormat),
            Prelude.Just
              ( "WrappingKeyCertificate"
                  Data..= wrappingKeyCertificate
              )
          ]
      )
