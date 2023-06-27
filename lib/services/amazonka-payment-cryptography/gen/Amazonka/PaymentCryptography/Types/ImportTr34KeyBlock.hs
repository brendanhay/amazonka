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
-- Module      : Amazonka.PaymentCryptography.Types.ImportTr34KeyBlock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.ImportTr34KeyBlock where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types.Tr34KeyBlockFormat
import qualified Amazonka.Prelude as Prelude

-- | Parameter information for key material import using TR-34 standard.
--
-- /See:/ 'newImportTr34KeyBlock' smart constructor.
data ImportTr34KeyBlock = ImportTr34KeyBlock'
  { -- | A random number value that is unique to the TR-34 key block generated
    -- using 2 pass. The operation will fail, if a random nonce value is not
    -- provided for a TR-34 key block generated using 2 pass.
    randomNonce :: Prelude.Maybe Prelude.Text,
    -- | The @KeyARN@ of the certificate chain that signs the signing key
    -- certificate during TR-34 key import.
    certificateAuthorityPublicKeyIdentifier :: Prelude.Text,
    -- | The import token that initiates key import into Amazon Web Services
    -- Payment Cryptography. It expires after 7 days. You can use the same
    -- import token to import multiple keys to the same service account.
    importToken :: Prelude.Text,
    -- | The key block format to use during key import. The only value allowed is
    -- @X9_TR34_2012@.
    keyBlockFormat :: Tr34KeyBlockFormat,
    -- | The public key component in PEM certificate format of the private key
    -- that signs the KDH TR-34 wrapped key block.
    signingKeyCertificate :: Data.Sensitive Prelude.Text,
    -- | The TR-34 wrapped key block to import.
    wrappedKeyBlock :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportTr34KeyBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'randomNonce', 'importTr34KeyBlock_randomNonce' - A random number value that is unique to the TR-34 key block generated
-- using 2 pass. The operation will fail, if a random nonce value is not
-- provided for a TR-34 key block generated using 2 pass.
--
-- 'certificateAuthorityPublicKeyIdentifier', 'importTr34KeyBlock_certificateAuthorityPublicKeyIdentifier' - The @KeyARN@ of the certificate chain that signs the signing key
-- certificate during TR-34 key import.
--
-- 'importToken', 'importTr34KeyBlock_importToken' - The import token that initiates key import into Amazon Web Services
-- Payment Cryptography. It expires after 7 days. You can use the same
-- import token to import multiple keys to the same service account.
--
-- 'keyBlockFormat', 'importTr34KeyBlock_keyBlockFormat' - The key block format to use during key import. The only value allowed is
-- @X9_TR34_2012@.
--
-- 'signingKeyCertificate', 'importTr34KeyBlock_signingKeyCertificate' - The public key component in PEM certificate format of the private key
-- that signs the KDH TR-34 wrapped key block.
--
-- 'wrappedKeyBlock', 'importTr34KeyBlock_wrappedKeyBlock' - The TR-34 wrapped key block to import.
newImportTr34KeyBlock ::
  -- | 'certificateAuthorityPublicKeyIdentifier'
  Prelude.Text ->
  -- | 'importToken'
  Prelude.Text ->
  -- | 'keyBlockFormat'
  Tr34KeyBlockFormat ->
  -- | 'signingKeyCertificate'
  Prelude.Text ->
  -- | 'wrappedKeyBlock'
  Prelude.Text ->
  ImportTr34KeyBlock
newImportTr34KeyBlock
  pCertificateAuthorityPublicKeyIdentifier_
  pImportToken_
  pKeyBlockFormat_
  pSigningKeyCertificate_
  pWrappedKeyBlock_ =
    ImportTr34KeyBlock'
      { randomNonce = Prelude.Nothing,
        certificateAuthorityPublicKeyIdentifier =
          pCertificateAuthorityPublicKeyIdentifier_,
        importToken = pImportToken_,
        keyBlockFormat = pKeyBlockFormat_,
        signingKeyCertificate =
          Data._Sensitive Lens.# pSigningKeyCertificate_,
        wrappedKeyBlock = pWrappedKeyBlock_
      }

-- | A random number value that is unique to the TR-34 key block generated
-- using 2 pass. The operation will fail, if a random nonce value is not
-- provided for a TR-34 key block generated using 2 pass.
importTr34KeyBlock_randomNonce :: Lens.Lens' ImportTr34KeyBlock (Prelude.Maybe Prelude.Text)
importTr34KeyBlock_randomNonce = Lens.lens (\ImportTr34KeyBlock' {randomNonce} -> randomNonce) (\s@ImportTr34KeyBlock' {} a -> s {randomNonce = a} :: ImportTr34KeyBlock)

-- | The @KeyARN@ of the certificate chain that signs the signing key
-- certificate during TR-34 key import.
importTr34KeyBlock_certificateAuthorityPublicKeyIdentifier :: Lens.Lens' ImportTr34KeyBlock Prelude.Text
importTr34KeyBlock_certificateAuthorityPublicKeyIdentifier = Lens.lens (\ImportTr34KeyBlock' {certificateAuthorityPublicKeyIdentifier} -> certificateAuthorityPublicKeyIdentifier) (\s@ImportTr34KeyBlock' {} a -> s {certificateAuthorityPublicKeyIdentifier = a} :: ImportTr34KeyBlock)

-- | The import token that initiates key import into Amazon Web Services
-- Payment Cryptography. It expires after 7 days. You can use the same
-- import token to import multiple keys to the same service account.
importTr34KeyBlock_importToken :: Lens.Lens' ImportTr34KeyBlock Prelude.Text
importTr34KeyBlock_importToken = Lens.lens (\ImportTr34KeyBlock' {importToken} -> importToken) (\s@ImportTr34KeyBlock' {} a -> s {importToken = a} :: ImportTr34KeyBlock)

-- | The key block format to use during key import. The only value allowed is
-- @X9_TR34_2012@.
importTr34KeyBlock_keyBlockFormat :: Lens.Lens' ImportTr34KeyBlock Tr34KeyBlockFormat
importTr34KeyBlock_keyBlockFormat = Lens.lens (\ImportTr34KeyBlock' {keyBlockFormat} -> keyBlockFormat) (\s@ImportTr34KeyBlock' {} a -> s {keyBlockFormat = a} :: ImportTr34KeyBlock)

-- | The public key component in PEM certificate format of the private key
-- that signs the KDH TR-34 wrapped key block.
importTr34KeyBlock_signingKeyCertificate :: Lens.Lens' ImportTr34KeyBlock Prelude.Text
importTr34KeyBlock_signingKeyCertificate = Lens.lens (\ImportTr34KeyBlock' {signingKeyCertificate} -> signingKeyCertificate) (\s@ImportTr34KeyBlock' {} a -> s {signingKeyCertificate = a} :: ImportTr34KeyBlock) Prelude.. Data._Sensitive

-- | The TR-34 wrapped key block to import.
importTr34KeyBlock_wrappedKeyBlock :: Lens.Lens' ImportTr34KeyBlock Prelude.Text
importTr34KeyBlock_wrappedKeyBlock = Lens.lens (\ImportTr34KeyBlock' {wrappedKeyBlock} -> wrappedKeyBlock) (\s@ImportTr34KeyBlock' {} a -> s {wrappedKeyBlock = a} :: ImportTr34KeyBlock)

instance Prelude.Hashable ImportTr34KeyBlock where
  hashWithSalt _salt ImportTr34KeyBlock' {..} =
    _salt
      `Prelude.hashWithSalt` randomNonce
      `Prelude.hashWithSalt` certificateAuthorityPublicKeyIdentifier
      `Prelude.hashWithSalt` importToken
      `Prelude.hashWithSalt` keyBlockFormat
      `Prelude.hashWithSalt` signingKeyCertificate
      `Prelude.hashWithSalt` wrappedKeyBlock

instance Prelude.NFData ImportTr34KeyBlock where
  rnf ImportTr34KeyBlock' {..} =
    Prelude.rnf randomNonce
      `Prelude.seq` Prelude.rnf certificateAuthorityPublicKeyIdentifier
      `Prelude.seq` Prelude.rnf importToken
      `Prelude.seq` Prelude.rnf keyBlockFormat
      `Prelude.seq` Prelude.rnf signingKeyCertificate
      `Prelude.seq` Prelude.rnf wrappedKeyBlock

instance Data.ToJSON ImportTr34KeyBlock where
  toJSON ImportTr34KeyBlock' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RandomNonce" Data..=) Prelude.<$> randomNonce,
            Prelude.Just
              ( "CertificateAuthorityPublicKeyIdentifier"
                  Data..= certificateAuthorityPublicKeyIdentifier
              ),
            Prelude.Just ("ImportToken" Data..= importToken),
            Prelude.Just
              ("KeyBlockFormat" Data..= keyBlockFormat),
            Prelude.Just
              ( "SigningKeyCertificate"
                  Data..= signingKeyCertificate
              ),
            Prelude.Just
              ("WrappedKeyBlock" Data..= wrappedKeyBlock)
          ]
      )
