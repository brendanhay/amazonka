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
-- Module      : Amazonka.PaymentCryptographyData.DecryptData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decrypts ciphertext data to plaintext using symmetric, asymmetric, or
-- DUKPT data encryption key. For more information, see
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/decrypt-data.html Decrypt data>
-- in the /Amazon Web Services Payment Cryptography User Guide/.
--
-- You can use an encryption key generated within Amazon Web Services
-- Payment Cryptography, or you can import your own encryption key by
-- calling
-- <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_ImportKey.html ImportKey>.
-- For this operation, the key must have @KeyModesOfUse@ set to @Decrypt@.
-- In asymmetric decryption, Amazon Web Services Payment Cryptography
-- decrypts the ciphertext using the private component of the asymmetric
-- encryption key pair. For data encryption outside of Amazon Web Services
-- Payment Cryptography, you can export the public component of the
-- asymmetric key pair by calling
-- <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_GetPublicKeyCertificate.html GetPublicCertificate>.
--
-- For symmetric and DUKPT decryption, Amazon Web Services Payment
-- Cryptography supports @TDES@ and @AES@ algorithms. For asymmetric
-- decryption, Amazon Web Services Payment Cryptography supports @RSA@.
-- When you use DUKPT, for @TDES@ algorithm, the ciphertext data length
-- must be a multiple of 16 bytes. For @AES@ algorithm, the ciphertext data
-- length must be a multiple of 32 bytes.
--
-- For information about valid keys for this operation, see
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/keys-validattributes.html Understanding key attributes>
-- and
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/crypto-ops-validkeys-ops.html Key types for specific data operations>
-- in the /Amazon Web Services Payment Cryptography User Guide/.
--
-- __Cross-account use__: This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   EncryptData
--
-- -   <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_GetPublicKeyCertificate.html GetPublicCertificate>
--
-- -   <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_ImportKey.html ImportKey>
module Amazonka.PaymentCryptographyData.DecryptData
  ( -- * Creating a Request
    DecryptData (..),
    newDecryptData,

    -- * Request Lenses
    decryptData_cipherText,
    decryptData_decryptionAttributes,
    decryptData_keyIdentifier,

    -- * Destructuring the Response
    DecryptDataResponse (..),
    newDecryptDataResponse,

    -- * Response Lenses
    decryptDataResponse_httpStatus,
    decryptDataResponse_keyArn,
    decryptDataResponse_keyCheckValue,
    decryptDataResponse_plainText,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDecryptData' smart constructor.
data DecryptData = DecryptData'
  { -- | The ciphertext to decrypt.
    cipherText :: Data.Sensitive Prelude.Text,
    -- | The encryption key type and attributes for ciphertext decryption.
    decryptionAttributes :: EncryptionDecryptionAttributes,
    -- | The @keyARN@ of the encryption key that Amazon Web Services Payment
    -- Cryptography uses for ciphertext decryption.
    keyIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecryptData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cipherText', 'decryptData_cipherText' - The ciphertext to decrypt.
--
-- 'decryptionAttributes', 'decryptData_decryptionAttributes' - The encryption key type and attributes for ciphertext decryption.
--
-- 'keyIdentifier', 'decryptData_keyIdentifier' - The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses for ciphertext decryption.
newDecryptData ::
  -- | 'cipherText'
  Prelude.Text ->
  -- | 'decryptionAttributes'
  EncryptionDecryptionAttributes ->
  -- | 'keyIdentifier'
  Prelude.Text ->
  DecryptData
newDecryptData
  pCipherText_
  pDecryptionAttributes_
  pKeyIdentifier_ =
    DecryptData'
      { cipherText =
          Data._Sensitive Lens.# pCipherText_,
        decryptionAttributes = pDecryptionAttributes_,
        keyIdentifier = pKeyIdentifier_
      }

-- | The ciphertext to decrypt.
decryptData_cipherText :: Lens.Lens' DecryptData Prelude.Text
decryptData_cipherText = Lens.lens (\DecryptData' {cipherText} -> cipherText) (\s@DecryptData' {} a -> s {cipherText = a} :: DecryptData) Prelude.. Data._Sensitive

-- | The encryption key type and attributes for ciphertext decryption.
decryptData_decryptionAttributes :: Lens.Lens' DecryptData EncryptionDecryptionAttributes
decryptData_decryptionAttributes = Lens.lens (\DecryptData' {decryptionAttributes} -> decryptionAttributes) (\s@DecryptData' {} a -> s {decryptionAttributes = a} :: DecryptData)

-- | The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses for ciphertext decryption.
decryptData_keyIdentifier :: Lens.Lens' DecryptData Prelude.Text
decryptData_keyIdentifier = Lens.lens (\DecryptData' {keyIdentifier} -> keyIdentifier) (\s@DecryptData' {} a -> s {keyIdentifier = a} :: DecryptData)

instance Core.AWSRequest DecryptData where
  type AWSResponse DecryptData = DecryptDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DecryptDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "KeyArn")
            Prelude.<*> (x Data..:> "KeyCheckValue")
            Prelude.<*> (x Data..:> "PlainText")
      )

instance Prelude.Hashable DecryptData where
  hashWithSalt _salt DecryptData' {..} =
    _salt
      `Prelude.hashWithSalt` cipherText
      `Prelude.hashWithSalt` decryptionAttributes
      `Prelude.hashWithSalt` keyIdentifier

instance Prelude.NFData DecryptData where
  rnf DecryptData' {..} =
    Prelude.rnf cipherText
      `Prelude.seq` Prelude.rnf decryptionAttributes
      `Prelude.seq` Prelude.rnf keyIdentifier

instance Data.ToHeaders DecryptData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DecryptData where
  toJSON DecryptData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("CipherText" Data..= cipherText),
            Prelude.Just
              ( "DecryptionAttributes"
                  Data..= decryptionAttributes
              )
          ]
      )

instance Data.ToPath DecryptData where
  toPath DecryptData' {..} =
    Prelude.mconcat
      ["/keys/", Data.toBS keyIdentifier, "/decrypt"]

instance Data.ToQuery DecryptData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDecryptDataResponse' smart constructor.
data DecryptDataResponse = DecryptDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @keyARN@ of the encryption key that Amazon Web Services Payment
    -- Cryptography uses for ciphertext decryption.
    keyArn :: Prelude.Text,
    -- | The key check value (KCV) of the encryption key. The KCV is used to
    -- check if all parties holding a given key have the same key or to detect
    -- that a key has changed. Amazon Web Services Payment Cryptography
    -- calculates the KCV by using standard algorithms, typically by encrypting
    -- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
    -- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
    keyCheckValue :: Prelude.Text,
    -- | The decrypted plaintext data.
    plainText :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecryptDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'decryptDataResponse_httpStatus' - The response's http status code.
--
-- 'keyArn', 'decryptDataResponse_keyArn' - The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses for ciphertext decryption.
--
-- 'keyCheckValue', 'decryptDataResponse_keyCheckValue' - The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
--
-- 'plainText', 'decryptDataResponse_plainText' - The decrypted plaintext data.
newDecryptDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'keyArn'
  Prelude.Text ->
  -- | 'keyCheckValue'
  Prelude.Text ->
  -- | 'plainText'
  Prelude.Text ->
  DecryptDataResponse
newDecryptDataResponse
  pHttpStatus_
  pKeyArn_
  pKeyCheckValue_
  pPlainText_ =
    DecryptDataResponse'
      { httpStatus = pHttpStatus_,
        keyArn = pKeyArn_,
        keyCheckValue = pKeyCheckValue_,
        plainText = Data._Sensitive Lens.# pPlainText_
      }

-- | The response's http status code.
decryptDataResponse_httpStatus :: Lens.Lens' DecryptDataResponse Prelude.Int
decryptDataResponse_httpStatus = Lens.lens (\DecryptDataResponse' {httpStatus} -> httpStatus) (\s@DecryptDataResponse' {} a -> s {httpStatus = a} :: DecryptDataResponse)

-- | The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses for ciphertext decryption.
decryptDataResponse_keyArn :: Lens.Lens' DecryptDataResponse Prelude.Text
decryptDataResponse_keyArn = Lens.lens (\DecryptDataResponse' {keyArn} -> keyArn) (\s@DecryptDataResponse' {} a -> s {keyArn = a} :: DecryptDataResponse)

-- | The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
decryptDataResponse_keyCheckValue :: Lens.Lens' DecryptDataResponse Prelude.Text
decryptDataResponse_keyCheckValue = Lens.lens (\DecryptDataResponse' {keyCheckValue} -> keyCheckValue) (\s@DecryptDataResponse' {} a -> s {keyCheckValue = a} :: DecryptDataResponse)

-- | The decrypted plaintext data.
decryptDataResponse_plainText :: Lens.Lens' DecryptDataResponse Prelude.Text
decryptDataResponse_plainText = Lens.lens (\DecryptDataResponse' {plainText} -> plainText) (\s@DecryptDataResponse' {} a -> s {plainText = a} :: DecryptDataResponse) Prelude.. Data._Sensitive

instance Prelude.NFData DecryptDataResponse where
  rnf DecryptDataResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyCheckValue
      `Prelude.seq` Prelude.rnf plainText
