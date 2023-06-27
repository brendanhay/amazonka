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
-- Module      : Amazonka.PaymentCryptographyData.EncryptData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Encrypts plaintext data to ciphertext using symmetric, asymmetric, or
-- DUKPT data encryption key. For more information, see
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/encrypt-data.html Encrypt data>
-- in the /Amazon Web Services Payment Cryptography User Guide/.
--
-- You can generate an encryption key within Amazon Web Services Payment
-- Cryptography by calling
-- <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_CreateKey.html CreateKey>.
-- You can import your own encryption key by calling
-- <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_ImportKey.html ImportKey>.
-- For this operation, the key must have @KeyModesOfUse@ set to @Encrypt@.
-- In asymmetric encryption, plaintext is encrypted using public component.
-- You can import the public component of an asymmetric key pair created
-- outside Amazon Web Services Payment Cryptography by calling
-- <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_ImportKey.html ImportKey>).
--
-- for symmetric and DUKPT encryption, Amazon Web Services Payment
-- Cryptography supports @TDES@ and @AES@ algorithms. For asymmetric
-- encryption, Amazon Web Services Payment Cryptography supports @RSA@. To
-- encrypt using DUKPT, you must already have a DUKPT key in your account
-- with @KeyModesOfUse@ set to @DeriveKey@, or you can generate a new DUKPT
-- key by calling
-- <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_CreateKey.html CreateKey>.
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
-- -   DecryptData
--
-- -   <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_GetPublicKeyCertificate.html GetPublicCertificate>
--
-- -   <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_ImportKey.html ImportKey>
--
-- -   ReEncryptData
module Amazonka.PaymentCryptographyData.EncryptData
  ( -- * Creating a Request
    EncryptData (..),
    newEncryptData,

    -- * Request Lenses
    encryptData_encryptionAttributes,
    encryptData_keyIdentifier,
    encryptData_plainText,

    -- * Destructuring the Response
    EncryptDataResponse (..),
    newEncryptDataResponse,

    -- * Response Lenses
    encryptDataResponse_httpStatus,
    encryptDataResponse_cipherText,
    encryptDataResponse_keyArn,
    encryptDataResponse_keyCheckValue,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEncryptData' smart constructor.
data EncryptData = EncryptData'
  { -- | The encryption key type and attributes for plaintext encryption.
    encryptionAttributes :: EncryptionDecryptionAttributes,
    -- | The @keyARN@ of the encryption key that Amazon Web Services Payment
    -- Cryptography uses for plaintext encryption.
    keyIdentifier :: Prelude.Text,
    -- | The plaintext to be encrypted.
    plainText :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionAttributes', 'encryptData_encryptionAttributes' - The encryption key type and attributes for plaintext encryption.
--
-- 'keyIdentifier', 'encryptData_keyIdentifier' - The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses for plaintext encryption.
--
-- 'plainText', 'encryptData_plainText' - The plaintext to be encrypted.
newEncryptData ::
  -- | 'encryptionAttributes'
  EncryptionDecryptionAttributes ->
  -- | 'keyIdentifier'
  Prelude.Text ->
  -- | 'plainText'
  Prelude.Text ->
  EncryptData
newEncryptData
  pEncryptionAttributes_
  pKeyIdentifier_
  pPlainText_ =
    EncryptData'
      { encryptionAttributes =
          pEncryptionAttributes_,
        keyIdentifier = pKeyIdentifier_,
        plainText = Data._Sensitive Lens.# pPlainText_
      }

-- | The encryption key type and attributes for plaintext encryption.
encryptData_encryptionAttributes :: Lens.Lens' EncryptData EncryptionDecryptionAttributes
encryptData_encryptionAttributes = Lens.lens (\EncryptData' {encryptionAttributes} -> encryptionAttributes) (\s@EncryptData' {} a -> s {encryptionAttributes = a} :: EncryptData)

-- | The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses for plaintext encryption.
encryptData_keyIdentifier :: Lens.Lens' EncryptData Prelude.Text
encryptData_keyIdentifier = Lens.lens (\EncryptData' {keyIdentifier} -> keyIdentifier) (\s@EncryptData' {} a -> s {keyIdentifier = a} :: EncryptData)

-- | The plaintext to be encrypted.
encryptData_plainText :: Lens.Lens' EncryptData Prelude.Text
encryptData_plainText = Lens.lens (\EncryptData' {plainText} -> plainText) (\s@EncryptData' {} a -> s {plainText = a} :: EncryptData) Prelude.. Data._Sensitive

instance Core.AWSRequest EncryptData where
  type AWSResponse EncryptData = EncryptDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EncryptDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CipherText")
            Prelude.<*> (x Data..:> "KeyArn")
            Prelude.<*> (x Data..:> "KeyCheckValue")
      )

instance Prelude.Hashable EncryptData where
  hashWithSalt _salt EncryptData' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionAttributes
      `Prelude.hashWithSalt` keyIdentifier
      `Prelude.hashWithSalt` plainText

instance Prelude.NFData EncryptData where
  rnf EncryptData' {..} =
    Prelude.rnf encryptionAttributes
      `Prelude.seq` Prelude.rnf keyIdentifier
      `Prelude.seq` Prelude.rnf plainText

instance Data.ToHeaders EncryptData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EncryptData where
  toJSON EncryptData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EncryptionAttributes"
                  Data..= encryptionAttributes
              ),
            Prelude.Just ("PlainText" Data..= plainText)
          ]
      )

instance Data.ToPath EncryptData where
  toPath EncryptData' {..} =
    Prelude.mconcat
      ["/keys/", Data.toBS keyIdentifier, "/encrypt"]

instance Data.ToQuery EncryptData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEncryptDataResponse' smart constructor.
data EncryptDataResponse = EncryptDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The encrypted ciphertext.
    cipherText :: Data.Sensitive Prelude.Text,
    -- | The @keyARN@ of the encryption key that Amazon Web Services Payment
    -- Cryptography uses for plaintext encryption.
    keyArn :: Prelude.Text,
    -- | The key check value (KCV) of the encryption key. The KCV is used to
    -- check if all parties holding a given key have the same key or to detect
    -- that a key has changed. Amazon Web Services Payment Cryptography
    -- calculates the KCV by using standard algorithms, typically by encrypting
    -- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
    -- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
    keyCheckValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'encryptDataResponse_httpStatus' - The response's http status code.
--
-- 'cipherText', 'encryptDataResponse_cipherText' - The encrypted ciphertext.
--
-- 'keyArn', 'encryptDataResponse_keyArn' - The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses for plaintext encryption.
--
-- 'keyCheckValue', 'encryptDataResponse_keyCheckValue' - The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
newEncryptDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'cipherText'
  Prelude.Text ->
  -- | 'keyArn'
  Prelude.Text ->
  -- | 'keyCheckValue'
  Prelude.Text ->
  EncryptDataResponse
newEncryptDataResponse
  pHttpStatus_
  pCipherText_
  pKeyArn_
  pKeyCheckValue_ =
    EncryptDataResponse'
      { httpStatus = pHttpStatus_,
        cipherText = Data._Sensitive Lens.# pCipherText_,
        keyArn = pKeyArn_,
        keyCheckValue = pKeyCheckValue_
      }

-- | The response's http status code.
encryptDataResponse_httpStatus :: Lens.Lens' EncryptDataResponse Prelude.Int
encryptDataResponse_httpStatus = Lens.lens (\EncryptDataResponse' {httpStatus} -> httpStatus) (\s@EncryptDataResponse' {} a -> s {httpStatus = a} :: EncryptDataResponse)

-- | The encrypted ciphertext.
encryptDataResponse_cipherText :: Lens.Lens' EncryptDataResponse Prelude.Text
encryptDataResponse_cipherText = Lens.lens (\EncryptDataResponse' {cipherText} -> cipherText) (\s@EncryptDataResponse' {} a -> s {cipherText = a} :: EncryptDataResponse) Prelude.. Data._Sensitive

-- | The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses for plaintext encryption.
encryptDataResponse_keyArn :: Lens.Lens' EncryptDataResponse Prelude.Text
encryptDataResponse_keyArn = Lens.lens (\EncryptDataResponse' {keyArn} -> keyArn) (\s@EncryptDataResponse' {} a -> s {keyArn = a} :: EncryptDataResponse)

-- | The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
encryptDataResponse_keyCheckValue :: Lens.Lens' EncryptDataResponse Prelude.Text
encryptDataResponse_keyCheckValue = Lens.lens (\EncryptDataResponse' {keyCheckValue} -> keyCheckValue) (\s@EncryptDataResponse' {} a -> s {keyCheckValue = a} :: EncryptDataResponse)

instance Prelude.NFData EncryptDataResponse where
  rnf EncryptDataResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf cipherText
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyCheckValue
