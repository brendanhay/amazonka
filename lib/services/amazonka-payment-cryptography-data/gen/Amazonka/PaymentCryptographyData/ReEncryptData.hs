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
-- Module      : Amazonka.PaymentCryptographyData.ReEncryptData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Re-encrypt ciphertext using DUKPT, Symmetric and Asymmetric Data
-- Encryption Keys.
--
-- You can either generate an encryption key within Amazon Web Services
-- Payment Cryptography by calling
-- <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_CreateKey.html CreateKey>
-- or import your own encryption key by calling
-- <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_ImportKey.html ImportKey>.
-- The @KeyArn@ for use with this operation must be in a compatible key
-- state with @KeyModesOfUse@ set to @Encrypt@. In asymmetric encryption,
-- ciphertext is encrypted using public component (imported by calling
-- <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_ImportKey.html ImportKey>)
-- of the asymmetric key pair created outside of Amazon Web Services
-- Payment Cryptography.
--
-- For symmetric and DUKPT encryption, Amazon Web Services Payment
-- Cryptography supports @TDES@ and @AES@ algorithms. For asymmetric
-- encryption, Amazon Web Services Payment Cryptography supports @RSA@. To
-- encrypt using DUKPT, a DUKPT key must already exist within your account
-- with @KeyModesOfUse@ set to @DeriveKey@ or a new DUKPT can be generated
-- by calling
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
-- -   EncryptData
--
-- -   <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_GetPublicKeyCertificate.html GetPublicCertificate>
--
-- -   <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_ImportKey.html ImportKey>
module Amazonka.PaymentCryptographyData.ReEncryptData
  ( -- * Creating a Request
    ReEncryptData (..),
    newReEncryptData,

    -- * Request Lenses
    reEncryptData_cipherText,
    reEncryptData_incomingEncryptionAttributes,
    reEncryptData_incomingKeyIdentifier,
    reEncryptData_outgoingEncryptionAttributes,
    reEncryptData_outgoingKeyIdentifier,

    -- * Destructuring the Response
    ReEncryptDataResponse (..),
    newReEncryptDataResponse,

    -- * Response Lenses
    reEncryptDataResponse_httpStatus,
    reEncryptDataResponse_cipherText,
    reEncryptDataResponse_keyArn,
    reEncryptDataResponse_keyCheckValue,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReEncryptData' smart constructor.
data ReEncryptData = ReEncryptData'
  { -- | Ciphertext to be encrypted. The minimum allowed length is 16 bytes and
    -- maximum allowed length is 4096 bytes.
    cipherText :: Data.Sensitive Prelude.Text,
    -- | The attributes and values for incoming ciphertext.
    incomingEncryptionAttributes :: ReEncryptionAttributes,
    -- | The @keyARN@ of the encryption key of incoming ciphertext data.
    incomingKeyIdentifier :: Prelude.Text,
    -- | The attributes and values for outgoing ciphertext data after encryption
    -- by Amazon Web Services Payment Cryptography.
    outgoingEncryptionAttributes :: ReEncryptionAttributes,
    -- | The @keyARN@ of the encryption key of outgoing ciphertext data after
    -- encryption by Amazon Web Services Payment Cryptography.
    outgoingKeyIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReEncryptData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cipherText', 'reEncryptData_cipherText' - Ciphertext to be encrypted. The minimum allowed length is 16 bytes and
-- maximum allowed length is 4096 bytes.
--
-- 'incomingEncryptionAttributes', 'reEncryptData_incomingEncryptionAttributes' - The attributes and values for incoming ciphertext.
--
-- 'incomingKeyIdentifier', 'reEncryptData_incomingKeyIdentifier' - The @keyARN@ of the encryption key of incoming ciphertext data.
--
-- 'outgoingEncryptionAttributes', 'reEncryptData_outgoingEncryptionAttributes' - The attributes and values for outgoing ciphertext data after encryption
-- by Amazon Web Services Payment Cryptography.
--
-- 'outgoingKeyIdentifier', 'reEncryptData_outgoingKeyIdentifier' - The @keyARN@ of the encryption key of outgoing ciphertext data after
-- encryption by Amazon Web Services Payment Cryptography.
newReEncryptData ::
  -- | 'cipherText'
  Prelude.Text ->
  -- | 'incomingEncryptionAttributes'
  ReEncryptionAttributes ->
  -- | 'incomingKeyIdentifier'
  Prelude.Text ->
  -- | 'outgoingEncryptionAttributes'
  ReEncryptionAttributes ->
  -- | 'outgoingKeyIdentifier'
  Prelude.Text ->
  ReEncryptData
newReEncryptData
  pCipherText_
  pIncomingEncryptionAttributes_
  pIncomingKeyIdentifier_
  pOutgoingEncryptionAttributes_
  pOutgoingKeyIdentifier_ =
    ReEncryptData'
      { cipherText =
          Data._Sensitive Lens.# pCipherText_,
        incomingEncryptionAttributes =
          pIncomingEncryptionAttributes_,
        incomingKeyIdentifier = pIncomingKeyIdentifier_,
        outgoingEncryptionAttributes =
          pOutgoingEncryptionAttributes_,
        outgoingKeyIdentifier = pOutgoingKeyIdentifier_
      }

-- | Ciphertext to be encrypted. The minimum allowed length is 16 bytes and
-- maximum allowed length is 4096 bytes.
reEncryptData_cipherText :: Lens.Lens' ReEncryptData Prelude.Text
reEncryptData_cipherText = Lens.lens (\ReEncryptData' {cipherText} -> cipherText) (\s@ReEncryptData' {} a -> s {cipherText = a} :: ReEncryptData) Prelude.. Data._Sensitive

-- | The attributes and values for incoming ciphertext.
reEncryptData_incomingEncryptionAttributes :: Lens.Lens' ReEncryptData ReEncryptionAttributes
reEncryptData_incomingEncryptionAttributes = Lens.lens (\ReEncryptData' {incomingEncryptionAttributes} -> incomingEncryptionAttributes) (\s@ReEncryptData' {} a -> s {incomingEncryptionAttributes = a} :: ReEncryptData)

-- | The @keyARN@ of the encryption key of incoming ciphertext data.
reEncryptData_incomingKeyIdentifier :: Lens.Lens' ReEncryptData Prelude.Text
reEncryptData_incomingKeyIdentifier = Lens.lens (\ReEncryptData' {incomingKeyIdentifier} -> incomingKeyIdentifier) (\s@ReEncryptData' {} a -> s {incomingKeyIdentifier = a} :: ReEncryptData)

-- | The attributes and values for outgoing ciphertext data after encryption
-- by Amazon Web Services Payment Cryptography.
reEncryptData_outgoingEncryptionAttributes :: Lens.Lens' ReEncryptData ReEncryptionAttributes
reEncryptData_outgoingEncryptionAttributes = Lens.lens (\ReEncryptData' {outgoingEncryptionAttributes} -> outgoingEncryptionAttributes) (\s@ReEncryptData' {} a -> s {outgoingEncryptionAttributes = a} :: ReEncryptData)

-- | The @keyARN@ of the encryption key of outgoing ciphertext data after
-- encryption by Amazon Web Services Payment Cryptography.
reEncryptData_outgoingKeyIdentifier :: Lens.Lens' ReEncryptData Prelude.Text
reEncryptData_outgoingKeyIdentifier = Lens.lens (\ReEncryptData' {outgoingKeyIdentifier} -> outgoingKeyIdentifier) (\s@ReEncryptData' {} a -> s {outgoingKeyIdentifier = a} :: ReEncryptData)

instance Core.AWSRequest ReEncryptData where
  type
    AWSResponse ReEncryptData =
      ReEncryptDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ReEncryptDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CipherText")
            Prelude.<*> (x Data..:> "KeyArn")
            Prelude.<*> (x Data..:> "KeyCheckValue")
      )

instance Prelude.Hashable ReEncryptData where
  hashWithSalt _salt ReEncryptData' {..} =
    _salt
      `Prelude.hashWithSalt` cipherText
      `Prelude.hashWithSalt` incomingEncryptionAttributes
      `Prelude.hashWithSalt` incomingKeyIdentifier
      `Prelude.hashWithSalt` outgoingEncryptionAttributes
      `Prelude.hashWithSalt` outgoingKeyIdentifier

instance Prelude.NFData ReEncryptData where
  rnf ReEncryptData' {..} =
    Prelude.rnf cipherText
      `Prelude.seq` Prelude.rnf incomingEncryptionAttributes
      `Prelude.seq` Prelude.rnf incomingKeyIdentifier
      `Prelude.seq` Prelude.rnf outgoingEncryptionAttributes
      `Prelude.seq` Prelude.rnf outgoingKeyIdentifier

instance Data.ToHeaders ReEncryptData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ReEncryptData where
  toJSON ReEncryptData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("CipherText" Data..= cipherText),
            Prelude.Just
              ( "IncomingEncryptionAttributes"
                  Data..= incomingEncryptionAttributes
              ),
            Prelude.Just
              ( "OutgoingEncryptionAttributes"
                  Data..= outgoingEncryptionAttributes
              ),
            Prelude.Just
              ( "OutgoingKeyIdentifier"
                  Data..= outgoingKeyIdentifier
              )
          ]
      )

instance Data.ToPath ReEncryptData where
  toPath ReEncryptData' {..} =
    Prelude.mconcat
      [ "/keys/",
        Data.toBS incomingKeyIdentifier,
        "/reencrypt"
      ]

instance Data.ToQuery ReEncryptData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newReEncryptDataResponse' smart constructor.
data ReEncryptDataResponse = ReEncryptDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The encrypted ciphertext.
    cipherText :: Data.Sensitive Prelude.Text,
    -- | The keyARN (Amazon Resource Name) of the encryption key that Amazon Web
    -- Services Payment Cryptography uses for plaintext encryption.
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
-- Create a value of 'ReEncryptDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'reEncryptDataResponse_httpStatus' - The response's http status code.
--
-- 'cipherText', 'reEncryptDataResponse_cipherText' - The encrypted ciphertext.
--
-- 'keyArn', 'reEncryptDataResponse_keyArn' - The keyARN (Amazon Resource Name) of the encryption key that Amazon Web
-- Services Payment Cryptography uses for plaintext encryption.
--
-- 'keyCheckValue', 'reEncryptDataResponse_keyCheckValue' - The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
newReEncryptDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'cipherText'
  Prelude.Text ->
  -- | 'keyArn'
  Prelude.Text ->
  -- | 'keyCheckValue'
  Prelude.Text ->
  ReEncryptDataResponse
newReEncryptDataResponse
  pHttpStatus_
  pCipherText_
  pKeyArn_
  pKeyCheckValue_ =
    ReEncryptDataResponse'
      { httpStatus = pHttpStatus_,
        cipherText = Data._Sensitive Lens.# pCipherText_,
        keyArn = pKeyArn_,
        keyCheckValue = pKeyCheckValue_
      }

-- | The response's http status code.
reEncryptDataResponse_httpStatus :: Lens.Lens' ReEncryptDataResponse Prelude.Int
reEncryptDataResponse_httpStatus = Lens.lens (\ReEncryptDataResponse' {httpStatus} -> httpStatus) (\s@ReEncryptDataResponse' {} a -> s {httpStatus = a} :: ReEncryptDataResponse)

-- | The encrypted ciphertext.
reEncryptDataResponse_cipherText :: Lens.Lens' ReEncryptDataResponse Prelude.Text
reEncryptDataResponse_cipherText = Lens.lens (\ReEncryptDataResponse' {cipherText} -> cipherText) (\s@ReEncryptDataResponse' {} a -> s {cipherText = a} :: ReEncryptDataResponse) Prelude.. Data._Sensitive

-- | The keyARN (Amazon Resource Name) of the encryption key that Amazon Web
-- Services Payment Cryptography uses for plaintext encryption.
reEncryptDataResponse_keyArn :: Lens.Lens' ReEncryptDataResponse Prelude.Text
reEncryptDataResponse_keyArn = Lens.lens (\ReEncryptDataResponse' {keyArn} -> keyArn) (\s@ReEncryptDataResponse' {} a -> s {keyArn = a} :: ReEncryptDataResponse)

-- | The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
reEncryptDataResponse_keyCheckValue :: Lens.Lens' ReEncryptDataResponse Prelude.Text
reEncryptDataResponse_keyCheckValue = Lens.lens (\ReEncryptDataResponse' {keyCheckValue} -> keyCheckValue) (\s@ReEncryptDataResponse' {} a -> s {keyCheckValue = a} :: ReEncryptDataResponse)

instance Prelude.NFData ReEncryptDataResponse where
  rnf ReEncryptDataResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf cipherText
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyCheckValue
