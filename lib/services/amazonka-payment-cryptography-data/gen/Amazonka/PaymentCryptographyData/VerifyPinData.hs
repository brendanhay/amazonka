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
-- Module      : Amazonka.PaymentCryptographyData.VerifyPinData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies pin-related data such as PIN and PIN Offset using algorithms
-- including VISA PVV and IBM3624. For more information, see
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/verify-pin-data.html Verify PIN data>
-- in the /Amazon Web Services Payment Cryptography User Guide/.
--
-- This operation verifies PIN data for user payment card. A card holder
-- PIN data is never transmitted in clear to or from Amazon Web Services
-- Payment Cryptography. This operation uses PIN Verification Key (PVK) for
-- PIN or PIN Offset generation and then encrypts it using PIN Encryption
-- Key (PEK) to create an @EncryptedPinBlock@ for transmission from Amazon
-- Web Services Payment Cryptography.
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
-- -   GeneratePinData
--
-- -   TranslatePinData
module Amazonka.PaymentCryptographyData.VerifyPinData
  ( -- * Creating a Request
    VerifyPinData (..),
    newVerifyPinData,

    -- * Request Lenses
    verifyPinData_dukptAttributes,
    verifyPinData_pinDataLength,
    verifyPinData_encryptedPinBlock,
    verifyPinData_encryptionKeyIdentifier,
    verifyPinData_pinBlockFormat,
    verifyPinData_primaryAccountNumber,
    verifyPinData_verificationAttributes,
    verifyPinData_verificationKeyIdentifier,

    -- * Destructuring the Response
    VerifyPinDataResponse (..),
    newVerifyPinDataResponse,

    -- * Response Lenses
    verifyPinDataResponse_httpStatus,
    verifyPinDataResponse_encryptionKeyArn,
    verifyPinDataResponse_encryptionKeyCheckValue,
    verifyPinDataResponse_verificationKeyArn,
    verifyPinDataResponse_verificationKeyCheckValue,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newVerifyPinData' smart constructor.
data VerifyPinData = VerifyPinData'
  { -- | The attributes and values for the DUKPT encrypted PIN block data.
    dukptAttributes :: Prelude.Maybe DukptAttributes,
    -- | The length of PIN being verified.
    pinDataLength :: Prelude.Maybe Prelude.Natural,
    -- | The encrypted PIN block data that Amazon Web Services Payment
    -- Cryptography verifies.
    encryptedPinBlock :: Prelude.Text,
    -- | The @keyARN@ of the encryption key under which the PIN block data is
    -- encrypted. This key type can be PEK or BDK.
    encryptionKeyIdentifier :: Prelude.Text,
    -- | The PIN encoding format for pin data generation as specified in ISO
    -- 9564. Amazon Web Services Payment Cryptography supports @ISO_Format_0@
    -- and @ISO_Format_3@.
    --
    -- The @ISO_Format_0@ PIN block format is equivalent to the ANSI X9.8,
    -- VISA-1, and ECI-1 PIN block formats. It is similar to a VISA-4 PIN block
    -- format. It supports a PIN from 4 to 12 digits in length.
    --
    -- The @ISO_Format_3@ PIN block format is the same as @ISO_Format_0@ except
    -- that the fill digits are random values from 10 to 15.
    pinBlockFormat :: PinBlockFormatForPinData,
    -- | The Primary Account Number (PAN), a unique identifier for a payment
    -- credit or debit card that associates the card with a specific account
    -- holder.
    primaryAccountNumber :: Data.Sensitive Prelude.Text,
    -- | The attributes and values for PIN data verification.
    verificationAttributes :: PinVerificationAttributes,
    -- | The @keyARN@ of the PIN verification key.
    verificationKeyIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyPinData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dukptAttributes', 'verifyPinData_dukptAttributes' - The attributes and values for the DUKPT encrypted PIN block data.
--
-- 'pinDataLength', 'verifyPinData_pinDataLength' - The length of PIN being verified.
--
-- 'encryptedPinBlock', 'verifyPinData_encryptedPinBlock' - The encrypted PIN block data that Amazon Web Services Payment
-- Cryptography verifies.
--
-- 'encryptionKeyIdentifier', 'verifyPinData_encryptionKeyIdentifier' - The @keyARN@ of the encryption key under which the PIN block data is
-- encrypted. This key type can be PEK or BDK.
--
-- 'pinBlockFormat', 'verifyPinData_pinBlockFormat' - The PIN encoding format for pin data generation as specified in ISO
-- 9564. Amazon Web Services Payment Cryptography supports @ISO_Format_0@
-- and @ISO_Format_3@.
--
-- The @ISO_Format_0@ PIN block format is equivalent to the ANSI X9.8,
-- VISA-1, and ECI-1 PIN block formats. It is similar to a VISA-4 PIN block
-- format. It supports a PIN from 4 to 12 digits in length.
--
-- The @ISO_Format_3@ PIN block format is the same as @ISO_Format_0@ except
-- that the fill digits are random values from 10 to 15.
--
-- 'primaryAccountNumber', 'verifyPinData_primaryAccountNumber' - The Primary Account Number (PAN), a unique identifier for a payment
-- credit or debit card that associates the card with a specific account
-- holder.
--
-- 'verificationAttributes', 'verifyPinData_verificationAttributes' - The attributes and values for PIN data verification.
--
-- 'verificationKeyIdentifier', 'verifyPinData_verificationKeyIdentifier' - The @keyARN@ of the PIN verification key.
newVerifyPinData ::
  -- | 'encryptedPinBlock'
  Prelude.Text ->
  -- | 'encryptionKeyIdentifier'
  Prelude.Text ->
  -- | 'pinBlockFormat'
  PinBlockFormatForPinData ->
  -- | 'primaryAccountNumber'
  Prelude.Text ->
  -- | 'verificationAttributes'
  PinVerificationAttributes ->
  -- | 'verificationKeyIdentifier'
  Prelude.Text ->
  VerifyPinData
newVerifyPinData
  pEncryptedPinBlock_
  pEncryptionKeyIdentifier_
  pPinBlockFormat_
  pPrimaryAccountNumber_
  pVerificationAttributes_
  pVerificationKeyIdentifier_ =
    VerifyPinData'
      { dukptAttributes = Prelude.Nothing,
        pinDataLength = Prelude.Nothing,
        encryptedPinBlock = pEncryptedPinBlock_,
        encryptionKeyIdentifier = pEncryptionKeyIdentifier_,
        pinBlockFormat = pPinBlockFormat_,
        primaryAccountNumber =
          Data._Sensitive Lens.# pPrimaryAccountNumber_,
        verificationAttributes = pVerificationAttributes_,
        verificationKeyIdentifier =
          pVerificationKeyIdentifier_
      }

-- | The attributes and values for the DUKPT encrypted PIN block data.
verifyPinData_dukptAttributes :: Lens.Lens' VerifyPinData (Prelude.Maybe DukptAttributes)
verifyPinData_dukptAttributes = Lens.lens (\VerifyPinData' {dukptAttributes} -> dukptAttributes) (\s@VerifyPinData' {} a -> s {dukptAttributes = a} :: VerifyPinData)

-- | The length of PIN being verified.
verifyPinData_pinDataLength :: Lens.Lens' VerifyPinData (Prelude.Maybe Prelude.Natural)
verifyPinData_pinDataLength = Lens.lens (\VerifyPinData' {pinDataLength} -> pinDataLength) (\s@VerifyPinData' {} a -> s {pinDataLength = a} :: VerifyPinData)

-- | The encrypted PIN block data that Amazon Web Services Payment
-- Cryptography verifies.
verifyPinData_encryptedPinBlock :: Lens.Lens' VerifyPinData Prelude.Text
verifyPinData_encryptedPinBlock = Lens.lens (\VerifyPinData' {encryptedPinBlock} -> encryptedPinBlock) (\s@VerifyPinData' {} a -> s {encryptedPinBlock = a} :: VerifyPinData)

-- | The @keyARN@ of the encryption key under which the PIN block data is
-- encrypted. This key type can be PEK or BDK.
verifyPinData_encryptionKeyIdentifier :: Lens.Lens' VerifyPinData Prelude.Text
verifyPinData_encryptionKeyIdentifier = Lens.lens (\VerifyPinData' {encryptionKeyIdentifier} -> encryptionKeyIdentifier) (\s@VerifyPinData' {} a -> s {encryptionKeyIdentifier = a} :: VerifyPinData)

-- | The PIN encoding format for pin data generation as specified in ISO
-- 9564. Amazon Web Services Payment Cryptography supports @ISO_Format_0@
-- and @ISO_Format_3@.
--
-- The @ISO_Format_0@ PIN block format is equivalent to the ANSI X9.8,
-- VISA-1, and ECI-1 PIN block formats. It is similar to a VISA-4 PIN block
-- format. It supports a PIN from 4 to 12 digits in length.
--
-- The @ISO_Format_3@ PIN block format is the same as @ISO_Format_0@ except
-- that the fill digits are random values from 10 to 15.
verifyPinData_pinBlockFormat :: Lens.Lens' VerifyPinData PinBlockFormatForPinData
verifyPinData_pinBlockFormat = Lens.lens (\VerifyPinData' {pinBlockFormat} -> pinBlockFormat) (\s@VerifyPinData' {} a -> s {pinBlockFormat = a} :: VerifyPinData)

-- | The Primary Account Number (PAN), a unique identifier for a payment
-- credit or debit card that associates the card with a specific account
-- holder.
verifyPinData_primaryAccountNumber :: Lens.Lens' VerifyPinData Prelude.Text
verifyPinData_primaryAccountNumber = Lens.lens (\VerifyPinData' {primaryAccountNumber} -> primaryAccountNumber) (\s@VerifyPinData' {} a -> s {primaryAccountNumber = a} :: VerifyPinData) Prelude.. Data._Sensitive

-- | The attributes and values for PIN data verification.
verifyPinData_verificationAttributes :: Lens.Lens' VerifyPinData PinVerificationAttributes
verifyPinData_verificationAttributes = Lens.lens (\VerifyPinData' {verificationAttributes} -> verificationAttributes) (\s@VerifyPinData' {} a -> s {verificationAttributes = a} :: VerifyPinData)

-- | The @keyARN@ of the PIN verification key.
verifyPinData_verificationKeyIdentifier :: Lens.Lens' VerifyPinData Prelude.Text
verifyPinData_verificationKeyIdentifier = Lens.lens (\VerifyPinData' {verificationKeyIdentifier} -> verificationKeyIdentifier) (\s@VerifyPinData' {} a -> s {verificationKeyIdentifier = a} :: VerifyPinData)

instance Core.AWSRequest VerifyPinData where
  type
    AWSResponse VerifyPinData =
      VerifyPinDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          VerifyPinDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EncryptionKeyArn")
            Prelude.<*> (x Data..:> "EncryptionKeyCheckValue")
            Prelude.<*> (x Data..:> "VerificationKeyArn")
            Prelude.<*> (x Data..:> "VerificationKeyCheckValue")
      )

instance Prelude.Hashable VerifyPinData where
  hashWithSalt _salt VerifyPinData' {..} =
    _salt
      `Prelude.hashWithSalt` dukptAttributes
      `Prelude.hashWithSalt` pinDataLength
      `Prelude.hashWithSalt` encryptedPinBlock
      `Prelude.hashWithSalt` encryptionKeyIdentifier
      `Prelude.hashWithSalt` pinBlockFormat
      `Prelude.hashWithSalt` primaryAccountNumber
      `Prelude.hashWithSalt` verificationAttributes
      `Prelude.hashWithSalt` verificationKeyIdentifier

instance Prelude.NFData VerifyPinData where
  rnf VerifyPinData' {..} =
    Prelude.rnf dukptAttributes
      `Prelude.seq` Prelude.rnf pinDataLength
      `Prelude.seq` Prelude.rnf encryptedPinBlock
      `Prelude.seq` Prelude.rnf encryptionKeyIdentifier
      `Prelude.seq` Prelude.rnf pinBlockFormat
      `Prelude.seq` Prelude.rnf primaryAccountNumber
      `Prelude.seq` Prelude.rnf verificationAttributes
      `Prelude.seq` Prelude.rnf verificationKeyIdentifier

instance Data.ToHeaders VerifyPinData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON VerifyPinData where
  toJSON VerifyPinData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DukptAttributes" Data..=)
              Prelude.<$> dukptAttributes,
            ("PinDataLength" Data..=) Prelude.<$> pinDataLength,
            Prelude.Just
              ("EncryptedPinBlock" Data..= encryptedPinBlock),
            Prelude.Just
              ( "EncryptionKeyIdentifier"
                  Data..= encryptionKeyIdentifier
              ),
            Prelude.Just
              ("PinBlockFormat" Data..= pinBlockFormat),
            Prelude.Just
              ( "PrimaryAccountNumber"
                  Data..= primaryAccountNumber
              ),
            Prelude.Just
              ( "VerificationAttributes"
                  Data..= verificationAttributes
              ),
            Prelude.Just
              ( "VerificationKeyIdentifier"
                  Data..= verificationKeyIdentifier
              )
          ]
      )

instance Data.ToPath VerifyPinData where
  toPath = Prelude.const "/pindata/verify"

instance Data.ToQuery VerifyPinData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newVerifyPinDataResponse' smart constructor.
data VerifyPinDataResponse = VerifyPinDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @keyARN@ of the PEK that Amazon Web Services Payment Cryptography
    -- uses for encrypted pin block generation.
    encryptionKeyArn :: Prelude.Text,
    -- | The key check value (KCV) of the encryption key. The KCV is used to
    -- check if all parties holding a given key have the same key or to detect
    -- that a key has changed. Amazon Web Services Payment Cryptography
    -- calculates the KCV by using standard algorithms, typically by encrypting
    -- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
    -- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
    encryptionKeyCheckValue :: Prelude.Text,
    -- | The @keyARN@ of the PIN encryption key that Amazon Web Services Payment
    -- Cryptography uses for PIN or PIN Offset verification.
    verificationKeyArn :: Prelude.Text,
    -- | The key check value (KCV) of the encryption key. The KCV is used to
    -- check if all parties holding a given key have the same key or to detect
    -- that a key has changed. Amazon Web Services Payment Cryptography
    -- calculates the KCV by using standard algorithms, typically by encrypting
    -- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
    -- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
    verificationKeyCheckValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyPinDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'verifyPinDataResponse_httpStatus' - The response's http status code.
--
-- 'encryptionKeyArn', 'verifyPinDataResponse_encryptionKeyArn' - The @keyARN@ of the PEK that Amazon Web Services Payment Cryptography
-- uses for encrypted pin block generation.
--
-- 'encryptionKeyCheckValue', 'verifyPinDataResponse_encryptionKeyCheckValue' - The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
--
-- 'verificationKeyArn', 'verifyPinDataResponse_verificationKeyArn' - The @keyARN@ of the PIN encryption key that Amazon Web Services Payment
-- Cryptography uses for PIN or PIN Offset verification.
--
-- 'verificationKeyCheckValue', 'verifyPinDataResponse_verificationKeyCheckValue' - The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
newVerifyPinDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'encryptionKeyArn'
  Prelude.Text ->
  -- | 'encryptionKeyCheckValue'
  Prelude.Text ->
  -- | 'verificationKeyArn'
  Prelude.Text ->
  -- | 'verificationKeyCheckValue'
  Prelude.Text ->
  VerifyPinDataResponse
newVerifyPinDataResponse
  pHttpStatus_
  pEncryptionKeyArn_
  pEncryptionKeyCheckValue_
  pVerificationKeyArn_
  pVerificationKeyCheckValue_ =
    VerifyPinDataResponse'
      { httpStatus = pHttpStatus_,
        encryptionKeyArn = pEncryptionKeyArn_,
        encryptionKeyCheckValue = pEncryptionKeyCheckValue_,
        verificationKeyArn = pVerificationKeyArn_,
        verificationKeyCheckValue =
          pVerificationKeyCheckValue_
      }

-- | The response's http status code.
verifyPinDataResponse_httpStatus :: Lens.Lens' VerifyPinDataResponse Prelude.Int
verifyPinDataResponse_httpStatus = Lens.lens (\VerifyPinDataResponse' {httpStatus} -> httpStatus) (\s@VerifyPinDataResponse' {} a -> s {httpStatus = a} :: VerifyPinDataResponse)

-- | The @keyARN@ of the PEK that Amazon Web Services Payment Cryptography
-- uses for encrypted pin block generation.
verifyPinDataResponse_encryptionKeyArn :: Lens.Lens' VerifyPinDataResponse Prelude.Text
verifyPinDataResponse_encryptionKeyArn = Lens.lens (\VerifyPinDataResponse' {encryptionKeyArn} -> encryptionKeyArn) (\s@VerifyPinDataResponse' {} a -> s {encryptionKeyArn = a} :: VerifyPinDataResponse)

-- | The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
verifyPinDataResponse_encryptionKeyCheckValue :: Lens.Lens' VerifyPinDataResponse Prelude.Text
verifyPinDataResponse_encryptionKeyCheckValue = Lens.lens (\VerifyPinDataResponse' {encryptionKeyCheckValue} -> encryptionKeyCheckValue) (\s@VerifyPinDataResponse' {} a -> s {encryptionKeyCheckValue = a} :: VerifyPinDataResponse)

-- | The @keyARN@ of the PIN encryption key that Amazon Web Services Payment
-- Cryptography uses for PIN or PIN Offset verification.
verifyPinDataResponse_verificationKeyArn :: Lens.Lens' VerifyPinDataResponse Prelude.Text
verifyPinDataResponse_verificationKeyArn = Lens.lens (\VerifyPinDataResponse' {verificationKeyArn} -> verificationKeyArn) (\s@VerifyPinDataResponse' {} a -> s {verificationKeyArn = a} :: VerifyPinDataResponse)

-- | The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
verifyPinDataResponse_verificationKeyCheckValue :: Lens.Lens' VerifyPinDataResponse Prelude.Text
verifyPinDataResponse_verificationKeyCheckValue = Lens.lens (\VerifyPinDataResponse' {verificationKeyCheckValue} -> verificationKeyCheckValue) (\s@VerifyPinDataResponse' {} a -> s {verificationKeyCheckValue = a} :: VerifyPinDataResponse)

instance Prelude.NFData VerifyPinDataResponse where
  rnf VerifyPinDataResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf encryptionKeyArn
      `Prelude.seq` Prelude.rnf encryptionKeyCheckValue
      `Prelude.seq` Prelude.rnf verificationKeyArn
      `Prelude.seq` Prelude.rnf verificationKeyCheckValue
