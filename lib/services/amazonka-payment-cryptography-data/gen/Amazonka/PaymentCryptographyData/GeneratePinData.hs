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
-- Module      : Amazonka.PaymentCryptographyData.GeneratePinData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates pin-related data such as PIN, PIN Verification Value (PVV),
-- PIN Block, and PIN Offset during new card issuance or reissuance. For
-- more information, see
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/generate-pin-data.html Generate PIN data>
-- in the /Amazon Web Services Payment Cryptography User Guide/.
--
-- PIN data is never transmitted in clear to or from Amazon Web Services
-- Payment Cryptography. This operation generates PIN, PVV, or PIN Offset
-- and then encrypts it using Pin Encryption Key (PEK) to create an
-- @EncryptedPinBlock@ for transmission from Amazon Web Services Payment
-- Cryptography. This operation uses a separate Pin Verification Key (PVK)
-- for VISA PVV generation.
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
-- -   GenerateCardValidationData
--
-- -   TranslatePinData
--
-- -   VerifyPinData
module Amazonka.PaymentCryptographyData.GeneratePinData
  ( -- * Creating a Request
    GeneratePinData (..),
    newGeneratePinData,

    -- * Request Lenses
    generatePinData_pinDataLength,
    generatePinData_encryptionKeyIdentifier,
    generatePinData_generationAttributes,
    generatePinData_generationKeyIdentifier,
    generatePinData_pinBlockFormat,
    generatePinData_primaryAccountNumber,

    -- * Destructuring the Response
    GeneratePinDataResponse (..),
    newGeneratePinDataResponse,

    -- * Response Lenses
    generatePinDataResponse_httpStatus,
    generatePinDataResponse_encryptedPinBlock,
    generatePinDataResponse_encryptionKeyArn,
    generatePinDataResponse_encryptionKeyCheckValue,
    generatePinDataResponse_generationKeyArn,
    generatePinDataResponse_generationKeyCheckValue,
    generatePinDataResponse_pinData,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGeneratePinData' smart constructor.
data GeneratePinData = GeneratePinData'
  { -- | The length of PIN under generation.
    pinDataLength :: Prelude.Maybe Prelude.Natural,
    -- | The @keyARN@ of the PEK that Amazon Web Services Payment Cryptography
    -- uses to encrypt the PIN Block.
    encryptionKeyIdentifier :: Prelude.Text,
    -- | The attributes and values to use for PIN, PVV, or PIN Offset generation.
    generationAttributes :: PinGenerationAttributes,
    -- | The @keyARN@ of the PEK that Amazon Web Services Payment Cryptography
    -- uses for pin data generation.
    generationKeyIdentifier :: Prelude.Text,
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
    primaryAccountNumber :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeneratePinData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pinDataLength', 'generatePinData_pinDataLength' - The length of PIN under generation.
--
-- 'encryptionKeyIdentifier', 'generatePinData_encryptionKeyIdentifier' - The @keyARN@ of the PEK that Amazon Web Services Payment Cryptography
-- uses to encrypt the PIN Block.
--
-- 'generationAttributes', 'generatePinData_generationAttributes' - The attributes and values to use for PIN, PVV, or PIN Offset generation.
--
-- 'generationKeyIdentifier', 'generatePinData_generationKeyIdentifier' - The @keyARN@ of the PEK that Amazon Web Services Payment Cryptography
-- uses for pin data generation.
--
-- 'pinBlockFormat', 'generatePinData_pinBlockFormat' - The PIN encoding format for pin data generation as specified in ISO
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
-- 'primaryAccountNumber', 'generatePinData_primaryAccountNumber' - The Primary Account Number (PAN), a unique identifier for a payment
-- credit or debit card that associates the card with a specific account
-- holder.
newGeneratePinData ::
  -- | 'encryptionKeyIdentifier'
  Prelude.Text ->
  -- | 'generationAttributes'
  PinGenerationAttributes ->
  -- | 'generationKeyIdentifier'
  Prelude.Text ->
  -- | 'pinBlockFormat'
  PinBlockFormatForPinData ->
  -- | 'primaryAccountNumber'
  Prelude.Text ->
  GeneratePinData
newGeneratePinData
  pEncryptionKeyIdentifier_
  pGenerationAttributes_
  pGenerationKeyIdentifier_
  pPinBlockFormat_
  pPrimaryAccountNumber_ =
    GeneratePinData'
      { pinDataLength = Prelude.Nothing,
        encryptionKeyIdentifier = pEncryptionKeyIdentifier_,
        generationAttributes = pGenerationAttributes_,
        generationKeyIdentifier = pGenerationKeyIdentifier_,
        pinBlockFormat = pPinBlockFormat_,
        primaryAccountNumber =
          Data._Sensitive Lens.# pPrimaryAccountNumber_
      }

-- | The length of PIN under generation.
generatePinData_pinDataLength :: Lens.Lens' GeneratePinData (Prelude.Maybe Prelude.Natural)
generatePinData_pinDataLength = Lens.lens (\GeneratePinData' {pinDataLength} -> pinDataLength) (\s@GeneratePinData' {} a -> s {pinDataLength = a} :: GeneratePinData)

-- | The @keyARN@ of the PEK that Amazon Web Services Payment Cryptography
-- uses to encrypt the PIN Block.
generatePinData_encryptionKeyIdentifier :: Lens.Lens' GeneratePinData Prelude.Text
generatePinData_encryptionKeyIdentifier = Lens.lens (\GeneratePinData' {encryptionKeyIdentifier} -> encryptionKeyIdentifier) (\s@GeneratePinData' {} a -> s {encryptionKeyIdentifier = a} :: GeneratePinData)

-- | The attributes and values to use for PIN, PVV, or PIN Offset generation.
generatePinData_generationAttributes :: Lens.Lens' GeneratePinData PinGenerationAttributes
generatePinData_generationAttributes = Lens.lens (\GeneratePinData' {generationAttributes} -> generationAttributes) (\s@GeneratePinData' {} a -> s {generationAttributes = a} :: GeneratePinData)

-- | The @keyARN@ of the PEK that Amazon Web Services Payment Cryptography
-- uses for pin data generation.
generatePinData_generationKeyIdentifier :: Lens.Lens' GeneratePinData Prelude.Text
generatePinData_generationKeyIdentifier = Lens.lens (\GeneratePinData' {generationKeyIdentifier} -> generationKeyIdentifier) (\s@GeneratePinData' {} a -> s {generationKeyIdentifier = a} :: GeneratePinData)

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
generatePinData_pinBlockFormat :: Lens.Lens' GeneratePinData PinBlockFormatForPinData
generatePinData_pinBlockFormat = Lens.lens (\GeneratePinData' {pinBlockFormat} -> pinBlockFormat) (\s@GeneratePinData' {} a -> s {pinBlockFormat = a} :: GeneratePinData)

-- | The Primary Account Number (PAN), a unique identifier for a payment
-- credit or debit card that associates the card with a specific account
-- holder.
generatePinData_primaryAccountNumber :: Lens.Lens' GeneratePinData Prelude.Text
generatePinData_primaryAccountNumber = Lens.lens (\GeneratePinData' {primaryAccountNumber} -> primaryAccountNumber) (\s@GeneratePinData' {} a -> s {primaryAccountNumber = a} :: GeneratePinData) Prelude.. Data._Sensitive

instance Core.AWSRequest GeneratePinData where
  type
    AWSResponse GeneratePinData =
      GeneratePinDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GeneratePinDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EncryptedPinBlock")
            Prelude.<*> (x Data..:> "EncryptionKeyArn")
            Prelude.<*> (x Data..:> "EncryptionKeyCheckValue")
            Prelude.<*> (x Data..:> "GenerationKeyArn")
            Prelude.<*> (x Data..:> "GenerationKeyCheckValue")
            Prelude.<*> (x Data..:> "PinData")
      )

instance Prelude.Hashable GeneratePinData where
  hashWithSalt _salt GeneratePinData' {..} =
    _salt
      `Prelude.hashWithSalt` pinDataLength
      `Prelude.hashWithSalt` encryptionKeyIdentifier
      `Prelude.hashWithSalt` generationAttributes
      `Prelude.hashWithSalt` generationKeyIdentifier
      `Prelude.hashWithSalt` pinBlockFormat
      `Prelude.hashWithSalt` primaryAccountNumber

instance Prelude.NFData GeneratePinData where
  rnf GeneratePinData' {..} =
    Prelude.rnf pinDataLength
      `Prelude.seq` Prelude.rnf encryptionKeyIdentifier
      `Prelude.seq` Prelude.rnf generationAttributes
      `Prelude.seq` Prelude.rnf generationKeyIdentifier
      `Prelude.seq` Prelude.rnf pinBlockFormat
      `Prelude.seq` Prelude.rnf primaryAccountNumber

instance Data.ToHeaders GeneratePinData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GeneratePinData where
  toJSON GeneratePinData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PinDataLength" Data..=) Prelude.<$> pinDataLength,
            Prelude.Just
              ( "EncryptionKeyIdentifier"
                  Data..= encryptionKeyIdentifier
              ),
            Prelude.Just
              ( "GenerationAttributes"
                  Data..= generationAttributes
              ),
            Prelude.Just
              ( "GenerationKeyIdentifier"
                  Data..= generationKeyIdentifier
              ),
            Prelude.Just
              ("PinBlockFormat" Data..= pinBlockFormat),
            Prelude.Just
              ( "PrimaryAccountNumber"
                  Data..= primaryAccountNumber
              )
          ]
      )

instance Data.ToPath GeneratePinData where
  toPath = Prelude.const "/pindata/generate"

instance Data.ToQuery GeneratePinData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGeneratePinDataResponse' smart constructor.
data GeneratePinDataResponse = GeneratePinDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The PIN block encrypted under PEK from Amazon Web Services Payment
    -- Cryptography. The encrypted PIN block is a composite of PAN (Primary
    -- Account Number) and PIN (Personal Identification Number), generated in
    -- accordance with ISO 9564 standard.
    encryptedPinBlock :: Prelude.Text,
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
    -- | The @keyARN@ of the pin data generation key that Amazon Web Services
    -- Payment Cryptography uses for PIN, PVV or PIN Offset generation.
    generationKeyArn :: Prelude.Text,
    -- | The key check value (KCV) of the encryption key. The KCV is used to
    -- check if all parties holding a given key have the same key or to detect
    -- that a key has changed. Amazon Web Services Payment Cryptography
    -- calculates the KCV by using standard algorithms, typically by encrypting
    -- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
    -- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
    generationKeyCheckValue :: Prelude.Text,
    -- | The attributes and values Amazon Web Services Payment Cryptography uses
    -- for pin data generation.
    pinData :: PinData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeneratePinDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'generatePinDataResponse_httpStatus' - The response's http status code.
--
-- 'encryptedPinBlock', 'generatePinDataResponse_encryptedPinBlock' - The PIN block encrypted under PEK from Amazon Web Services Payment
-- Cryptography. The encrypted PIN block is a composite of PAN (Primary
-- Account Number) and PIN (Personal Identification Number), generated in
-- accordance with ISO 9564 standard.
--
-- 'encryptionKeyArn', 'generatePinDataResponse_encryptionKeyArn' - The @keyARN@ of the PEK that Amazon Web Services Payment Cryptography
-- uses for encrypted pin block generation.
--
-- 'encryptionKeyCheckValue', 'generatePinDataResponse_encryptionKeyCheckValue' - The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
--
-- 'generationKeyArn', 'generatePinDataResponse_generationKeyArn' - The @keyARN@ of the pin data generation key that Amazon Web Services
-- Payment Cryptography uses for PIN, PVV or PIN Offset generation.
--
-- 'generationKeyCheckValue', 'generatePinDataResponse_generationKeyCheckValue' - The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
--
-- 'pinData', 'generatePinDataResponse_pinData' - The attributes and values Amazon Web Services Payment Cryptography uses
-- for pin data generation.
newGeneratePinDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'encryptedPinBlock'
  Prelude.Text ->
  -- | 'encryptionKeyArn'
  Prelude.Text ->
  -- | 'encryptionKeyCheckValue'
  Prelude.Text ->
  -- | 'generationKeyArn'
  Prelude.Text ->
  -- | 'generationKeyCheckValue'
  Prelude.Text ->
  -- | 'pinData'
  PinData ->
  GeneratePinDataResponse
newGeneratePinDataResponse
  pHttpStatus_
  pEncryptedPinBlock_
  pEncryptionKeyArn_
  pEncryptionKeyCheckValue_
  pGenerationKeyArn_
  pGenerationKeyCheckValue_
  pPinData_ =
    GeneratePinDataResponse'
      { httpStatus = pHttpStatus_,
        encryptedPinBlock = pEncryptedPinBlock_,
        encryptionKeyArn = pEncryptionKeyArn_,
        encryptionKeyCheckValue =
          pEncryptionKeyCheckValue_,
        generationKeyArn = pGenerationKeyArn_,
        generationKeyCheckValue =
          pGenerationKeyCheckValue_,
        pinData = pPinData_
      }

-- | The response's http status code.
generatePinDataResponse_httpStatus :: Lens.Lens' GeneratePinDataResponse Prelude.Int
generatePinDataResponse_httpStatus = Lens.lens (\GeneratePinDataResponse' {httpStatus} -> httpStatus) (\s@GeneratePinDataResponse' {} a -> s {httpStatus = a} :: GeneratePinDataResponse)

-- | The PIN block encrypted under PEK from Amazon Web Services Payment
-- Cryptography. The encrypted PIN block is a composite of PAN (Primary
-- Account Number) and PIN (Personal Identification Number), generated in
-- accordance with ISO 9564 standard.
generatePinDataResponse_encryptedPinBlock :: Lens.Lens' GeneratePinDataResponse Prelude.Text
generatePinDataResponse_encryptedPinBlock = Lens.lens (\GeneratePinDataResponse' {encryptedPinBlock} -> encryptedPinBlock) (\s@GeneratePinDataResponse' {} a -> s {encryptedPinBlock = a} :: GeneratePinDataResponse)

-- | The @keyARN@ of the PEK that Amazon Web Services Payment Cryptography
-- uses for encrypted pin block generation.
generatePinDataResponse_encryptionKeyArn :: Lens.Lens' GeneratePinDataResponse Prelude.Text
generatePinDataResponse_encryptionKeyArn = Lens.lens (\GeneratePinDataResponse' {encryptionKeyArn} -> encryptionKeyArn) (\s@GeneratePinDataResponse' {} a -> s {encryptionKeyArn = a} :: GeneratePinDataResponse)

-- | The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
generatePinDataResponse_encryptionKeyCheckValue :: Lens.Lens' GeneratePinDataResponse Prelude.Text
generatePinDataResponse_encryptionKeyCheckValue = Lens.lens (\GeneratePinDataResponse' {encryptionKeyCheckValue} -> encryptionKeyCheckValue) (\s@GeneratePinDataResponse' {} a -> s {encryptionKeyCheckValue = a} :: GeneratePinDataResponse)

-- | The @keyARN@ of the pin data generation key that Amazon Web Services
-- Payment Cryptography uses for PIN, PVV or PIN Offset generation.
generatePinDataResponse_generationKeyArn :: Lens.Lens' GeneratePinDataResponse Prelude.Text
generatePinDataResponse_generationKeyArn = Lens.lens (\GeneratePinDataResponse' {generationKeyArn} -> generationKeyArn) (\s@GeneratePinDataResponse' {} a -> s {generationKeyArn = a} :: GeneratePinDataResponse)

-- | The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
generatePinDataResponse_generationKeyCheckValue :: Lens.Lens' GeneratePinDataResponse Prelude.Text
generatePinDataResponse_generationKeyCheckValue = Lens.lens (\GeneratePinDataResponse' {generationKeyCheckValue} -> generationKeyCheckValue) (\s@GeneratePinDataResponse' {} a -> s {generationKeyCheckValue = a} :: GeneratePinDataResponse)

-- | The attributes and values Amazon Web Services Payment Cryptography uses
-- for pin data generation.
generatePinDataResponse_pinData :: Lens.Lens' GeneratePinDataResponse PinData
generatePinDataResponse_pinData = Lens.lens (\GeneratePinDataResponse' {pinData} -> pinData) (\s@GeneratePinDataResponse' {} a -> s {pinData = a} :: GeneratePinDataResponse)

instance Prelude.NFData GeneratePinDataResponse where
  rnf GeneratePinDataResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf encryptedPinBlock
      `Prelude.seq` Prelude.rnf encryptionKeyArn
      `Prelude.seq` Prelude.rnf encryptionKeyCheckValue
      `Prelude.seq` Prelude.rnf generationKeyArn
      `Prelude.seq` Prelude.rnf generationKeyCheckValue
      `Prelude.seq` Prelude.rnf pinData
