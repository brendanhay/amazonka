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
-- Module      : Amazonka.PaymentCryptographyData.GenerateCardValidationData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates card-related validation data using algorithms such as Card
-- Verification Values (CVV\/CVV2), Dynamic Card Verification Values
-- (dCVV\/dCVV2), or Card Security Codes (CSC). For more information, see
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/generate-card-data.html Generate card data>
-- in the /Amazon Web Services Payment Cryptography User Guide/.
--
-- This operation generates a CVV or CSC value that is printed on a payment
-- credit or debit card during card production. The CVV or CSC, PAN
-- (Primary Account Number) and expiration date of the card are required to
-- check its validity during transaction processing. To begin this
-- operation, a CVK (Card Verification Key) encryption key is required. You
-- can use
-- <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_CreateKey.html CreateKey>
-- or
-- <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_ImportKey.html ImportKey>
-- to establish a CVK within Amazon Web Services Payment Cryptography. The
-- @KeyModesOfUse@ should be set to @Generate@ and @Verify@ for a CVK
-- encryption key.
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
-- -   <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_ImportKey.html ImportKey>
--
-- -   VerifyCardValidationData
module Amazonka.PaymentCryptographyData.GenerateCardValidationData
  ( -- * Creating a Request
    GenerateCardValidationData (..),
    newGenerateCardValidationData,

    -- * Request Lenses
    generateCardValidationData_validationDataLength,
    generateCardValidationData_generationAttributes,
    generateCardValidationData_keyIdentifier,
    generateCardValidationData_primaryAccountNumber,

    -- * Destructuring the Response
    GenerateCardValidationDataResponse (..),
    newGenerateCardValidationDataResponse,

    -- * Response Lenses
    generateCardValidationDataResponse_httpStatus,
    generateCardValidationDataResponse_keyArn,
    generateCardValidationDataResponse_keyCheckValue,
    generateCardValidationDataResponse_validationData,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGenerateCardValidationData' smart constructor.
data GenerateCardValidationData = GenerateCardValidationData'
  { -- | The length of the CVV or CSC to be generated. The default value is 3.
    validationDataLength :: Prelude.Maybe Prelude.Natural,
    -- | The algorithm for generating CVV or CSC values for the card within
    -- Amazon Web Services Payment Cryptography.
    generationAttributes :: CardGenerationAttributes,
    -- | The @keyARN@ of the CVK encryption key that Amazon Web Services Payment
    -- Cryptography uses to generate card data.
    keyIdentifier :: Prelude.Text,
    -- | The Primary Account Number (PAN), a unique identifier for a payment
    -- credit or debit card that associates the card with a specific account
    -- holder.
    primaryAccountNumber :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateCardValidationData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationDataLength', 'generateCardValidationData_validationDataLength' - The length of the CVV or CSC to be generated. The default value is 3.
--
-- 'generationAttributes', 'generateCardValidationData_generationAttributes' - The algorithm for generating CVV or CSC values for the card within
-- Amazon Web Services Payment Cryptography.
--
-- 'keyIdentifier', 'generateCardValidationData_keyIdentifier' - The @keyARN@ of the CVK encryption key that Amazon Web Services Payment
-- Cryptography uses to generate card data.
--
-- 'primaryAccountNumber', 'generateCardValidationData_primaryAccountNumber' - The Primary Account Number (PAN), a unique identifier for a payment
-- credit or debit card that associates the card with a specific account
-- holder.
newGenerateCardValidationData ::
  -- | 'generationAttributes'
  CardGenerationAttributes ->
  -- | 'keyIdentifier'
  Prelude.Text ->
  -- | 'primaryAccountNumber'
  Prelude.Text ->
  GenerateCardValidationData
newGenerateCardValidationData
  pGenerationAttributes_
  pKeyIdentifier_
  pPrimaryAccountNumber_ =
    GenerateCardValidationData'
      { validationDataLength =
          Prelude.Nothing,
        generationAttributes = pGenerationAttributes_,
        keyIdentifier = pKeyIdentifier_,
        primaryAccountNumber =
          Data._Sensitive Lens.# pPrimaryAccountNumber_
      }

-- | The length of the CVV or CSC to be generated. The default value is 3.
generateCardValidationData_validationDataLength :: Lens.Lens' GenerateCardValidationData (Prelude.Maybe Prelude.Natural)
generateCardValidationData_validationDataLength = Lens.lens (\GenerateCardValidationData' {validationDataLength} -> validationDataLength) (\s@GenerateCardValidationData' {} a -> s {validationDataLength = a} :: GenerateCardValidationData)

-- | The algorithm for generating CVV or CSC values for the card within
-- Amazon Web Services Payment Cryptography.
generateCardValidationData_generationAttributes :: Lens.Lens' GenerateCardValidationData CardGenerationAttributes
generateCardValidationData_generationAttributes = Lens.lens (\GenerateCardValidationData' {generationAttributes} -> generationAttributes) (\s@GenerateCardValidationData' {} a -> s {generationAttributes = a} :: GenerateCardValidationData)

-- | The @keyARN@ of the CVK encryption key that Amazon Web Services Payment
-- Cryptography uses to generate card data.
generateCardValidationData_keyIdentifier :: Lens.Lens' GenerateCardValidationData Prelude.Text
generateCardValidationData_keyIdentifier = Lens.lens (\GenerateCardValidationData' {keyIdentifier} -> keyIdentifier) (\s@GenerateCardValidationData' {} a -> s {keyIdentifier = a} :: GenerateCardValidationData)

-- | The Primary Account Number (PAN), a unique identifier for a payment
-- credit or debit card that associates the card with a specific account
-- holder.
generateCardValidationData_primaryAccountNumber :: Lens.Lens' GenerateCardValidationData Prelude.Text
generateCardValidationData_primaryAccountNumber = Lens.lens (\GenerateCardValidationData' {primaryAccountNumber} -> primaryAccountNumber) (\s@GenerateCardValidationData' {} a -> s {primaryAccountNumber = a} :: GenerateCardValidationData) Prelude.. Data._Sensitive

instance Core.AWSRequest GenerateCardValidationData where
  type
    AWSResponse GenerateCardValidationData =
      GenerateCardValidationDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateCardValidationDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "KeyArn")
            Prelude.<*> (x Data..:> "KeyCheckValue")
            Prelude.<*> (x Data..:> "ValidationData")
      )

instance Prelude.Hashable GenerateCardValidationData where
  hashWithSalt _salt GenerateCardValidationData' {..} =
    _salt
      `Prelude.hashWithSalt` validationDataLength
      `Prelude.hashWithSalt` generationAttributes
      `Prelude.hashWithSalt` keyIdentifier
      `Prelude.hashWithSalt` primaryAccountNumber

instance Prelude.NFData GenerateCardValidationData where
  rnf GenerateCardValidationData' {..} =
    Prelude.rnf validationDataLength
      `Prelude.seq` Prelude.rnf generationAttributes
      `Prelude.seq` Prelude.rnf keyIdentifier
      `Prelude.seq` Prelude.rnf primaryAccountNumber

instance Data.ToHeaders GenerateCardValidationData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GenerateCardValidationData where
  toJSON GenerateCardValidationData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ValidationDataLength" Data..=)
              Prelude.<$> validationDataLength,
            Prelude.Just
              ( "GenerationAttributes"
                  Data..= generationAttributes
              ),
            Prelude.Just ("KeyIdentifier" Data..= keyIdentifier),
            Prelude.Just
              ( "PrimaryAccountNumber"
                  Data..= primaryAccountNumber
              )
          ]
      )

instance Data.ToPath GenerateCardValidationData where
  toPath = Prelude.const "/cardvalidationdata/generate"

instance Data.ToQuery GenerateCardValidationData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGenerateCardValidationDataResponse' smart constructor.
data GenerateCardValidationDataResponse = GenerateCardValidationDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @keyARN@ of the CVK encryption key that Amazon Web Services Payment
    -- Cryptography uses to generate CVV or CSC.
    keyArn :: Prelude.Text,
    -- | The key check value (KCV) of the encryption key. The KCV is used to
    -- check if all parties holding a given key have the same key or to detect
    -- that a key has changed. Amazon Web Services Payment Cryptography
    -- calculates the KCV by using standard algorithms, typically by encrypting
    -- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
    -- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
    keyCheckValue :: Prelude.Text,
    -- | The CVV or CSC value that Amazon Web Services Payment Cryptography
    -- generates for the card.
    validationData :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateCardValidationDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'generateCardValidationDataResponse_httpStatus' - The response's http status code.
--
-- 'keyArn', 'generateCardValidationDataResponse_keyArn' - The @keyARN@ of the CVK encryption key that Amazon Web Services Payment
-- Cryptography uses to generate CVV or CSC.
--
-- 'keyCheckValue', 'generateCardValidationDataResponse_keyCheckValue' - The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
--
-- 'validationData', 'generateCardValidationDataResponse_validationData' - The CVV or CSC value that Amazon Web Services Payment Cryptography
-- generates for the card.
newGenerateCardValidationDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'keyArn'
  Prelude.Text ->
  -- | 'keyCheckValue'
  Prelude.Text ->
  -- | 'validationData'
  Prelude.Text ->
  GenerateCardValidationDataResponse
newGenerateCardValidationDataResponse
  pHttpStatus_
  pKeyArn_
  pKeyCheckValue_
  pValidationData_ =
    GenerateCardValidationDataResponse'
      { httpStatus =
          pHttpStatus_,
        keyArn = pKeyArn_,
        keyCheckValue = pKeyCheckValue_,
        validationData = pValidationData_
      }

-- | The response's http status code.
generateCardValidationDataResponse_httpStatus :: Lens.Lens' GenerateCardValidationDataResponse Prelude.Int
generateCardValidationDataResponse_httpStatus = Lens.lens (\GenerateCardValidationDataResponse' {httpStatus} -> httpStatus) (\s@GenerateCardValidationDataResponse' {} a -> s {httpStatus = a} :: GenerateCardValidationDataResponse)

-- | The @keyARN@ of the CVK encryption key that Amazon Web Services Payment
-- Cryptography uses to generate CVV or CSC.
generateCardValidationDataResponse_keyArn :: Lens.Lens' GenerateCardValidationDataResponse Prelude.Text
generateCardValidationDataResponse_keyArn = Lens.lens (\GenerateCardValidationDataResponse' {keyArn} -> keyArn) (\s@GenerateCardValidationDataResponse' {} a -> s {keyArn = a} :: GenerateCardValidationDataResponse)

-- | The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
generateCardValidationDataResponse_keyCheckValue :: Lens.Lens' GenerateCardValidationDataResponse Prelude.Text
generateCardValidationDataResponse_keyCheckValue = Lens.lens (\GenerateCardValidationDataResponse' {keyCheckValue} -> keyCheckValue) (\s@GenerateCardValidationDataResponse' {} a -> s {keyCheckValue = a} :: GenerateCardValidationDataResponse)

-- | The CVV or CSC value that Amazon Web Services Payment Cryptography
-- generates for the card.
generateCardValidationDataResponse_validationData :: Lens.Lens' GenerateCardValidationDataResponse Prelude.Text
generateCardValidationDataResponse_validationData = Lens.lens (\GenerateCardValidationDataResponse' {validationData} -> validationData) (\s@GenerateCardValidationDataResponse' {} a -> s {validationData = a} :: GenerateCardValidationDataResponse)

instance
  Prelude.NFData
    GenerateCardValidationDataResponse
  where
  rnf GenerateCardValidationDataResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyCheckValue
      `Prelude.seq` Prelude.rnf validationData
