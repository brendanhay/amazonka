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
-- Module      : Amazonka.PaymentCryptographyData.VerifyCardValidationData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies card-related validation data using algorithms such as Card
-- Verification Values (CVV\/CVV2), Dynamic Card Verification Values
-- (dCVV\/dCVV2) and Card Security Codes (CSC). For more information, see
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/verify-card-data.html Verify card data>
-- in the /Amazon Web Services Payment Cryptography User Guide/.
--
-- This operation validates the CVV or CSC codes that is printed on a
-- payment credit or debit card during card payment transaction. The input
-- values are typically provided as part of an inbound transaction to an
-- issuer or supporting platform partner. Amazon Web Services Payment
-- Cryptography uses CVV or CSC, PAN (Primary Account Number) and
-- expiration date of the card to check its validity during transaction
-- processing. In this operation, the CVK (Card Verification Key)
-- encryption key for use with card data verification is same as the one in
-- used for GenerateCardValidationData.
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
-- -   VerifyAuthRequestCryptogram
--
-- -   VerifyPinData
module Amazonka.PaymentCryptographyData.VerifyCardValidationData
  ( -- * Creating a Request
    VerifyCardValidationData (..),
    newVerifyCardValidationData,

    -- * Request Lenses
    verifyCardValidationData_keyIdentifier,
    verifyCardValidationData_primaryAccountNumber,
    verifyCardValidationData_validationData,
    verifyCardValidationData_verificationAttributes,

    -- * Destructuring the Response
    VerifyCardValidationDataResponse (..),
    newVerifyCardValidationDataResponse,

    -- * Response Lenses
    verifyCardValidationDataResponse_httpStatus,
    verifyCardValidationDataResponse_keyArn,
    verifyCardValidationDataResponse_keyCheckValue,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newVerifyCardValidationData' smart constructor.
data VerifyCardValidationData = VerifyCardValidationData'
  { -- | The @keyARN@ of the CVK encryption key that Amazon Web Services Payment
    -- Cryptography uses to verify card data.
    keyIdentifier :: Prelude.Text,
    -- | The Primary Account Number (PAN), a unique identifier for a payment
    -- credit or debit card that associates the card with a specific account
    -- holder.
    primaryAccountNumber :: Data.Sensitive Prelude.Text,
    -- | The CVV or CSC value for use for card data verification within Amazon
    -- Web Services Payment Cryptography.
    validationData :: Prelude.Text,
    -- | The algorithm to use for verification of card data within Amazon Web
    -- Services Payment Cryptography.
    verificationAttributes :: CardVerificationAttributes
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyCardValidationData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyIdentifier', 'verifyCardValidationData_keyIdentifier' - The @keyARN@ of the CVK encryption key that Amazon Web Services Payment
-- Cryptography uses to verify card data.
--
-- 'primaryAccountNumber', 'verifyCardValidationData_primaryAccountNumber' - The Primary Account Number (PAN), a unique identifier for a payment
-- credit or debit card that associates the card with a specific account
-- holder.
--
-- 'validationData', 'verifyCardValidationData_validationData' - The CVV or CSC value for use for card data verification within Amazon
-- Web Services Payment Cryptography.
--
-- 'verificationAttributes', 'verifyCardValidationData_verificationAttributes' - The algorithm to use for verification of card data within Amazon Web
-- Services Payment Cryptography.
newVerifyCardValidationData ::
  -- | 'keyIdentifier'
  Prelude.Text ->
  -- | 'primaryAccountNumber'
  Prelude.Text ->
  -- | 'validationData'
  Prelude.Text ->
  -- | 'verificationAttributes'
  CardVerificationAttributes ->
  VerifyCardValidationData
newVerifyCardValidationData
  pKeyIdentifier_
  pPrimaryAccountNumber_
  pValidationData_
  pVerificationAttributes_ =
    VerifyCardValidationData'
      { keyIdentifier =
          pKeyIdentifier_,
        primaryAccountNumber =
          Data._Sensitive Lens.# pPrimaryAccountNumber_,
        validationData = pValidationData_,
        verificationAttributes = pVerificationAttributes_
      }

-- | The @keyARN@ of the CVK encryption key that Amazon Web Services Payment
-- Cryptography uses to verify card data.
verifyCardValidationData_keyIdentifier :: Lens.Lens' VerifyCardValidationData Prelude.Text
verifyCardValidationData_keyIdentifier = Lens.lens (\VerifyCardValidationData' {keyIdentifier} -> keyIdentifier) (\s@VerifyCardValidationData' {} a -> s {keyIdentifier = a} :: VerifyCardValidationData)

-- | The Primary Account Number (PAN), a unique identifier for a payment
-- credit or debit card that associates the card with a specific account
-- holder.
verifyCardValidationData_primaryAccountNumber :: Lens.Lens' VerifyCardValidationData Prelude.Text
verifyCardValidationData_primaryAccountNumber = Lens.lens (\VerifyCardValidationData' {primaryAccountNumber} -> primaryAccountNumber) (\s@VerifyCardValidationData' {} a -> s {primaryAccountNumber = a} :: VerifyCardValidationData) Prelude.. Data._Sensitive

-- | The CVV or CSC value for use for card data verification within Amazon
-- Web Services Payment Cryptography.
verifyCardValidationData_validationData :: Lens.Lens' VerifyCardValidationData Prelude.Text
verifyCardValidationData_validationData = Lens.lens (\VerifyCardValidationData' {validationData} -> validationData) (\s@VerifyCardValidationData' {} a -> s {validationData = a} :: VerifyCardValidationData)

-- | The algorithm to use for verification of card data within Amazon Web
-- Services Payment Cryptography.
verifyCardValidationData_verificationAttributes :: Lens.Lens' VerifyCardValidationData CardVerificationAttributes
verifyCardValidationData_verificationAttributes = Lens.lens (\VerifyCardValidationData' {verificationAttributes} -> verificationAttributes) (\s@VerifyCardValidationData' {} a -> s {verificationAttributes = a} :: VerifyCardValidationData)

instance Core.AWSRequest VerifyCardValidationData where
  type
    AWSResponse VerifyCardValidationData =
      VerifyCardValidationDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          VerifyCardValidationDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "KeyArn")
            Prelude.<*> (x Data..:> "KeyCheckValue")
      )

instance Prelude.Hashable VerifyCardValidationData where
  hashWithSalt _salt VerifyCardValidationData' {..} =
    _salt
      `Prelude.hashWithSalt` keyIdentifier
      `Prelude.hashWithSalt` primaryAccountNumber
      `Prelude.hashWithSalt` validationData
      `Prelude.hashWithSalt` verificationAttributes

instance Prelude.NFData VerifyCardValidationData where
  rnf VerifyCardValidationData' {..} =
    Prelude.rnf keyIdentifier
      `Prelude.seq` Prelude.rnf primaryAccountNumber
      `Prelude.seq` Prelude.rnf validationData
      `Prelude.seq` Prelude.rnf verificationAttributes

instance Data.ToHeaders VerifyCardValidationData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON VerifyCardValidationData where
  toJSON VerifyCardValidationData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("KeyIdentifier" Data..= keyIdentifier),
            Prelude.Just
              ( "PrimaryAccountNumber"
                  Data..= primaryAccountNumber
              ),
            Prelude.Just
              ("ValidationData" Data..= validationData),
            Prelude.Just
              ( "VerificationAttributes"
                  Data..= verificationAttributes
              )
          ]
      )

instance Data.ToPath VerifyCardValidationData where
  toPath = Prelude.const "/cardvalidationdata/verify"

instance Data.ToQuery VerifyCardValidationData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newVerifyCardValidationDataResponse' smart constructor.
data VerifyCardValidationDataResponse = VerifyCardValidationDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @keyARN@ of the CVK encryption key that Amazon Web Services Payment
    -- Cryptography uses to verify CVV or CSC.
    keyArn :: Prelude.Text,
    -- | The key check value (KCV) of the encryption key. The KCV is used to
    -- check if all parties holding a given key have the same key or to detect
    -- that a key has changed. Amazon Web Services Payment Cryptography
    -- calculates the KCV by using standard algorithms, typically by encrypting
    -- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
    -- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
    keyCheckValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyCardValidationDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'verifyCardValidationDataResponse_httpStatus' - The response's http status code.
--
-- 'keyArn', 'verifyCardValidationDataResponse_keyArn' - The @keyARN@ of the CVK encryption key that Amazon Web Services Payment
-- Cryptography uses to verify CVV or CSC.
--
-- 'keyCheckValue', 'verifyCardValidationDataResponse_keyCheckValue' - The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
newVerifyCardValidationDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'keyArn'
  Prelude.Text ->
  -- | 'keyCheckValue'
  Prelude.Text ->
  VerifyCardValidationDataResponse
newVerifyCardValidationDataResponse
  pHttpStatus_
  pKeyArn_
  pKeyCheckValue_ =
    VerifyCardValidationDataResponse'
      { httpStatus =
          pHttpStatus_,
        keyArn = pKeyArn_,
        keyCheckValue = pKeyCheckValue_
      }

-- | The response's http status code.
verifyCardValidationDataResponse_httpStatus :: Lens.Lens' VerifyCardValidationDataResponse Prelude.Int
verifyCardValidationDataResponse_httpStatus = Lens.lens (\VerifyCardValidationDataResponse' {httpStatus} -> httpStatus) (\s@VerifyCardValidationDataResponse' {} a -> s {httpStatus = a} :: VerifyCardValidationDataResponse)

-- | The @keyARN@ of the CVK encryption key that Amazon Web Services Payment
-- Cryptography uses to verify CVV or CSC.
verifyCardValidationDataResponse_keyArn :: Lens.Lens' VerifyCardValidationDataResponse Prelude.Text
verifyCardValidationDataResponse_keyArn = Lens.lens (\VerifyCardValidationDataResponse' {keyArn} -> keyArn) (\s@VerifyCardValidationDataResponse' {} a -> s {keyArn = a} :: VerifyCardValidationDataResponse)

-- | The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
verifyCardValidationDataResponse_keyCheckValue :: Lens.Lens' VerifyCardValidationDataResponse Prelude.Text
verifyCardValidationDataResponse_keyCheckValue = Lens.lens (\VerifyCardValidationDataResponse' {keyCheckValue} -> keyCheckValue) (\s@VerifyCardValidationDataResponse' {} a -> s {keyCheckValue = a} :: VerifyCardValidationDataResponse)

instance
  Prelude.NFData
    VerifyCardValidationDataResponse
  where
  rnf VerifyCardValidationDataResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyCheckValue
