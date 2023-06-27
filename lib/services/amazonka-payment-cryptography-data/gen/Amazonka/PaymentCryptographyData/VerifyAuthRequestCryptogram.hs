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
-- Module      : Amazonka.PaymentCryptographyData.VerifyAuthRequestCryptogram
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies Authorization Request Cryptogram (ARQC) for a EMV chip payment
-- card authorization. For more information, see
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/data-operations.verifyauthrequestcryptogram.html Verify auth request cryptogram>
-- in the /Amazon Web Services Payment Cryptography User Guide/.
--
-- ARQC generation is done outside of Amazon Web Services Payment
-- Cryptography and is typically generated on a point of sale terminal for
-- an EMV chip card to obtain payment authorization during transaction
-- time. For ARQC verification, you must first import the ARQC generated
-- outside of Amazon Web Services Payment Cryptography by calling
-- <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_ImportKey.html ImportKey>.
-- This operation uses the imported ARQC and an major encryption key
-- (DUKPT) created by calling
-- <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/API_CreateKey.html CreateKey>
-- to either provide a boolean ARQC verification result or provide an APRC
-- (Authorization Response Cryptogram) response using Method 1 or Method 2.
-- The @ARPC_METHOD_1@ uses @AuthResponseCode@ to generate ARPC and
-- @ARPC_METHOD_2@ uses @CardStatusUpdate@ to generate ARPC.
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
-- -   VerifyCardValidationData
--
-- -   VerifyPinData
module Amazonka.PaymentCryptographyData.VerifyAuthRequestCryptogram
  ( -- * Creating a Request
    VerifyAuthRequestCryptogram (..),
    newVerifyAuthRequestCryptogram,

    -- * Request Lenses
    verifyAuthRequestCryptogram_authResponseAttributes,
    verifyAuthRequestCryptogram_authRequestCryptogram,
    verifyAuthRequestCryptogram_keyIdentifier,
    verifyAuthRequestCryptogram_majorKeyDerivationMode,
    verifyAuthRequestCryptogram_sessionKeyDerivationAttributes,
    verifyAuthRequestCryptogram_transactionData,

    -- * Destructuring the Response
    VerifyAuthRequestCryptogramResponse (..),
    newVerifyAuthRequestCryptogramResponse,

    -- * Response Lenses
    verifyAuthRequestCryptogramResponse_authResponseValue,
    verifyAuthRequestCryptogramResponse_httpStatus,
    verifyAuthRequestCryptogramResponse_keyArn,
    verifyAuthRequestCryptogramResponse_keyCheckValue,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newVerifyAuthRequestCryptogram' smart constructor.
data VerifyAuthRequestCryptogram = VerifyAuthRequestCryptogram'
  { -- | The attributes and values for auth request cryptogram verification.
    -- These parameters are required in case using ARPC Method 1 or Method 2
    -- for ARQC verification.
    authResponseAttributes :: Prelude.Maybe CryptogramAuthResponse,
    -- | The auth request cryptogram imported into Amazon Web Services Payment
    -- Cryptography for ARQC verification using a major encryption key and
    -- transaction data.
    authRequestCryptogram :: Prelude.Text,
    -- | The @keyARN@ of the major encryption key that Amazon Web Services
    -- Payment Cryptography uses for ARQC verification.
    keyIdentifier :: Prelude.Text,
    -- | The method to use when deriving the major encryption key for ARQC
    -- verification within Amazon Web Services Payment Cryptography. The same
    -- key derivation mode was used for ARQC generation outside of Amazon Web
    -- Services Payment Cryptography.
    majorKeyDerivationMode :: MajorKeyDerivationMode,
    -- | The attributes and values to use for deriving a session key for ARQC
    -- verification within Amazon Web Services Payment Cryptography. The same
    -- attributes were used for ARQC generation outside of Amazon Web Services
    -- Payment Cryptography.
    sessionKeyDerivationAttributes :: SessionKeyDerivation,
    -- | The transaction data that Amazon Web Services Payment Cryptography uses
    -- for ARQC verification. The same transaction is used for ARQC generation
    -- outside of Amazon Web Services Payment Cryptography.
    transactionData :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyAuthRequestCryptogram' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authResponseAttributes', 'verifyAuthRequestCryptogram_authResponseAttributes' - The attributes and values for auth request cryptogram verification.
-- These parameters are required in case using ARPC Method 1 or Method 2
-- for ARQC verification.
--
-- 'authRequestCryptogram', 'verifyAuthRequestCryptogram_authRequestCryptogram' - The auth request cryptogram imported into Amazon Web Services Payment
-- Cryptography for ARQC verification using a major encryption key and
-- transaction data.
--
-- 'keyIdentifier', 'verifyAuthRequestCryptogram_keyIdentifier' - The @keyARN@ of the major encryption key that Amazon Web Services
-- Payment Cryptography uses for ARQC verification.
--
-- 'majorKeyDerivationMode', 'verifyAuthRequestCryptogram_majorKeyDerivationMode' - The method to use when deriving the major encryption key for ARQC
-- verification within Amazon Web Services Payment Cryptography. The same
-- key derivation mode was used for ARQC generation outside of Amazon Web
-- Services Payment Cryptography.
--
-- 'sessionKeyDerivationAttributes', 'verifyAuthRequestCryptogram_sessionKeyDerivationAttributes' - The attributes and values to use for deriving a session key for ARQC
-- verification within Amazon Web Services Payment Cryptography. The same
-- attributes were used for ARQC generation outside of Amazon Web Services
-- Payment Cryptography.
--
-- 'transactionData', 'verifyAuthRequestCryptogram_transactionData' - The transaction data that Amazon Web Services Payment Cryptography uses
-- for ARQC verification. The same transaction is used for ARQC generation
-- outside of Amazon Web Services Payment Cryptography.
newVerifyAuthRequestCryptogram ::
  -- | 'authRequestCryptogram'
  Prelude.Text ->
  -- | 'keyIdentifier'
  Prelude.Text ->
  -- | 'majorKeyDerivationMode'
  MajorKeyDerivationMode ->
  -- | 'sessionKeyDerivationAttributes'
  SessionKeyDerivation ->
  -- | 'transactionData'
  Prelude.Text ->
  VerifyAuthRequestCryptogram
newVerifyAuthRequestCryptogram
  pAuthRequestCryptogram_
  pKeyIdentifier_
  pMajorKeyDerivationMode_
  pSessionKeyDerivationAttributes_
  pTransactionData_ =
    VerifyAuthRequestCryptogram'
      { authResponseAttributes =
          Prelude.Nothing,
        authRequestCryptogram =
          pAuthRequestCryptogram_,
        keyIdentifier = pKeyIdentifier_,
        majorKeyDerivationMode =
          pMajorKeyDerivationMode_,
        sessionKeyDerivationAttributes =
          pSessionKeyDerivationAttributes_,
        transactionData = pTransactionData_
      }

-- | The attributes and values for auth request cryptogram verification.
-- These parameters are required in case using ARPC Method 1 or Method 2
-- for ARQC verification.
verifyAuthRequestCryptogram_authResponseAttributes :: Lens.Lens' VerifyAuthRequestCryptogram (Prelude.Maybe CryptogramAuthResponse)
verifyAuthRequestCryptogram_authResponseAttributes = Lens.lens (\VerifyAuthRequestCryptogram' {authResponseAttributes} -> authResponseAttributes) (\s@VerifyAuthRequestCryptogram' {} a -> s {authResponseAttributes = a} :: VerifyAuthRequestCryptogram)

-- | The auth request cryptogram imported into Amazon Web Services Payment
-- Cryptography for ARQC verification using a major encryption key and
-- transaction data.
verifyAuthRequestCryptogram_authRequestCryptogram :: Lens.Lens' VerifyAuthRequestCryptogram Prelude.Text
verifyAuthRequestCryptogram_authRequestCryptogram = Lens.lens (\VerifyAuthRequestCryptogram' {authRequestCryptogram} -> authRequestCryptogram) (\s@VerifyAuthRequestCryptogram' {} a -> s {authRequestCryptogram = a} :: VerifyAuthRequestCryptogram)

-- | The @keyARN@ of the major encryption key that Amazon Web Services
-- Payment Cryptography uses for ARQC verification.
verifyAuthRequestCryptogram_keyIdentifier :: Lens.Lens' VerifyAuthRequestCryptogram Prelude.Text
verifyAuthRequestCryptogram_keyIdentifier = Lens.lens (\VerifyAuthRequestCryptogram' {keyIdentifier} -> keyIdentifier) (\s@VerifyAuthRequestCryptogram' {} a -> s {keyIdentifier = a} :: VerifyAuthRequestCryptogram)

-- | The method to use when deriving the major encryption key for ARQC
-- verification within Amazon Web Services Payment Cryptography. The same
-- key derivation mode was used for ARQC generation outside of Amazon Web
-- Services Payment Cryptography.
verifyAuthRequestCryptogram_majorKeyDerivationMode :: Lens.Lens' VerifyAuthRequestCryptogram MajorKeyDerivationMode
verifyAuthRequestCryptogram_majorKeyDerivationMode = Lens.lens (\VerifyAuthRequestCryptogram' {majorKeyDerivationMode} -> majorKeyDerivationMode) (\s@VerifyAuthRequestCryptogram' {} a -> s {majorKeyDerivationMode = a} :: VerifyAuthRequestCryptogram)

-- | The attributes and values to use for deriving a session key for ARQC
-- verification within Amazon Web Services Payment Cryptography. The same
-- attributes were used for ARQC generation outside of Amazon Web Services
-- Payment Cryptography.
verifyAuthRequestCryptogram_sessionKeyDerivationAttributes :: Lens.Lens' VerifyAuthRequestCryptogram SessionKeyDerivation
verifyAuthRequestCryptogram_sessionKeyDerivationAttributes = Lens.lens (\VerifyAuthRequestCryptogram' {sessionKeyDerivationAttributes} -> sessionKeyDerivationAttributes) (\s@VerifyAuthRequestCryptogram' {} a -> s {sessionKeyDerivationAttributes = a} :: VerifyAuthRequestCryptogram)

-- | The transaction data that Amazon Web Services Payment Cryptography uses
-- for ARQC verification. The same transaction is used for ARQC generation
-- outside of Amazon Web Services Payment Cryptography.
verifyAuthRequestCryptogram_transactionData :: Lens.Lens' VerifyAuthRequestCryptogram Prelude.Text
verifyAuthRequestCryptogram_transactionData = Lens.lens (\VerifyAuthRequestCryptogram' {transactionData} -> transactionData) (\s@VerifyAuthRequestCryptogram' {} a -> s {transactionData = a} :: VerifyAuthRequestCryptogram)

instance Core.AWSRequest VerifyAuthRequestCryptogram where
  type
    AWSResponse VerifyAuthRequestCryptogram =
      VerifyAuthRequestCryptogramResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          VerifyAuthRequestCryptogramResponse'
            Prelude.<$> (x Data..?> "AuthResponseValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "KeyArn")
            Prelude.<*> (x Data..:> "KeyCheckValue")
      )

instance Prelude.Hashable VerifyAuthRequestCryptogram where
  hashWithSalt _salt VerifyAuthRequestCryptogram' {..} =
    _salt
      `Prelude.hashWithSalt` authResponseAttributes
      `Prelude.hashWithSalt` authRequestCryptogram
      `Prelude.hashWithSalt` keyIdentifier
      `Prelude.hashWithSalt` majorKeyDerivationMode
      `Prelude.hashWithSalt` sessionKeyDerivationAttributes
      `Prelude.hashWithSalt` transactionData

instance Prelude.NFData VerifyAuthRequestCryptogram where
  rnf VerifyAuthRequestCryptogram' {..} =
    Prelude.rnf authResponseAttributes
      `Prelude.seq` Prelude.rnf authRequestCryptogram
      `Prelude.seq` Prelude.rnf keyIdentifier
      `Prelude.seq` Prelude.rnf majorKeyDerivationMode
      `Prelude.seq` Prelude.rnf sessionKeyDerivationAttributes
      `Prelude.seq` Prelude.rnf transactionData

instance Data.ToHeaders VerifyAuthRequestCryptogram where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON VerifyAuthRequestCryptogram where
  toJSON VerifyAuthRequestCryptogram' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuthResponseAttributes" Data..=)
              Prelude.<$> authResponseAttributes,
            Prelude.Just
              ( "AuthRequestCryptogram"
                  Data..= authRequestCryptogram
              ),
            Prelude.Just ("KeyIdentifier" Data..= keyIdentifier),
            Prelude.Just
              ( "MajorKeyDerivationMode"
                  Data..= majorKeyDerivationMode
              ),
            Prelude.Just
              ( "SessionKeyDerivationAttributes"
                  Data..= sessionKeyDerivationAttributes
              ),
            Prelude.Just
              ("TransactionData" Data..= transactionData)
          ]
      )

instance Data.ToPath VerifyAuthRequestCryptogram where
  toPath = Prelude.const "/cryptogram/verify"

instance Data.ToQuery VerifyAuthRequestCryptogram where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newVerifyAuthRequestCryptogramResponse' smart constructor.
data VerifyAuthRequestCryptogramResponse = VerifyAuthRequestCryptogramResponse'
  { -- | The result for ARQC verification or ARPC generation within Amazon Web
    -- Services Payment Cryptography.
    authResponseValue :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @keyARN@ of the major encryption key that Amazon Web Services
    -- Payment Cryptography uses for ARQC verification.
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
-- Create a value of 'VerifyAuthRequestCryptogramResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authResponseValue', 'verifyAuthRequestCryptogramResponse_authResponseValue' - The result for ARQC verification or ARPC generation within Amazon Web
-- Services Payment Cryptography.
--
-- 'httpStatus', 'verifyAuthRequestCryptogramResponse_httpStatus' - The response's http status code.
--
-- 'keyArn', 'verifyAuthRequestCryptogramResponse_keyArn' - The @keyARN@ of the major encryption key that Amazon Web Services
-- Payment Cryptography uses for ARQC verification.
--
-- 'keyCheckValue', 'verifyAuthRequestCryptogramResponse_keyCheckValue' - The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
newVerifyAuthRequestCryptogramResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'keyArn'
  Prelude.Text ->
  -- | 'keyCheckValue'
  Prelude.Text ->
  VerifyAuthRequestCryptogramResponse
newVerifyAuthRequestCryptogramResponse
  pHttpStatus_
  pKeyArn_
  pKeyCheckValue_ =
    VerifyAuthRequestCryptogramResponse'
      { authResponseValue =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        keyArn = pKeyArn_,
        keyCheckValue = pKeyCheckValue_
      }

-- | The result for ARQC verification or ARPC generation within Amazon Web
-- Services Payment Cryptography.
verifyAuthRequestCryptogramResponse_authResponseValue :: Lens.Lens' VerifyAuthRequestCryptogramResponse (Prelude.Maybe Prelude.Text)
verifyAuthRequestCryptogramResponse_authResponseValue = Lens.lens (\VerifyAuthRequestCryptogramResponse' {authResponseValue} -> authResponseValue) (\s@VerifyAuthRequestCryptogramResponse' {} a -> s {authResponseValue = a} :: VerifyAuthRequestCryptogramResponse)

-- | The response's http status code.
verifyAuthRequestCryptogramResponse_httpStatus :: Lens.Lens' VerifyAuthRequestCryptogramResponse Prelude.Int
verifyAuthRequestCryptogramResponse_httpStatus = Lens.lens (\VerifyAuthRequestCryptogramResponse' {httpStatus} -> httpStatus) (\s@VerifyAuthRequestCryptogramResponse' {} a -> s {httpStatus = a} :: VerifyAuthRequestCryptogramResponse)

-- | The @keyARN@ of the major encryption key that Amazon Web Services
-- Payment Cryptography uses for ARQC verification.
verifyAuthRequestCryptogramResponse_keyArn :: Lens.Lens' VerifyAuthRequestCryptogramResponse Prelude.Text
verifyAuthRequestCryptogramResponse_keyArn = Lens.lens (\VerifyAuthRequestCryptogramResponse' {keyArn} -> keyArn) (\s@VerifyAuthRequestCryptogramResponse' {} a -> s {keyArn = a} :: VerifyAuthRequestCryptogramResponse)

-- | The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
verifyAuthRequestCryptogramResponse_keyCheckValue :: Lens.Lens' VerifyAuthRequestCryptogramResponse Prelude.Text
verifyAuthRequestCryptogramResponse_keyCheckValue = Lens.lens (\VerifyAuthRequestCryptogramResponse' {keyCheckValue} -> keyCheckValue) (\s@VerifyAuthRequestCryptogramResponse' {} a -> s {keyCheckValue = a} :: VerifyAuthRequestCryptogramResponse)

instance
  Prelude.NFData
    VerifyAuthRequestCryptogramResponse
  where
  rnf VerifyAuthRequestCryptogramResponse' {..} =
    Prelude.rnf authResponseValue
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyCheckValue
