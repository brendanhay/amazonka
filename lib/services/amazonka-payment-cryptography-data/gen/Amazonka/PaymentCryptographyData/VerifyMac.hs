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
-- Module      : Amazonka.PaymentCryptographyData.VerifyMac
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies a Message Authentication Code (MAC).
--
-- You can use this operation when keys won\'t be shared but mutual data is
-- present on both ends for validation. In this case, known data values are
-- used to generate a MAC on both ends for verification without sending or
-- receiving data in ciphertext or plaintext. You can use this operation to
-- verify a DUPKT, HMAC or EMV MAC by setting generation attributes and
-- algorithm to the associated values. Use the same encryption key for MAC
-- verification as you use for GenerateMac.
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
-- -   GenerateMac
module Amazonka.PaymentCryptographyData.VerifyMac
  ( -- * Creating a Request
    VerifyMac (..),
    newVerifyMac,

    -- * Request Lenses
    verifyMac_macLength,
    verifyMac_keyIdentifier,
    verifyMac_mac,
    verifyMac_messageData,
    verifyMac_verificationAttributes,

    -- * Destructuring the Response
    VerifyMacResponse (..),
    newVerifyMacResponse,

    -- * Response Lenses
    verifyMacResponse_httpStatus,
    verifyMacResponse_keyArn,
    verifyMacResponse_keyCheckValue,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newVerifyMac' smart constructor.
data VerifyMac = VerifyMac'
  { -- | The length of the MAC.
    macLength :: Prelude.Maybe Prelude.Natural,
    -- | The @keyARN@ of the encryption key that Amazon Web Services Payment
    -- Cryptography uses to verify MAC data.
    keyIdentifier :: Prelude.Text,
    -- | The MAC being verified.
    mac :: Prelude.Text,
    -- | The data on for which MAC is under verification.
    messageData :: Prelude.Text,
    -- | The attributes and data values to use for MAC verification within Amazon
    -- Web Services Payment Cryptography.
    verificationAttributes :: MacAttributes
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyMac' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'macLength', 'verifyMac_macLength' - The length of the MAC.
--
-- 'keyIdentifier', 'verifyMac_keyIdentifier' - The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses to verify MAC data.
--
-- 'mac', 'verifyMac_mac' - The MAC being verified.
--
-- 'messageData', 'verifyMac_messageData' - The data on for which MAC is under verification.
--
-- 'verificationAttributes', 'verifyMac_verificationAttributes' - The attributes and data values to use for MAC verification within Amazon
-- Web Services Payment Cryptography.
newVerifyMac ::
  -- | 'keyIdentifier'
  Prelude.Text ->
  -- | 'mac'
  Prelude.Text ->
  -- | 'messageData'
  Prelude.Text ->
  -- | 'verificationAttributes'
  MacAttributes ->
  VerifyMac
newVerifyMac
  pKeyIdentifier_
  pMac_
  pMessageData_
  pVerificationAttributes_ =
    VerifyMac'
      { macLength = Prelude.Nothing,
        keyIdentifier = pKeyIdentifier_,
        mac = pMac_,
        messageData = pMessageData_,
        verificationAttributes = pVerificationAttributes_
      }

-- | The length of the MAC.
verifyMac_macLength :: Lens.Lens' VerifyMac (Prelude.Maybe Prelude.Natural)
verifyMac_macLength = Lens.lens (\VerifyMac' {macLength} -> macLength) (\s@VerifyMac' {} a -> s {macLength = a} :: VerifyMac)

-- | The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses to verify MAC data.
verifyMac_keyIdentifier :: Lens.Lens' VerifyMac Prelude.Text
verifyMac_keyIdentifier = Lens.lens (\VerifyMac' {keyIdentifier} -> keyIdentifier) (\s@VerifyMac' {} a -> s {keyIdentifier = a} :: VerifyMac)

-- | The MAC being verified.
verifyMac_mac :: Lens.Lens' VerifyMac Prelude.Text
verifyMac_mac = Lens.lens (\VerifyMac' {mac} -> mac) (\s@VerifyMac' {} a -> s {mac = a} :: VerifyMac)

-- | The data on for which MAC is under verification.
verifyMac_messageData :: Lens.Lens' VerifyMac Prelude.Text
verifyMac_messageData = Lens.lens (\VerifyMac' {messageData} -> messageData) (\s@VerifyMac' {} a -> s {messageData = a} :: VerifyMac)

-- | The attributes and data values to use for MAC verification within Amazon
-- Web Services Payment Cryptography.
verifyMac_verificationAttributes :: Lens.Lens' VerifyMac MacAttributes
verifyMac_verificationAttributes = Lens.lens (\VerifyMac' {verificationAttributes} -> verificationAttributes) (\s@VerifyMac' {} a -> s {verificationAttributes = a} :: VerifyMac)

instance Core.AWSRequest VerifyMac where
  type AWSResponse VerifyMac = VerifyMacResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          VerifyMacResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "KeyArn")
            Prelude.<*> (x Data..:> "KeyCheckValue")
      )

instance Prelude.Hashable VerifyMac where
  hashWithSalt _salt VerifyMac' {..} =
    _salt
      `Prelude.hashWithSalt` macLength
      `Prelude.hashWithSalt` keyIdentifier
      `Prelude.hashWithSalt` mac
      `Prelude.hashWithSalt` messageData
      `Prelude.hashWithSalt` verificationAttributes

instance Prelude.NFData VerifyMac where
  rnf VerifyMac' {..} =
    Prelude.rnf macLength
      `Prelude.seq` Prelude.rnf keyIdentifier
      `Prelude.seq` Prelude.rnf mac
      `Prelude.seq` Prelude.rnf messageData
      `Prelude.seq` Prelude.rnf verificationAttributes

instance Data.ToHeaders VerifyMac where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON VerifyMac where
  toJSON VerifyMac' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MacLength" Data..=) Prelude.<$> macLength,
            Prelude.Just ("KeyIdentifier" Data..= keyIdentifier),
            Prelude.Just ("Mac" Data..= mac),
            Prelude.Just ("MessageData" Data..= messageData),
            Prelude.Just
              ( "VerificationAttributes"
                  Data..= verificationAttributes
              )
          ]
      )

instance Data.ToPath VerifyMac where
  toPath = Prelude.const "/mac/verify"

instance Data.ToQuery VerifyMac where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newVerifyMacResponse' smart constructor.
data VerifyMacResponse = VerifyMacResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @keyARN@ of the encryption key that Amazon Web Services Payment
    -- Cryptography uses for MAC verification.
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
-- Create a value of 'VerifyMacResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'verifyMacResponse_httpStatus' - The response's http status code.
--
-- 'keyArn', 'verifyMacResponse_keyArn' - The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses for MAC verification.
--
-- 'keyCheckValue', 'verifyMacResponse_keyCheckValue' - The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
newVerifyMacResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'keyArn'
  Prelude.Text ->
  -- | 'keyCheckValue'
  Prelude.Text ->
  VerifyMacResponse
newVerifyMacResponse
  pHttpStatus_
  pKeyArn_
  pKeyCheckValue_ =
    VerifyMacResponse'
      { httpStatus = pHttpStatus_,
        keyArn = pKeyArn_,
        keyCheckValue = pKeyCheckValue_
      }

-- | The response's http status code.
verifyMacResponse_httpStatus :: Lens.Lens' VerifyMacResponse Prelude.Int
verifyMacResponse_httpStatus = Lens.lens (\VerifyMacResponse' {httpStatus} -> httpStatus) (\s@VerifyMacResponse' {} a -> s {httpStatus = a} :: VerifyMacResponse)

-- | The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses for MAC verification.
verifyMacResponse_keyArn :: Lens.Lens' VerifyMacResponse Prelude.Text
verifyMacResponse_keyArn = Lens.lens (\VerifyMacResponse' {keyArn} -> keyArn) (\s@VerifyMacResponse' {} a -> s {keyArn = a} :: VerifyMacResponse)

-- | The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
verifyMacResponse_keyCheckValue :: Lens.Lens' VerifyMacResponse Prelude.Text
verifyMacResponse_keyCheckValue = Lens.lens (\VerifyMacResponse' {keyCheckValue} -> keyCheckValue) (\s@VerifyMacResponse' {} a -> s {keyCheckValue = a} :: VerifyMacResponse)

instance Prelude.NFData VerifyMacResponse where
  rnf VerifyMacResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyCheckValue
