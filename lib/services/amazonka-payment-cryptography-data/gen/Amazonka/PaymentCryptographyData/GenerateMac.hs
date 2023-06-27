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
-- Module      : Amazonka.PaymentCryptographyData.GenerateMac
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a Message Authentication Code (MAC) cryptogram within Amazon
-- Web Services Payment Cryptography.
--
-- You can use this operation when keys won\'t be shared but mutual data is
-- present on both ends for validation. In this case, known data values are
-- used to generate a MAC on both ends for comparision without sending or
-- receiving data in ciphertext or plaintext. You can use this operation to
-- generate a DUPKT, HMAC or EMV MAC by setting generation attributes and
-- algorithm to the associated values. The MAC generation encryption key
-- must have valid values for @KeyUsage@ such as @TR31_M7_HMAC_KEY@ for
-- HMAC generation, and they key must have @KeyModesOfUse@ set to
-- @Generate@ and @Verify@.
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
-- -   VerifyMac
module Amazonka.PaymentCryptographyData.GenerateMac
  ( -- * Creating a Request
    GenerateMac (..),
    newGenerateMac,

    -- * Request Lenses
    generateMac_macLength,
    generateMac_generationAttributes,
    generateMac_keyIdentifier,
    generateMac_messageData,

    -- * Destructuring the Response
    GenerateMacResponse (..),
    newGenerateMacResponse,

    -- * Response Lenses
    generateMacResponse_httpStatus,
    generateMacResponse_keyArn,
    generateMacResponse_keyCheckValue,
    generateMacResponse_mac,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGenerateMac' smart constructor.
data GenerateMac = GenerateMac'
  { -- | The length of a MAC under generation.
    macLength :: Prelude.Maybe Prelude.Natural,
    -- | The attributes and data values to use for MAC generation within Amazon
    -- Web Services Payment Cryptography.
    generationAttributes :: MacAttributes,
    -- | The @keyARN@ of the MAC generation encryption key.
    keyIdentifier :: Prelude.Text,
    -- | The data for which a MAC is under generation.
    messageData :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateMac' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'macLength', 'generateMac_macLength' - The length of a MAC under generation.
--
-- 'generationAttributes', 'generateMac_generationAttributes' - The attributes and data values to use for MAC generation within Amazon
-- Web Services Payment Cryptography.
--
-- 'keyIdentifier', 'generateMac_keyIdentifier' - The @keyARN@ of the MAC generation encryption key.
--
-- 'messageData', 'generateMac_messageData' - The data for which a MAC is under generation.
newGenerateMac ::
  -- | 'generationAttributes'
  MacAttributes ->
  -- | 'keyIdentifier'
  Prelude.Text ->
  -- | 'messageData'
  Prelude.Text ->
  GenerateMac
newGenerateMac
  pGenerationAttributes_
  pKeyIdentifier_
  pMessageData_ =
    GenerateMac'
      { macLength = Prelude.Nothing,
        generationAttributes = pGenerationAttributes_,
        keyIdentifier = pKeyIdentifier_,
        messageData = pMessageData_
      }

-- | The length of a MAC under generation.
generateMac_macLength :: Lens.Lens' GenerateMac (Prelude.Maybe Prelude.Natural)
generateMac_macLength = Lens.lens (\GenerateMac' {macLength} -> macLength) (\s@GenerateMac' {} a -> s {macLength = a} :: GenerateMac)

-- | The attributes and data values to use for MAC generation within Amazon
-- Web Services Payment Cryptography.
generateMac_generationAttributes :: Lens.Lens' GenerateMac MacAttributes
generateMac_generationAttributes = Lens.lens (\GenerateMac' {generationAttributes} -> generationAttributes) (\s@GenerateMac' {} a -> s {generationAttributes = a} :: GenerateMac)

-- | The @keyARN@ of the MAC generation encryption key.
generateMac_keyIdentifier :: Lens.Lens' GenerateMac Prelude.Text
generateMac_keyIdentifier = Lens.lens (\GenerateMac' {keyIdentifier} -> keyIdentifier) (\s@GenerateMac' {} a -> s {keyIdentifier = a} :: GenerateMac)

-- | The data for which a MAC is under generation.
generateMac_messageData :: Lens.Lens' GenerateMac Prelude.Text
generateMac_messageData = Lens.lens (\GenerateMac' {messageData} -> messageData) (\s@GenerateMac' {} a -> s {messageData = a} :: GenerateMac)

instance Core.AWSRequest GenerateMac where
  type AWSResponse GenerateMac = GenerateMacResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateMacResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "KeyArn")
            Prelude.<*> (x Data..:> "KeyCheckValue")
            Prelude.<*> (x Data..:> "Mac")
      )

instance Prelude.Hashable GenerateMac where
  hashWithSalt _salt GenerateMac' {..} =
    _salt
      `Prelude.hashWithSalt` macLength
      `Prelude.hashWithSalt` generationAttributes
      `Prelude.hashWithSalt` keyIdentifier
      `Prelude.hashWithSalt` messageData

instance Prelude.NFData GenerateMac where
  rnf GenerateMac' {..} =
    Prelude.rnf macLength
      `Prelude.seq` Prelude.rnf generationAttributes
      `Prelude.seq` Prelude.rnf keyIdentifier
      `Prelude.seq` Prelude.rnf messageData

instance Data.ToHeaders GenerateMac where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GenerateMac where
  toJSON GenerateMac' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MacLength" Data..=) Prelude.<$> macLength,
            Prelude.Just
              ( "GenerationAttributes"
                  Data..= generationAttributes
              ),
            Prelude.Just ("KeyIdentifier" Data..= keyIdentifier),
            Prelude.Just ("MessageData" Data..= messageData)
          ]
      )

instance Data.ToPath GenerateMac where
  toPath = Prelude.const "/mac/generate"

instance Data.ToQuery GenerateMac where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGenerateMacResponse' smart constructor.
data GenerateMacResponse = GenerateMacResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @keyARN@ of the encryption key that Amazon Web Services Payment
    -- Cryptography uses for MAC generation.
    keyArn :: Prelude.Text,
    -- | The key check value (KCV) of the encryption key. The KCV is used to
    -- check if all parties holding a given key have the same key or to detect
    -- that a key has changed. Amazon Web Services Payment Cryptography
    -- calculates the KCV by using standard algorithms, typically by encrypting
    -- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
    -- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
    keyCheckValue :: Prelude.Text,
    -- | The MAC cryptogram generated within Amazon Web Services Payment
    -- Cryptography.
    mac :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateMacResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'generateMacResponse_httpStatus' - The response's http status code.
--
-- 'keyArn', 'generateMacResponse_keyArn' - The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses for MAC generation.
--
-- 'keyCheckValue', 'generateMacResponse_keyCheckValue' - The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
--
-- 'mac', 'generateMacResponse_mac' - The MAC cryptogram generated within Amazon Web Services Payment
-- Cryptography.
newGenerateMacResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'keyArn'
  Prelude.Text ->
  -- | 'keyCheckValue'
  Prelude.Text ->
  -- | 'mac'
  Prelude.Text ->
  GenerateMacResponse
newGenerateMacResponse
  pHttpStatus_
  pKeyArn_
  pKeyCheckValue_
  pMac_ =
    GenerateMacResponse'
      { httpStatus = pHttpStatus_,
        keyArn = pKeyArn_,
        keyCheckValue = pKeyCheckValue_,
        mac = pMac_
      }

-- | The response's http status code.
generateMacResponse_httpStatus :: Lens.Lens' GenerateMacResponse Prelude.Int
generateMacResponse_httpStatus = Lens.lens (\GenerateMacResponse' {httpStatus} -> httpStatus) (\s@GenerateMacResponse' {} a -> s {httpStatus = a} :: GenerateMacResponse)

-- | The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses for MAC generation.
generateMacResponse_keyArn :: Lens.Lens' GenerateMacResponse Prelude.Text
generateMacResponse_keyArn = Lens.lens (\GenerateMacResponse' {keyArn} -> keyArn) (\s@GenerateMacResponse' {} a -> s {keyArn = a} :: GenerateMacResponse)

-- | The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
generateMacResponse_keyCheckValue :: Lens.Lens' GenerateMacResponse Prelude.Text
generateMacResponse_keyCheckValue = Lens.lens (\GenerateMacResponse' {keyCheckValue} -> keyCheckValue) (\s@GenerateMacResponse' {} a -> s {keyCheckValue = a} :: GenerateMacResponse)

-- | The MAC cryptogram generated within Amazon Web Services Payment
-- Cryptography.
generateMacResponse_mac :: Lens.Lens' GenerateMacResponse Prelude.Text
generateMacResponse_mac = Lens.lens (\GenerateMacResponse' {mac} -> mac) (\s@GenerateMacResponse' {} a -> s {mac = a} :: GenerateMacResponse)

instance Prelude.NFData GenerateMacResponse where
  rnf GenerateMacResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyCheckValue
      `Prelude.seq` Prelude.rnf mac
