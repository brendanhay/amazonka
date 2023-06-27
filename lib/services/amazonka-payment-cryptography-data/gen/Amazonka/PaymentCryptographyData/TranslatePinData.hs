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
-- Module      : Amazonka.PaymentCryptographyData.TranslatePinData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Translates encrypted PIN block from and to ISO 9564 formats 0,1,3,4. For
-- more information, see
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/translate-pin-data.html Translate PIN data>
-- in the /Amazon Web Services Payment Cryptography User Guide/.
--
-- PIN block translation involves changing the encrytion of PIN block from
-- one encryption key to another encryption key and changing PIN block
-- format from one to another without PIN block data leaving Amazon Web
-- Services Payment Cryptography. The encryption key transformation can be
-- from PEK (Pin Encryption Key) to BDK (Base Derivation Key) for DUKPT or
-- from BDK for DUKPT to PEK. Amazon Web Services Payment Cryptography
-- supports @TDES@ and @AES@ key derivation type for DUKPT tranlations. You
-- can use this operation for P2PE (Point to Point Encryption) use cases
-- where the encryption keys should change but the processing system either
-- does not need to, or is not permitted to, decrypt the data.
--
-- The allowed combinations of PIN block format translations are guided by
-- PCI. It is important to note that not all encrypted PIN block formats
-- (example, format 1) require PAN (Primary Account Number) as input. And
-- as such, PIN block format that requires PAN (example, formats 0,3,4)
-- cannot be translated to a format (format 1) that does not require a PAN
-- for generation.
--
-- For information about valid keys for this operation, see
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/keys-validattributes.html Understanding key attributes>
-- and
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/crypto-ops-validkeys-ops.html Key types for specific data operations>
-- in the /Amazon Web Services Payment Cryptography User Guide/.
--
-- At this time, Amazon Web Services Payment Cryptography does not support
-- translations to PIN format 4.
--
-- __Cross-account use__: This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   GeneratePinData
--
-- -   VerifyPinData
module Amazonka.PaymentCryptographyData.TranslatePinData
  ( -- * Creating a Request
    TranslatePinData (..),
    newTranslatePinData,

    -- * Request Lenses
    translatePinData_incomingDukptAttributes,
    translatePinData_outgoingDukptAttributes,
    translatePinData_encryptedPinBlock,
    translatePinData_incomingKeyIdentifier,
    translatePinData_incomingTranslationAttributes,
    translatePinData_outgoingKeyIdentifier,
    translatePinData_outgoingTranslationAttributes,

    -- * Destructuring the Response
    TranslatePinDataResponse (..),
    newTranslatePinDataResponse,

    -- * Response Lenses
    translatePinDataResponse_httpStatus,
    translatePinDataResponse_keyArn,
    translatePinDataResponse_keyCheckValue,
    translatePinDataResponse_pinBlock,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTranslatePinData' smart constructor.
data TranslatePinData = TranslatePinData'
  { -- | The attributes and values to use for incoming DUKPT encryption key for
    -- PIN block tranlation.
    incomingDukptAttributes :: Prelude.Maybe DukptDerivationAttributes,
    -- | The attributes and values to use for outgoing DUKPT encryption key after
    -- PIN block translation.
    outgoingDukptAttributes :: Prelude.Maybe DukptDerivationAttributes,
    -- | The encrypted PIN block data that Amazon Web Services Payment
    -- Cryptography translates.
    encryptedPinBlock :: Prelude.Text,
    -- | The @keyARN@ of the encryption key under which incoming PIN block data
    -- is encrypted. This key type can be PEK or BDK.
    incomingKeyIdentifier :: Prelude.Text,
    -- | The format of the incoming PIN block data for tranlation within Amazon
    -- Web Services Payment Cryptography.
    incomingTranslationAttributes :: TranslationIsoFormats,
    -- | The @keyARN@ of the encryption key for encrypting outgoing PIN block
    -- data. This key type can be PEK or BDK.
    outgoingKeyIdentifier :: Prelude.Text,
    -- | The format of the outgoing PIN block data after tranlation by Amazon Web
    -- Services Payment Cryptography.
    outgoingTranslationAttributes :: TranslationIsoFormats
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranslatePinData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'incomingDukptAttributes', 'translatePinData_incomingDukptAttributes' - The attributes and values to use for incoming DUKPT encryption key for
-- PIN block tranlation.
--
-- 'outgoingDukptAttributes', 'translatePinData_outgoingDukptAttributes' - The attributes and values to use for outgoing DUKPT encryption key after
-- PIN block translation.
--
-- 'encryptedPinBlock', 'translatePinData_encryptedPinBlock' - The encrypted PIN block data that Amazon Web Services Payment
-- Cryptography translates.
--
-- 'incomingKeyIdentifier', 'translatePinData_incomingKeyIdentifier' - The @keyARN@ of the encryption key under which incoming PIN block data
-- is encrypted. This key type can be PEK or BDK.
--
-- 'incomingTranslationAttributes', 'translatePinData_incomingTranslationAttributes' - The format of the incoming PIN block data for tranlation within Amazon
-- Web Services Payment Cryptography.
--
-- 'outgoingKeyIdentifier', 'translatePinData_outgoingKeyIdentifier' - The @keyARN@ of the encryption key for encrypting outgoing PIN block
-- data. This key type can be PEK or BDK.
--
-- 'outgoingTranslationAttributes', 'translatePinData_outgoingTranslationAttributes' - The format of the outgoing PIN block data after tranlation by Amazon Web
-- Services Payment Cryptography.
newTranslatePinData ::
  -- | 'encryptedPinBlock'
  Prelude.Text ->
  -- | 'incomingKeyIdentifier'
  Prelude.Text ->
  -- | 'incomingTranslationAttributes'
  TranslationIsoFormats ->
  -- | 'outgoingKeyIdentifier'
  Prelude.Text ->
  -- | 'outgoingTranslationAttributes'
  TranslationIsoFormats ->
  TranslatePinData
newTranslatePinData
  pEncryptedPinBlock_
  pIncomingKeyIdentifier_
  pIncomingTranslationAttributes_
  pOutgoingKeyIdentifier_
  pOutgoingTranslationAttributes_ =
    TranslatePinData'
      { incomingDukptAttributes =
          Prelude.Nothing,
        outgoingDukptAttributes = Prelude.Nothing,
        encryptedPinBlock = pEncryptedPinBlock_,
        incomingKeyIdentifier = pIncomingKeyIdentifier_,
        incomingTranslationAttributes =
          pIncomingTranslationAttributes_,
        outgoingKeyIdentifier = pOutgoingKeyIdentifier_,
        outgoingTranslationAttributes =
          pOutgoingTranslationAttributes_
      }

-- | The attributes and values to use for incoming DUKPT encryption key for
-- PIN block tranlation.
translatePinData_incomingDukptAttributes :: Lens.Lens' TranslatePinData (Prelude.Maybe DukptDerivationAttributes)
translatePinData_incomingDukptAttributes = Lens.lens (\TranslatePinData' {incomingDukptAttributes} -> incomingDukptAttributes) (\s@TranslatePinData' {} a -> s {incomingDukptAttributes = a} :: TranslatePinData)

-- | The attributes and values to use for outgoing DUKPT encryption key after
-- PIN block translation.
translatePinData_outgoingDukptAttributes :: Lens.Lens' TranslatePinData (Prelude.Maybe DukptDerivationAttributes)
translatePinData_outgoingDukptAttributes = Lens.lens (\TranslatePinData' {outgoingDukptAttributes} -> outgoingDukptAttributes) (\s@TranslatePinData' {} a -> s {outgoingDukptAttributes = a} :: TranslatePinData)

-- | The encrypted PIN block data that Amazon Web Services Payment
-- Cryptography translates.
translatePinData_encryptedPinBlock :: Lens.Lens' TranslatePinData Prelude.Text
translatePinData_encryptedPinBlock = Lens.lens (\TranslatePinData' {encryptedPinBlock} -> encryptedPinBlock) (\s@TranslatePinData' {} a -> s {encryptedPinBlock = a} :: TranslatePinData)

-- | The @keyARN@ of the encryption key under which incoming PIN block data
-- is encrypted. This key type can be PEK or BDK.
translatePinData_incomingKeyIdentifier :: Lens.Lens' TranslatePinData Prelude.Text
translatePinData_incomingKeyIdentifier = Lens.lens (\TranslatePinData' {incomingKeyIdentifier} -> incomingKeyIdentifier) (\s@TranslatePinData' {} a -> s {incomingKeyIdentifier = a} :: TranslatePinData)

-- | The format of the incoming PIN block data for tranlation within Amazon
-- Web Services Payment Cryptography.
translatePinData_incomingTranslationAttributes :: Lens.Lens' TranslatePinData TranslationIsoFormats
translatePinData_incomingTranslationAttributes = Lens.lens (\TranslatePinData' {incomingTranslationAttributes} -> incomingTranslationAttributes) (\s@TranslatePinData' {} a -> s {incomingTranslationAttributes = a} :: TranslatePinData)

-- | The @keyARN@ of the encryption key for encrypting outgoing PIN block
-- data. This key type can be PEK or BDK.
translatePinData_outgoingKeyIdentifier :: Lens.Lens' TranslatePinData Prelude.Text
translatePinData_outgoingKeyIdentifier = Lens.lens (\TranslatePinData' {outgoingKeyIdentifier} -> outgoingKeyIdentifier) (\s@TranslatePinData' {} a -> s {outgoingKeyIdentifier = a} :: TranslatePinData)

-- | The format of the outgoing PIN block data after tranlation by Amazon Web
-- Services Payment Cryptography.
translatePinData_outgoingTranslationAttributes :: Lens.Lens' TranslatePinData TranslationIsoFormats
translatePinData_outgoingTranslationAttributes = Lens.lens (\TranslatePinData' {outgoingTranslationAttributes} -> outgoingTranslationAttributes) (\s@TranslatePinData' {} a -> s {outgoingTranslationAttributes = a} :: TranslatePinData)

instance Core.AWSRequest TranslatePinData where
  type
    AWSResponse TranslatePinData =
      TranslatePinDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TranslatePinDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "KeyArn")
            Prelude.<*> (x Data..:> "KeyCheckValue")
            Prelude.<*> (x Data..:> "PinBlock")
      )

instance Prelude.Hashable TranslatePinData where
  hashWithSalt _salt TranslatePinData' {..} =
    _salt
      `Prelude.hashWithSalt` incomingDukptAttributes
      `Prelude.hashWithSalt` outgoingDukptAttributes
      `Prelude.hashWithSalt` encryptedPinBlock
      `Prelude.hashWithSalt` incomingKeyIdentifier
      `Prelude.hashWithSalt` incomingTranslationAttributes
      `Prelude.hashWithSalt` outgoingKeyIdentifier
      `Prelude.hashWithSalt` outgoingTranslationAttributes

instance Prelude.NFData TranslatePinData where
  rnf TranslatePinData' {..} =
    Prelude.rnf incomingDukptAttributes
      `Prelude.seq` Prelude.rnf outgoingDukptAttributes
      `Prelude.seq` Prelude.rnf encryptedPinBlock
      `Prelude.seq` Prelude.rnf incomingKeyIdentifier
      `Prelude.seq` Prelude.rnf incomingTranslationAttributes
      `Prelude.seq` Prelude.rnf outgoingKeyIdentifier
      `Prelude.seq` Prelude.rnf outgoingTranslationAttributes

instance Data.ToHeaders TranslatePinData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TranslatePinData where
  toJSON TranslatePinData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IncomingDukptAttributes" Data..=)
              Prelude.<$> incomingDukptAttributes,
            ("OutgoingDukptAttributes" Data..=)
              Prelude.<$> outgoingDukptAttributes,
            Prelude.Just
              ("EncryptedPinBlock" Data..= encryptedPinBlock),
            Prelude.Just
              ( "IncomingKeyIdentifier"
                  Data..= incomingKeyIdentifier
              ),
            Prelude.Just
              ( "IncomingTranslationAttributes"
                  Data..= incomingTranslationAttributes
              ),
            Prelude.Just
              ( "OutgoingKeyIdentifier"
                  Data..= outgoingKeyIdentifier
              ),
            Prelude.Just
              ( "OutgoingTranslationAttributes"
                  Data..= outgoingTranslationAttributes
              )
          ]
      )

instance Data.ToPath TranslatePinData where
  toPath = Prelude.const "/pindata/translate"

instance Data.ToQuery TranslatePinData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTranslatePinDataResponse' smart constructor.
data TranslatePinDataResponse = TranslatePinDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @keyARN@ of the encryption key that Amazon Web Services Payment
    -- Cryptography uses to encrypt outgoing PIN block data after translation.
    keyArn :: Prelude.Text,
    -- | The key check value (KCV) of the encryption key. The KCV is used to
    -- check if all parties holding a given key have the same key or to detect
    -- that a key has changed. Amazon Web Services Payment Cryptography
    -- calculates the KCV by using standard algorithms, typically by encrypting
    -- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
    -- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
    keyCheckValue :: Prelude.Text,
    -- | The ougoing encrypted PIN block data after tranlation.
    pinBlock :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranslatePinDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'translatePinDataResponse_httpStatus' - The response's http status code.
--
-- 'keyArn', 'translatePinDataResponse_keyArn' - The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses to encrypt outgoing PIN block data after translation.
--
-- 'keyCheckValue', 'translatePinDataResponse_keyCheckValue' - The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
--
-- 'pinBlock', 'translatePinDataResponse_pinBlock' - The ougoing encrypted PIN block data after tranlation.
newTranslatePinDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'keyArn'
  Prelude.Text ->
  -- | 'keyCheckValue'
  Prelude.Text ->
  -- | 'pinBlock'
  Prelude.Text ->
  TranslatePinDataResponse
newTranslatePinDataResponse
  pHttpStatus_
  pKeyArn_
  pKeyCheckValue_
  pPinBlock_ =
    TranslatePinDataResponse'
      { httpStatus =
          pHttpStatus_,
        keyArn = pKeyArn_,
        keyCheckValue = pKeyCheckValue_,
        pinBlock = pPinBlock_
      }

-- | The response's http status code.
translatePinDataResponse_httpStatus :: Lens.Lens' TranslatePinDataResponse Prelude.Int
translatePinDataResponse_httpStatus = Lens.lens (\TranslatePinDataResponse' {httpStatus} -> httpStatus) (\s@TranslatePinDataResponse' {} a -> s {httpStatus = a} :: TranslatePinDataResponse)

-- | The @keyARN@ of the encryption key that Amazon Web Services Payment
-- Cryptography uses to encrypt outgoing PIN block data after translation.
translatePinDataResponse_keyArn :: Lens.Lens' TranslatePinDataResponse Prelude.Text
translatePinDataResponse_keyArn = Lens.lens (\TranslatePinDataResponse' {keyArn} -> keyArn) (\s@TranslatePinDataResponse' {} a -> s {keyArn = a} :: TranslatePinDataResponse)

-- | The key check value (KCV) of the encryption key. The KCV is used to
-- check if all parties holding a given key have the same key or to detect
-- that a key has changed. Amazon Web Services Payment Cryptography
-- calculates the KCV by using standard algorithms, typically by encrypting
-- 8 or 16 bytes or \"00\" or \"01\" and then truncating the result to the
-- first 3 bytes, or 6 hex digits, of the resulting cryptogram.
translatePinDataResponse_keyCheckValue :: Lens.Lens' TranslatePinDataResponse Prelude.Text
translatePinDataResponse_keyCheckValue = Lens.lens (\TranslatePinDataResponse' {keyCheckValue} -> keyCheckValue) (\s@TranslatePinDataResponse' {} a -> s {keyCheckValue = a} :: TranslatePinDataResponse)

-- | The ougoing encrypted PIN block data after tranlation.
translatePinDataResponse_pinBlock :: Lens.Lens' TranslatePinDataResponse Prelude.Text
translatePinDataResponse_pinBlock = Lens.lens (\TranslatePinDataResponse' {pinBlock} -> pinBlock) (\s@TranslatePinDataResponse' {} a -> s {pinBlock = a} :: TranslatePinDataResponse)

instance Prelude.NFData TranslatePinDataResponse where
  rnf TranslatePinDataResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyCheckValue
      `Prelude.seq` Prelude.rnf pinBlock
