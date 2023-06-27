{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PaymentCryptography.Types.Key
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.Key where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types.KeyAttributes
import Amazonka.PaymentCryptography.Types.KeyCheckValueAlgorithm
import Amazonka.PaymentCryptography.Types.KeyOrigin
import Amazonka.PaymentCryptography.Types.KeyState
import qualified Amazonka.Prelude as Prelude

-- | Metadata about an Amazon Web Services Payment Cryptography key.
--
-- /See:/ 'newKey' smart constructor.
data Key = Key'
  { -- | The date and time after which Amazon Web Services Payment Cryptography
    -- will delete the key. This value is present only when @KeyState@ is
    -- @DELETE_PENDING@ and the key is scheduled for deletion.
    deletePendingTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The date and time after which Amazon Web Services Payment Cryptography
    -- will delete the key. This value is present only when when the @KeyState@
    -- is @DELETE_COMPLETE@ and the Amazon Web Services Payment Cryptography
    -- key is deleted.
    deleteTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The date and time after which Amazon Web Services Payment Cryptography
    -- will start using the key material for cryptographic operations.
    usageStartTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The date and time after which Amazon Web Services Payment Cryptography
    -- will stop using the key material for cryptographic operations.
    usageStopTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The date and time when the key was created.
    createTimestamp :: Data.POSIX,
    -- | Specifies whether the key is enabled.
    enabled :: Prelude.Bool,
    -- | Specifies whether the key is exportable. This data is immutable after
    -- the key is created.
    exportable :: Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the key.
    keyArn :: Prelude.Text,
    -- | The role of the key, the algorithm it supports, and the cryptographic
    -- operations allowed with the key. This data is immutable after the key is
    -- created.
    keyAttributes :: KeyAttributes,
    -- | The key check value (KCV) is used to check if all parties holding a
    -- given key have the same key or to detect that a key has changed. Amazon
    -- Web Services Payment Cryptography calculates the KCV by using standard
    -- algorithms, typically by encrypting 8 or 16 bytes or \"00\" or \"01\"
    -- and then truncating the result to the first 3 bytes, or 6 hex digits, of
    -- the resulting cryptogram.
    keyCheckValue :: Prelude.Text,
    -- | The algorithm used for calculating key check value (KCV) for DES and AES
    -- keys. For a DES key, Amazon Web Services Payment Cryptography computes
    -- the KCV by encrypting 8 bytes, each with value \'00\', with the key to
    -- be checked and retaining the 3 highest order bytes of the encrypted
    -- result. For an AES key, Amazon Web Services Payment Cryptography
    -- computes the KCV by encrypting 8 bytes, each with value \'01\', with the
    -- key to be checked and retaining the 3 highest order bytes of the
    -- encrypted result.
    keyCheckValueAlgorithm :: KeyCheckValueAlgorithm,
    -- | The source of the key material. For keys created within Amazon Web
    -- Services Payment Cryptography, the value is @AWS_PAYMENT_CRYPTOGRAPHY@.
    -- For keys imported into Amazon Web Services Payment Cryptography, the
    -- value is @EXTERNAL@.
    keyOrigin :: KeyOrigin,
    -- | The state of key that is being created or deleted.
    keyState :: KeyState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Key' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletePendingTimestamp', 'key_deletePendingTimestamp' - The date and time after which Amazon Web Services Payment Cryptography
-- will delete the key. This value is present only when @KeyState@ is
-- @DELETE_PENDING@ and the key is scheduled for deletion.
--
-- 'deleteTimestamp', 'key_deleteTimestamp' - The date and time after which Amazon Web Services Payment Cryptography
-- will delete the key. This value is present only when when the @KeyState@
-- is @DELETE_COMPLETE@ and the Amazon Web Services Payment Cryptography
-- key is deleted.
--
-- 'usageStartTimestamp', 'key_usageStartTimestamp' - The date and time after which Amazon Web Services Payment Cryptography
-- will start using the key material for cryptographic operations.
--
-- 'usageStopTimestamp', 'key_usageStopTimestamp' - The date and time after which Amazon Web Services Payment Cryptography
-- will stop using the key material for cryptographic operations.
--
-- 'createTimestamp', 'key_createTimestamp' - The date and time when the key was created.
--
-- 'enabled', 'key_enabled' - Specifies whether the key is enabled.
--
-- 'exportable', 'key_exportable' - Specifies whether the key is exportable. This data is immutable after
-- the key is created.
--
-- 'keyArn', 'key_keyArn' - The Amazon Resource Name (ARN) of the key.
--
-- 'keyAttributes', 'key_keyAttributes' - The role of the key, the algorithm it supports, and the cryptographic
-- operations allowed with the key. This data is immutable after the key is
-- created.
--
-- 'keyCheckValue', 'key_keyCheckValue' - The key check value (KCV) is used to check if all parties holding a
-- given key have the same key or to detect that a key has changed. Amazon
-- Web Services Payment Cryptography calculates the KCV by using standard
-- algorithms, typically by encrypting 8 or 16 bytes or \"00\" or \"01\"
-- and then truncating the result to the first 3 bytes, or 6 hex digits, of
-- the resulting cryptogram.
--
-- 'keyCheckValueAlgorithm', 'key_keyCheckValueAlgorithm' - The algorithm used for calculating key check value (KCV) for DES and AES
-- keys. For a DES key, Amazon Web Services Payment Cryptography computes
-- the KCV by encrypting 8 bytes, each with value \'00\', with the key to
-- be checked and retaining the 3 highest order bytes of the encrypted
-- result. For an AES key, Amazon Web Services Payment Cryptography
-- computes the KCV by encrypting 8 bytes, each with value \'01\', with the
-- key to be checked and retaining the 3 highest order bytes of the
-- encrypted result.
--
-- 'keyOrigin', 'key_keyOrigin' - The source of the key material. For keys created within Amazon Web
-- Services Payment Cryptography, the value is @AWS_PAYMENT_CRYPTOGRAPHY@.
-- For keys imported into Amazon Web Services Payment Cryptography, the
-- value is @EXTERNAL@.
--
-- 'keyState', 'key_keyState' - The state of key that is being created or deleted.
newKey ::
  -- | 'createTimestamp'
  Prelude.UTCTime ->
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'exportable'
  Prelude.Bool ->
  -- | 'keyArn'
  Prelude.Text ->
  -- | 'keyAttributes'
  KeyAttributes ->
  -- | 'keyCheckValue'
  Prelude.Text ->
  -- | 'keyCheckValueAlgorithm'
  KeyCheckValueAlgorithm ->
  -- | 'keyOrigin'
  KeyOrigin ->
  -- | 'keyState'
  KeyState ->
  Key
newKey
  pCreateTimestamp_
  pEnabled_
  pExportable_
  pKeyArn_
  pKeyAttributes_
  pKeyCheckValue_
  pKeyCheckValueAlgorithm_
  pKeyOrigin_
  pKeyState_ =
    Key'
      { deletePendingTimestamp = Prelude.Nothing,
        deleteTimestamp = Prelude.Nothing,
        usageStartTimestamp = Prelude.Nothing,
        usageStopTimestamp = Prelude.Nothing,
        createTimestamp =
          Data._Time Lens.# pCreateTimestamp_,
        enabled = pEnabled_,
        exportable = pExportable_,
        keyArn = pKeyArn_,
        keyAttributes = pKeyAttributes_,
        keyCheckValue = pKeyCheckValue_,
        keyCheckValueAlgorithm = pKeyCheckValueAlgorithm_,
        keyOrigin = pKeyOrigin_,
        keyState = pKeyState_
      }

-- | The date and time after which Amazon Web Services Payment Cryptography
-- will delete the key. This value is present only when @KeyState@ is
-- @DELETE_PENDING@ and the key is scheduled for deletion.
key_deletePendingTimestamp :: Lens.Lens' Key (Prelude.Maybe Prelude.UTCTime)
key_deletePendingTimestamp = Lens.lens (\Key' {deletePendingTimestamp} -> deletePendingTimestamp) (\s@Key' {} a -> s {deletePendingTimestamp = a} :: Key) Prelude.. Lens.mapping Data._Time

-- | The date and time after which Amazon Web Services Payment Cryptography
-- will delete the key. This value is present only when when the @KeyState@
-- is @DELETE_COMPLETE@ and the Amazon Web Services Payment Cryptography
-- key is deleted.
key_deleteTimestamp :: Lens.Lens' Key (Prelude.Maybe Prelude.UTCTime)
key_deleteTimestamp = Lens.lens (\Key' {deleteTimestamp} -> deleteTimestamp) (\s@Key' {} a -> s {deleteTimestamp = a} :: Key) Prelude.. Lens.mapping Data._Time

-- | The date and time after which Amazon Web Services Payment Cryptography
-- will start using the key material for cryptographic operations.
key_usageStartTimestamp :: Lens.Lens' Key (Prelude.Maybe Prelude.UTCTime)
key_usageStartTimestamp = Lens.lens (\Key' {usageStartTimestamp} -> usageStartTimestamp) (\s@Key' {} a -> s {usageStartTimestamp = a} :: Key) Prelude.. Lens.mapping Data._Time

-- | The date and time after which Amazon Web Services Payment Cryptography
-- will stop using the key material for cryptographic operations.
key_usageStopTimestamp :: Lens.Lens' Key (Prelude.Maybe Prelude.UTCTime)
key_usageStopTimestamp = Lens.lens (\Key' {usageStopTimestamp} -> usageStopTimestamp) (\s@Key' {} a -> s {usageStopTimestamp = a} :: Key) Prelude.. Lens.mapping Data._Time

-- | The date and time when the key was created.
key_createTimestamp :: Lens.Lens' Key Prelude.UTCTime
key_createTimestamp = Lens.lens (\Key' {createTimestamp} -> createTimestamp) (\s@Key' {} a -> s {createTimestamp = a} :: Key) Prelude.. Data._Time

-- | Specifies whether the key is enabled.
key_enabled :: Lens.Lens' Key Prelude.Bool
key_enabled = Lens.lens (\Key' {enabled} -> enabled) (\s@Key' {} a -> s {enabled = a} :: Key)

-- | Specifies whether the key is exportable. This data is immutable after
-- the key is created.
key_exportable :: Lens.Lens' Key Prelude.Bool
key_exportable = Lens.lens (\Key' {exportable} -> exportable) (\s@Key' {} a -> s {exportable = a} :: Key)

-- | The Amazon Resource Name (ARN) of the key.
key_keyArn :: Lens.Lens' Key Prelude.Text
key_keyArn = Lens.lens (\Key' {keyArn} -> keyArn) (\s@Key' {} a -> s {keyArn = a} :: Key)

-- | The role of the key, the algorithm it supports, and the cryptographic
-- operations allowed with the key. This data is immutable after the key is
-- created.
key_keyAttributes :: Lens.Lens' Key KeyAttributes
key_keyAttributes = Lens.lens (\Key' {keyAttributes} -> keyAttributes) (\s@Key' {} a -> s {keyAttributes = a} :: Key)

-- | The key check value (KCV) is used to check if all parties holding a
-- given key have the same key or to detect that a key has changed. Amazon
-- Web Services Payment Cryptography calculates the KCV by using standard
-- algorithms, typically by encrypting 8 or 16 bytes or \"00\" or \"01\"
-- and then truncating the result to the first 3 bytes, or 6 hex digits, of
-- the resulting cryptogram.
key_keyCheckValue :: Lens.Lens' Key Prelude.Text
key_keyCheckValue = Lens.lens (\Key' {keyCheckValue} -> keyCheckValue) (\s@Key' {} a -> s {keyCheckValue = a} :: Key)

-- | The algorithm used for calculating key check value (KCV) for DES and AES
-- keys. For a DES key, Amazon Web Services Payment Cryptography computes
-- the KCV by encrypting 8 bytes, each with value \'00\', with the key to
-- be checked and retaining the 3 highest order bytes of the encrypted
-- result. For an AES key, Amazon Web Services Payment Cryptography
-- computes the KCV by encrypting 8 bytes, each with value \'01\', with the
-- key to be checked and retaining the 3 highest order bytes of the
-- encrypted result.
key_keyCheckValueAlgorithm :: Lens.Lens' Key KeyCheckValueAlgorithm
key_keyCheckValueAlgorithm = Lens.lens (\Key' {keyCheckValueAlgorithm} -> keyCheckValueAlgorithm) (\s@Key' {} a -> s {keyCheckValueAlgorithm = a} :: Key)

-- | The source of the key material. For keys created within Amazon Web
-- Services Payment Cryptography, the value is @AWS_PAYMENT_CRYPTOGRAPHY@.
-- For keys imported into Amazon Web Services Payment Cryptography, the
-- value is @EXTERNAL@.
key_keyOrigin :: Lens.Lens' Key KeyOrigin
key_keyOrigin = Lens.lens (\Key' {keyOrigin} -> keyOrigin) (\s@Key' {} a -> s {keyOrigin = a} :: Key)

-- | The state of key that is being created or deleted.
key_keyState :: Lens.Lens' Key KeyState
key_keyState = Lens.lens (\Key' {keyState} -> keyState) (\s@Key' {} a -> s {keyState = a} :: Key)

instance Data.FromJSON Key where
  parseJSON =
    Data.withObject
      "Key"
      ( \x ->
          Key'
            Prelude.<$> (x Data..:? "DeletePendingTimestamp")
            Prelude.<*> (x Data..:? "DeleteTimestamp")
            Prelude.<*> (x Data..:? "UsageStartTimestamp")
            Prelude.<*> (x Data..:? "UsageStopTimestamp")
            Prelude.<*> (x Data..: "CreateTimestamp")
            Prelude.<*> (x Data..: "Enabled")
            Prelude.<*> (x Data..: "Exportable")
            Prelude.<*> (x Data..: "KeyArn")
            Prelude.<*> (x Data..: "KeyAttributes")
            Prelude.<*> (x Data..: "KeyCheckValue")
            Prelude.<*> (x Data..: "KeyCheckValueAlgorithm")
            Prelude.<*> (x Data..: "KeyOrigin")
            Prelude.<*> (x Data..: "KeyState")
      )

instance Prelude.Hashable Key where
  hashWithSalt _salt Key' {..} =
    _salt
      `Prelude.hashWithSalt` deletePendingTimestamp
      `Prelude.hashWithSalt` deleteTimestamp
      `Prelude.hashWithSalt` usageStartTimestamp
      `Prelude.hashWithSalt` usageStopTimestamp
      `Prelude.hashWithSalt` createTimestamp
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` exportable
      `Prelude.hashWithSalt` keyArn
      `Prelude.hashWithSalt` keyAttributes
      `Prelude.hashWithSalt` keyCheckValue
      `Prelude.hashWithSalt` keyCheckValueAlgorithm
      `Prelude.hashWithSalt` keyOrigin
      `Prelude.hashWithSalt` keyState

instance Prelude.NFData Key where
  rnf Key' {..} =
    Prelude.rnf deletePendingTimestamp
      `Prelude.seq` Prelude.rnf deleteTimestamp
      `Prelude.seq` Prelude.rnf usageStartTimestamp
      `Prelude.seq` Prelude.rnf usageStopTimestamp
      `Prelude.seq` Prelude.rnf createTimestamp
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf exportable
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyAttributes
      `Prelude.seq` Prelude.rnf keyCheckValue
      `Prelude.seq` Prelude.rnf keyCheckValueAlgorithm
      `Prelude.seq` Prelude.rnf keyOrigin
      `Prelude.seq` Prelude.rnf keyState
