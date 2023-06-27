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
-- Module      : Amazonka.PaymentCryptography.Types.KeySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.KeySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types.KeyAttributes
import Amazonka.PaymentCryptography.Types.KeyState
import qualified Amazonka.Prelude as Prelude

-- | Metadata about an Amazon Web Services Payment Cryptography key.
--
-- /See:/ 'newKeySummary' smart constructor.
data KeySummary = KeySummary'
  { -- | Specifies whether the key is enabled.
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
    -- | The state of an Amazon Web Services Payment Cryptography that is being
    -- created or deleted.
    keyState :: KeyState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'keySummary_enabled' - Specifies whether the key is enabled.
--
-- 'exportable', 'keySummary_exportable' - Specifies whether the key is exportable. This data is immutable after
-- the key is created.
--
-- 'keyArn', 'keySummary_keyArn' - The Amazon Resource Name (ARN) of the key.
--
-- 'keyAttributes', 'keySummary_keyAttributes' - The role of the key, the algorithm it supports, and the cryptographic
-- operations allowed with the key. This data is immutable after the key is
-- created.
--
-- 'keyCheckValue', 'keySummary_keyCheckValue' - The key check value (KCV) is used to check if all parties holding a
-- given key have the same key or to detect that a key has changed. Amazon
-- Web Services Payment Cryptography calculates the KCV by using standard
-- algorithms, typically by encrypting 8 or 16 bytes or \"00\" or \"01\"
-- and then truncating the result to the first 3 bytes, or 6 hex digits, of
-- the resulting cryptogram.
--
-- 'keyState', 'keySummary_keyState' - The state of an Amazon Web Services Payment Cryptography that is being
-- created or deleted.
newKeySummary ::
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
  -- | 'keyState'
  KeyState ->
  KeySummary
newKeySummary
  pEnabled_
  pExportable_
  pKeyArn_
  pKeyAttributes_
  pKeyCheckValue_
  pKeyState_ =
    KeySummary'
      { enabled = pEnabled_,
        exportable = pExportable_,
        keyArn = pKeyArn_,
        keyAttributes = pKeyAttributes_,
        keyCheckValue = pKeyCheckValue_,
        keyState = pKeyState_
      }

-- | Specifies whether the key is enabled.
keySummary_enabled :: Lens.Lens' KeySummary Prelude.Bool
keySummary_enabled = Lens.lens (\KeySummary' {enabled} -> enabled) (\s@KeySummary' {} a -> s {enabled = a} :: KeySummary)

-- | Specifies whether the key is exportable. This data is immutable after
-- the key is created.
keySummary_exportable :: Lens.Lens' KeySummary Prelude.Bool
keySummary_exportable = Lens.lens (\KeySummary' {exportable} -> exportable) (\s@KeySummary' {} a -> s {exportable = a} :: KeySummary)

-- | The Amazon Resource Name (ARN) of the key.
keySummary_keyArn :: Lens.Lens' KeySummary Prelude.Text
keySummary_keyArn = Lens.lens (\KeySummary' {keyArn} -> keyArn) (\s@KeySummary' {} a -> s {keyArn = a} :: KeySummary)

-- | The role of the key, the algorithm it supports, and the cryptographic
-- operations allowed with the key. This data is immutable after the key is
-- created.
keySummary_keyAttributes :: Lens.Lens' KeySummary KeyAttributes
keySummary_keyAttributes = Lens.lens (\KeySummary' {keyAttributes} -> keyAttributes) (\s@KeySummary' {} a -> s {keyAttributes = a} :: KeySummary)

-- | The key check value (KCV) is used to check if all parties holding a
-- given key have the same key or to detect that a key has changed. Amazon
-- Web Services Payment Cryptography calculates the KCV by using standard
-- algorithms, typically by encrypting 8 or 16 bytes or \"00\" or \"01\"
-- and then truncating the result to the first 3 bytes, or 6 hex digits, of
-- the resulting cryptogram.
keySummary_keyCheckValue :: Lens.Lens' KeySummary Prelude.Text
keySummary_keyCheckValue = Lens.lens (\KeySummary' {keyCheckValue} -> keyCheckValue) (\s@KeySummary' {} a -> s {keyCheckValue = a} :: KeySummary)

-- | The state of an Amazon Web Services Payment Cryptography that is being
-- created or deleted.
keySummary_keyState :: Lens.Lens' KeySummary KeyState
keySummary_keyState = Lens.lens (\KeySummary' {keyState} -> keyState) (\s@KeySummary' {} a -> s {keyState = a} :: KeySummary)

instance Data.FromJSON KeySummary where
  parseJSON =
    Data.withObject
      "KeySummary"
      ( \x ->
          KeySummary'
            Prelude.<$> (x Data..: "Enabled")
            Prelude.<*> (x Data..: "Exportable")
            Prelude.<*> (x Data..: "KeyArn")
            Prelude.<*> (x Data..: "KeyAttributes")
            Prelude.<*> (x Data..: "KeyCheckValue")
            Prelude.<*> (x Data..: "KeyState")
      )

instance Prelude.Hashable KeySummary where
  hashWithSalt _salt KeySummary' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` exportable
      `Prelude.hashWithSalt` keyArn
      `Prelude.hashWithSalt` keyAttributes
      `Prelude.hashWithSalt` keyCheckValue
      `Prelude.hashWithSalt` keyState

instance Prelude.NFData KeySummary where
  rnf KeySummary' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf exportable
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyAttributes
      `Prelude.seq` Prelude.rnf keyCheckValue
      `Prelude.seq` Prelude.rnf keyState
