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
-- Module      : Amazonka.PaymentCryptography.Types.WrappedKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.WrappedKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types.WrappedKeyMaterialFormat
import qualified Amazonka.Prelude as Prelude

-- | Parameter information for generating a wrapped key using TR-31 or TR-34
-- standard.
--
-- /See:/ 'newWrappedKey' smart constructor.
data WrappedKey = WrappedKey'
  { -- | Parameter information for generating a wrapped key using TR-31 or TR-34
    -- standard.
    keyMaterial :: Data.Sensitive Prelude.Text,
    -- | The key block format of a wrapped key.
    wrappedKeyMaterialFormat :: WrappedKeyMaterialFormat,
    -- | The @KeyARN@ of the wrapped key.
    wrappingKeyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WrappedKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyMaterial', 'wrappedKey_keyMaterial' - Parameter information for generating a wrapped key using TR-31 or TR-34
-- standard.
--
-- 'wrappedKeyMaterialFormat', 'wrappedKey_wrappedKeyMaterialFormat' - The key block format of a wrapped key.
--
-- 'wrappingKeyArn', 'wrappedKey_wrappingKeyArn' - The @KeyARN@ of the wrapped key.
newWrappedKey ::
  -- | 'keyMaterial'
  Prelude.Text ->
  -- | 'wrappedKeyMaterialFormat'
  WrappedKeyMaterialFormat ->
  -- | 'wrappingKeyArn'
  Prelude.Text ->
  WrappedKey
newWrappedKey
  pKeyMaterial_
  pWrappedKeyMaterialFormat_
  pWrappingKeyArn_ =
    WrappedKey'
      { keyMaterial =
          Data._Sensitive Lens.# pKeyMaterial_,
        wrappedKeyMaterialFormat =
          pWrappedKeyMaterialFormat_,
        wrappingKeyArn = pWrappingKeyArn_
      }

-- | Parameter information for generating a wrapped key using TR-31 or TR-34
-- standard.
wrappedKey_keyMaterial :: Lens.Lens' WrappedKey Prelude.Text
wrappedKey_keyMaterial = Lens.lens (\WrappedKey' {keyMaterial} -> keyMaterial) (\s@WrappedKey' {} a -> s {keyMaterial = a} :: WrappedKey) Prelude.. Data._Sensitive

-- | The key block format of a wrapped key.
wrappedKey_wrappedKeyMaterialFormat :: Lens.Lens' WrappedKey WrappedKeyMaterialFormat
wrappedKey_wrappedKeyMaterialFormat = Lens.lens (\WrappedKey' {wrappedKeyMaterialFormat} -> wrappedKeyMaterialFormat) (\s@WrappedKey' {} a -> s {wrappedKeyMaterialFormat = a} :: WrappedKey)

-- | The @KeyARN@ of the wrapped key.
wrappedKey_wrappingKeyArn :: Lens.Lens' WrappedKey Prelude.Text
wrappedKey_wrappingKeyArn = Lens.lens (\WrappedKey' {wrappingKeyArn} -> wrappingKeyArn) (\s@WrappedKey' {} a -> s {wrappingKeyArn = a} :: WrappedKey)

instance Data.FromJSON WrappedKey where
  parseJSON =
    Data.withObject
      "WrappedKey"
      ( \x ->
          WrappedKey'
            Prelude.<$> (x Data..: "KeyMaterial")
            Prelude.<*> (x Data..: "WrappedKeyMaterialFormat")
            Prelude.<*> (x Data..: "WrappingKeyArn")
      )

instance Prelude.Hashable WrappedKey where
  hashWithSalt _salt WrappedKey' {..} =
    _salt
      `Prelude.hashWithSalt` keyMaterial
      `Prelude.hashWithSalt` wrappedKeyMaterialFormat
      `Prelude.hashWithSalt` wrappingKeyArn

instance Prelude.NFData WrappedKey where
  rnf WrappedKey' {..} =
    Prelude.rnf keyMaterial
      `Prelude.seq` Prelude.rnf wrappedKeyMaterialFormat
      `Prelude.seq` Prelude.rnf wrappingKeyArn
