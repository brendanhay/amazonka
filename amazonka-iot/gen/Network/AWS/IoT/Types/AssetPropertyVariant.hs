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
-- Module      : Network.AWS.IoT.Types.AssetPropertyVariant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AssetPropertyVariant where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains an asset property value (of a single type).
--
-- /See:/ 'newAssetPropertyVariant' smart constructor.
data AssetPropertyVariant = AssetPropertyVariant'
  { -- | Optional. A string that contains the double value of the value entry.
    -- Accepts substitution templates.
    doubleValue :: Core.Maybe Core.Text,
    -- | Optional. The string value of the value entry. Accepts substitution
    -- templates.
    stringValue :: Core.Maybe Core.Text,
    -- | Optional. A string that contains the boolean value (@true@ or @false@)
    -- of the value entry. Accepts substitution templates.
    booleanValue :: Core.Maybe Core.Text,
    -- | Optional. A string that contains the integer value of the value entry.
    -- Accepts substitution templates.
    integerValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssetPropertyVariant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'doubleValue', 'assetPropertyVariant_doubleValue' - Optional. A string that contains the double value of the value entry.
-- Accepts substitution templates.
--
-- 'stringValue', 'assetPropertyVariant_stringValue' - Optional. The string value of the value entry. Accepts substitution
-- templates.
--
-- 'booleanValue', 'assetPropertyVariant_booleanValue' - Optional. A string that contains the boolean value (@true@ or @false@)
-- of the value entry. Accepts substitution templates.
--
-- 'integerValue', 'assetPropertyVariant_integerValue' - Optional. A string that contains the integer value of the value entry.
-- Accepts substitution templates.
newAssetPropertyVariant ::
  AssetPropertyVariant
newAssetPropertyVariant =
  AssetPropertyVariant'
    { doubleValue = Core.Nothing,
      stringValue = Core.Nothing,
      booleanValue = Core.Nothing,
      integerValue = Core.Nothing
    }

-- | Optional. A string that contains the double value of the value entry.
-- Accepts substitution templates.
assetPropertyVariant_doubleValue :: Lens.Lens' AssetPropertyVariant (Core.Maybe Core.Text)
assetPropertyVariant_doubleValue = Lens.lens (\AssetPropertyVariant' {doubleValue} -> doubleValue) (\s@AssetPropertyVariant' {} a -> s {doubleValue = a} :: AssetPropertyVariant)

-- | Optional. The string value of the value entry. Accepts substitution
-- templates.
assetPropertyVariant_stringValue :: Lens.Lens' AssetPropertyVariant (Core.Maybe Core.Text)
assetPropertyVariant_stringValue = Lens.lens (\AssetPropertyVariant' {stringValue} -> stringValue) (\s@AssetPropertyVariant' {} a -> s {stringValue = a} :: AssetPropertyVariant)

-- | Optional. A string that contains the boolean value (@true@ or @false@)
-- of the value entry. Accepts substitution templates.
assetPropertyVariant_booleanValue :: Lens.Lens' AssetPropertyVariant (Core.Maybe Core.Text)
assetPropertyVariant_booleanValue = Lens.lens (\AssetPropertyVariant' {booleanValue} -> booleanValue) (\s@AssetPropertyVariant' {} a -> s {booleanValue = a} :: AssetPropertyVariant)

-- | Optional. A string that contains the integer value of the value entry.
-- Accepts substitution templates.
assetPropertyVariant_integerValue :: Lens.Lens' AssetPropertyVariant (Core.Maybe Core.Text)
assetPropertyVariant_integerValue = Lens.lens (\AssetPropertyVariant' {integerValue} -> integerValue) (\s@AssetPropertyVariant' {} a -> s {integerValue = a} :: AssetPropertyVariant)

instance Core.FromJSON AssetPropertyVariant where
  parseJSON =
    Core.withObject
      "AssetPropertyVariant"
      ( \x ->
          AssetPropertyVariant'
            Core.<$> (x Core..:? "doubleValue")
            Core.<*> (x Core..:? "stringValue")
            Core.<*> (x Core..:? "booleanValue")
            Core.<*> (x Core..:? "integerValue")
      )

instance Core.Hashable AssetPropertyVariant

instance Core.NFData AssetPropertyVariant

instance Core.ToJSON AssetPropertyVariant where
  toJSON AssetPropertyVariant' {..} =
    Core.object
      ( Core.catMaybes
          [ ("doubleValue" Core..=) Core.<$> doubleValue,
            ("stringValue" Core..=) Core.<$> stringValue,
            ("booleanValue" Core..=) Core.<$> booleanValue,
            ("integerValue" Core..=) Core.<$> integerValue
          ]
      )
