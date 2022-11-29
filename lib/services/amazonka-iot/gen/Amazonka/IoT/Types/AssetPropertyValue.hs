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
-- Module      : Amazonka.IoT.Types.AssetPropertyValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AssetPropertyValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.AssetPropertyTimestamp
import Amazonka.IoT.Types.AssetPropertyVariant
import qualified Amazonka.Prelude as Prelude

-- | An asset property value entry containing the following information.
--
-- /See:/ 'newAssetPropertyValue' smart constructor.
data AssetPropertyValue = AssetPropertyValue'
  { -- | Optional. A string that describes the quality of the value. Accepts
    -- substitution templates. Must be @GOOD@, @BAD@, or @UNCERTAIN@.
    quality :: Prelude.Maybe Prelude.Text,
    -- | The value of the asset property.
    value :: AssetPropertyVariant,
    -- | The asset property value timestamp.
    timestamp :: AssetPropertyTimestamp
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetPropertyValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quality', 'assetPropertyValue_quality' - Optional. A string that describes the quality of the value. Accepts
-- substitution templates. Must be @GOOD@, @BAD@, or @UNCERTAIN@.
--
-- 'value', 'assetPropertyValue_value' - The value of the asset property.
--
-- 'timestamp', 'assetPropertyValue_timestamp' - The asset property value timestamp.
newAssetPropertyValue ::
  -- | 'value'
  AssetPropertyVariant ->
  -- | 'timestamp'
  AssetPropertyTimestamp ->
  AssetPropertyValue
newAssetPropertyValue pValue_ pTimestamp_ =
  AssetPropertyValue'
    { quality = Prelude.Nothing,
      value = pValue_,
      timestamp = pTimestamp_
    }

-- | Optional. A string that describes the quality of the value. Accepts
-- substitution templates. Must be @GOOD@, @BAD@, or @UNCERTAIN@.
assetPropertyValue_quality :: Lens.Lens' AssetPropertyValue (Prelude.Maybe Prelude.Text)
assetPropertyValue_quality = Lens.lens (\AssetPropertyValue' {quality} -> quality) (\s@AssetPropertyValue' {} a -> s {quality = a} :: AssetPropertyValue)

-- | The value of the asset property.
assetPropertyValue_value :: Lens.Lens' AssetPropertyValue AssetPropertyVariant
assetPropertyValue_value = Lens.lens (\AssetPropertyValue' {value} -> value) (\s@AssetPropertyValue' {} a -> s {value = a} :: AssetPropertyValue)

-- | The asset property value timestamp.
assetPropertyValue_timestamp :: Lens.Lens' AssetPropertyValue AssetPropertyTimestamp
assetPropertyValue_timestamp = Lens.lens (\AssetPropertyValue' {timestamp} -> timestamp) (\s@AssetPropertyValue' {} a -> s {timestamp = a} :: AssetPropertyValue)

instance Core.FromJSON AssetPropertyValue where
  parseJSON =
    Core.withObject
      "AssetPropertyValue"
      ( \x ->
          AssetPropertyValue'
            Prelude.<$> (x Core..:? "quality")
            Prelude.<*> (x Core..: "value")
            Prelude.<*> (x Core..: "timestamp")
      )

instance Prelude.Hashable AssetPropertyValue where
  hashWithSalt _salt AssetPropertyValue' {..} =
    _salt `Prelude.hashWithSalt` quality
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData AssetPropertyValue where
  rnf AssetPropertyValue' {..} =
    Prelude.rnf quality
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf timestamp

instance Core.ToJSON AssetPropertyValue where
  toJSON AssetPropertyValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("quality" Core..=) Prelude.<$> quality,
            Prelude.Just ("value" Core..= value),
            Prelude.Just ("timestamp" Core..= timestamp)
          ]
      )
