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
-- Module      : Amazonka.IoTSiteWise.Types.AssetPropertyValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetPropertyValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.Quality
import Amazonka.IoTSiteWise.Types.TimeInNanos
import Amazonka.IoTSiteWise.Types.Variant
import qualified Amazonka.Prelude as Prelude

-- | Contains asset property value information.
--
-- /See:/ 'newAssetPropertyValue' smart constructor.
data AssetPropertyValue = AssetPropertyValue'
  { -- | The quality of the asset property value.
    quality :: Prelude.Maybe Quality,
    -- | The value of the asset property (see @Variant@).
    value :: Variant,
    -- | The timestamp of the asset property value.
    timestamp :: TimeInNanos
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
-- 'quality', 'assetPropertyValue_quality' - The quality of the asset property value.
--
-- 'value', 'assetPropertyValue_value' - The value of the asset property (see @Variant@).
--
-- 'timestamp', 'assetPropertyValue_timestamp' - The timestamp of the asset property value.
newAssetPropertyValue ::
  -- | 'value'
  Variant ->
  -- | 'timestamp'
  TimeInNanos ->
  AssetPropertyValue
newAssetPropertyValue pValue_ pTimestamp_ =
  AssetPropertyValue'
    { quality = Prelude.Nothing,
      value = pValue_,
      timestamp = pTimestamp_
    }

-- | The quality of the asset property value.
assetPropertyValue_quality :: Lens.Lens' AssetPropertyValue (Prelude.Maybe Quality)
assetPropertyValue_quality = Lens.lens (\AssetPropertyValue' {quality} -> quality) (\s@AssetPropertyValue' {} a -> s {quality = a} :: AssetPropertyValue)

-- | The value of the asset property (see @Variant@).
assetPropertyValue_value :: Lens.Lens' AssetPropertyValue Variant
assetPropertyValue_value = Lens.lens (\AssetPropertyValue' {value} -> value) (\s@AssetPropertyValue' {} a -> s {value = a} :: AssetPropertyValue)

-- | The timestamp of the asset property value.
assetPropertyValue_timestamp :: Lens.Lens' AssetPropertyValue TimeInNanos
assetPropertyValue_timestamp = Lens.lens (\AssetPropertyValue' {timestamp} -> timestamp) (\s@AssetPropertyValue' {} a -> s {timestamp = a} :: AssetPropertyValue)

instance Data.FromJSON AssetPropertyValue where
  parseJSON =
    Data.withObject
      "AssetPropertyValue"
      ( \x ->
          AssetPropertyValue'
            Prelude.<$> (x Data..:? "quality")
            Prelude.<*> (x Data..: "value")
            Prelude.<*> (x Data..: "timestamp")
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

instance Data.ToJSON AssetPropertyValue where
  toJSON AssetPropertyValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("quality" Data..=) Prelude.<$> quality,
            Prelude.Just ("value" Data..= value),
            Prelude.Just ("timestamp" Data..= timestamp)
          ]
      )
