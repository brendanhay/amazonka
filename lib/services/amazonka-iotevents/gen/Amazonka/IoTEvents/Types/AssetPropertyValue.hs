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
-- Module      : Amazonka.IoTEvents.Types.AssetPropertyValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.AssetPropertyValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.AssetPropertyTimestamp
import Amazonka.IoTEvents.Types.AssetPropertyVariant
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains value information. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_AssetPropertyValue.html AssetPropertyValue>
-- in the /AWS IoT SiteWise API Reference/.
--
-- You must use expressions for all parameters in @AssetPropertyValue@. The
-- expressions accept literals, operators, functions, references, and
-- substitution templates.
--
-- __Examples__
--
-- -   For literal values, the expressions must contain single quotes. For
--     example, the value for the @quality@ parameter can be @\'GOOD\'@.
--
-- -   For references, you must specify either variables or input values.
--     For example, the value for the @quality@ parameter can be
--     @$input.TemperatureInput.sensorData.quality@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/iotevents-expressions.html Expressions>
-- in the /AWS IoT Events Developer Guide/.
--
-- /See:/ 'newAssetPropertyValue' smart constructor.
data AssetPropertyValue = AssetPropertyValue'
  { -- | The quality of the asset property value. The value must be @\'GOOD\'@,
    -- @\'BAD\'@, or @\'UNCERTAIN\'@.
    quality :: Prelude.Maybe Prelude.Text,
    -- | The timestamp associated with the asset property value. The default is
    -- the current event time.
    timestamp :: Prelude.Maybe AssetPropertyTimestamp,
    -- | The value to send to an asset property.
    value :: Prelude.Maybe AssetPropertyVariant
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
-- 'quality', 'assetPropertyValue_quality' - The quality of the asset property value. The value must be @\'GOOD\'@,
-- @\'BAD\'@, or @\'UNCERTAIN\'@.
--
-- 'timestamp', 'assetPropertyValue_timestamp' - The timestamp associated with the asset property value. The default is
-- the current event time.
--
-- 'value', 'assetPropertyValue_value' - The value to send to an asset property.
newAssetPropertyValue ::
  AssetPropertyValue
newAssetPropertyValue =
  AssetPropertyValue'
    { quality = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The quality of the asset property value. The value must be @\'GOOD\'@,
-- @\'BAD\'@, or @\'UNCERTAIN\'@.
assetPropertyValue_quality :: Lens.Lens' AssetPropertyValue (Prelude.Maybe Prelude.Text)
assetPropertyValue_quality = Lens.lens (\AssetPropertyValue' {quality} -> quality) (\s@AssetPropertyValue' {} a -> s {quality = a} :: AssetPropertyValue)

-- | The timestamp associated with the asset property value. The default is
-- the current event time.
assetPropertyValue_timestamp :: Lens.Lens' AssetPropertyValue (Prelude.Maybe AssetPropertyTimestamp)
assetPropertyValue_timestamp = Lens.lens (\AssetPropertyValue' {timestamp} -> timestamp) (\s@AssetPropertyValue' {} a -> s {timestamp = a} :: AssetPropertyValue)

-- | The value to send to an asset property.
assetPropertyValue_value :: Lens.Lens' AssetPropertyValue (Prelude.Maybe AssetPropertyVariant)
assetPropertyValue_value = Lens.lens (\AssetPropertyValue' {value} -> value) (\s@AssetPropertyValue' {} a -> s {value = a} :: AssetPropertyValue)

instance Data.FromJSON AssetPropertyValue where
  parseJSON =
    Data.withObject
      "AssetPropertyValue"
      ( \x ->
          AssetPropertyValue'
            Prelude.<$> (x Data..:? "quality")
            Prelude.<*> (x Data..:? "timestamp")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable AssetPropertyValue where
  hashWithSalt _salt AssetPropertyValue' {..} =
    _salt
      `Prelude.hashWithSalt` quality
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` value

instance Prelude.NFData AssetPropertyValue where
  rnf AssetPropertyValue' {..} =
    Prelude.rnf quality
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON AssetPropertyValue where
  toJSON AssetPropertyValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("quality" Data..=) Prelude.<$> quality,
            ("timestamp" Data..=) Prelude.<$> timestamp,
            ("value" Data..=) Prelude.<$> value
          ]
      )
