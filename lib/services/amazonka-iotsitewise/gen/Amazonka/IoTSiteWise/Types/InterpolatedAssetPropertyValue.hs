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
-- Module      : Amazonka.IoTSiteWise.Types.InterpolatedAssetPropertyValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.InterpolatedAssetPropertyValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.TimeInNanos
import Amazonka.IoTSiteWise.Types.Variant
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an interpolated asset property value.
--
-- /See:/ 'newInterpolatedAssetPropertyValue' smart constructor.
data InterpolatedAssetPropertyValue = InterpolatedAssetPropertyValue'
  { timestamp :: TimeInNanos,
    value :: Variant
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InterpolatedAssetPropertyValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'interpolatedAssetPropertyValue_timestamp' - Undocumented member.
--
-- 'value', 'interpolatedAssetPropertyValue_value' - Undocumented member.
newInterpolatedAssetPropertyValue ::
  -- | 'timestamp'
  TimeInNanos ->
  -- | 'value'
  Variant ->
  InterpolatedAssetPropertyValue
newInterpolatedAssetPropertyValue pTimestamp_ pValue_ =
  InterpolatedAssetPropertyValue'
    { timestamp =
        pTimestamp_,
      value = pValue_
    }

-- | Undocumented member.
interpolatedAssetPropertyValue_timestamp :: Lens.Lens' InterpolatedAssetPropertyValue TimeInNanos
interpolatedAssetPropertyValue_timestamp = Lens.lens (\InterpolatedAssetPropertyValue' {timestamp} -> timestamp) (\s@InterpolatedAssetPropertyValue' {} a -> s {timestamp = a} :: InterpolatedAssetPropertyValue)

-- | Undocumented member.
interpolatedAssetPropertyValue_value :: Lens.Lens' InterpolatedAssetPropertyValue Variant
interpolatedAssetPropertyValue_value = Lens.lens (\InterpolatedAssetPropertyValue' {value} -> value) (\s@InterpolatedAssetPropertyValue' {} a -> s {value = a} :: InterpolatedAssetPropertyValue)

instance Data.FromJSON InterpolatedAssetPropertyValue where
  parseJSON =
    Data.withObject
      "InterpolatedAssetPropertyValue"
      ( \x ->
          InterpolatedAssetPropertyValue'
            Prelude.<$> (x Data..: "timestamp")
            Prelude.<*> (x Data..: "value")
      )

instance
  Prelude.Hashable
    InterpolatedAssetPropertyValue
  where
  hashWithSalt
    _salt
    InterpolatedAssetPropertyValue' {..} =
      _salt
        `Prelude.hashWithSalt` timestamp
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    InterpolatedAssetPropertyValue
  where
  rnf InterpolatedAssetPropertyValue' {..} =
    Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf value
