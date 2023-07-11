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
-- Module      : Amazonka.GroundStation.Types.Frequency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.Frequency where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.FrequencyUnits
import qualified Amazonka.Prelude as Prelude

-- | Object that describes the frequency.
--
-- /See:/ 'newFrequency' smart constructor.
data Frequency = Frequency'
  { -- | Frequency units.
    units :: FrequencyUnits,
    -- | Frequency value. Valid values are between 2200 to 2300 MHz and 7750 to
    -- 8400 MHz for downlink and 2025 to 2120 MHz for uplink.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Frequency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'units', 'frequency_units' - Frequency units.
--
-- 'value', 'frequency_value' - Frequency value. Valid values are between 2200 to 2300 MHz and 7750 to
-- 8400 MHz for downlink and 2025 to 2120 MHz for uplink.
newFrequency ::
  -- | 'units'
  FrequencyUnits ->
  -- | 'value'
  Prelude.Double ->
  Frequency
newFrequency pUnits_ pValue_ =
  Frequency' {units = pUnits_, value = pValue_}

-- | Frequency units.
frequency_units :: Lens.Lens' Frequency FrequencyUnits
frequency_units = Lens.lens (\Frequency' {units} -> units) (\s@Frequency' {} a -> s {units = a} :: Frequency)

-- | Frequency value. Valid values are between 2200 to 2300 MHz and 7750 to
-- 8400 MHz for downlink and 2025 to 2120 MHz for uplink.
frequency_value :: Lens.Lens' Frequency Prelude.Double
frequency_value = Lens.lens (\Frequency' {value} -> value) (\s@Frequency' {} a -> s {value = a} :: Frequency)

instance Data.FromJSON Frequency where
  parseJSON =
    Data.withObject
      "Frequency"
      ( \x ->
          Frequency'
            Prelude.<$> (x Data..: "units")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable Frequency where
  hashWithSalt _salt Frequency' {..} =
    _salt
      `Prelude.hashWithSalt` units
      `Prelude.hashWithSalt` value

instance Prelude.NFData Frequency where
  rnf Frequency' {..} =
    Prelude.rnf units `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Frequency where
  toJSON Frequency' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("units" Data..= units),
            Prelude.Just ("value" Data..= value)
          ]
      )
