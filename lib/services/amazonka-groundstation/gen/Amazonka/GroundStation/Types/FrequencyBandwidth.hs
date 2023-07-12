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
-- Module      : Amazonka.GroundStation.Types.FrequencyBandwidth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.FrequencyBandwidth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.BandwidthUnits
import qualified Amazonka.Prelude as Prelude

-- | Object that describes the frequency bandwidth.
--
-- /See:/ 'newFrequencyBandwidth' smart constructor.
data FrequencyBandwidth = FrequencyBandwidth'
  { -- | Frequency bandwidth units.
    units :: BandwidthUnits,
    -- | Frequency bandwidth value. AWS Ground Station currently has the
    -- following bandwidth limitations:
    --
    -- -   For @AntennaDownlinkDemodDecodeconfig@, valid values are between 125
    --     kHz to 650 MHz.
    --
    -- -   For @AntennaDownlinkconfig@, valid values are between 10 kHz to 54
    --     MHz.
    --
    -- -   For @AntennaUplinkConfig@, valid values are between 10 kHz to 54
    --     MHz.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FrequencyBandwidth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'units', 'frequencyBandwidth_units' - Frequency bandwidth units.
--
-- 'value', 'frequencyBandwidth_value' - Frequency bandwidth value. AWS Ground Station currently has the
-- following bandwidth limitations:
--
-- -   For @AntennaDownlinkDemodDecodeconfig@, valid values are between 125
--     kHz to 650 MHz.
--
-- -   For @AntennaDownlinkconfig@, valid values are between 10 kHz to 54
--     MHz.
--
-- -   For @AntennaUplinkConfig@, valid values are between 10 kHz to 54
--     MHz.
newFrequencyBandwidth ::
  -- | 'units'
  BandwidthUnits ->
  -- | 'value'
  Prelude.Double ->
  FrequencyBandwidth
newFrequencyBandwidth pUnits_ pValue_ =
  FrequencyBandwidth'
    { units = pUnits_,
      value = pValue_
    }

-- | Frequency bandwidth units.
frequencyBandwidth_units :: Lens.Lens' FrequencyBandwidth BandwidthUnits
frequencyBandwidth_units = Lens.lens (\FrequencyBandwidth' {units} -> units) (\s@FrequencyBandwidth' {} a -> s {units = a} :: FrequencyBandwidth)

-- | Frequency bandwidth value. AWS Ground Station currently has the
-- following bandwidth limitations:
--
-- -   For @AntennaDownlinkDemodDecodeconfig@, valid values are between 125
--     kHz to 650 MHz.
--
-- -   For @AntennaDownlinkconfig@, valid values are between 10 kHz to 54
--     MHz.
--
-- -   For @AntennaUplinkConfig@, valid values are between 10 kHz to 54
--     MHz.
frequencyBandwidth_value :: Lens.Lens' FrequencyBandwidth Prelude.Double
frequencyBandwidth_value = Lens.lens (\FrequencyBandwidth' {value} -> value) (\s@FrequencyBandwidth' {} a -> s {value = a} :: FrequencyBandwidth)

instance Data.FromJSON FrequencyBandwidth where
  parseJSON =
    Data.withObject
      "FrequencyBandwidth"
      ( \x ->
          FrequencyBandwidth'
            Prelude.<$> (x Data..: "units")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable FrequencyBandwidth where
  hashWithSalt _salt FrequencyBandwidth' {..} =
    _salt
      `Prelude.hashWithSalt` units
      `Prelude.hashWithSalt` value

instance Prelude.NFData FrequencyBandwidth where
  rnf FrequencyBandwidth' {..} =
    Prelude.rnf units `Prelude.seq` Prelude.rnf value

instance Data.ToJSON FrequencyBandwidth where
  toJSON FrequencyBandwidth' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("units" Data..= units),
            Prelude.Just ("value" Data..= value)
          ]
      )
