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
-- Module      : Amazonka.GroundStation.Types.SpectrumConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.SpectrumConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.Frequency
import Amazonka.GroundStation.Types.FrequencyBandwidth
import Amazonka.GroundStation.Types.Polarization
import qualified Amazonka.Prelude as Prelude

-- | Object that describes a spectral @Config@.
--
-- /See:/ 'newSpectrumConfig' smart constructor.
data SpectrumConfig = SpectrumConfig'
  { -- | Polarization of a spectral @Config@. Capturing both @\"RIGHT_HAND\"@ and
    -- @\"LEFT_HAND\"@ polarization requires two separate configs.
    polarization :: Prelude.Maybe Polarization,
    -- | Bandwidth of a spectral @Config@. AWS Ground Station currently has the
    -- following bandwidth limitations:
    --
    -- -   For @AntennaDownlinkDemodDecodeconfig@, valid values are between 125
    --     kHz to 650 MHz.
    --
    -- -   For @AntennaDownlinkconfig@ valid values are between 10 kHz to 54
    --     MHz.
    --
    -- -   For @AntennaUplinkConfig@, valid values are between 10 kHz to 54
    --     MHz.
    bandwidth :: FrequencyBandwidth,
    -- | Center frequency of a spectral @Config@. Valid values are between 2200
    -- to 2300 MHz and 7750 to 8400 MHz for downlink and 2025 to 2120 MHz for
    -- uplink.
    centerFrequency :: Frequency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpectrumConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'polarization', 'spectrumConfig_polarization' - Polarization of a spectral @Config@. Capturing both @\"RIGHT_HAND\"@ and
-- @\"LEFT_HAND\"@ polarization requires two separate configs.
--
-- 'bandwidth', 'spectrumConfig_bandwidth' - Bandwidth of a spectral @Config@. AWS Ground Station currently has the
-- following bandwidth limitations:
--
-- -   For @AntennaDownlinkDemodDecodeconfig@, valid values are between 125
--     kHz to 650 MHz.
--
-- -   For @AntennaDownlinkconfig@ valid values are between 10 kHz to 54
--     MHz.
--
-- -   For @AntennaUplinkConfig@, valid values are between 10 kHz to 54
--     MHz.
--
-- 'centerFrequency', 'spectrumConfig_centerFrequency' - Center frequency of a spectral @Config@. Valid values are between 2200
-- to 2300 MHz and 7750 to 8400 MHz for downlink and 2025 to 2120 MHz for
-- uplink.
newSpectrumConfig ::
  -- | 'bandwidth'
  FrequencyBandwidth ->
  -- | 'centerFrequency'
  Frequency ->
  SpectrumConfig
newSpectrumConfig pBandwidth_ pCenterFrequency_ =
  SpectrumConfig'
    { polarization = Prelude.Nothing,
      bandwidth = pBandwidth_,
      centerFrequency = pCenterFrequency_
    }

-- | Polarization of a spectral @Config@. Capturing both @\"RIGHT_HAND\"@ and
-- @\"LEFT_HAND\"@ polarization requires two separate configs.
spectrumConfig_polarization :: Lens.Lens' SpectrumConfig (Prelude.Maybe Polarization)
spectrumConfig_polarization = Lens.lens (\SpectrumConfig' {polarization} -> polarization) (\s@SpectrumConfig' {} a -> s {polarization = a} :: SpectrumConfig)

-- | Bandwidth of a spectral @Config@. AWS Ground Station currently has the
-- following bandwidth limitations:
--
-- -   For @AntennaDownlinkDemodDecodeconfig@, valid values are between 125
--     kHz to 650 MHz.
--
-- -   For @AntennaDownlinkconfig@ valid values are between 10 kHz to 54
--     MHz.
--
-- -   For @AntennaUplinkConfig@, valid values are between 10 kHz to 54
--     MHz.
spectrumConfig_bandwidth :: Lens.Lens' SpectrumConfig FrequencyBandwidth
spectrumConfig_bandwidth = Lens.lens (\SpectrumConfig' {bandwidth} -> bandwidth) (\s@SpectrumConfig' {} a -> s {bandwidth = a} :: SpectrumConfig)

-- | Center frequency of a spectral @Config@. Valid values are between 2200
-- to 2300 MHz and 7750 to 8400 MHz for downlink and 2025 to 2120 MHz for
-- uplink.
spectrumConfig_centerFrequency :: Lens.Lens' SpectrumConfig Frequency
spectrumConfig_centerFrequency = Lens.lens (\SpectrumConfig' {centerFrequency} -> centerFrequency) (\s@SpectrumConfig' {} a -> s {centerFrequency = a} :: SpectrumConfig)

instance Data.FromJSON SpectrumConfig where
  parseJSON =
    Data.withObject
      "SpectrumConfig"
      ( \x ->
          SpectrumConfig'
            Prelude.<$> (x Data..:? "polarization")
            Prelude.<*> (x Data..: "bandwidth")
            Prelude.<*> (x Data..: "centerFrequency")
      )

instance Prelude.Hashable SpectrumConfig where
  hashWithSalt _salt SpectrumConfig' {..} =
    _salt `Prelude.hashWithSalt` polarization
      `Prelude.hashWithSalt` bandwidth
      `Prelude.hashWithSalt` centerFrequency

instance Prelude.NFData SpectrumConfig where
  rnf SpectrumConfig' {..} =
    Prelude.rnf polarization
      `Prelude.seq` Prelude.rnf bandwidth
      `Prelude.seq` Prelude.rnf centerFrequency

instance Data.ToJSON SpectrumConfig where
  toJSON SpectrumConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("polarization" Data..=) Prelude.<$> polarization,
            Prelude.Just ("bandwidth" Data..= bandwidth),
            Prelude.Just
              ("centerFrequency" Data..= centerFrequency)
          ]
      )
