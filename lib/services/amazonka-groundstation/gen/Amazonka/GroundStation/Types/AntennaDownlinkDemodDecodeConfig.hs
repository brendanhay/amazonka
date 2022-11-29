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
-- Module      : Amazonka.GroundStation.Types.AntennaDownlinkDemodDecodeConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.AntennaDownlinkDemodDecodeConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types.DecodeConfig
import Amazonka.GroundStation.Types.DemodulationConfig
import Amazonka.GroundStation.Types.SpectrumConfig
import qualified Amazonka.Prelude as Prelude

-- | Information about how AWS Ground Station should conﬁgure an antenna for
-- downlink demod decode during a contact.
--
-- /See:/ 'newAntennaDownlinkDemodDecodeConfig' smart constructor.
data AntennaDownlinkDemodDecodeConfig = AntennaDownlinkDemodDecodeConfig'
  { -- | Information about the decode @Config@.
    decodeConfig :: DecodeConfig,
    -- | Information about the demodulation @Config@.
    demodulationConfig :: DemodulationConfig,
    -- | Information about the spectral @Config@.
    spectrumConfig :: SpectrumConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AntennaDownlinkDemodDecodeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decodeConfig', 'antennaDownlinkDemodDecodeConfig_decodeConfig' - Information about the decode @Config@.
--
-- 'demodulationConfig', 'antennaDownlinkDemodDecodeConfig_demodulationConfig' - Information about the demodulation @Config@.
--
-- 'spectrumConfig', 'antennaDownlinkDemodDecodeConfig_spectrumConfig' - Information about the spectral @Config@.
newAntennaDownlinkDemodDecodeConfig ::
  -- | 'decodeConfig'
  DecodeConfig ->
  -- | 'demodulationConfig'
  DemodulationConfig ->
  -- | 'spectrumConfig'
  SpectrumConfig ->
  AntennaDownlinkDemodDecodeConfig
newAntennaDownlinkDemodDecodeConfig
  pDecodeConfig_
  pDemodulationConfig_
  pSpectrumConfig_ =
    AntennaDownlinkDemodDecodeConfig'
      { decodeConfig =
          pDecodeConfig_,
        demodulationConfig = pDemodulationConfig_,
        spectrumConfig = pSpectrumConfig_
      }

-- | Information about the decode @Config@.
antennaDownlinkDemodDecodeConfig_decodeConfig :: Lens.Lens' AntennaDownlinkDemodDecodeConfig DecodeConfig
antennaDownlinkDemodDecodeConfig_decodeConfig = Lens.lens (\AntennaDownlinkDemodDecodeConfig' {decodeConfig} -> decodeConfig) (\s@AntennaDownlinkDemodDecodeConfig' {} a -> s {decodeConfig = a} :: AntennaDownlinkDemodDecodeConfig)

-- | Information about the demodulation @Config@.
antennaDownlinkDemodDecodeConfig_demodulationConfig :: Lens.Lens' AntennaDownlinkDemodDecodeConfig DemodulationConfig
antennaDownlinkDemodDecodeConfig_demodulationConfig = Lens.lens (\AntennaDownlinkDemodDecodeConfig' {demodulationConfig} -> demodulationConfig) (\s@AntennaDownlinkDemodDecodeConfig' {} a -> s {demodulationConfig = a} :: AntennaDownlinkDemodDecodeConfig)

-- | Information about the spectral @Config@.
antennaDownlinkDemodDecodeConfig_spectrumConfig :: Lens.Lens' AntennaDownlinkDemodDecodeConfig SpectrumConfig
antennaDownlinkDemodDecodeConfig_spectrumConfig = Lens.lens (\AntennaDownlinkDemodDecodeConfig' {spectrumConfig} -> spectrumConfig) (\s@AntennaDownlinkDemodDecodeConfig' {} a -> s {spectrumConfig = a} :: AntennaDownlinkDemodDecodeConfig)

instance
  Core.FromJSON
    AntennaDownlinkDemodDecodeConfig
  where
  parseJSON =
    Core.withObject
      "AntennaDownlinkDemodDecodeConfig"
      ( \x ->
          AntennaDownlinkDemodDecodeConfig'
            Prelude.<$> (x Core..: "decodeConfig")
            Prelude.<*> (x Core..: "demodulationConfig")
            Prelude.<*> (x Core..: "spectrumConfig")
      )

instance
  Prelude.Hashable
    AntennaDownlinkDemodDecodeConfig
  where
  hashWithSalt
    _salt
    AntennaDownlinkDemodDecodeConfig' {..} =
      _salt `Prelude.hashWithSalt` decodeConfig
        `Prelude.hashWithSalt` demodulationConfig
        `Prelude.hashWithSalt` spectrumConfig

instance
  Prelude.NFData
    AntennaDownlinkDemodDecodeConfig
  where
  rnf AntennaDownlinkDemodDecodeConfig' {..} =
    Prelude.rnf decodeConfig
      `Prelude.seq` Prelude.rnf demodulationConfig
      `Prelude.seq` Prelude.rnf spectrumConfig

instance Core.ToJSON AntennaDownlinkDemodDecodeConfig where
  toJSON AntennaDownlinkDemodDecodeConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("decodeConfig" Core..= decodeConfig),
            Prelude.Just
              ("demodulationConfig" Core..= demodulationConfig),
            Prelude.Just
              ("spectrumConfig" Core..= spectrumConfig)
          ]
      )
