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
-- Module      : Amazonka.GroundStation.Types.AntennaUplinkConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.AntennaUplinkConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.Eirp
import Amazonka.GroundStation.Types.UplinkSpectrumConfig
import qualified Amazonka.Prelude as Prelude

-- | Information about the uplink @Config@ of an antenna.
--
-- /See:/ 'newAntennaUplinkConfig' smart constructor.
data AntennaUplinkConfig = AntennaUplinkConfig'
  { -- | Whether or not uplink transmit is disabled.
    transmitDisabled :: Prelude.Maybe Prelude.Bool,
    -- | Information about the uplink spectral @Config@.
    spectrumConfig :: UplinkSpectrumConfig,
    -- | EIRP of the target.
    targetEirp :: Eirp
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AntennaUplinkConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transmitDisabled', 'antennaUplinkConfig_transmitDisabled' - Whether or not uplink transmit is disabled.
--
-- 'spectrumConfig', 'antennaUplinkConfig_spectrumConfig' - Information about the uplink spectral @Config@.
--
-- 'targetEirp', 'antennaUplinkConfig_targetEirp' - EIRP of the target.
newAntennaUplinkConfig ::
  -- | 'spectrumConfig'
  UplinkSpectrumConfig ->
  -- | 'targetEirp'
  Eirp ->
  AntennaUplinkConfig
newAntennaUplinkConfig pSpectrumConfig_ pTargetEirp_ =
  AntennaUplinkConfig'
    { transmitDisabled =
        Prelude.Nothing,
      spectrumConfig = pSpectrumConfig_,
      targetEirp = pTargetEirp_
    }

-- | Whether or not uplink transmit is disabled.
antennaUplinkConfig_transmitDisabled :: Lens.Lens' AntennaUplinkConfig (Prelude.Maybe Prelude.Bool)
antennaUplinkConfig_transmitDisabled = Lens.lens (\AntennaUplinkConfig' {transmitDisabled} -> transmitDisabled) (\s@AntennaUplinkConfig' {} a -> s {transmitDisabled = a} :: AntennaUplinkConfig)

-- | Information about the uplink spectral @Config@.
antennaUplinkConfig_spectrumConfig :: Lens.Lens' AntennaUplinkConfig UplinkSpectrumConfig
antennaUplinkConfig_spectrumConfig = Lens.lens (\AntennaUplinkConfig' {spectrumConfig} -> spectrumConfig) (\s@AntennaUplinkConfig' {} a -> s {spectrumConfig = a} :: AntennaUplinkConfig)

-- | EIRP of the target.
antennaUplinkConfig_targetEirp :: Lens.Lens' AntennaUplinkConfig Eirp
antennaUplinkConfig_targetEirp = Lens.lens (\AntennaUplinkConfig' {targetEirp} -> targetEirp) (\s@AntennaUplinkConfig' {} a -> s {targetEirp = a} :: AntennaUplinkConfig)

instance Data.FromJSON AntennaUplinkConfig where
  parseJSON =
    Data.withObject
      "AntennaUplinkConfig"
      ( \x ->
          AntennaUplinkConfig'
            Prelude.<$> (x Data..:? "transmitDisabled")
            Prelude.<*> (x Data..: "spectrumConfig")
            Prelude.<*> (x Data..: "targetEirp")
      )

instance Prelude.Hashable AntennaUplinkConfig where
  hashWithSalt _salt AntennaUplinkConfig' {..} =
    _salt `Prelude.hashWithSalt` transmitDisabled
      `Prelude.hashWithSalt` spectrumConfig
      `Prelude.hashWithSalt` targetEirp

instance Prelude.NFData AntennaUplinkConfig where
  rnf AntennaUplinkConfig' {..} =
    Prelude.rnf transmitDisabled
      `Prelude.seq` Prelude.rnf spectrumConfig
      `Prelude.seq` Prelude.rnf targetEirp

instance Data.ToJSON AntennaUplinkConfig where
  toJSON AntennaUplinkConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("transmitDisabled" Data..=)
              Prelude.<$> transmitDisabled,
            Prelude.Just
              ("spectrumConfig" Data..= spectrumConfig),
            Prelude.Just ("targetEirp" Data..= targetEirp)
          ]
      )
