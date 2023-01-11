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
-- Module      : Amazonka.GroundStation.Types.UplinkSpectrumConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.UplinkSpectrumConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.Frequency
import Amazonka.GroundStation.Types.Polarization
import qualified Amazonka.Prelude as Prelude

-- | Information about the uplink spectral @Config@.
--
-- /See:/ 'newUplinkSpectrumConfig' smart constructor.
data UplinkSpectrumConfig = UplinkSpectrumConfig'
  { -- | Polarization of an uplink spectral @Config@. Capturing both
    -- @\"RIGHT_HAND\"@ and @\"LEFT_HAND\"@ polarization requires two separate
    -- configs.
    polarization :: Prelude.Maybe Polarization,
    -- | Center frequency of an uplink spectral @Config@. Valid values are
    -- between 2025 to 2120 MHz.
    centerFrequency :: Frequency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UplinkSpectrumConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'polarization', 'uplinkSpectrumConfig_polarization' - Polarization of an uplink spectral @Config@. Capturing both
-- @\"RIGHT_HAND\"@ and @\"LEFT_HAND\"@ polarization requires two separate
-- configs.
--
-- 'centerFrequency', 'uplinkSpectrumConfig_centerFrequency' - Center frequency of an uplink spectral @Config@. Valid values are
-- between 2025 to 2120 MHz.
newUplinkSpectrumConfig ::
  -- | 'centerFrequency'
  Frequency ->
  UplinkSpectrumConfig
newUplinkSpectrumConfig pCenterFrequency_ =
  UplinkSpectrumConfig'
    { polarization =
        Prelude.Nothing,
      centerFrequency = pCenterFrequency_
    }

-- | Polarization of an uplink spectral @Config@. Capturing both
-- @\"RIGHT_HAND\"@ and @\"LEFT_HAND\"@ polarization requires two separate
-- configs.
uplinkSpectrumConfig_polarization :: Lens.Lens' UplinkSpectrumConfig (Prelude.Maybe Polarization)
uplinkSpectrumConfig_polarization = Lens.lens (\UplinkSpectrumConfig' {polarization} -> polarization) (\s@UplinkSpectrumConfig' {} a -> s {polarization = a} :: UplinkSpectrumConfig)

-- | Center frequency of an uplink spectral @Config@. Valid values are
-- between 2025 to 2120 MHz.
uplinkSpectrumConfig_centerFrequency :: Lens.Lens' UplinkSpectrumConfig Frequency
uplinkSpectrumConfig_centerFrequency = Lens.lens (\UplinkSpectrumConfig' {centerFrequency} -> centerFrequency) (\s@UplinkSpectrumConfig' {} a -> s {centerFrequency = a} :: UplinkSpectrumConfig)

instance Data.FromJSON UplinkSpectrumConfig where
  parseJSON =
    Data.withObject
      "UplinkSpectrumConfig"
      ( \x ->
          UplinkSpectrumConfig'
            Prelude.<$> (x Data..:? "polarization")
            Prelude.<*> (x Data..: "centerFrequency")
      )

instance Prelude.Hashable UplinkSpectrumConfig where
  hashWithSalt _salt UplinkSpectrumConfig' {..} =
    _salt `Prelude.hashWithSalt` polarization
      `Prelude.hashWithSalt` centerFrequency

instance Prelude.NFData UplinkSpectrumConfig where
  rnf UplinkSpectrumConfig' {..} =
    Prelude.rnf polarization
      `Prelude.seq` Prelude.rnf centerFrequency

instance Data.ToJSON UplinkSpectrumConfig where
  toJSON UplinkSpectrumConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("polarization" Data..=) Prelude.<$> polarization,
            Prelude.Just
              ("centerFrequency" Data..= centerFrequency)
          ]
      )
