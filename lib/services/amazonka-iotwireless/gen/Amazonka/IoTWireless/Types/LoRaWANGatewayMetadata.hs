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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANGatewayMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANGatewayMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | LoRaWAN gateway metatdata.
--
-- /See:/ 'newLoRaWANGatewayMetadata' smart constructor.
data LoRaWANGatewayMetadata = LoRaWANGatewayMetadata'
  { -- | The gateway\'s EUI value.
    gatewayEui :: Prelude.Maybe Prelude.Text,
    -- | The RSSI value.
    rssi :: Prelude.Maybe Prelude.Double,
    -- | The SNR value.
    snr :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANGatewayMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayEui', 'loRaWANGatewayMetadata_gatewayEui' - The gateway\'s EUI value.
--
-- 'rssi', 'loRaWANGatewayMetadata_rssi' - The RSSI value.
--
-- 'snr', 'loRaWANGatewayMetadata_snr' - The SNR value.
newLoRaWANGatewayMetadata ::
  LoRaWANGatewayMetadata
newLoRaWANGatewayMetadata =
  LoRaWANGatewayMetadata'
    { gatewayEui =
        Prelude.Nothing,
      rssi = Prelude.Nothing,
      snr = Prelude.Nothing
    }

-- | The gateway\'s EUI value.
loRaWANGatewayMetadata_gatewayEui :: Lens.Lens' LoRaWANGatewayMetadata (Prelude.Maybe Prelude.Text)
loRaWANGatewayMetadata_gatewayEui = Lens.lens (\LoRaWANGatewayMetadata' {gatewayEui} -> gatewayEui) (\s@LoRaWANGatewayMetadata' {} a -> s {gatewayEui = a} :: LoRaWANGatewayMetadata)

-- | The RSSI value.
loRaWANGatewayMetadata_rssi :: Lens.Lens' LoRaWANGatewayMetadata (Prelude.Maybe Prelude.Double)
loRaWANGatewayMetadata_rssi = Lens.lens (\LoRaWANGatewayMetadata' {rssi} -> rssi) (\s@LoRaWANGatewayMetadata' {} a -> s {rssi = a} :: LoRaWANGatewayMetadata)

-- | The SNR value.
loRaWANGatewayMetadata_snr :: Lens.Lens' LoRaWANGatewayMetadata (Prelude.Maybe Prelude.Double)
loRaWANGatewayMetadata_snr = Lens.lens (\LoRaWANGatewayMetadata' {snr} -> snr) (\s@LoRaWANGatewayMetadata' {} a -> s {snr = a} :: LoRaWANGatewayMetadata)

instance Data.FromJSON LoRaWANGatewayMetadata where
  parseJSON =
    Data.withObject
      "LoRaWANGatewayMetadata"
      ( \x ->
          LoRaWANGatewayMetadata'
            Prelude.<$> (x Data..:? "GatewayEui")
            Prelude.<*> (x Data..:? "Rssi")
            Prelude.<*> (x Data..:? "Snr")
      )

instance Prelude.Hashable LoRaWANGatewayMetadata where
  hashWithSalt _salt LoRaWANGatewayMetadata' {..} =
    _salt `Prelude.hashWithSalt` gatewayEui
      `Prelude.hashWithSalt` rssi
      `Prelude.hashWithSalt` snr

instance Prelude.NFData LoRaWANGatewayMetadata where
  rnf LoRaWANGatewayMetadata' {..} =
    Prelude.rnf gatewayEui
      `Prelude.seq` Prelude.rnf rssi
      `Prelude.seq` Prelude.rnf snr
