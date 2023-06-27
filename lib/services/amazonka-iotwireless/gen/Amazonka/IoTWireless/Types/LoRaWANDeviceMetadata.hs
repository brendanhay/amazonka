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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANDeviceMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANDeviceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.LoRaWANGatewayMetadata
import qualified Amazonka.Prelude as Prelude

-- | LoRaWAN device metatdata.
--
-- /See:/ 'newLoRaWANDeviceMetadata' smart constructor.
data LoRaWANDeviceMetadata = LoRaWANDeviceMetadata'
  { -- | The DataRate value.
    dataRate :: Prelude.Maybe Prelude.Int,
    -- | The DevEUI value.
    devEui :: Prelude.Maybe Prelude.Text,
    -- | The FPort value.
    fPort :: Prelude.Maybe Prelude.Int,
    -- | The device\'s channel frequency in Hz.
    frequency :: Prelude.Maybe Prelude.Int,
    -- | Information about the gateways accessed by the device.
    gateways :: Prelude.Maybe [LoRaWANGatewayMetadata],
    -- | The date and time of the metadata.
    timestamp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANDeviceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataRate', 'loRaWANDeviceMetadata_dataRate' - The DataRate value.
--
-- 'devEui', 'loRaWANDeviceMetadata_devEui' - The DevEUI value.
--
-- 'fPort', 'loRaWANDeviceMetadata_fPort' - The FPort value.
--
-- 'frequency', 'loRaWANDeviceMetadata_frequency' - The device\'s channel frequency in Hz.
--
-- 'gateways', 'loRaWANDeviceMetadata_gateways' - Information about the gateways accessed by the device.
--
-- 'timestamp', 'loRaWANDeviceMetadata_timestamp' - The date and time of the metadata.
newLoRaWANDeviceMetadata ::
  LoRaWANDeviceMetadata
newLoRaWANDeviceMetadata =
  LoRaWANDeviceMetadata'
    { dataRate = Prelude.Nothing,
      devEui = Prelude.Nothing,
      fPort = Prelude.Nothing,
      frequency = Prelude.Nothing,
      gateways = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The DataRate value.
loRaWANDeviceMetadata_dataRate :: Lens.Lens' LoRaWANDeviceMetadata (Prelude.Maybe Prelude.Int)
loRaWANDeviceMetadata_dataRate = Lens.lens (\LoRaWANDeviceMetadata' {dataRate} -> dataRate) (\s@LoRaWANDeviceMetadata' {} a -> s {dataRate = a} :: LoRaWANDeviceMetadata)

-- | The DevEUI value.
loRaWANDeviceMetadata_devEui :: Lens.Lens' LoRaWANDeviceMetadata (Prelude.Maybe Prelude.Text)
loRaWANDeviceMetadata_devEui = Lens.lens (\LoRaWANDeviceMetadata' {devEui} -> devEui) (\s@LoRaWANDeviceMetadata' {} a -> s {devEui = a} :: LoRaWANDeviceMetadata)

-- | The FPort value.
loRaWANDeviceMetadata_fPort :: Lens.Lens' LoRaWANDeviceMetadata (Prelude.Maybe Prelude.Int)
loRaWANDeviceMetadata_fPort = Lens.lens (\LoRaWANDeviceMetadata' {fPort} -> fPort) (\s@LoRaWANDeviceMetadata' {} a -> s {fPort = a} :: LoRaWANDeviceMetadata)

-- | The device\'s channel frequency in Hz.
loRaWANDeviceMetadata_frequency :: Lens.Lens' LoRaWANDeviceMetadata (Prelude.Maybe Prelude.Int)
loRaWANDeviceMetadata_frequency = Lens.lens (\LoRaWANDeviceMetadata' {frequency} -> frequency) (\s@LoRaWANDeviceMetadata' {} a -> s {frequency = a} :: LoRaWANDeviceMetadata)

-- | Information about the gateways accessed by the device.
loRaWANDeviceMetadata_gateways :: Lens.Lens' LoRaWANDeviceMetadata (Prelude.Maybe [LoRaWANGatewayMetadata])
loRaWANDeviceMetadata_gateways = Lens.lens (\LoRaWANDeviceMetadata' {gateways} -> gateways) (\s@LoRaWANDeviceMetadata' {} a -> s {gateways = a} :: LoRaWANDeviceMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The date and time of the metadata.
loRaWANDeviceMetadata_timestamp :: Lens.Lens' LoRaWANDeviceMetadata (Prelude.Maybe Prelude.Text)
loRaWANDeviceMetadata_timestamp = Lens.lens (\LoRaWANDeviceMetadata' {timestamp} -> timestamp) (\s@LoRaWANDeviceMetadata' {} a -> s {timestamp = a} :: LoRaWANDeviceMetadata)

instance Data.FromJSON LoRaWANDeviceMetadata where
  parseJSON =
    Data.withObject
      "LoRaWANDeviceMetadata"
      ( \x ->
          LoRaWANDeviceMetadata'
            Prelude.<$> (x Data..:? "DataRate")
            Prelude.<*> (x Data..:? "DevEui")
            Prelude.<*> (x Data..:? "FPort")
            Prelude.<*> (x Data..:? "Frequency")
            Prelude.<*> (x Data..:? "Gateways" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable LoRaWANDeviceMetadata where
  hashWithSalt _salt LoRaWANDeviceMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` dataRate
      `Prelude.hashWithSalt` devEui
      `Prelude.hashWithSalt` fPort
      `Prelude.hashWithSalt` frequency
      `Prelude.hashWithSalt` gateways
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData LoRaWANDeviceMetadata where
  rnf LoRaWANDeviceMetadata' {..} =
    Prelude.rnf dataRate
      `Prelude.seq` Prelude.rnf devEui
      `Prelude.seq` Prelude.rnf fPort
      `Prelude.seq` Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf gateways
      `Prelude.seq` Prelude.rnf timestamp
