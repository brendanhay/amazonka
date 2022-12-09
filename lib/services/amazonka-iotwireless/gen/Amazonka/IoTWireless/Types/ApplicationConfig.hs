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
-- Module      : Amazonka.IoTWireless.Types.ApplicationConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.ApplicationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.ApplicationConfigType
import qualified Amazonka.Prelude as Prelude

-- | LoRaWAN application configuration, which can be used to perform
-- geolocation.
--
-- /See:/ 'newApplicationConfig' smart constructor.
data ApplicationConfig = ApplicationConfig'
  { -- | The name of the position data destination that describes the AWS IoT
    -- rule that processes the device\'s position data for use by AWS IoT Core
    -- for LoRaWAN.
    destinationName :: Prelude.Maybe Prelude.Text,
    fPort :: Prelude.Maybe Prelude.Natural,
    -- | Application type, which can be specified to obtain real-time position
    -- information of your LoRaWAN device.
    type' :: Prelude.Maybe ApplicationConfigType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationName', 'applicationConfig_destinationName' - The name of the position data destination that describes the AWS IoT
-- rule that processes the device\'s position data for use by AWS IoT Core
-- for LoRaWAN.
--
-- 'fPort', 'applicationConfig_fPort' - Undocumented member.
--
-- 'type'', 'applicationConfig_type' - Application type, which can be specified to obtain real-time position
-- information of your LoRaWAN device.
newApplicationConfig ::
  ApplicationConfig
newApplicationConfig =
  ApplicationConfig'
    { destinationName =
        Prelude.Nothing,
      fPort = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the position data destination that describes the AWS IoT
-- rule that processes the device\'s position data for use by AWS IoT Core
-- for LoRaWAN.
applicationConfig_destinationName :: Lens.Lens' ApplicationConfig (Prelude.Maybe Prelude.Text)
applicationConfig_destinationName = Lens.lens (\ApplicationConfig' {destinationName} -> destinationName) (\s@ApplicationConfig' {} a -> s {destinationName = a} :: ApplicationConfig)

-- | Undocumented member.
applicationConfig_fPort :: Lens.Lens' ApplicationConfig (Prelude.Maybe Prelude.Natural)
applicationConfig_fPort = Lens.lens (\ApplicationConfig' {fPort} -> fPort) (\s@ApplicationConfig' {} a -> s {fPort = a} :: ApplicationConfig)

-- | Application type, which can be specified to obtain real-time position
-- information of your LoRaWAN device.
applicationConfig_type :: Lens.Lens' ApplicationConfig (Prelude.Maybe ApplicationConfigType)
applicationConfig_type = Lens.lens (\ApplicationConfig' {type'} -> type') (\s@ApplicationConfig' {} a -> s {type' = a} :: ApplicationConfig)

instance Data.FromJSON ApplicationConfig where
  parseJSON =
    Data.withObject
      "ApplicationConfig"
      ( \x ->
          ApplicationConfig'
            Prelude.<$> (x Data..:? "DestinationName")
            Prelude.<*> (x Data..:? "FPort")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable ApplicationConfig where
  hashWithSalt _salt ApplicationConfig' {..} =
    _salt `Prelude.hashWithSalt` destinationName
      `Prelude.hashWithSalt` fPort
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ApplicationConfig where
  rnf ApplicationConfig' {..} =
    Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf fPort
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON ApplicationConfig where
  toJSON ApplicationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationName" Data..=)
              Prelude.<$> destinationName,
            ("FPort" Data..=) Prelude.<$> fPort,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
