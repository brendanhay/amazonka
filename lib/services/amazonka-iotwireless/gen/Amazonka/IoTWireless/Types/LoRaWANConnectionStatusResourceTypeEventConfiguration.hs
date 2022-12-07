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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANConnectionStatusResourceTypeEventConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANConnectionStatusResourceTypeEventConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.EventNotificationTopicStatus
import qualified Amazonka.Prelude as Prelude

-- | Object for LoRaWAN connection status resource type event configuration.
--
-- /See:/ 'newLoRaWANConnectionStatusResourceTypeEventConfiguration' smart constructor.
data LoRaWANConnectionStatusResourceTypeEventConfiguration = LoRaWANConnectionStatusResourceTypeEventConfiguration'
  { -- | Denotes whether the wireless gateway connection status event topic is
    -- enabled or disabled.
    wirelessGatewayEventTopic :: Prelude.Maybe EventNotificationTopicStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANConnectionStatusResourceTypeEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'wirelessGatewayEventTopic', 'loRaWANConnectionStatusResourceTypeEventConfiguration_wirelessGatewayEventTopic' - Denotes whether the wireless gateway connection status event topic is
-- enabled or disabled.
newLoRaWANConnectionStatusResourceTypeEventConfiguration ::
  LoRaWANConnectionStatusResourceTypeEventConfiguration
newLoRaWANConnectionStatusResourceTypeEventConfiguration =
  LoRaWANConnectionStatusResourceTypeEventConfiguration'
    { wirelessGatewayEventTopic =
        Prelude.Nothing
    }

-- | Denotes whether the wireless gateway connection status event topic is
-- enabled or disabled.
loRaWANConnectionStatusResourceTypeEventConfiguration_wirelessGatewayEventTopic :: Lens.Lens' LoRaWANConnectionStatusResourceTypeEventConfiguration (Prelude.Maybe EventNotificationTopicStatus)
loRaWANConnectionStatusResourceTypeEventConfiguration_wirelessGatewayEventTopic = Lens.lens (\LoRaWANConnectionStatusResourceTypeEventConfiguration' {wirelessGatewayEventTopic} -> wirelessGatewayEventTopic) (\s@LoRaWANConnectionStatusResourceTypeEventConfiguration' {} a -> s {wirelessGatewayEventTopic = a} :: LoRaWANConnectionStatusResourceTypeEventConfiguration)

instance
  Data.FromJSON
    LoRaWANConnectionStatusResourceTypeEventConfiguration
  where
  parseJSON =
    Data.withObject
      "LoRaWANConnectionStatusResourceTypeEventConfiguration"
      ( \x ->
          LoRaWANConnectionStatusResourceTypeEventConfiguration'
            Prelude.<$> (x Data..:? "WirelessGatewayEventTopic")
      )

instance
  Prelude.Hashable
    LoRaWANConnectionStatusResourceTypeEventConfiguration
  where
  hashWithSalt
    _salt
    LoRaWANConnectionStatusResourceTypeEventConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` wirelessGatewayEventTopic

instance
  Prelude.NFData
    LoRaWANConnectionStatusResourceTypeEventConfiguration
  where
  rnf
    LoRaWANConnectionStatusResourceTypeEventConfiguration' {..} =
      Prelude.rnf wirelessGatewayEventTopic

instance
  Data.ToJSON
    LoRaWANConnectionStatusResourceTypeEventConfiguration
  where
  toJSON
    LoRaWANConnectionStatusResourceTypeEventConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("WirelessGatewayEventTopic" Data..=)
                Prelude.<$> wirelessGatewayEventTopic
            ]
        )
