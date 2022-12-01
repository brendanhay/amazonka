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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANJoinResourceTypeEventConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANJoinResourceTypeEventConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.EventNotificationTopicStatus
import qualified Amazonka.Prelude as Prelude

-- | Object for LoRaWAN join resource type event configuration.
--
-- /See:/ 'newLoRaWANJoinResourceTypeEventConfiguration' smart constructor.
data LoRaWANJoinResourceTypeEventConfiguration = LoRaWANJoinResourceTypeEventConfiguration'
  { -- | Denotes whether the wireless device join event topic is enabled or
    -- disabled.
    wirelessDeviceEventTopic :: Prelude.Maybe EventNotificationTopicStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANJoinResourceTypeEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'wirelessDeviceEventTopic', 'loRaWANJoinResourceTypeEventConfiguration_wirelessDeviceEventTopic' - Denotes whether the wireless device join event topic is enabled or
-- disabled.
newLoRaWANJoinResourceTypeEventConfiguration ::
  LoRaWANJoinResourceTypeEventConfiguration
newLoRaWANJoinResourceTypeEventConfiguration =
  LoRaWANJoinResourceTypeEventConfiguration'
    { wirelessDeviceEventTopic =
        Prelude.Nothing
    }

-- | Denotes whether the wireless device join event topic is enabled or
-- disabled.
loRaWANJoinResourceTypeEventConfiguration_wirelessDeviceEventTopic :: Lens.Lens' LoRaWANJoinResourceTypeEventConfiguration (Prelude.Maybe EventNotificationTopicStatus)
loRaWANJoinResourceTypeEventConfiguration_wirelessDeviceEventTopic = Lens.lens (\LoRaWANJoinResourceTypeEventConfiguration' {wirelessDeviceEventTopic} -> wirelessDeviceEventTopic) (\s@LoRaWANJoinResourceTypeEventConfiguration' {} a -> s {wirelessDeviceEventTopic = a} :: LoRaWANJoinResourceTypeEventConfiguration)

instance
  Core.FromJSON
    LoRaWANJoinResourceTypeEventConfiguration
  where
  parseJSON =
    Core.withObject
      "LoRaWANJoinResourceTypeEventConfiguration"
      ( \x ->
          LoRaWANJoinResourceTypeEventConfiguration'
            Prelude.<$> (x Core..:? "WirelessDeviceEventTopic")
      )

instance
  Prelude.Hashable
    LoRaWANJoinResourceTypeEventConfiguration
  where
  hashWithSalt
    _salt
    LoRaWANJoinResourceTypeEventConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` wirelessDeviceEventTopic

instance
  Prelude.NFData
    LoRaWANJoinResourceTypeEventConfiguration
  where
  rnf LoRaWANJoinResourceTypeEventConfiguration' {..} =
    Prelude.rnf wirelessDeviceEventTopic

instance
  Core.ToJSON
    LoRaWANJoinResourceTypeEventConfiguration
  where
  toJSON LoRaWANJoinResourceTypeEventConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("WirelessDeviceEventTopic" Core..=)
              Prelude.<$> wirelessDeviceEventTopic
          ]
      )
