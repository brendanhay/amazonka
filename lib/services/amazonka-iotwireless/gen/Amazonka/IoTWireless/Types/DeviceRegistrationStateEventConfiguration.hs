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
-- Module      : Amazonka.IoTWireless.Types.DeviceRegistrationStateEventConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.DeviceRegistrationStateEventConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.EventNotificationTopicStatus
import Amazonka.IoTWireless.Types.SidewalkEventNotificationConfigurations
import qualified Amazonka.Prelude as Prelude

-- | Device registration state event configuration object for enabling and
-- disabling relevant topics.
--
-- /See:/ 'newDeviceRegistrationStateEventConfiguration' smart constructor.
data DeviceRegistrationStateEventConfiguration = DeviceRegistrationStateEventConfiguration'
  { -- | Device registration state event configuration object for enabling or
    -- disabling Sidewalk related event topics.
    sidewalk :: Prelude.Maybe SidewalkEventNotificationConfigurations,
    -- | Denotes whether the wireless device ID device registration state event
    -- topic is enabled or disabled.
    wirelessDeviceIdEventTopic :: Prelude.Maybe EventNotificationTopicStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceRegistrationStateEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sidewalk', 'deviceRegistrationStateEventConfiguration_sidewalk' - Device registration state event configuration object for enabling or
-- disabling Sidewalk related event topics.
--
-- 'wirelessDeviceIdEventTopic', 'deviceRegistrationStateEventConfiguration_wirelessDeviceIdEventTopic' - Denotes whether the wireless device ID device registration state event
-- topic is enabled or disabled.
newDeviceRegistrationStateEventConfiguration ::
  DeviceRegistrationStateEventConfiguration
newDeviceRegistrationStateEventConfiguration =
  DeviceRegistrationStateEventConfiguration'
    { sidewalk =
        Prelude.Nothing,
      wirelessDeviceIdEventTopic =
        Prelude.Nothing
    }

-- | Device registration state event configuration object for enabling or
-- disabling Sidewalk related event topics.
deviceRegistrationStateEventConfiguration_sidewalk :: Lens.Lens' DeviceRegistrationStateEventConfiguration (Prelude.Maybe SidewalkEventNotificationConfigurations)
deviceRegistrationStateEventConfiguration_sidewalk = Lens.lens (\DeviceRegistrationStateEventConfiguration' {sidewalk} -> sidewalk) (\s@DeviceRegistrationStateEventConfiguration' {} a -> s {sidewalk = a} :: DeviceRegistrationStateEventConfiguration)

-- | Denotes whether the wireless device ID device registration state event
-- topic is enabled or disabled.
deviceRegistrationStateEventConfiguration_wirelessDeviceIdEventTopic :: Lens.Lens' DeviceRegistrationStateEventConfiguration (Prelude.Maybe EventNotificationTopicStatus)
deviceRegistrationStateEventConfiguration_wirelessDeviceIdEventTopic = Lens.lens (\DeviceRegistrationStateEventConfiguration' {wirelessDeviceIdEventTopic} -> wirelessDeviceIdEventTopic) (\s@DeviceRegistrationStateEventConfiguration' {} a -> s {wirelessDeviceIdEventTopic = a} :: DeviceRegistrationStateEventConfiguration)

instance
  Data.FromJSON
    DeviceRegistrationStateEventConfiguration
  where
  parseJSON =
    Data.withObject
      "DeviceRegistrationStateEventConfiguration"
      ( \x ->
          DeviceRegistrationStateEventConfiguration'
            Prelude.<$> (x Data..:? "Sidewalk")
              Prelude.<*> (x Data..:? "WirelessDeviceIdEventTopic")
      )

instance
  Prelude.Hashable
    DeviceRegistrationStateEventConfiguration
  where
  hashWithSalt
    _salt
    DeviceRegistrationStateEventConfiguration' {..} =
      _salt `Prelude.hashWithSalt` sidewalk
        `Prelude.hashWithSalt` wirelessDeviceIdEventTopic

instance
  Prelude.NFData
    DeviceRegistrationStateEventConfiguration
  where
  rnf DeviceRegistrationStateEventConfiguration' {..} =
    Prelude.rnf sidewalk
      `Prelude.seq` Prelude.rnf wirelessDeviceIdEventTopic

instance
  Data.ToJSON
    DeviceRegistrationStateEventConfiguration
  where
  toJSON DeviceRegistrationStateEventConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Sidewalk" Data..=) Prelude.<$> sidewalk,
            ("WirelessDeviceIdEventTopic" Data..=)
              Prelude.<$> wirelessDeviceIdEventTopic
          ]
      )
