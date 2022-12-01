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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.DeviceRegistrationStateEventConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.EventNotificationTopicStatus
import Amazonka.IoTWireless.Types.SidewalkEventNotificationConfigurations
import qualified Amazonka.Prelude as Prelude

-- | Device registration state event configuration object for enabling and
-- disabling relevant topics.
--
-- /See:/ 'newDeviceRegistrationStateEventConfiguration' smart constructor.
data DeviceRegistrationStateEventConfiguration = DeviceRegistrationStateEventConfiguration'
  { -- | Denotes whether the wireless device ID device registration state event
    -- topic is enabled or disabled.
    wirelessDeviceIdEventTopic :: Prelude.Maybe EventNotificationTopicStatus,
    -- | Device registration state event configuration object for enabling or
    -- disabling Sidewalk related event topics.
    sidewalk :: Prelude.Maybe SidewalkEventNotificationConfigurations
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
-- 'wirelessDeviceIdEventTopic', 'deviceRegistrationStateEventConfiguration_wirelessDeviceIdEventTopic' - Denotes whether the wireless device ID device registration state event
-- topic is enabled or disabled.
--
-- 'sidewalk', 'deviceRegistrationStateEventConfiguration_sidewalk' - Device registration state event configuration object for enabling or
-- disabling Sidewalk related event topics.
newDeviceRegistrationStateEventConfiguration ::
  DeviceRegistrationStateEventConfiguration
newDeviceRegistrationStateEventConfiguration =
  DeviceRegistrationStateEventConfiguration'
    { wirelessDeviceIdEventTopic =
        Prelude.Nothing,
      sidewalk = Prelude.Nothing
    }

-- | Denotes whether the wireless device ID device registration state event
-- topic is enabled or disabled.
deviceRegistrationStateEventConfiguration_wirelessDeviceIdEventTopic :: Lens.Lens' DeviceRegistrationStateEventConfiguration (Prelude.Maybe EventNotificationTopicStatus)
deviceRegistrationStateEventConfiguration_wirelessDeviceIdEventTopic = Lens.lens (\DeviceRegistrationStateEventConfiguration' {wirelessDeviceIdEventTopic} -> wirelessDeviceIdEventTopic) (\s@DeviceRegistrationStateEventConfiguration' {} a -> s {wirelessDeviceIdEventTopic = a} :: DeviceRegistrationStateEventConfiguration)

-- | Device registration state event configuration object for enabling or
-- disabling Sidewalk related event topics.
deviceRegistrationStateEventConfiguration_sidewalk :: Lens.Lens' DeviceRegistrationStateEventConfiguration (Prelude.Maybe SidewalkEventNotificationConfigurations)
deviceRegistrationStateEventConfiguration_sidewalk = Lens.lens (\DeviceRegistrationStateEventConfiguration' {sidewalk} -> sidewalk) (\s@DeviceRegistrationStateEventConfiguration' {} a -> s {sidewalk = a} :: DeviceRegistrationStateEventConfiguration)

instance
  Core.FromJSON
    DeviceRegistrationStateEventConfiguration
  where
  parseJSON =
    Core.withObject
      "DeviceRegistrationStateEventConfiguration"
      ( \x ->
          DeviceRegistrationStateEventConfiguration'
            Prelude.<$> (x Core..:? "WirelessDeviceIdEventTopic")
              Prelude.<*> (x Core..:? "Sidewalk")
      )

instance
  Prelude.Hashable
    DeviceRegistrationStateEventConfiguration
  where
  hashWithSalt
    _salt
    DeviceRegistrationStateEventConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` wirelessDeviceIdEventTopic
        `Prelude.hashWithSalt` sidewalk

instance
  Prelude.NFData
    DeviceRegistrationStateEventConfiguration
  where
  rnf DeviceRegistrationStateEventConfiguration' {..} =
    Prelude.rnf wirelessDeviceIdEventTopic
      `Prelude.seq` Prelude.rnf sidewalk

instance
  Core.ToJSON
    DeviceRegistrationStateEventConfiguration
  where
  toJSON DeviceRegistrationStateEventConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("WirelessDeviceIdEventTopic" Core..=)
              Prelude.<$> wirelessDeviceIdEventTopic,
            ("Sidewalk" Core..=) Prelude.<$> sidewalk
          ]
      )
