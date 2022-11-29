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
-- Module      : Amazonka.IoTWireless.Types.EventNotificationItemConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.EventNotificationItemConfigurations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.ConnectionStatusEventConfiguration
import Amazonka.IoTWireless.Types.DeviceRegistrationStateEventConfiguration
import Amazonka.IoTWireless.Types.JoinEventConfiguration
import Amazonka.IoTWireless.Types.MessageDeliveryStatusEventConfiguration
import Amazonka.IoTWireless.Types.ProximityEventConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Object of all event configurations and the status of the event topics.
--
-- /See:/ 'newEventNotificationItemConfigurations' smart constructor.
data EventNotificationItemConfigurations = EventNotificationItemConfigurations'
  { -- | Device registration state event configuration for an event configuration
    -- item.
    deviceRegistrationState :: Prelude.Maybe DeviceRegistrationStateEventConfiguration,
    -- | Connection status event configuration for an event configuration item.
    connectionStatus :: Prelude.Maybe ConnectionStatusEventConfiguration,
    -- | Message delivery status event configuration for an event configuration
    -- item.
    messageDeliveryStatus :: Prelude.Maybe MessageDeliveryStatusEventConfiguration,
    -- | Join event configuration for an event configuration item.
    join :: Prelude.Maybe JoinEventConfiguration,
    -- | Proximity event configuration for an event configuration item.
    proximity :: Prelude.Maybe ProximityEventConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventNotificationItemConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceRegistrationState', 'eventNotificationItemConfigurations_deviceRegistrationState' - Device registration state event configuration for an event configuration
-- item.
--
-- 'connectionStatus', 'eventNotificationItemConfigurations_connectionStatus' - Connection status event configuration for an event configuration item.
--
-- 'messageDeliveryStatus', 'eventNotificationItemConfigurations_messageDeliveryStatus' - Message delivery status event configuration for an event configuration
-- item.
--
-- 'join', 'eventNotificationItemConfigurations_join' - Join event configuration for an event configuration item.
--
-- 'proximity', 'eventNotificationItemConfigurations_proximity' - Proximity event configuration for an event configuration item.
newEventNotificationItemConfigurations ::
  EventNotificationItemConfigurations
newEventNotificationItemConfigurations =
  EventNotificationItemConfigurations'
    { deviceRegistrationState =
        Prelude.Nothing,
      connectionStatus = Prelude.Nothing,
      messageDeliveryStatus =
        Prelude.Nothing,
      join = Prelude.Nothing,
      proximity = Prelude.Nothing
    }

-- | Device registration state event configuration for an event configuration
-- item.
eventNotificationItemConfigurations_deviceRegistrationState :: Lens.Lens' EventNotificationItemConfigurations (Prelude.Maybe DeviceRegistrationStateEventConfiguration)
eventNotificationItemConfigurations_deviceRegistrationState = Lens.lens (\EventNotificationItemConfigurations' {deviceRegistrationState} -> deviceRegistrationState) (\s@EventNotificationItemConfigurations' {} a -> s {deviceRegistrationState = a} :: EventNotificationItemConfigurations)

-- | Connection status event configuration for an event configuration item.
eventNotificationItemConfigurations_connectionStatus :: Lens.Lens' EventNotificationItemConfigurations (Prelude.Maybe ConnectionStatusEventConfiguration)
eventNotificationItemConfigurations_connectionStatus = Lens.lens (\EventNotificationItemConfigurations' {connectionStatus} -> connectionStatus) (\s@EventNotificationItemConfigurations' {} a -> s {connectionStatus = a} :: EventNotificationItemConfigurations)

-- | Message delivery status event configuration for an event configuration
-- item.
eventNotificationItemConfigurations_messageDeliveryStatus :: Lens.Lens' EventNotificationItemConfigurations (Prelude.Maybe MessageDeliveryStatusEventConfiguration)
eventNotificationItemConfigurations_messageDeliveryStatus = Lens.lens (\EventNotificationItemConfigurations' {messageDeliveryStatus} -> messageDeliveryStatus) (\s@EventNotificationItemConfigurations' {} a -> s {messageDeliveryStatus = a} :: EventNotificationItemConfigurations)

-- | Join event configuration for an event configuration item.
eventNotificationItemConfigurations_join :: Lens.Lens' EventNotificationItemConfigurations (Prelude.Maybe JoinEventConfiguration)
eventNotificationItemConfigurations_join = Lens.lens (\EventNotificationItemConfigurations' {join} -> join) (\s@EventNotificationItemConfigurations' {} a -> s {join = a} :: EventNotificationItemConfigurations)

-- | Proximity event configuration for an event configuration item.
eventNotificationItemConfigurations_proximity :: Lens.Lens' EventNotificationItemConfigurations (Prelude.Maybe ProximityEventConfiguration)
eventNotificationItemConfigurations_proximity = Lens.lens (\EventNotificationItemConfigurations' {proximity} -> proximity) (\s@EventNotificationItemConfigurations' {} a -> s {proximity = a} :: EventNotificationItemConfigurations)

instance
  Core.FromJSON
    EventNotificationItemConfigurations
  where
  parseJSON =
    Core.withObject
      "EventNotificationItemConfigurations"
      ( \x ->
          EventNotificationItemConfigurations'
            Prelude.<$> (x Core..:? "DeviceRegistrationState")
            Prelude.<*> (x Core..:? "ConnectionStatus")
            Prelude.<*> (x Core..:? "MessageDeliveryStatus")
            Prelude.<*> (x Core..:? "Join")
            Prelude.<*> (x Core..:? "Proximity")
      )

instance
  Prelude.Hashable
    EventNotificationItemConfigurations
  where
  hashWithSalt
    _salt
    EventNotificationItemConfigurations' {..} =
      _salt
        `Prelude.hashWithSalt` deviceRegistrationState
        `Prelude.hashWithSalt` connectionStatus
        `Prelude.hashWithSalt` messageDeliveryStatus
        `Prelude.hashWithSalt` join
        `Prelude.hashWithSalt` proximity

instance
  Prelude.NFData
    EventNotificationItemConfigurations
  where
  rnf EventNotificationItemConfigurations' {..} =
    Prelude.rnf deviceRegistrationState
      `Prelude.seq` Prelude.rnf connectionStatus
      `Prelude.seq` Prelude.rnf messageDeliveryStatus
      `Prelude.seq` Prelude.rnf join
      `Prelude.seq` Prelude.rnf proximity
