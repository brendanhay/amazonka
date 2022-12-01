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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANConnectionStatusEventNotificationConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANConnectionStatusEventNotificationConfigurations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.EventNotificationTopicStatus
import qualified Amazonka.Prelude as Prelude

-- | Object for LoRaWAN connection status resource type event configuration.
--
-- /See:/ 'newLoRaWANConnectionStatusEventNotificationConfigurations' smart constructor.
data LoRaWANConnectionStatusEventNotificationConfigurations = LoRaWANConnectionStatusEventNotificationConfigurations'
  { -- | Denotes whether the gateway EUI connection status event topic is enabled
    -- or disabled.
    gatewayEuiEventTopic :: Prelude.Maybe EventNotificationTopicStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANConnectionStatusEventNotificationConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayEuiEventTopic', 'loRaWANConnectionStatusEventNotificationConfigurations_gatewayEuiEventTopic' - Denotes whether the gateway EUI connection status event topic is enabled
-- or disabled.
newLoRaWANConnectionStatusEventNotificationConfigurations ::
  LoRaWANConnectionStatusEventNotificationConfigurations
newLoRaWANConnectionStatusEventNotificationConfigurations =
  LoRaWANConnectionStatusEventNotificationConfigurations'
    { gatewayEuiEventTopic =
        Prelude.Nothing
    }

-- | Denotes whether the gateway EUI connection status event topic is enabled
-- or disabled.
loRaWANConnectionStatusEventNotificationConfigurations_gatewayEuiEventTopic :: Lens.Lens' LoRaWANConnectionStatusEventNotificationConfigurations (Prelude.Maybe EventNotificationTopicStatus)
loRaWANConnectionStatusEventNotificationConfigurations_gatewayEuiEventTopic = Lens.lens (\LoRaWANConnectionStatusEventNotificationConfigurations' {gatewayEuiEventTopic} -> gatewayEuiEventTopic) (\s@LoRaWANConnectionStatusEventNotificationConfigurations' {} a -> s {gatewayEuiEventTopic = a} :: LoRaWANConnectionStatusEventNotificationConfigurations)

instance
  Core.FromJSON
    LoRaWANConnectionStatusEventNotificationConfigurations
  where
  parseJSON =
    Core.withObject
      "LoRaWANConnectionStatusEventNotificationConfigurations"
      ( \x ->
          LoRaWANConnectionStatusEventNotificationConfigurations'
            Prelude.<$> (x Core..:? "GatewayEuiEventTopic")
      )

instance
  Prelude.Hashable
    LoRaWANConnectionStatusEventNotificationConfigurations
  where
  hashWithSalt
    _salt
    LoRaWANConnectionStatusEventNotificationConfigurations' {..} =
      _salt `Prelude.hashWithSalt` gatewayEuiEventTopic

instance
  Prelude.NFData
    LoRaWANConnectionStatusEventNotificationConfigurations
  where
  rnf
    LoRaWANConnectionStatusEventNotificationConfigurations' {..} =
      Prelude.rnf gatewayEuiEventTopic

instance
  Core.ToJSON
    LoRaWANConnectionStatusEventNotificationConfigurations
  where
  toJSON
    LoRaWANConnectionStatusEventNotificationConfigurations' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("GatewayEuiEventTopic" Core..=)
                Prelude.<$> gatewayEuiEventTopic
            ]
        )
