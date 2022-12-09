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
-- Module      : Amazonka.IoTWireless.Types.MessageDeliveryStatusEventConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.MessageDeliveryStatusEventConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.EventNotificationTopicStatus
import Amazonka.IoTWireless.Types.SidewalkEventNotificationConfigurations
import qualified Amazonka.Prelude as Prelude

-- | Message delivery status event configuration object for enabling and
-- disabling relevant topics.
--
-- /See:/ 'newMessageDeliveryStatusEventConfiguration' smart constructor.
data MessageDeliveryStatusEventConfiguration = MessageDeliveryStatusEventConfiguration'
  { sidewalk :: Prelude.Maybe SidewalkEventNotificationConfigurations,
    -- | Denotes whether the wireless device ID message delivery status event
    -- topic is enabled or disabled.
    wirelessDeviceIdEventTopic :: Prelude.Maybe EventNotificationTopicStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageDeliveryStatusEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sidewalk', 'messageDeliveryStatusEventConfiguration_sidewalk' - Undocumented member.
--
-- 'wirelessDeviceIdEventTopic', 'messageDeliveryStatusEventConfiguration_wirelessDeviceIdEventTopic' - Denotes whether the wireless device ID message delivery status event
-- topic is enabled or disabled.
newMessageDeliveryStatusEventConfiguration ::
  MessageDeliveryStatusEventConfiguration
newMessageDeliveryStatusEventConfiguration =
  MessageDeliveryStatusEventConfiguration'
    { sidewalk =
        Prelude.Nothing,
      wirelessDeviceIdEventTopic =
        Prelude.Nothing
    }

-- | Undocumented member.
messageDeliveryStatusEventConfiguration_sidewalk :: Lens.Lens' MessageDeliveryStatusEventConfiguration (Prelude.Maybe SidewalkEventNotificationConfigurations)
messageDeliveryStatusEventConfiguration_sidewalk = Lens.lens (\MessageDeliveryStatusEventConfiguration' {sidewalk} -> sidewalk) (\s@MessageDeliveryStatusEventConfiguration' {} a -> s {sidewalk = a} :: MessageDeliveryStatusEventConfiguration)

-- | Denotes whether the wireless device ID message delivery status event
-- topic is enabled or disabled.
messageDeliveryStatusEventConfiguration_wirelessDeviceIdEventTopic :: Lens.Lens' MessageDeliveryStatusEventConfiguration (Prelude.Maybe EventNotificationTopicStatus)
messageDeliveryStatusEventConfiguration_wirelessDeviceIdEventTopic = Lens.lens (\MessageDeliveryStatusEventConfiguration' {wirelessDeviceIdEventTopic} -> wirelessDeviceIdEventTopic) (\s@MessageDeliveryStatusEventConfiguration' {} a -> s {wirelessDeviceIdEventTopic = a} :: MessageDeliveryStatusEventConfiguration)

instance
  Data.FromJSON
    MessageDeliveryStatusEventConfiguration
  where
  parseJSON =
    Data.withObject
      "MessageDeliveryStatusEventConfiguration"
      ( \x ->
          MessageDeliveryStatusEventConfiguration'
            Prelude.<$> (x Data..:? "Sidewalk")
            Prelude.<*> (x Data..:? "WirelessDeviceIdEventTopic")
      )

instance
  Prelude.Hashable
    MessageDeliveryStatusEventConfiguration
  where
  hashWithSalt
    _salt
    MessageDeliveryStatusEventConfiguration' {..} =
      _salt `Prelude.hashWithSalt` sidewalk
        `Prelude.hashWithSalt` wirelessDeviceIdEventTopic

instance
  Prelude.NFData
    MessageDeliveryStatusEventConfiguration
  where
  rnf MessageDeliveryStatusEventConfiguration' {..} =
    Prelude.rnf sidewalk
      `Prelude.seq` Prelude.rnf wirelessDeviceIdEventTopic

instance
  Data.ToJSON
    MessageDeliveryStatusEventConfiguration
  where
  toJSON MessageDeliveryStatusEventConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Sidewalk" Data..=) Prelude.<$> sidewalk,
            ("WirelessDeviceIdEventTopic" Data..=)
              Prelude.<$> wirelessDeviceIdEventTopic
          ]
      )
