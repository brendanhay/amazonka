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
import Amazonka.IoTWireless.Types.EventNotificationTopicStatus
import Amazonka.IoTWireless.Types.SidewalkEventNotificationConfigurations
import qualified Amazonka.Prelude as Prelude

-- | Message delivery status event configuration object for enabling and
-- disabling relevant topics.
--
-- /See:/ 'newMessageDeliveryStatusEventConfiguration' smart constructor.
data MessageDeliveryStatusEventConfiguration = MessageDeliveryStatusEventConfiguration'
  { -- | Denotes whether the wireless device ID device registration state event
    -- topic is enabled or disabled.
    wirelessDeviceIdEventTopic :: Prelude.Maybe EventNotificationTopicStatus,
    sidewalk :: Prelude.Maybe SidewalkEventNotificationConfigurations
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
-- 'wirelessDeviceIdEventTopic', 'messageDeliveryStatusEventConfiguration_wirelessDeviceIdEventTopic' - Denotes whether the wireless device ID device registration state event
-- topic is enabled or disabled.
--
-- 'sidewalk', 'messageDeliveryStatusEventConfiguration_sidewalk' - Undocumented member.
newMessageDeliveryStatusEventConfiguration ::
  MessageDeliveryStatusEventConfiguration
newMessageDeliveryStatusEventConfiguration =
  MessageDeliveryStatusEventConfiguration'
    { wirelessDeviceIdEventTopic =
        Prelude.Nothing,
      sidewalk = Prelude.Nothing
    }

-- | Denotes whether the wireless device ID device registration state event
-- topic is enabled or disabled.
messageDeliveryStatusEventConfiguration_wirelessDeviceIdEventTopic :: Lens.Lens' MessageDeliveryStatusEventConfiguration (Prelude.Maybe EventNotificationTopicStatus)
messageDeliveryStatusEventConfiguration_wirelessDeviceIdEventTopic = Lens.lens (\MessageDeliveryStatusEventConfiguration' {wirelessDeviceIdEventTopic} -> wirelessDeviceIdEventTopic) (\s@MessageDeliveryStatusEventConfiguration' {} a -> s {wirelessDeviceIdEventTopic = a} :: MessageDeliveryStatusEventConfiguration)

-- | Undocumented member.
messageDeliveryStatusEventConfiguration_sidewalk :: Lens.Lens' MessageDeliveryStatusEventConfiguration (Prelude.Maybe SidewalkEventNotificationConfigurations)
messageDeliveryStatusEventConfiguration_sidewalk = Lens.lens (\MessageDeliveryStatusEventConfiguration' {sidewalk} -> sidewalk) (\s@MessageDeliveryStatusEventConfiguration' {} a -> s {sidewalk = a} :: MessageDeliveryStatusEventConfiguration)

instance
  Core.FromJSON
    MessageDeliveryStatusEventConfiguration
  where
  parseJSON =
    Core.withObject
      "MessageDeliveryStatusEventConfiguration"
      ( \x ->
          MessageDeliveryStatusEventConfiguration'
            Prelude.<$> (x Core..:? "WirelessDeviceIdEventTopic")
            Prelude.<*> (x Core..:? "Sidewalk")
      )

instance
  Prelude.Hashable
    MessageDeliveryStatusEventConfiguration
  where
  hashWithSalt
    _salt
    MessageDeliveryStatusEventConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` wirelessDeviceIdEventTopic
        `Prelude.hashWithSalt` sidewalk

instance
  Prelude.NFData
    MessageDeliveryStatusEventConfiguration
  where
  rnf MessageDeliveryStatusEventConfiguration' {..} =
    Prelude.rnf wirelessDeviceIdEventTopic
      `Prelude.seq` Prelude.rnf sidewalk

instance
  Core.ToJSON
    MessageDeliveryStatusEventConfiguration
  where
  toJSON MessageDeliveryStatusEventConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("WirelessDeviceIdEventTopic" Core..=)
              Prelude.<$> wirelessDeviceIdEventTopic,
            ("Sidewalk" Core..=) Prelude.<$> sidewalk
          ]
      )
