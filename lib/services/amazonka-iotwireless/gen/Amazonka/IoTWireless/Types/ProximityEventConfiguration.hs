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
-- Module      : Amazonka.IoTWireless.Types.ProximityEventConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.ProximityEventConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.EventNotificationTopicStatus
import Amazonka.IoTWireless.Types.SidewalkEventNotificationConfigurations
import qualified Amazonka.Prelude as Prelude

-- | Proximity event configuration object for enabling and disabling relevant
-- topics.
--
-- /See:/ 'newProximityEventConfiguration' smart constructor.
data ProximityEventConfiguration = ProximityEventConfiguration'
  { -- | Proximity event configuration object for enabling or disabling Sidewalk
    -- related event topics.
    sidewalk :: Prelude.Maybe SidewalkEventNotificationConfigurations,
    -- | Denotes whether the wireless device ID proximity event topic is enabled
    -- or disabled.
    wirelessDeviceIdEventTopic :: Prelude.Maybe EventNotificationTopicStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProximityEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sidewalk', 'proximityEventConfiguration_sidewalk' - Proximity event configuration object for enabling or disabling Sidewalk
-- related event topics.
--
-- 'wirelessDeviceIdEventTopic', 'proximityEventConfiguration_wirelessDeviceIdEventTopic' - Denotes whether the wireless device ID proximity event topic is enabled
-- or disabled.
newProximityEventConfiguration ::
  ProximityEventConfiguration
newProximityEventConfiguration =
  ProximityEventConfiguration'
    { sidewalk =
        Prelude.Nothing,
      wirelessDeviceIdEventTopic = Prelude.Nothing
    }

-- | Proximity event configuration object for enabling or disabling Sidewalk
-- related event topics.
proximityEventConfiguration_sidewalk :: Lens.Lens' ProximityEventConfiguration (Prelude.Maybe SidewalkEventNotificationConfigurations)
proximityEventConfiguration_sidewalk = Lens.lens (\ProximityEventConfiguration' {sidewalk} -> sidewalk) (\s@ProximityEventConfiguration' {} a -> s {sidewalk = a} :: ProximityEventConfiguration)

-- | Denotes whether the wireless device ID proximity event topic is enabled
-- or disabled.
proximityEventConfiguration_wirelessDeviceIdEventTopic :: Lens.Lens' ProximityEventConfiguration (Prelude.Maybe EventNotificationTopicStatus)
proximityEventConfiguration_wirelessDeviceIdEventTopic = Lens.lens (\ProximityEventConfiguration' {wirelessDeviceIdEventTopic} -> wirelessDeviceIdEventTopic) (\s@ProximityEventConfiguration' {} a -> s {wirelessDeviceIdEventTopic = a} :: ProximityEventConfiguration)

instance Data.FromJSON ProximityEventConfiguration where
  parseJSON =
    Data.withObject
      "ProximityEventConfiguration"
      ( \x ->
          ProximityEventConfiguration'
            Prelude.<$> (x Data..:? "Sidewalk")
            Prelude.<*> (x Data..:? "WirelessDeviceIdEventTopic")
      )

instance Prelude.Hashable ProximityEventConfiguration where
  hashWithSalt _salt ProximityEventConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` sidewalk
      `Prelude.hashWithSalt` wirelessDeviceIdEventTopic

instance Prelude.NFData ProximityEventConfiguration where
  rnf ProximityEventConfiguration' {..} =
    Prelude.rnf sidewalk
      `Prelude.seq` Prelude.rnf wirelessDeviceIdEventTopic

instance Data.ToJSON ProximityEventConfiguration where
  toJSON ProximityEventConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Sidewalk" Data..=) Prelude.<$> sidewalk,
            ("WirelessDeviceIdEventTopic" Data..=)
              Prelude.<$> wirelessDeviceIdEventTopic
          ]
      )
