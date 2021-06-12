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
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceEvent where

import Network.AWS.AlexaBusiness.Types.DeviceEventType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The list of device events.
--
-- /See:/ 'newDeviceEvent' smart constructor.
data DeviceEvent = DeviceEvent'
  { -- | The time (in epoch) when the event occurred.
    timestamp :: Core.Maybe Core.POSIX,
    -- | The value of the event.
    value :: Core.Maybe Core.Text,
    -- | The type of device event.
    type' :: Core.Maybe DeviceEventType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'deviceEvent_timestamp' - The time (in epoch) when the event occurred.
--
-- 'value', 'deviceEvent_value' - The value of the event.
--
-- 'type'', 'deviceEvent_type' - The type of device event.
newDeviceEvent ::
  DeviceEvent
newDeviceEvent =
  DeviceEvent'
    { timestamp = Core.Nothing,
      value = Core.Nothing,
      type' = Core.Nothing
    }

-- | The time (in epoch) when the event occurred.
deviceEvent_timestamp :: Lens.Lens' DeviceEvent (Core.Maybe Core.UTCTime)
deviceEvent_timestamp = Lens.lens (\DeviceEvent' {timestamp} -> timestamp) (\s@DeviceEvent' {} a -> s {timestamp = a} :: DeviceEvent) Core.. Lens.mapping Core._Time

-- | The value of the event.
deviceEvent_value :: Lens.Lens' DeviceEvent (Core.Maybe Core.Text)
deviceEvent_value = Lens.lens (\DeviceEvent' {value} -> value) (\s@DeviceEvent' {} a -> s {value = a} :: DeviceEvent)

-- | The type of device event.
deviceEvent_type :: Lens.Lens' DeviceEvent (Core.Maybe DeviceEventType)
deviceEvent_type = Lens.lens (\DeviceEvent' {type'} -> type') (\s@DeviceEvent' {} a -> s {type' = a} :: DeviceEvent)

instance Core.FromJSON DeviceEvent where
  parseJSON =
    Core.withObject
      "DeviceEvent"
      ( \x ->
          DeviceEvent'
            Core.<$> (x Core..:? "Timestamp")
            Core.<*> (x Core..:? "Value")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable DeviceEvent

instance Core.NFData DeviceEvent
