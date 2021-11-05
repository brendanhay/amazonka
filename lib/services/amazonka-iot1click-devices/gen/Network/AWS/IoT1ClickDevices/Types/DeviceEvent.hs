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
-- Module      : Network.AWS.IoT1ClickDevices.Types.DeviceEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT1ClickDevices.Types.DeviceEvent where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT1ClickDevices.Types.Device
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newDeviceEvent' smart constructor.
data DeviceEvent = DeviceEvent'
  { -- | A serialized JSON object representing the device-type specific event.
    stdEvent :: Prelude.Maybe Prelude.Text,
    -- | An object representing the device associated with the event.
    device :: Prelude.Maybe Device
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stdEvent', 'deviceEvent_stdEvent' - A serialized JSON object representing the device-type specific event.
--
-- 'device', 'deviceEvent_device' - An object representing the device associated with the event.
newDeviceEvent ::
  DeviceEvent
newDeviceEvent =
  DeviceEvent'
    { stdEvent = Prelude.Nothing,
      device = Prelude.Nothing
    }

-- | A serialized JSON object representing the device-type specific event.
deviceEvent_stdEvent :: Lens.Lens' DeviceEvent (Prelude.Maybe Prelude.Text)
deviceEvent_stdEvent = Lens.lens (\DeviceEvent' {stdEvent} -> stdEvent) (\s@DeviceEvent' {} a -> s {stdEvent = a} :: DeviceEvent)

-- | An object representing the device associated with the event.
deviceEvent_device :: Lens.Lens' DeviceEvent (Prelude.Maybe Device)
deviceEvent_device = Lens.lens (\DeviceEvent' {device} -> device) (\s@DeviceEvent' {} a -> s {device = a} :: DeviceEvent)

instance Core.FromJSON DeviceEvent where
  parseJSON =
    Core.withObject
      "DeviceEvent"
      ( \x ->
          DeviceEvent'
            Prelude.<$> (x Core..:? "stdEvent")
            Prelude.<*> (x Core..:? "device")
      )

instance Prelude.Hashable DeviceEvent

instance Prelude.NFData DeviceEvent
