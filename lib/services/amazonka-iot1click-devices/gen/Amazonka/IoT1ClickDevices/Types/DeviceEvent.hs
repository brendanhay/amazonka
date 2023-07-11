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
-- Module      : Amazonka.IoT1ClickDevices.Types.DeviceEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT1ClickDevices.Types.DeviceEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT1ClickDevices.Types.Device
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newDeviceEvent' smart constructor.
data DeviceEvent = DeviceEvent'
  { -- | An object representing the device associated with the event.
    device :: Prelude.Maybe Device,
    -- | A serialized JSON object representing the device-type specific event.
    stdEvent :: Prelude.Maybe Prelude.Text
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
-- 'device', 'deviceEvent_device' - An object representing the device associated with the event.
--
-- 'stdEvent', 'deviceEvent_stdEvent' - A serialized JSON object representing the device-type specific event.
newDeviceEvent ::
  DeviceEvent
newDeviceEvent =
  DeviceEvent'
    { device = Prelude.Nothing,
      stdEvent = Prelude.Nothing
    }

-- | An object representing the device associated with the event.
deviceEvent_device :: Lens.Lens' DeviceEvent (Prelude.Maybe Device)
deviceEvent_device = Lens.lens (\DeviceEvent' {device} -> device) (\s@DeviceEvent' {} a -> s {device = a} :: DeviceEvent)

-- | A serialized JSON object representing the device-type specific event.
deviceEvent_stdEvent :: Lens.Lens' DeviceEvent (Prelude.Maybe Prelude.Text)
deviceEvent_stdEvent = Lens.lens (\DeviceEvent' {stdEvent} -> stdEvent) (\s@DeviceEvent' {} a -> s {stdEvent = a} :: DeviceEvent)

instance Data.FromJSON DeviceEvent where
  parseJSON =
    Data.withObject
      "DeviceEvent"
      ( \x ->
          DeviceEvent'
            Prelude.<$> (x Data..:? "device")
            Prelude.<*> (x Data..:? "stdEvent")
      )

instance Prelude.Hashable DeviceEvent where
  hashWithSalt _salt DeviceEvent' {..} =
    _salt
      `Prelude.hashWithSalt` device
      `Prelude.hashWithSalt` stdEvent

instance Prelude.NFData DeviceEvent where
  rnf DeviceEvent' {..} =
    Prelude.rnf device
      `Prelude.seq` Prelude.rnf stdEvent
