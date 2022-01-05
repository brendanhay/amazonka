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
-- Module      : Amazonka.AlexaBusiness.Types.DeviceEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.DeviceEvent where

import Amazonka.AlexaBusiness.Types.DeviceEventType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The list of device events.
--
-- /See:/ 'newDeviceEvent' smart constructor.
data DeviceEvent = DeviceEvent'
  { -- | The value of the event.
    value :: Prelude.Maybe Prelude.Text,
    -- | The type of device event.
    type' :: Prelude.Maybe DeviceEventType,
    -- | The time (in epoch) when the event occurred.
    timestamp :: Prelude.Maybe Core.POSIX
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
-- 'value', 'deviceEvent_value' - The value of the event.
--
-- 'type'', 'deviceEvent_type' - The type of device event.
--
-- 'timestamp', 'deviceEvent_timestamp' - The time (in epoch) when the event occurred.
newDeviceEvent ::
  DeviceEvent
newDeviceEvent =
  DeviceEvent'
    { value = Prelude.Nothing,
      type' = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The value of the event.
deviceEvent_value :: Lens.Lens' DeviceEvent (Prelude.Maybe Prelude.Text)
deviceEvent_value = Lens.lens (\DeviceEvent' {value} -> value) (\s@DeviceEvent' {} a -> s {value = a} :: DeviceEvent)

-- | The type of device event.
deviceEvent_type :: Lens.Lens' DeviceEvent (Prelude.Maybe DeviceEventType)
deviceEvent_type = Lens.lens (\DeviceEvent' {type'} -> type') (\s@DeviceEvent' {} a -> s {type' = a} :: DeviceEvent)

-- | The time (in epoch) when the event occurred.
deviceEvent_timestamp :: Lens.Lens' DeviceEvent (Prelude.Maybe Prelude.UTCTime)
deviceEvent_timestamp = Lens.lens (\DeviceEvent' {timestamp} -> timestamp) (\s@DeviceEvent' {} a -> s {timestamp = a} :: DeviceEvent) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DeviceEvent where
  parseJSON =
    Core.withObject
      "DeviceEvent"
      ( \x ->
          DeviceEvent'
            Prelude.<$> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Timestamp")
      )

instance Prelude.Hashable DeviceEvent where
  hashWithSalt _salt DeviceEvent' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData DeviceEvent where
  rnf DeviceEvent' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf timestamp
