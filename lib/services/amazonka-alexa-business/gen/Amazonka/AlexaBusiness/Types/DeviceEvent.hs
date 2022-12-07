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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.DeviceEvent where

import Amazonka.AlexaBusiness.Types.DeviceEventType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The list of device events.
--
-- /See:/ 'newDeviceEvent' smart constructor.
data DeviceEvent = DeviceEvent'
  { -- | The type of device event.
    type' :: Prelude.Maybe DeviceEventType,
    -- | The time (in epoch) when the event occurred.
    timestamp :: Prelude.Maybe Data.POSIX,
    -- | The value of the event.
    value :: Prelude.Maybe Prelude.Text
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
-- 'type'', 'deviceEvent_type' - The type of device event.
--
-- 'timestamp', 'deviceEvent_timestamp' - The time (in epoch) when the event occurred.
--
-- 'value', 'deviceEvent_value' - The value of the event.
newDeviceEvent ::
  DeviceEvent
newDeviceEvent =
  DeviceEvent'
    { type' = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The type of device event.
deviceEvent_type :: Lens.Lens' DeviceEvent (Prelude.Maybe DeviceEventType)
deviceEvent_type = Lens.lens (\DeviceEvent' {type'} -> type') (\s@DeviceEvent' {} a -> s {type' = a} :: DeviceEvent)

-- | The time (in epoch) when the event occurred.
deviceEvent_timestamp :: Lens.Lens' DeviceEvent (Prelude.Maybe Prelude.UTCTime)
deviceEvent_timestamp = Lens.lens (\DeviceEvent' {timestamp} -> timestamp) (\s@DeviceEvent' {} a -> s {timestamp = a} :: DeviceEvent) Prelude.. Lens.mapping Data._Time

-- | The value of the event.
deviceEvent_value :: Lens.Lens' DeviceEvent (Prelude.Maybe Prelude.Text)
deviceEvent_value = Lens.lens (\DeviceEvent' {value} -> value) (\s@DeviceEvent' {} a -> s {value = a} :: DeviceEvent)

instance Data.FromJSON DeviceEvent where
  parseJSON =
    Data.withObject
      "DeviceEvent"
      ( \x ->
          DeviceEvent'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Timestamp")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable DeviceEvent where
  hashWithSalt _salt DeviceEvent' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` value

instance Prelude.NFData DeviceEvent where
  rnf DeviceEvent' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf value
