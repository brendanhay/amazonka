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
-- Module      : Amazonka.IoTWireless.Types.WirelessDeviceEventLogOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessDeviceEventLogOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.LogLevel
import Amazonka.IoTWireless.Types.WirelessDeviceEvent
import qualified Amazonka.Prelude as Prelude

-- | The log options for a wireless device event and can be used to set log
-- levels for a specific wireless device event.
--
-- For a LoRaWAN device, possible events for a log messsage are: @Join@,
-- @Rejoin@, @Downlink_Data@, and @Uplink_Data@. For a Sidewalk device,
-- possible events for a log message are @Registration@, @Downlink_Data@,
-- and @Uplink_Data@.
--
-- /See:/ 'newWirelessDeviceEventLogOption' smart constructor.
data WirelessDeviceEventLogOption = WirelessDeviceEventLogOption'
  { event :: WirelessDeviceEvent,
    logLevel :: LogLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WirelessDeviceEventLogOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'event', 'wirelessDeviceEventLogOption_event' - Undocumented member.
--
-- 'logLevel', 'wirelessDeviceEventLogOption_logLevel' - Undocumented member.
newWirelessDeviceEventLogOption ::
  -- | 'event'
  WirelessDeviceEvent ->
  -- | 'logLevel'
  LogLevel ->
  WirelessDeviceEventLogOption
newWirelessDeviceEventLogOption pEvent_ pLogLevel_ =
  WirelessDeviceEventLogOption'
    { event = pEvent_,
      logLevel = pLogLevel_
    }

-- | Undocumented member.
wirelessDeviceEventLogOption_event :: Lens.Lens' WirelessDeviceEventLogOption WirelessDeviceEvent
wirelessDeviceEventLogOption_event = Lens.lens (\WirelessDeviceEventLogOption' {event} -> event) (\s@WirelessDeviceEventLogOption' {} a -> s {event = a} :: WirelessDeviceEventLogOption)

-- | Undocumented member.
wirelessDeviceEventLogOption_logLevel :: Lens.Lens' WirelessDeviceEventLogOption LogLevel
wirelessDeviceEventLogOption_logLevel = Lens.lens (\WirelessDeviceEventLogOption' {logLevel} -> logLevel) (\s@WirelessDeviceEventLogOption' {} a -> s {logLevel = a} :: WirelessDeviceEventLogOption)

instance Core.FromJSON WirelessDeviceEventLogOption where
  parseJSON =
    Core.withObject
      "WirelessDeviceEventLogOption"
      ( \x ->
          WirelessDeviceEventLogOption'
            Prelude.<$> (x Core..: "Event")
            Prelude.<*> (x Core..: "LogLevel")
      )

instance
  Prelude.Hashable
    WirelessDeviceEventLogOption
  where
  hashWithSalt _salt WirelessDeviceEventLogOption' {..} =
    _salt `Prelude.hashWithSalt` event
      `Prelude.hashWithSalt` logLevel

instance Prelude.NFData WirelessDeviceEventLogOption where
  rnf WirelessDeviceEventLogOption' {..} =
    Prelude.rnf event
      `Prelude.seq` Prelude.rnf logLevel

instance Core.ToJSON WirelessDeviceEventLogOption where
  toJSON WirelessDeviceEventLogOption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Event" Core..= event),
            Prelude.Just ("LogLevel" Core..= logLevel)
          ]
      )
