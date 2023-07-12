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
-- Module      : Amazonka.IoTWireless.Types.WirelessGatewayEventLogOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessGatewayEventLogOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.LogLevel
import Amazonka.IoTWireless.Types.WirelessGatewayEvent
import qualified Amazonka.Prelude as Prelude

-- | The log options for a wireless gateway event and can be used to set log
-- levels for a specific wireless gateway event.
--
-- For a LoRaWAN gateway, possible events for a log message are
-- @CUPS_Request@ and @Certificate@.
--
-- /See:/ 'newWirelessGatewayEventLogOption' smart constructor.
data WirelessGatewayEventLogOption = WirelessGatewayEventLogOption'
  { event :: WirelessGatewayEvent,
    logLevel :: LogLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WirelessGatewayEventLogOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'event', 'wirelessGatewayEventLogOption_event' - Undocumented member.
--
-- 'logLevel', 'wirelessGatewayEventLogOption_logLevel' - Undocumented member.
newWirelessGatewayEventLogOption ::
  -- | 'event'
  WirelessGatewayEvent ->
  -- | 'logLevel'
  LogLevel ->
  WirelessGatewayEventLogOption
newWirelessGatewayEventLogOption pEvent_ pLogLevel_ =
  WirelessGatewayEventLogOption'
    { event = pEvent_,
      logLevel = pLogLevel_
    }

-- | Undocumented member.
wirelessGatewayEventLogOption_event :: Lens.Lens' WirelessGatewayEventLogOption WirelessGatewayEvent
wirelessGatewayEventLogOption_event = Lens.lens (\WirelessGatewayEventLogOption' {event} -> event) (\s@WirelessGatewayEventLogOption' {} a -> s {event = a} :: WirelessGatewayEventLogOption)

-- | Undocumented member.
wirelessGatewayEventLogOption_logLevel :: Lens.Lens' WirelessGatewayEventLogOption LogLevel
wirelessGatewayEventLogOption_logLevel = Lens.lens (\WirelessGatewayEventLogOption' {logLevel} -> logLevel) (\s@WirelessGatewayEventLogOption' {} a -> s {logLevel = a} :: WirelessGatewayEventLogOption)

instance Data.FromJSON WirelessGatewayEventLogOption where
  parseJSON =
    Data.withObject
      "WirelessGatewayEventLogOption"
      ( \x ->
          WirelessGatewayEventLogOption'
            Prelude.<$> (x Data..: "Event")
            Prelude.<*> (x Data..: "LogLevel")
      )

instance
  Prelude.Hashable
    WirelessGatewayEventLogOption
  where
  hashWithSalt _salt WirelessGatewayEventLogOption' {..} =
    _salt
      `Prelude.hashWithSalt` event
      `Prelude.hashWithSalt` logLevel

instance Prelude.NFData WirelessGatewayEventLogOption where
  rnf WirelessGatewayEventLogOption' {..} =
    Prelude.rnf event
      `Prelude.seq` Prelude.rnf logLevel

instance Data.ToJSON WirelessGatewayEventLogOption where
  toJSON WirelessGatewayEventLogOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Event" Data..= event),
            Prelude.Just ("LogLevel" Data..= logLevel)
          ]
      )
