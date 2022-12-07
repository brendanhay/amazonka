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
-- Module      : Amazonka.IoTWireless.Types.WirelessGatewayLogOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessGatewayLogOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.LogLevel
import Amazonka.IoTWireless.Types.WirelessGatewayEventLogOption
import Amazonka.IoTWireless.Types.WirelessGatewayType
import qualified Amazonka.Prelude as Prelude

-- | The log options for wireless gateways and can be used to set log levels
-- for a specific type of wireless gateway.
--
-- /See:/ 'newWirelessGatewayLogOption' smart constructor.
data WirelessGatewayLogOption = WirelessGatewayLogOption'
  { events :: Prelude.Maybe [WirelessGatewayEventLogOption],
    type' :: WirelessGatewayType,
    logLevel :: LogLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WirelessGatewayLogOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'wirelessGatewayLogOption_events' - Undocumented member.
--
-- 'type'', 'wirelessGatewayLogOption_type' - Undocumented member.
--
-- 'logLevel', 'wirelessGatewayLogOption_logLevel' - Undocumented member.
newWirelessGatewayLogOption ::
  -- | 'type''
  WirelessGatewayType ->
  -- | 'logLevel'
  LogLevel ->
  WirelessGatewayLogOption
newWirelessGatewayLogOption pType_ pLogLevel_ =
  WirelessGatewayLogOption'
    { events = Prelude.Nothing,
      type' = pType_,
      logLevel = pLogLevel_
    }

-- | Undocumented member.
wirelessGatewayLogOption_events :: Lens.Lens' WirelessGatewayLogOption (Prelude.Maybe [WirelessGatewayEventLogOption])
wirelessGatewayLogOption_events = Lens.lens (\WirelessGatewayLogOption' {events} -> events) (\s@WirelessGatewayLogOption' {} a -> s {events = a} :: WirelessGatewayLogOption) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
wirelessGatewayLogOption_type :: Lens.Lens' WirelessGatewayLogOption WirelessGatewayType
wirelessGatewayLogOption_type = Lens.lens (\WirelessGatewayLogOption' {type'} -> type') (\s@WirelessGatewayLogOption' {} a -> s {type' = a} :: WirelessGatewayLogOption)

-- | Undocumented member.
wirelessGatewayLogOption_logLevel :: Lens.Lens' WirelessGatewayLogOption LogLevel
wirelessGatewayLogOption_logLevel = Lens.lens (\WirelessGatewayLogOption' {logLevel} -> logLevel) (\s@WirelessGatewayLogOption' {} a -> s {logLevel = a} :: WirelessGatewayLogOption)

instance Data.FromJSON WirelessGatewayLogOption where
  parseJSON =
    Data.withObject
      "WirelessGatewayLogOption"
      ( \x ->
          WirelessGatewayLogOption'
            Prelude.<$> (x Data..:? "Events" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "LogLevel")
      )

instance Prelude.Hashable WirelessGatewayLogOption where
  hashWithSalt _salt WirelessGatewayLogOption' {..} =
    _salt `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` logLevel

instance Prelude.NFData WirelessGatewayLogOption where
  rnf WirelessGatewayLogOption' {..} =
    Prelude.rnf events
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf logLevel

instance Data.ToJSON WirelessGatewayLogOption where
  toJSON WirelessGatewayLogOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Events" Data..=) Prelude.<$> events,
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("LogLevel" Data..= logLevel)
          ]
      )
