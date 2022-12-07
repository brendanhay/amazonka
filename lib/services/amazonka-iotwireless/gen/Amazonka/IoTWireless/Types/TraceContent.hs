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
-- Module      : Amazonka.IoTWireless.Types.TraceContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.TraceContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.LogLevel
import Amazonka.IoTWireless.Types.WirelessDeviceFrameInfo
import qualified Amazonka.Prelude as Prelude

-- | Trace content for your wireless gateway and wireless device resources.
--
-- /See:/ 'newTraceContent' smart constructor.
data TraceContent = TraceContent'
  { logLevel :: Prelude.Maybe LogLevel,
    wirelessDeviceFrameInfo :: Prelude.Maybe WirelessDeviceFrameInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TraceContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logLevel', 'traceContent_logLevel' - Undocumented member.
--
-- 'wirelessDeviceFrameInfo', 'traceContent_wirelessDeviceFrameInfo' - Undocumented member.
newTraceContent ::
  TraceContent
newTraceContent =
  TraceContent'
    { logLevel = Prelude.Nothing,
      wirelessDeviceFrameInfo = Prelude.Nothing
    }

-- | Undocumented member.
traceContent_logLevel :: Lens.Lens' TraceContent (Prelude.Maybe LogLevel)
traceContent_logLevel = Lens.lens (\TraceContent' {logLevel} -> logLevel) (\s@TraceContent' {} a -> s {logLevel = a} :: TraceContent)

-- | Undocumented member.
traceContent_wirelessDeviceFrameInfo :: Lens.Lens' TraceContent (Prelude.Maybe WirelessDeviceFrameInfo)
traceContent_wirelessDeviceFrameInfo = Lens.lens (\TraceContent' {wirelessDeviceFrameInfo} -> wirelessDeviceFrameInfo) (\s@TraceContent' {} a -> s {wirelessDeviceFrameInfo = a} :: TraceContent)

instance Data.FromJSON TraceContent where
  parseJSON =
    Data.withObject
      "TraceContent"
      ( \x ->
          TraceContent'
            Prelude.<$> (x Data..:? "LogLevel")
            Prelude.<*> (x Data..:? "WirelessDeviceFrameInfo")
      )

instance Prelude.Hashable TraceContent where
  hashWithSalt _salt TraceContent' {..} =
    _salt `Prelude.hashWithSalt` logLevel
      `Prelude.hashWithSalt` wirelessDeviceFrameInfo

instance Prelude.NFData TraceContent where
  rnf TraceContent' {..} =
    Prelude.rnf logLevel
      `Prelude.seq` Prelude.rnf wirelessDeviceFrameInfo

instance Data.ToJSON TraceContent where
  toJSON TraceContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LogLevel" Data..=) Prelude.<$> logLevel,
            ("WirelessDeviceFrameInfo" Data..=)
              Prelude.<$> wirelessDeviceFrameInfo
          ]
      )
