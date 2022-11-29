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
-- Module      : Amazonka.IoTWireless.Types.ConnectionStatusResourceTypeEventConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.ConnectionStatusResourceTypeEventConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.LoRaWANConnectionStatusResourceTypeEventConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Connection status resource type event configuration object for enabling
-- or disabling topic.
--
-- /See:/ 'newConnectionStatusResourceTypeEventConfiguration' smart constructor.
data ConnectionStatusResourceTypeEventConfiguration = ConnectionStatusResourceTypeEventConfiguration'
  { -- | Connection status resource type event configuration object for enabling
    -- or disabling LoRaWAN related event topics.
    loRaWAN :: Prelude.Maybe LoRaWANConnectionStatusResourceTypeEventConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionStatusResourceTypeEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loRaWAN', 'connectionStatusResourceTypeEventConfiguration_loRaWAN' - Connection status resource type event configuration object for enabling
-- or disabling LoRaWAN related event topics.
newConnectionStatusResourceTypeEventConfiguration ::
  ConnectionStatusResourceTypeEventConfiguration
newConnectionStatusResourceTypeEventConfiguration =
  ConnectionStatusResourceTypeEventConfiguration'
    { loRaWAN =
        Prelude.Nothing
    }

-- | Connection status resource type event configuration object for enabling
-- or disabling LoRaWAN related event topics.
connectionStatusResourceTypeEventConfiguration_loRaWAN :: Lens.Lens' ConnectionStatusResourceTypeEventConfiguration (Prelude.Maybe LoRaWANConnectionStatusResourceTypeEventConfiguration)
connectionStatusResourceTypeEventConfiguration_loRaWAN = Lens.lens (\ConnectionStatusResourceTypeEventConfiguration' {loRaWAN} -> loRaWAN) (\s@ConnectionStatusResourceTypeEventConfiguration' {} a -> s {loRaWAN = a} :: ConnectionStatusResourceTypeEventConfiguration)

instance
  Core.FromJSON
    ConnectionStatusResourceTypeEventConfiguration
  where
  parseJSON =
    Core.withObject
      "ConnectionStatusResourceTypeEventConfiguration"
      ( \x ->
          ConnectionStatusResourceTypeEventConfiguration'
            Prelude.<$> (x Core..:? "LoRaWAN")
      )

instance
  Prelude.Hashable
    ConnectionStatusResourceTypeEventConfiguration
  where
  hashWithSalt
    _salt
    ConnectionStatusResourceTypeEventConfiguration' {..} =
      _salt `Prelude.hashWithSalt` loRaWAN

instance
  Prelude.NFData
    ConnectionStatusResourceTypeEventConfiguration
  where
  rnf
    ConnectionStatusResourceTypeEventConfiguration' {..} =
      Prelude.rnf loRaWAN

instance
  Core.ToJSON
    ConnectionStatusResourceTypeEventConfiguration
  where
  toJSON
    ConnectionStatusResourceTypeEventConfiguration' {..} =
      Core.object
        ( Prelude.catMaybes
            [("LoRaWAN" Core..=) Prelude.<$> loRaWAN]
        )
