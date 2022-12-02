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
-- Module      : Amazonka.IoT.Types.ThingConnectivity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingConnectivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connectivity status of the thing.
--
-- /See:/ 'newThingConnectivity' smart constructor.
data ThingConnectivity = ThingConnectivity'
  { -- | The epoch time (in milliseconds) when the thing last connected or
    -- disconnected. If the thing has been disconnected for approximately an
    -- hour, the time value might be missing.
    timestamp :: Prelude.Maybe Prelude.Integer,
    -- | True if the thing is connected to the Amazon Web Services IoT Core
    -- service; false if it is not connected.
    connected :: Prelude.Maybe Prelude.Bool,
    -- | The reason why the client is disconnected. If the thing has been
    -- disconnected for approximately an hour, the @disconnectReason@ value
    -- might be missing.
    disconnectReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThingConnectivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'thingConnectivity_timestamp' - The epoch time (in milliseconds) when the thing last connected or
-- disconnected. If the thing has been disconnected for approximately an
-- hour, the time value might be missing.
--
-- 'connected', 'thingConnectivity_connected' - True if the thing is connected to the Amazon Web Services IoT Core
-- service; false if it is not connected.
--
-- 'disconnectReason', 'thingConnectivity_disconnectReason' - The reason why the client is disconnected. If the thing has been
-- disconnected for approximately an hour, the @disconnectReason@ value
-- might be missing.
newThingConnectivity ::
  ThingConnectivity
newThingConnectivity =
  ThingConnectivity'
    { timestamp = Prelude.Nothing,
      connected = Prelude.Nothing,
      disconnectReason = Prelude.Nothing
    }

-- | The epoch time (in milliseconds) when the thing last connected or
-- disconnected. If the thing has been disconnected for approximately an
-- hour, the time value might be missing.
thingConnectivity_timestamp :: Lens.Lens' ThingConnectivity (Prelude.Maybe Prelude.Integer)
thingConnectivity_timestamp = Lens.lens (\ThingConnectivity' {timestamp} -> timestamp) (\s@ThingConnectivity' {} a -> s {timestamp = a} :: ThingConnectivity)

-- | True if the thing is connected to the Amazon Web Services IoT Core
-- service; false if it is not connected.
thingConnectivity_connected :: Lens.Lens' ThingConnectivity (Prelude.Maybe Prelude.Bool)
thingConnectivity_connected = Lens.lens (\ThingConnectivity' {connected} -> connected) (\s@ThingConnectivity' {} a -> s {connected = a} :: ThingConnectivity)

-- | The reason why the client is disconnected. If the thing has been
-- disconnected for approximately an hour, the @disconnectReason@ value
-- might be missing.
thingConnectivity_disconnectReason :: Lens.Lens' ThingConnectivity (Prelude.Maybe Prelude.Text)
thingConnectivity_disconnectReason = Lens.lens (\ThingConnectivity' {disconnectReason} -> disconnectReason) (\s@ThingConnectivity' {} a -> s {disconnectReason = a} :: ThingConnectivity)

instance Data.FromJSON ThingConnectivity where
  parseJSON =
    Data.withObject
      "ThingConnectivity"
      ( \x ->
          ThingConnectivity'
            Prelude.<$> (x Data..:? "timestamp")
            Prelude.<*> (x Data..:? "connected")
            Prelude.<*> (x Data..:? "disconnectReason")
      )

instance Prelude.Hashable ThingConnectivity where
  hashWithSalt _salt ThingConnectivity' {..} =
    _salt `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` connected
      `Prelude.hashWithSalt` disconnectReason

instance Prelude.NFData ThingConnectivity where
  rnf ThingConnectivity' {..} =
    Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf connected
      `Prelude.seq` Prelude.rnf disconnectReason
