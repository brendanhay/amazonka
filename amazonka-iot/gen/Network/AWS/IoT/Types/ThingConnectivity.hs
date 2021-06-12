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
-- Module      : Network.AWS.IoT.Types.ThingConnectivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingConnectivity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The connectivity status of the thing.
--
-- /See:/ 'newThingConnectivity' smart constructor.
data ThingConnectivity = ThingConnectivity'
  { -- | True if the thing is connected to the AWS IoT service; false if it is
    -- not connected.
    connected :: Core.Maybe Core.Bool,
    -- | The epoch time (in milliseconds) when the thing last connected or
    -- disconnected. If the thing has been disconnected for more than a few
    -- weeks, the time value might be missing.
    timestamp :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ThingConnectivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connected', 'thingConnectivity_connected' - True if the thing is connected to the AWS IoT service; false if it is
-- not connected.
--
-- 'timestamp', 'thingConnectivity_timestamp' - The epoch time (in milliseconds) when the thing last connected or
-- disconnected. If the thing has been disconnected for more than a few
-- weeks, the time value might be missing.
newThingConnectivity ::
  ThingConnectivity
newThingConnectivity =
  ThingConnectivity'
    { connected = Core.Nothing,
      timestamp = Core.Nothing
    }

-- | True if the thing is connected to the AWS IoT service; false if it is
-- not connected.
thingConnectivity_connected :: Lens.Lens' ThingConnectivity (Core.Maybe Core.Bool)
thingConnectivity_connected = Lens.lens (\ThingConnectivity' {connected} -> connected) (\s@ThingConnectivity' {} a -> s {connected = a} :: ThingConnectivity)

-- | The epoch time (in milliseconds) when the thing last connected or
-- disconnected. If the thing has been disconnected for more than a few
-- weeks, the time value might be missing.
thingConnectivity_timestamp :: Lens.Lens' ThingConnectivity (Core.Maybe Core.Integer)
thingConnectivity_timestamp = Lens.lens (\ThingConnectivity' {timestamp} -> timestamp) (\s@ThingConnectivity' {} a -> s {timestamp = a} :: ThingConnectivity)

instance Core.FromJSON ThingConnectivity where
  parseJSON =
    Core.withObject
      "ThingConnectivity"
      ( \x ->
          ThingConnectivity'
            Core.<$> (x Core..:? "connected")
            Core.<*> (x Core..:? "timestamp")
      )

instance Core.Hashable ThingConnectivity

instance Core.NFData ThingConnectivity
