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
-- Module      : Network.AWS.SageMaker.Types.DeviceStats
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DeviceStats where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Status of devices.
--
-- /See:/ 'newDeviceStats' smart constructor.
data DeviceStats = DeviceStats'
  { -- | The number of devices connected with a heartbeat.
    connectedDeviceCount :: Core.Integer,
    -- | The number of registered devices.
    registeredDeviceCount :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectedDeviceCount', 'deviceStats_connectedDeviceCount' - The number of devices connected with a heartbeat.
--
-- 'registeredDeviceCount', 'deviceStats_registeredDeviceCount' - The number of registered devices.
newDeviceStats ::
  -- | 'connectedDeviceCount'
  Core.Integer ->
  -- | 'registeredDeviceCount'
  Core.Integer ->
  DeviceStats
newDeviceStats
  pConnectedDeviceCount_
  pRegisteredDeviceCount_ =
    DeviceStats'
      { connectedDeviceCount =
          pConnectedDeviceCount_,
        registeredDeviceCount = pRegisteredDeviceCount_
      }

-- | The number of devices connected with a heartbeat.
deviceStats_connectedDeviceCount :: Lens.Lens' DeviceStats Core.Integer
deviceStats_connectedDeviceCount = Lens.lens (\DeviceStats' {connectedDeviceCount} -> connectedDeviceCount) (\s@DeviceStats' {} a -> s {connectedDeviceCount = a} :: DeviceStats)

-- | The number of registered devices.
deviceStats_registeredDeviceCount :: Lens.Lens' DeviceStats Core.Integer
deviceStats_registeredDeviceCount = Lens.lens (\DeviceStats' {registeredDeviceCount} -> registeredDeviceCount) (\s@DeviceStats' {} a -> s {registeredDeviceCount = a} :: DeviceStats)

instance Core.FromJSON DeviceStats where
  parseJSON =
    Core.withObject
      "DeviceStats"
      ( \x ->
          DeviceStats'
            Core.<$> (x Core..: "ConnectedDeviceCount")
            Core.<*> (x Core..: "RegisteredDeviceCount")
      )

instance Core.Hashable DeviceStats

instance Core.NFData DeviceStats
