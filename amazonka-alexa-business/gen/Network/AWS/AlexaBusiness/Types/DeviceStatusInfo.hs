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
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceStatusInfo where

import Network.AWS.AlexaBusiness.Types.ConnectionStatus
import Network.AWS.AlexaBusiness.Types.DeviceStatusDetail
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Detailed information about a device\'s status.
--
-- /See:/ 'newDeviceStatusInfo' smart constructor.
data DeviceStatusInfo = DeviceStatusInfo'
  { -- | One or more device status detail descriptions.
    deviceStatusDetails :: Core.Maybe [DeviceStatusDetail],
    -- | The time (in epoch) when the device connection status changed.
    connectionStatusUpdatedTime :: Core.Maybe Core.POSIX,
    -- | The latest available information about the connection status of a
    -- device.
    connectionStatus :: Core.Maybe ConnectionStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceStatusInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceStatusDetails', 'deviceStatusInfo_deviceStatusDetails' - One or more device status detail descriptions.
--
-- 'connectionStatusUpdatedTime', 'deviceStatusInfo_connectionStatusUpdatedTime' - The time (in epoch) when the device connection status changed.
--
-- 'connectionStatus', 'deviceStatusInfo_connectionStatus' - The latest available information about the connection status of a
-- device.
newDeviceStatusInfo ::
  DeviceStatusInfo
newDeviceStatusInfo =
  DeviceStatusInfo'
    { deviceStatusDetails =
        Core.Nothing,
      connectionStatusUpdatedTime = Core.Nothing,
      connectionStatus = Core.Nothing
    }

-- | One or more device status detail descriptions.
deviceStatusInfo_deviceStatusDetails :: Lens.Lens' DeviceStatusInfo (Core.Maybe [DeviceStatusDetail])
deviceStatusInfo_deviceStatusDetails = Lens.lens (\DeviceStatusInfo' {deviceStatusDetails} -> deviceStatusDetails) (\s@DeviceStatusInfo' {} a -> s {deviceStatusDetails = a} :: DeviceStatusInfo) Core.. Lens.mapping Lens._Coerce

-- | The time (in epoch) when the device connection status changed.
deviceStatusInfo_connectionStatusUpdatedTime :: Lens.Lens' DeviceStatusInfo (Core.Maybe Core.UTCTime)
deviceStatusInfo_connectionStatusUpdatedTime = Lens.lens (\DeviceStatusInfo' {connectionStatusUpdatedTime} -> connectionStatusUpdatedTime) (\s@DeviceStatusInfo' {} a -> s {connectionStatusUpdatedTime = a} :: DeviceStatusInfo) Core.. Lens.mapping Core._Time

-- | The latest available information about the connection status of a
-- device.
deviceStatusInfo_connectionStatus :: Lens.Lens' DeviceStatusInfo (Core.Maybe ConnectionStatus)
deviceStatusInfo_connectionStatus = Lens.lens (\DeviceStatusInfo' {connectionStatus} -> connectionStatus) (\s@DeviceStatusInfo' {} a -> s {connectionStatus = a} :: DeviceStatusInfo)

instance Core.FromJSON DeviceStatusInfo where
  parseJSON =
    Core.withObject
      "DeviceStatusInfo"
      ( \x ->
          DeviceStatusInfo'
            Core.<$> ( x Core..:? "DeviceStatusDetails"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "ConnectionStatusUpdatedTime")
            Core.<*> (x Core..:? "ConnectionStatus")
      )

instance Core.Hashable DeviceStatusInfo

instance Core.NFData DeviceStatusInfo
