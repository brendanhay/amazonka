{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Detailed information about a device\'s status.
--
-- /See:/ 'newDeviceStatusInfo' smart constructor.
data DeviceStatusInfo = DeviceStatusInfo'
  { -- | One or more device status detail descriptions.
    deviceStatusDetails :: Prelude.Maybe [DeviceStatusDetail],
    -- | The time (in epoch) when the device connection status changed.
    connectionStatusUpdatedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The latest available information about the connection status of a
    -- device.
    connectionStatus :: Prelude.Maybe ConnectionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      connectionStatusUpdatedTime = Prelude.Nothing,
      connectionStatus = Prelude.Nothing
    }

-- | One or more device status detail descriptions.
deviceStatusInfo_deviceStatusDetails :: Lens.Lens' DeviceStatusInfo (Prelude.Maybe [DeviceStatusDetail])
deviceStatusInfo_deviceStatusDetails = Lens.lens (\DeviceStatusInfo' {deviceStatusDetails} -> deviceStatusDetails) (\s@DeviceStatusInfo' {} a -> s {deviceStatusDetails = a} :: DeviceStatusInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The time (in epoch) when the device connection status changed.
deviceStatusInfo_connectionStatusUpdatedTime :: Lens.Lens' DeviceStatusInfo (Prelude.Maybe Prelude.UTCTime)
deviceStatusInfo_connectionStatusUpdatedTime = Lens.lens (\DeviceStatusInfo' {connectionStatusUpdatedTime} -> connectionStatusUpdatedTime) (\s@DeviceStatusInfo' {} a -> s {connectionStatusUpdatedTime = a} :: DeviceStatusInfo) Prelude.. Lens.mapping Prelude._Time

-- | The latest available information about the connection status of a
-- device.
deviceStatusInfo_connectionStatus :: Lens.Lens' DeviceStatusInfo (Prelude.Maybe ConnectionStatus)
deviceStatusInfo_connectionStatus = Lens.lens (\DeviceStatusInfo' {connectionStatus} -> connectionStatus) (\s@DeviceStatusInfo' {} a -> s {connectionStatus = a} :: DeviceStatusInfo)

instance Prelude.FromJSON DeviceStatusInfo where
  parseJSON =
    Prelude.withObject
      "DeviceStatusInfo"
      ( \x ->
          DeviceStatusInfo'
            Prelude.<$> ( x Prelude..:? "DeviceStatusDetails"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ConnectionStatusUpdatedTime")
            Prelude.<*> (x Prelude..:? "ConnectionStatus")
      )

instance Prelude.Hashable DeviceStatusInfo

instance Prelude.NFData DeviceStatusInfo
