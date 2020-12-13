{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
  ( DeviceStatusInfo (..),

    -- * Smart constructor
    mkDeviceStatusInfo,

    -- * Lenses
    dsiConnectionStatusUpdatedTime,
    dsiDeviceStatusDetails,
    dsiConnectionStatus,
  )
where

import Network.AWS.AlexaBusiness.Types.ConnectionStatus
import Network.AWS.AlexaBusiness.Types.DeviceStatusDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Detailed information about a device's status.
--
-- /See:/ 'mkDeviceStatusInfo' smart constructor.
data DeviceStatusInfo = DeviceStatusInfo'
  { -- | The time (in epoch) when the device connection status changed.
    connectionStatusUpdatedTime :: Lude.Maybe Lude.Timestamp,
    -- | One or more device status detail descriptions.
    deviceStatusDetails :: Lude.Maybe [DeviceStatusDetail],
    -- | The latest available information about the connection status of a device.
    connectionStatus :: Lude.Maybe ConnectionStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeviceStatusInfo' with the minimum fields required to make a request.
--
-- * 'connectionStatusUpdatedTime' - The time (in epoch) when the device connection status changed.
-- * 'deviceStatusDetails' - One or more device status detail descriptions.
-- * 'connectionStatus' - The latest available information about the connection status of a device.
mkDeviceStatusInfo ::
  DeviceStatusInfo
mkDeviceStatusInfo =
  DeviceStatusInfo'
    { connectionStatusUpdatedTime = Lude.Nothing,
      deviceStatusDetails = Lude.Nothing,
      connectionStatus = Lude.Nothing
    }

-- | The time (in epoch) when the device connection status changed.
--
-- /Note:/ Consider using 'connectionStatusUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiConnectionStatusUpdatedTime :: Lens.Lens' DeviceStatusInfo (Lude.Maybe Lude.Timestamp)
dsiConnectionStatusUpdatedTime = Lens.lens (connectionStatusUpdatedTime :: DeviceStatusInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {connectionStatusUpdatedTime = a} :: DeviceStatusInfo)
{-# DEPRECATED dsiConnectionStatusUpdatedTime "Use generic-lens or generic-optics with 'connectionStatusUpdatedTime' instead." #-}

-- | One or more device status detail descriptions.
--
-- /Note:/ Consider using 'deviceStatusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiDeviceStatusDetails :: Lens.Lens' DeviceStatusInfo (Lude.Maybe [DeviceStatusDetail])
dsiDeviceStatusDetails = Lens.lens (deviceStatusDetails :: DeviceStatusInfo -> Lude.Maybe [DeviceStatusDetail]) (\s a -> s {deviceStatusDetails = a} :: DeviceStatusInfo)
{-# DEPRECATED dsiDeviceStatusDetails "Use generic-lens or generic-optics with 'deviceStatusDetails' instead." #-}

-- | The latest available information about the connection status of a device.
--
-- /Note:/ Consider using 'connectionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiConnectionStatus :: Lens.Lens' DeviceStatusInfo (Lude.Maybe ConnectionStatus)
dsiConnectionStatus = Lens.lens (connectionStatus :: DeviceStatusInfo -> Lude.Maybe ConnectionStatus) (\s a -> s {connectionStatus = a} :: DeviceStatusInfo)
{-# DEPRECATED dsiConnectionStatus "Use generic-lens or generic-optics with 'connectionStatus' instead." #-}

instance Lude.FromJSON DeviceStatusInfo where
  parseJSON =
    Lude.withObject
      "DeviceStatusInfo"
      ( \x ->
          DeviceStatusInfo'
            Lude.<$> (x Lude..:? "ConnectionStatusUpdatedTime")
            Lude.<*> (x Lude..:? "DeviceStatusDetails" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ConnectionStatus")
      )
