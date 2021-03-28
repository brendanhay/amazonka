{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
  ( DeviceStatusInfo (..)
  -- * Smart constructor
  , mkDeviceStatusInfo
  -- * Lenses
  , dsiConnectionStatus
  , dsiConnectionStatusUpdatedTime
  , dsiDeviceStatusDetails
  ) where

import qualified Network.AWS.AlexaBusiness.Types.ConnectionStatus as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceStatusDetail as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Detailed information about a device's status.
--
-- /See:/ 'mkDeviceStatusInfo' smart constructor.
data DeviceStatusInfo = DeviceStatusInfo'
  { connectionStatus :: Core.Maybe Types.ConnectionStatus
    -- ^ The latest available information about the connection status of a device. 
  , connectionStatusUpdatedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time (in epoch) when the device connection status changed.
  , deviceStatusDetails :: Core.Maybe [Types.DeviceStatusDetail]
    -- ^ One or more device status detail descriptions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeviceStatusInfo' value with any optional fields omitted.
mkDeviceStatusInfo
    :: DeviceStatusInfo
mkDeviceStatusInfo
  = DeviceStatusInfo'{connectionStatus = Core.Nothing,
                      connectionStatusUpdatedTime = Core.Nothing,
                      deviceStatusDetails = Core.Nothing}

-- | The latest available information about the connection status of a device. 
--
-- /Note:/ Consider using 'connectionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiConnectionStatus :: Lens.Lens' DeviceStatusInfo (Core.Maybe Types.ConnectionStatus)
dsiConnectionStatus = Lens.field @"connectionStatus"
{-# INLINEABLE dsiConnectionStatus #-}
{-# DEPRECATED connectionStatus "Use generic-lens or generic-optics with 'connectionStatus' instead"  #-}

-- | The time (in epoch) when the device connection status changed.
--
-- /Note:/ Consider using 'connectionStatusUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiConnectionStatusUpdatedTime :: Lens.Lens' DeviceStatusInfo (Core.Maybe Core.NominalDiffTime)
dsiConnectionStatusUpdatedTime = Lens.field @"connectionStatusUpdatedTime"
{-# INLINEABLE dsiConnectionStatusUpdatedTime #-}
{-# DEPRECATED connectionStatusUpdatedTime "Use generic-lens or generic-optics with 'connectionStatusUpdatedTime' instead"  #-}

-- | One or more device status detail descriptions.
--
-- /Note:/ Consider using 'deviceStatusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiDeviceStatusDetails :: Lens.Lens' DeviceStatusInfo (Core.Maybe [Types.DeviceStatusDetail])
dsiDeviceStatusDetails = Lens.field @"deviceStatusDetails"
{-# INLINEABLE dsiDeviceStatusDetails #-}
{-# DEPRECATED deviceStatusDetails "Use generic-lens or generic-optics with 'deviceStatusDetails' instead"  #-}

instance Core.FromJSON DeviceStatusInfo where
        parseJSON
          = Core.withObject "DeviceStatusInfo" Core.$
              \ x ->
                DeviceStatusInfo' Core.<$>
                  (x Core..:? "ConnectionStatus") Core.<*>
                    x Core..:? "ConnectionStatusUpdatedTime"
                    Core.<*> x Core..:? "DeviceStatusDetails"
