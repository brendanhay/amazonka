{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatusDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceStatusDetail
  ( DeviceStatusDetail (..),

    -- * Smart constructor
    mkDeviceStatusDetail,

    -- * Lenses
    dsdCode,
    dsdFeature,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.DeviceStatusDetailCode as Types
import qualified Network.AWS.AlexaBusiness.Types.Feature as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details of a device’s status.
--
-- /See:/ 'mkDeviceStatusDetail' smart constructor.
data DeviceStatusDetail = DeviceStatusDetail'
  { -- | The device status detail code.
    code :: Core.Maybe Types.DeviceStatusDetailCode,
    -- | The list of available features on the device.
    feature :: Core.Maybe Types.Feature
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeviceStatusDetail' value with any optional fields omitted.
mkDeviceStatusDetail ::
  DeviceStatusDetail
mkDeviceStatusDetail =
  DeviceStatusDetail' {code = Core.Nothing, feature = Core.Nothing}

-- | The device status detail code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdCode :: Lens.Lens' DeviceStatusDetail (Core.Maybe Types.DeviceStatusDetailCode)
dsdCode = Lens.field @"code"
{-# DEPRECATED dsdCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The list of available features on the device.
--
-- /Note:/ Consider using 'feature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdFeature :: Lens.Lens' DeviceStatusDetail (Core.Maybe Types.Feature)
dsdFeature = Lens.field @"feature"
{-# DEPRECATED dsdFeature "Use generic-lens or generic-optics with 'feature' instead." #-}

instance Core.FromJSON DeviceStatusDetail where
  parseJSON =
    Core.withObject "DeviceStatusDetail" Core.$
      \x ->
        DeviceStatusDetail'
          Core.<$> (x Core..:? "Code") Core.<*> (x Core..:? "Feature")
