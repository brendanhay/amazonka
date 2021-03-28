{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.DeviceInstance
  ( DeviceInstance (..)
  -- * Smart constructor
  , mkDeviceInstance
  -- * Lenses
  , diArn
  , diDeviceArn
  , diInstanceProfile
  , diLabels
  , diStatus
  , diUdid
  ) where

import qualified Network.AWS.DeviceFarm.Types.AmazonResourceName as Types
import qualified Network.AWS.DeviceFarm.Types.InstanceProfile as Types
import qualified Network.AWS.DeviceFarm.Types.InstanceStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the device instance.
--
-- /See:/ 'mkDeviceInstance' smart constructor.
data DeviceInstance = DeviceInstance'
  { arn :: Core.Maybe Types.AmazonResourceName
    -- ^ The Amazon Resource Name (ARN) of the device instance.
  , deviceArn :: Core.Maybe Types.AmazonResourceName
    -- ^ The ARN of the device.
  , instanceProfile :: Core.Maybe Types.InstanceProfile
    -- ^ A object that contains information about the instance profile.
  , labels :: Core.Maybe [Core.Text]
    -- ^ An array of strings that describe the device instance.
  , status :: Core.Maybe Types.InstanceStatus
    -- ^ The status of the device instance. Valid values are listed here.
  , udid :: Core.Maybe Core.Text
    -- ^ Unique device identifier for the device instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeviceInstance' value with any optional fields omitted.
mkDeviceInstance
    :: DeviceInstance
mkDeviceInstance
  = DeviceInstance'{arn = Core.Nothing, deviceArn = Core.Nothing,
                    instanceProfile = Core.Nothing, labels = Core.Nothing,
                    status = Core.Nothing, udid = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the device instance.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diArn :: Lens.Lens' DeviceInstance (Core.Maybe Types.AmazonResourceName)
diArn = Lens.field @"arn"
{-# INLINEABLE diArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The ARN of the device.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeviceArn :: Lens.Lens' DeviceInstance (Core.Maybe Types.AmazonResourceName)
diDeviceArn = Lens.field @"deviceArn"
{-# INLINEABLE diDeviceArn #-}
{-# DEPRECATED deviceArn "Use generic-lens or generic-optics with 'deviceArn' instead"  #-}

-- | A object that contains information about the instance profile.
--
-- /Note:/ Consider using 'instanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceProfile :: Lens.Lens' DeviceInstance (Core.Maybe Types.InstanceProfile)
diInstanceProfile = Lens.field @"instanceProfile"
{-# INLINEABLE diInstanceProfile #-}
{-# DEPRECATED instanceProfile "Use generic-lens or generic-optics with 'instanceProfile' instead"  #-}

-- | An array of strings that describe the device instance.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLabels :: Lens.Lens' DeviceInstance (Core.Maybe [Core.Text])
diLabels = Lens.field @"labels"
{-# INLINEABLE diLabels #-}
{-# DEPRECATED labels "Use generic-lens or generic-optics with 'labels' instead"  #-}

-- | The status of the device instance. Valid values are listed here.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStatus :: Lens.Lens' DeviceInstance (Core.Maybe Types.InstanceStatus)
diStatus = Lens.field @"status"
{-# INLINEABLE diStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Unique device identifier for the device instance.
--
-- /Note:/ Consider using 'udid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diUdid :: Lens.Lens' DeviceInstance (Core.Maybe Core.Text)
diUdid = Lens.field @"udid"
{-# INLINEABLE diUdid #-}
{-# DEPRECATED udid "Use generic-lens or generic-optics with 'udid' instead"  #-}

instance Core.FromJSON DeviceInstance where
        parseJSON
          = Core.withObject "DeviceInstance" Core.$
              \ x ->
                DeviceInstance' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "deviceArn" Core.<*>
                    x Core..:? "instanceProfile"
                    Core.<*> x Core..:? "labels"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "udid"
