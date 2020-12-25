{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DeviceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DeviceType
  ( DeviceType (..),

    -- * Smart constructor
    mkDeviceType,

    -- * Lenses
    dtDeviceAttributes,
    dtDeviceCreateDate,
    dtDeviceKey,
    dtDeviceLastAuthenticatedDate,
    dtDeviceLastModifiedDate,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.AttributeType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.DeviceKeyType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The device type.
--
-- /See:/ 'mkDeviceType' smart constructor.
data DeviceType = DeviceType'
  { -- | The device attributes.
    deviceAttributes :: Core.Maybe [Types.AttributeType],
    -- | The creation date of the device.
    deviceCreateDate :: Core.Maybe Core.NominalDiffTime,
    -- | The device key.
    deviceKey :: Core.Maybe Types.DeviceKeyType,
    -- | The date in which the device was last authenticated.
    deviceLastAuthenticatedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The last modified date of the device.
    deviceLastModifiedDate :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeviceType' value with any optional fields omitted.
mkDeviceType ::
  DeviceType
mkDeviceType =
  DeviceType'
    { deviceAttributes = Core.Nothing,
      deviceCreateDate = Core.Nothing,
      deviceKey = Core.Nothing,
      deviceLastAuthenticatedDate = Core.Nothing,
      deviceLastModifiedDate = Core.Nothing
    }

-- | The device attributes.
--
-- /Note:/ Consider using 'deviceAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceAttributes :: Lens.Lens' DeviceType (Core.Maybe [Types.AttributeType])
dtDeviceAttributes = Lens.field @"deviceAttributes"
{-# DEPRECATED dtDeviceAttributes "Use generic-lens or generic-optics with 'deviceAttributes' instead." #-}

-- | The creation date of the device.
--
-- /Note:/ Consider using 'deviceCreateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceCreateDate :: Lens.Lens' DeviceType (Core.Maybe Core.NominalDiffTime)
dtDeviceCreateDate = Lens.field @"deviceCreateDate"
{-# DEPRECATED dtDeviceCreateDate "Use generic-lens or generic-optics with 'deviceCreateDate' instead." #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceKey :: Lens.Lens' DeviceType (Core.Maybe Types.DeviceKeyType)
dtDeviceKey = Lens.field @"deviceKey"
{-# DEPRECATED dtDeviceKey "Use generic-lens or generic-optics with 'deviceKey' instead." #-}

-- | The date in which the device was last authenticated.
--
-- /Note:/ Consider using 'deviceLastAuthenticatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceLastAuthenticatedDate :: Lens.Lens' DeviceType (Core.Maybe Core.NominalDiffTime)
dtDeviceLastAuthenticatedDate = Lens.field @"deviceLastAuthenticatedDate"
{-# DEPRECATED dtDeviceLastAuthenticatedDate "Use generic-lens or generic-optics with 'deviceLastAuthenticatedDate' instead." #-}

-- | The last modified date of the device.
--
-- /Note:/ Consider using 'deviceLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceLastModifiedDate :: Lens.Lens' DeviceType (Core.Maybe Core.NominalDiffTime)
dtDeviceLastModifiedDate = Lens.field @"deviceLastModifiedDate"
{-# DEPRECATED dtDeviceLastModifiedDate "Use generic-lens or generic-optics with 'deviceLastModifiedDate' instead." #-}

instance Core.FromJSON DeviceType where
  parseJSON =
    Core.withObject "DeviceType" Core.$
      \x ->
        DeviceType'
          Core.<$> (x Core..:? "DeviceAttributes")
          Core.<*> (x Core..:? "DeviceCreateDate")
          Core.<*> (x Core..:? "DeviceKey")
          Core.<*> (x Core..:? "DeviceLastAuthenticatedDate")
          Core.<*> (x Core..:? "DeviceLastModifiedDate")
