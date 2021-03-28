{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DeviceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.DeviceType
  ( DeviceType (..)
  -- * Smart constructor
  , mkDeviceType
  -- * Lenses
  , dtDeviceAttributes
  , dtDeviceCreateDate
  , dtDeviceKey
  , dtDeviceLastAuthenticatedDate
  , dtDeviceLastModifiedDate
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.AttributeType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.DeviceKeyType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The device type.
--
-- /See:/ 'mkDeviceType' smart constructor.
data DeviceType = DeviceType'
  { deviceAttributes :: Core.Maybe [Types.AttributeType]
    -- ^ The device attributes.
  , deviceCreateDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The creation date of the device.
  , deviceKey :: Core.Maybe Types.DeviceKeyType
    -- ^ The device key.
  , deviceLastAuthenticatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date in which the device was last authenticated.
  , deviceLastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The last modified date of the device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeviceType' value with any optional fields omitted.
mkDeviceType
    :: DeviceType
mkDeviceType
  = DeviceType'{deviceAttributes = Core.Nothing,
                deviceCreateDate = Core.Nothing, deviceKey = Core.Nothing,
                deviceLastAuthenticatedDate = Core.Nothing,
                deviceLastModifiedDate = Core.Nothing}

-- | The device attributes.
--
-- /Note:/ Consider using 'deviceAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceAttributes :: Lens.Lens' DeviceType (Core.Maybe [Types.AttributeType])
dtDeviceAttributes = Lens.field @"deviceAttributes"
{-# INLINEABLE dtDeviceAttributes #-}
{-# DEPRECATED deviceAttributes "Use generic-lens or generic-optics with 'deviceAttributes' instead"  #-}

-- | The creation date of the device.
--
-- /Note:/ Consider using 'deviceCreateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceCreateDate :: Lens.Lens' DeviceType (Core.Maybe Core.NominalDiffTime)
dtDeviceCreateDate = Lens.field @"deviceCreateDate"
{-# INLINEABLE dtDeviceCreateDate #-}
{-# DEPRECATED deviceCreateDate "Use generic-lens or generic-optics with 'deviceCreateDate' instead"  #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceKey :: Lens.Lens' DeviceType (Core.Maybe Types.DeviceKeyType)
dtDeviceKey = Lens.field @"deviceKey"
{-# INLINEABLE dtDeviceKey #-}
{-# DEPRECATED deviceKey "Use generic-lens or generic-optics with 'deviceKey' instead"  #-}

-- | The date in which the device was last authenticated.
--
-- /Note:/ Consider using 'deviceLastAuthenticatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceLastAuthenticatedDate :: Lens.Lens' DeviceType (Core.Maybe Core.NominalDiffTime)
dtDeviceLastAuthenticatedDate = Lens.field @"deviceLastAuthenticatedDate"
{-# INLINEABLE dtDeviceLastAuthenticatedDate #-}
{-# DEPRECATED deviceLastAuthenticatedDate "Use generic-lens or generic-optics with 'deviceLastAuthenticatedDate' instead"  #-}

-- | The last modified date of the device.
--
-- /Note:/ Consider using 'deviceLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceLastModifiedDate :: Lens.Lens' DeviceType (Core.Maybe Core.NominalDiffTime)
dtDeviceLastModifiedDate = Lens.field @"deviceLastModifiedDate"
{-# INLINEABLE dtDeviceLastModifiedDate #-}
{-# DEPRECATED deviceLastModifiedDate "Use generic-lens or generic-optics with 'deviceLastModifiedDate' instead"  #-}

instance Core.FromJSON DeviceType where
        parseJSON
          = Core.withObject "DeviceType" Core.$
              \ x ->
                DeviceType' Core.<$>
                  (x Core..:? "DeviceAttributes") Core.<*>
                    x Core..:? "DeviceCreateDate"
                    Core.<*> x Core..:? "DeviceKey"
                    Core.<*> x Core..:? "DeviceLastAuthenticatedDate"
                    Core.<*> x Core..:? "DeviceLastModifiedDate"
