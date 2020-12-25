{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.NewDeviceMetadataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.NewDeviceMetadataType
  ( NewDeviceMetadataType (..),

    -- * Smart constructor
    mkNewDeviceMetadataType,

    -- * Lenses
    ndmtDeviceGroupKey,
    ndmtDeviceKey,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.DeviceKeyType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.StringType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The new device metadata type.
--
-- /See:/ 'mkNewDeviceMetadataType' smart constructor.
data NewDeviceMetadataType = NewDeviceMetadataType'
  { -- | The device group key.
    deviceGroupKey :: Core.Maybe Types.StringType,
    -- | The device key.
    deviceKey :: Core.Maybe Types.DeviceKeyType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NewDeviceMetadataType' value with any optional fields omitted.
mkNewDeviceMetadataType ::
  NewDeviceMetadataType
mkNewDeviceMetadataType =
  NewDeviceMetadataType'
    { deviceGroupKey = Core.Nothing,
      deviceKey = Core.Nothing
    }

-- | The device group key.
--
-- /Note:/ Consider using 'deviceGroupKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ndmtDeviceGroupKey :: Lens.Lens' NewDeviceMetadataType (Core.Maybe Types.StringType)
ndmtDeviceGroupKey = Lens.field @"deviceGroupKey"
{-# DEPRECATED ndmtDeviceGroupKey "Use generic-lens or generic-optics with 'deviceGroupKey' instead." #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ndmtDeviceKey :: Lens.Lens' NewDeviceMetadataType (Core.Maybe Types.DeviceKeyType)
ndmtDeviceKey = Lens.field @"deviceKey"
{-# DEPRECATED ndmtDeviceKey "Use generic-lens or generic-optics with 'deviceKey' instead." #-}

instance Core.FromJSON NewDeviceMetadataType where
  parseJSON =
    Core.withObject "NewDeviceMetadataType" Core.$
      \x ->
        NewDeviceMetadataType'
          Core.<$> (x Core..:? "DeviceGroupKey") Core.<*> (x Core..:? "DeviceKey")
