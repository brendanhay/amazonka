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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The new device metadata type.
--
-- /See:/ 'mkNewDeviceMetadataType' smart constructor.
data NewDeviceMetadataType = NewDeviceMetadataType'
  { deviceGroupKey ::
      Lude.Maybe Lude.Text,
    deviceKey :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NewDeviceMetadataType' with the minimum fields required to make a request.
--
-- * 'deviceGroupKey' - The device group key.
-- * 'deviceKey' - The device key.
mkNewDeviceMetadataType ::
  NewDeviceMetadataType
mkNewDeviceMetadataType =
  NewDeviceMetadataType'
    { deviceGroupKey = Lude.Nothing,
      deviceKey = Lude.Nothing
    }

-- | The device group key.
--
-- /Note:/ Consider using 'deviceGroupKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ndmtDeviceGroupKey :: Lens.Lens' NewDeviceMetadataType (Lude.Maybe Lude.Text)
ndmtDeviceGroupKey = Lens.lens (deviceGroupKey :: NewDeviceMetadataType -> Lude.Maybe Lude.Text) (\s a -> s {deviceGroupKey = a} :: NewDeviceMetadataType)
{-# DEPRECATED ndmtDeviceGroupKey "Use generic-lens or generic-optics with 'deviceGroupKey' instead." #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ndmtDeviceKey :: Lens.Lens' NewDeviceMetadataType (Lude.Maybe Lude.Text)
ndmtDeviceKey = Lens.lens (deviceKey :: NewDeviceMetadataType -> Lude.Maybe Lude.Text) (\s a -> s {deviceKey = a} :: NewDeviceMetadataType)
{-# DEPRECATED ndmtDeviceKey "Use generic-lens or generic-optics with 'deviceKey' instead." #-}

instance Lude.FromJSON NewDeviceMetadataType where
  parseJSON =
    Lude.withObject
      "NewDeviceMetadataType"
      ( \x ->
          NewDeviceMetadataType'
            Lude.<$> (x Lude..:? "DeviceGroupKey") Lude.<*> (x Lude..:? "DeviceKey")
      )
