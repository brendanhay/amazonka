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
    dtDeviceLastModifiedDate,
    dtDeviceCreateDate,
    dtDeviceAttributes,
    dtDeviceKey,
    dtDeviceLastAuthenticatedDate,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.AttributeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The device type.
--
-- /See:/ 'mkDeviceType' smart constructor.
data DeviceType = DeviceType'
  { deviceLastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    deviceCreateDate :: Lude.Maybe Lude.Timestamp,
    deviceAttributes :: Lude.Maybe [AttributeType],
    deviceKey :: Lude.Maybe Lude.Text,
    deviceLastAuthenticatedDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeviceType' with the minimum fields required to make a request.
--
-- * 'deviceAttributes' - The device attributes.
-- * 'deviceCreateDate' - The creation date of the device.
-- * 'deviceKey' - The device key.
-- * 'deviceLastAuthenticatedDate' - The date in which the device was last authenticated.
-- * 'deviceLastModifiedDate' - The last modified date of the device.
mkDeviceType ::
  DeviceType
mkDeviceType =
  DeviceType'
    { deviceLastModifiedDate = Lude.Nothing,
      deviceCreateDate = Lude.Nothing,
      deviceAttributes = Lude.Nothing,
      deviceKey = Lude.Nothing,
      deviceLastAuthenticatedDate = Lude.Nothing
    }

-- | The last modified date of the device.
--
-- /Note:/ Consider using 'deviceLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceLastModifiedDate :: Lens.Lens' DeviceType (Lude.Maybe Lude.Timestamp)
dtDeviceLastModifiedDate = Lens.lens (deviceLastModifiedDate :: DeviceType -> Lude.Maybe Lude.Timestamp) (\s a -> s {deviceLastModifiedDate = a} :: DeviceType)
{-# DEPRECATED dtDeviceLastModifiedDate "Use generic-lens or generic-optics with 'deviceLastModifiedDate' instead." #-}

-- | The creation date of the device.
--
-- /Note:/ Consider using 'deviceCreateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceCreateDate :: Lens.Lens' DeviceType (Lude.Maybe Lude.Timestamp)
dtDeviceCreateDate = Lens.lens (deviceCreateDate :: DeviceType -> Lude.Maybe Lude.Timestamp) (\s a -> s {deviceCreateDate = a} :: DeviceType)
{-# DEPRECATED dtDeviceCreateDate "Use generic-lens or generic-optics with 'deviceCreateDate' instead." #-}

-- | The device attributes.
--
-- /Note:/ Consider using 'deviceAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceAttributes :: Lens.Lens' DeviceType (Lude.Maybe [AttributeType])
dtDeviceAttributes = Lens.lens (deviceAttributes :: DeviceType -> Lude.Maybe [AttributeType]) (\s a -> s {deviceAttributes = a} :: DeviceType)
{-# DEPRECATED dtDeviceAttributes "Use generic-lens or generic-optics with 'deviceAttributes' instead." #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceKey :: Lens.Lens' DeviceType (Lude.Maybe Lude.Text)
dtDeviceKey = Lens.lens (deviceKey :: DeviceType -> Lude.Maybe Lude.Text) (\s a -> s {deviceKey = a} :: DeviceType)
{-# DEPRECATED dtDeviceKey "Use generic-lens or generic-optics with 'deviceKey' instead." #-}

-- | The date in which the device was last authenticated.
--
-- /Note:/ Consider using 'deviceLastAuthenticatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeviceLastAuthenticatedDate :: Lens.Lens' DeviceType (Lude.Maybe Lude.Timestamp)
dtDeviceLastAuthenticatedDate = Lens.lens (deviceLastAuthenticatedDate :: DeviceType -> Lude.Maybe Lude.Timestamp) (\s a -> s {deviceLastAuthenticatedDate = a} :: DeviceType)
{-# DEPRECATED dtDeviceLastAuthenticatedDate "Use generic-lens or generic-optics with 'deviceLastAuthenticatedDate' instead." #-}

instance Lude.FromJSON DeviceType where
  parseJSON =
    Lude.withObject
      "DeviceType"
      ( \x ->
          DeviceType'
            Lude.<$> (x Lude..:? "DeviceLastModifiedDate")
            Lude.<*> (x Lude..:? "DeviceCreateDate")
            Lude.<*> (x Lude..:? "DeviceAttributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DeviceKey")
            Lude.<*> (x Lude..:? "DeviceLastAuthenticatedDate")
      )
