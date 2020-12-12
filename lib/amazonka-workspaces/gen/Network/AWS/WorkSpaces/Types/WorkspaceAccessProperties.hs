{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceAccessProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceAccessProperties
  ( WorkspaceAccessProperties (..),

    -- * Smart constructor
    mkWorkspaceAccessProperties,

    -- * Lenses
    wapDeviceTypeWindows,
    wapDeviceTypeWeb,
    wapDeviceTypeAndroid,
    wapDeviceTypeOSx,
    wapDeviceTypeChromeOS,
    wapDeviceTypeIOS,
    wapDeviceTypeZeroClient,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.AccessPropertyValue

-- | The device types and operating systems that can be used to access a WorkSpace. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspaces-network-requirements.html Amazon WorkSpaces Client Network Requirements> .
--
-- /See:/ 'mkWorkspaceAccessProperties' smart constructor.
data WorkspaceAccessProperties = WorkspaceAccessProperties'
  { deviceTypeWindows ::
      Lude.Maybe AccessPropertyValue,
    deviceTypeWeb ::
      Lude.Maybe AccessPropertyValue,
    deviceTypeAndroid ::
      Lude.Maybe AccessPropertyValue,
    deviceTypeOSx ::
      Lude.Maybe AccessPropertyValue,
    deviceTypeChromeOS ::
      Lude.Maybe AccessPropertyValue,
    deviceTypeIOS ::
      Lude.Maybe AccessPropertyValue,
    deviceTypeZeroClient ::
      Lude.Maybe AccessPropertyValue
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkspaceAccessProperties' with the minimum fields required to make a request.
--
-- * 'deviceTypeAndroid' - Indicates whether users can use Android devices to access their WorkSpaces.
-- * 'deviceTypeChromeOS' - Indicates whether users can use Chromebooks to access their WorkSpaces.
-- * 'deviceTypeIOS' - Indicates whether users can use iOS devices to access their WorkSpaces.
-- * 'deviceTypeOSx' - Indicates whether users can use macOS clients to access their WorkSpaces. To restrict WorkSpaces access to trusted devices (also known as managed devices) with valid certificates, specify a value of @TRUST@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/trusted-devices.html Restrict WorkSpaces Access to Trusted Devices> .
-- * 'deviceTypeWeb' - Indicates whether users can access their WorkSpaces through a web browser.
-- * 'deviceTypeWindows' - Indicates whether users can use Windows clients to access their WorkSpaces. To restrict WorkSpaces access to trusted devices (also known as managed devices) with valid certificates, specify a value of @TRUST@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/trusted-devices.html Restrict WorkSpaces Access to Trusted Devices> .
-- * 'deviceTypeZeroClient' - Indicates whether users can use zero client devices to access their WorkSpaces.
mkWorkspaceAccessProperties ::
  WorkspaceAccessProperties
mkWorkspaceAccessProperties =
  WorkspaceAccessProperties'
    { deviceTypeWindows = Lude.Nothing,
      deviceTypeWeb = Lude.Nothing,
      deviceTypeAndroid = Lude.Nothing,
      deviceTypeOSx = Lude.Nothing,
      deviceTypeChromeOS = Lude.Nothing,
      deviceTypeIOS = Lude.Nothing,
      deviceTypeZeroClient = Lude.Nothing
    }

-- | Indicates whether users can use Windows clients to access their WorkSpaces. To restrict WorkSpaces access to trusted devices (also known as managed devices) with valid certificates, specify a value of @TRUST@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/trusted-devices.html Restrict WorkSpaces Access to Trusted Devices> .
--
-- /Note:/ Consider using 'deviceTypeWindows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wapDeviceTypeWindows :: Lens.Lens' WorkspaceAccessProperties (Lude.Maybe AccessPropertyValue)
wapDeviceTypeWindows = Lens.lens (deviceTypeWindows :: WorkspaceAccessProperties -> Lude.Maybe AccessPropertyValue) (\s a -> s {deviceTypeWindows = a} :: WorkspaceAccessProperties)
{-# DEPRECATED wapDeviceTypeWindows "Use generic-lens or generic-optics with 'deviceTypeWindows' instead." #-}

-- | Indicates whether users can access their WorkSpaces through a web browser.
--
-- /Note:/ Consider using 'deviceTypeWeb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wapDeviceTypeWeb :: Lens.Lens' WorkspaceAccessProperties (Lude.Maybe AccessPropertyValue)
wapDeviceTypeWeb = Lens.lens (deviceTypeWeb :: WorkspaceAccessProperties -> Lude.Maybe AccessPropertyValue) (\s a -> s {deviceTypeWeb = a} :: WorkspaceAccessProperties)
{-# DEPRECATED wapDeviceTypeWeb "Use generic-lens or generic-optics with 'deviceTypeWeb' instead." #-}

-- | Indicates whether users can use Android devices to access their WorkSpaces.
--
-- /Note:/ Consider using 'deviceTypeAndroid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wapDeviceTypeAndroid :: Lens.Lens' WorkspaceAccessProperties (Lude.Maybe AccessPropertyValue)
wapDeviceTypeAndroid = Lens.lens (deviceTypeAndroid :: WorkspaceAccessProperties -> Lude.Maybe AccessPropertyValue) (\s a -> s {deviceTypeAndroid = a} :: WorkspaceAccessProperties)
{-# DEPRECATED wapDeviceTypeAndroid "Use generic-lens or generic-optics with 'deviceTypeAndroid' instead." #-}

-- | Indicates whether users can use macOS clients to access their WorkSpaces. To restrict WorkSpaces access to trusted devices (also known as managed devices) with valid certificates, specify a value of @TRUST@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/trusted-devices.html Restrict WorkSpaces Access to Trusted Devices> .
--
-- /Note:/ Consider using 'deviceTypeOSx' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wapDeviceTypeOSx :: Lens.Lens' WorkspaceAccessProperties (Lude.Maybe AccessPropertyValue)
wapDeviceTypeOSx = Lens.lens (deviceTypeOSx :: WorkspaceAccessProperties -> Lude.Maybe AccessPropertyValue) (\s a -> s {deviceTypeOSx = a} :: WorkspaceAccessProperties)
{-# DEPRECATED wapDeviceTypeOSx "Use generic-lens or generic-optics with 'deviceTypeOSx' instead." #-}

-- | Indicates whether users can use Chromebooks to access their WorkSpaces.
--
-- /Note:/ Consider using 'deviceTypeChromeOS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wapDeviceTypeChromeOS :: Lens.Lens' WorkspaceAccessProperties (Lude.Maybe AccessPropertyValue)
wapDeviceTypeChromeOS = Lens.lens (deviceTypeChromeOS :: WorkspaceAccessProperties -> Lude.Maybe AccessPropertyValue) (\s a -> s {deviceTypeChromeOS = a} :: WorkspaceAccessProperties)
{-# DEPRECATED wapDeviceTypeChromeOS "Use generic-lens or generic-optics with 'deviceTypeChromeOS' instead." #-}

-- | Indicates whether users can use iOS devices to access their WorkSpaces.
--
-- /Note:/ Consider using 'deviceTypeIOS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wapDeviceTypeIOS :: Lens.Lens' WorkspaceAccessProperties (Lude.Maybe AccessPropertyValue)
wapDeviceTypeIOS = Lens.lens (deviceTypeIOS :: WorkspaceAccessProperties -> Lude.Maybe AccessPropertyValue) (\s a -> s {deviceTypeIOS = a} :: WorkspaceAccessProperties)
{-# DEPRECATED wapDeviceTypeIOS "Use generic-lens or generic-optics with 'deviceTypeIOS' instead." #-}

-- | Indicates whether users can use zero client devices to access their WorkSpaces.
--
-- /Note:/ Consider using 'deviceTypeZeroClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wapDeviceTypeZeroClient :: Lens.Lens' WorkspaceAccessProperties (Lude.Maybe AccessPropertyValue)
wapDeviceTypeZeroClient = Lens.lens (deviceTypeZeroClient :: WorkspaceAccessProperties -> Lude.Maybe AccessPropertyValue) (\s a -> s {deviceTypeZeroClient = a} :: WorkspaceAccessProperties)
{-# DEPRECATED wapDeviceTypeZeroClient "Use generic-lens or generic-optics with 'deviceTypeZeroClient' instead." #-}

instance Lude.FromJSON WorkspaceAccessProperties where
  parseJSON =
    Lude.withObject
      "WorkspaceAccessProperties"
      ( \x ->
          WorkspaceAccessProperties'
            Lude.<$> (x Lude..:? "DeviceTypeWindows")
            Lude.<*> (x Lude..:? "DeviceTypeWeb")
            Lude.<*> (x Lude..:? "DeviceTypeAndroid")
            Lude.<*> (x Lude..:? "DeviceTypeOsx")
            Lude.<*> (x Lude..:? "DeviceTypeChromeOs")
            Lude.<*> (x Lude..:? "DeviceTypeIos")
            Lude.<*> (x Lude..:? "DeviceTypeZeroClient")
      )

instance Lude.ToJSON WorkspaceAccessProperties where
  toJSON WorkspaceAccessProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeviceTypeWindows" Lude..=) Lude.<$> deviceTypeWindows,
            ("DeviceTypeWeb" Lude..=) Lude.<$> deviceTypeWeb,
            ("DeviceTypeAndroid" Lude..=) Lude.<$> deviceTypeAndroid,
            ("DeviceTypeOsx" Lude..=) Lude.<$> deviceTypeOSx,
            ("DeviceTypeChromeOs" Lude..=) Lude.<$> deviceTypeChromeOS,
            ("DeviceTypeIos" Lude..=) Lude.<$> deviceTypeIOS,
            ("DeviceTypeZeroClient" Lude..=) Lude.<$> deviceTypeZeroClient
          ]
      )
