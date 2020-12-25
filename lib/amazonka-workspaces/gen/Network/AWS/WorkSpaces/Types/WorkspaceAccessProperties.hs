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
    wapDeviceTypeAndroid,
    wapDeviceTypeChromeOs,
    wapDeviceTypeIos,
    wapDeviceTypeOsx,
    wapDeviceTypeWeb,
    wapDeviceTypeWindows,
    wapDeviceTypeZeroClient,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.AccessPropertyValue as Types

-- | The device types and operating systems that can be used to access a WorkSpace. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspaces-network-requirements.html Amazon WorkSpaces Client Network Requirements> .
--
-- /See:/ 'mkWorkspaceAccessProperties' smart constructor.
data WorkspaceAccessProperties = WorkspaceAccessProperties'
  { -- | Indicates whether users can use Android devices to access their WorkSpaces.
    deviceTypeAndroid :: Core.Maybe Types.AccessPropertyValue,
    -- | Indicates whether users can use Chromebooks to access their WorkSpaces.
    deviceTypeChromeOs :: Core.Maybe Types.AccessPropertyValue,
    -- | Indicates whether users can use iOS devices to access their WorkSpaces.
    deviceTypeIos :: Core.Maybe Types.AccessPropertyValue,
    -- | Indicates whether users can use macOS clients to access their WorkSpaces. To restrict WorkSpaces access to trusted devices (also known as managed devices) with valid certificates, specify a value of @TRUST@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/trusted-devices.html Restrict WorkSpaces Access to Trusted Devices> .
    deviceTypeOsx :: Core.Maybe Types.AccessPropertyValue,
    -- | Indicates whether users can access their WorkSpaces through a web browser.
    deviceTypeWeb :: Core.Maybe Types.AccessPropertyValue,
    -- | Indicates whether users can use Windows clients to access their WorkSpaces. To restrict WorkSpaces access to trusted devices (also known as managed devices) with valid certificates, specify a value of @TRUST@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/trusted-devices.html Restrict WorkSpaces Access to Trusted Devices> .
    deviceTypeWindows :: Core.Maybe Types.AccessPropertyValue,
    -- | Indicates whether users can use zero client devices to access their WorkSpaces.
    deviceTypeZeroClient :: Core.Maybe Types.AccessPropertyValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkspaceAccessProperties' value with any optional fields omitted.
mkWorkspaceAccessProperties ::
  WorkspaceAccessProperties
mkWorkspaceAccessProperties =
  WorkspaceAccessProperties'
    { deviceTypeAndroid = Core.Nothing,
      deviceTypeChromeOs = Core.Nothing,
      deviceTypeIos = Core.Nothing,
      deviceTypeOsx = Core.Nothing,
      deviceTypeWeb = Core.Nothing,
      deviceTypeWindows = Core.Nothing,
      deviceTypeZeroClient = Core.Nothing
    }

-- | Indicates whether users can use Android devices to access their WorkSpaces.
--
-- /Note:/ Consider using 'deviceTypeAndroid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wapDeviceTypeAndroid :: Lens.Lens' WorkspaceAccessProperties (Core.Maybe Types.AccessPropertyValue)
wapDeviceTypeAndroid = Lens.field @"deviceTypeAndroid"
{-# DEPRECATED wapDeviceTypeAndroid "Use generic-lens or generic-optics with 'deviceTypeAndroid' instead." #-}

-- | Indicates whether users can use Chromebooks to access their WorkSpaces.
--
-- /Note:/ Consider using 'deviceTypeChromeOs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wapDeviceTypeChromeOs :: Lens.Lens' WorkspaceAccessProperties (Core.Maybe Types.AccessPropertyValue)
wapDeviceTypeChromeOs = Lens.field @"deviceTypeChromeOs"
{-# DEPRECATED wapDeviceTypeChromeOs "Use generic-lens or generic-optics with 'deviceTypeChromeOs' instead." #-}

-- | Indicates whether users can use iOS devices to access their WorkSpaces.
--
-- /Note:/ Consider using 'deviceTypeIos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wapDeviceTypeIos :: Lens.Lens' WorkspaceAccessProperties (Core.Maybe Types.AccessPropertyValue)
wapDeviceTypeIos = Lens.field @"deviceTypeIos"
{-# DEPRECATED wapDeviceTypeIos "Use generic-lens or generic-optics with 'deviceTypeIos' instead." #-}

-- | Indicates whether users can use macOS clients to access their WorkSpaces. To restrict WorkSpaces access to trusted devices (also known as managed devices) with valid certificates, specify a value of @TRUST@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/trusted-devices.html Restrict WorkSpaces Access to Trusted Devices> .
--
-- /Note:/ Consider using 'deviceTypeOsx' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wapDeviceTypeOsx :: Lens.Lens' WorkspaceAccessProperties (Core.Maybe Types.AccessPropertyValue)
wapDeviceTypeOsx = Lens.field @"deviceTypeOsx"
{-# DEPRECATED wapDeviceTypeOsx "Use generic-lens or generic-optics with 'deviceTypeOsx' instead." #-}

-- | Indicates whether users can access their WorkSpaces through a web browser.
--
-- /Note:/ Consider using 'deviceTypeWeb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wapDeviceTypeWeb :: Lens.Lens' WorkspaceAccessProperties (Core.Maybe Types.AccessPropertyValue)
wapDeviceTypeWeb = Lens.field @"deviceTypeWeb"
{-# DEPRECATED wapDeviceTypeWeb "Use generic-lens or generic-optics with 'deviceTypeWeb' instead." #-}

-- | Indicates whether users can use Windows clients to access their WorkSpaces. To restrict WorkSpaces access to trusted devices (also known as managed devices) with valid certificates, specify a value of @TRUST@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/trusted-devices.html Restrict WorkSpaces Access to Trusted Devices> .
--
-- /Note:/ Consider using 'deviceTypeWindows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wapDeviceTypeWindows :: Lens.Lens' WorkspaceAccessProperties (Core.Maybe Types.AccessPropertyValue)
wapDeviceTypeWindows = Lens.field @"deviceTypeWindows"
{-# DEPRECATED wapDeviceTypeWindows "Use generic-lens or generic-optics with 'deviceTypeWindows' instead." #-}

-- | Indicates whether users can use zero client devices to access their WorkSpaces.
--
-- /Note:/ Consider using 'deviceTypeZeroClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wapDeviceTypeZeroClient :: Lens.Lens' WorkspaceAccessProperties (Core.Maybe Types.AccessPropertyValue)
wapDeviceTypeZeroClient = Lens.field @"deviceTypeZeroClient"
{-# DEPRECATED wapDeviceTypeZeroClient "Use generic-lens or generic-optics with 'deviceTypeZeroClient' instead." #-}

instance Core.FromJSON WorkspaceAccessProperties where
  toJSON WorkspaceAccessProperties {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeviceTypeAndroid" Core..=) Core.<$> deviceTypeAndroid,
            ("DeviceTypeChromeOs" Core..=) Core.<$> deviceTypeChromeOs,
            ("DeviceTypeIos" Core..=) Core.<$> deviceTypeIos,
            ("DeviceTypeOsx" Core..=) Core.<$> deviceTypeOsx,
            ("DeviceTypeWeb" Core..=) Core.<$> deviceTypeWeb,
            ("DeviceTypeWindows" Core..=) Core.<$> deviceTypeWindows,
            ("DeviceTypeZeroClient" Core..=) Core.<$> deviceTypeZeroClient
          ]
      )

instance Core.FromJSON WorkspaceAccessProperties where
  parseJSON =
    Core.withObject "WorkspaceAccessProperties" Core.$
      \x ->
        WorkspaceAccessProperties'
          Core.<$> (x Core..:? "DeviceTypeAndroid")
          Core.<*> (x Core..:? "DeviceTypeChromeOs")
          Core.<*> (x Core..:? "DeviceTypeIos")
          Core.<*> (x Core..:? "DeviceTypeOsx")
          Core.<*> (x Core..:? "DeviceTypeWeb")
          Core.<*> (x Core..:? "DeviceTypeWindows")
          Core.<*> (x Core..:? "DeviceTypeZeroClient")
