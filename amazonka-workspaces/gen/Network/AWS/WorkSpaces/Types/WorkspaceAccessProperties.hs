{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceAccessProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceAccessProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkSpaces.Types.AccessPropertyValue

-- | The device types and operating systems that can be used to access a
-- WorkSpace. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspaces-network-requirements.html Amazon WorkSpaces Client Network Requirements>.
--
-- /See:/ 'newWorkspaceAccessProperties' smart constructor.
data WorkspaceAccessProperties = WorkspaceAccessProperties'
  { -- | Indicates whether users can use macOS clients to access their
    -- WorkSpaces. To restrict WorkSpaces access to trusted devices (also known
    -- as managed devices) with valid certificates, specify a value of @TRUST@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/trusted-devices.html Restrict WorkSpaces Access to Trusted Devices>.
    deviceTypeOsx :: Core.Maybe AccessPropertyValue,
    -- | Indicates whether users can use Windows clients to access their
    -- WorkSpaces. To restrict WorkSpaces access to trusted devices (also known
    -- as managed devices) with valid certificates, specify a value of @TRUST@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/trusted-devices.html Restrict WorkSpaces Access to Trusted Devices>.
    deviceTypeWindows :: Core.Maybe AccessPropertyValue,
    -- | Indicates whether users can use Android devices to access their
    -- WorkSpaces.
    deviceTypeAndroid :: Core.Maybe AccessPropertyValue,
    -- | Indicates whether users can use zero client devices to access their
    -- WorkSpaces.
    deviceTypeZeroClient :: Core.Maybe AccessPropertyValue,
    -- | Indicates whether users can access their WorkSpaces through a web
    -- browser.
    deviceTypeWeb :: Core.Maybe AccessPropertyValue,
    -- | Indicates whether users can use iOS devices to access their WorkSpaces.
    deviceTypeIos :: Core.Maybe AccessPropertyValue,
    -- | Indicates whether users can use Chromebooks to access their WorkSpaces.
    deviceTypeChromeOs :: Core.Maybe AccessPropertyValue
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WorkspaceAccessProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceTypeOsx', 'workspaceAccessProperties_deviceTypeOsx' - Indicates whether users can use macOS clients to access their
-- WorkSpaces. To restrict WorkSpaces access to trusted devices (also known
-- as managed devices) with valid certificates, specify a value of @TRUST@.
-- For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/trusted-devices.html Restrict WorkSpaces Access to Trusted Devices>.
--
-- 'deviceTypeWindows', 'workspaceAccessProperties_deviceTypeWindows' - Indicates whether users can use Windows clients to access their
-- WorkSpaces. To restrict WorkSpaces access to trusted devices (also known
-- as managed devices) with valid certificates, specify a value of @TRUST@.
-- For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/trusted-devices.html Restrict WorkSpaces Access to Trusted Devices>.
--
-- 'deviceTypeAndroid', 'workspaceAccessProperties_deviceTypeAndroid' - Indicates whether users can use Android devices to access their
-- WorkSpaces.
--
-- 'deviceTypeZeroClient', 'workspaceAccessProperties_deviceTypeZeroClient' - Indicates whether users can use zero client devices to access their
-- WorkSpaces.
--
-- 'deviceTypeWeb', 'workspaceAccessProperties_deviceTypeWeb' - Indicates whether users can access their WorkSpaces through a web
-- browser.
--
-- 'deviceTypeIos', 'workspaceAccessProperties_deviceTypeIos' - Indicates whether users can use iOS devices to access their WorkSpaces.
--
-- 'deviceTypeChromeOs', 'workspaceAccessProperties_deviceTypeChromeOs' - Indicates whether users can use Chromebooks to access their WorkSpaces.
newWorkspaceAccessProperties ::
  WorkspaceAccessProperties
newWorkspaceAccessProperties =
  WorkspaceAccessProperties'
    { deviceTypeOsx =
        Core.Nothing,
      deviceTypeWindows = Core.Nothing,
      deviceTypeAndroid = Core.Nothing,
      deviceTypeZeroClient = Core.Nothing,
      deviceTypeWeb = Core.Nothing,
      deviceTypeIos = Core.Nothing,
      deviceTypeChromeOs = Core.Nothing
    }

-- | Indicates whether users can use macOS clients to access their
-- WorkSpaces. To restrict WorkSpaces access to trusted devices (also known
-- as managed devices) with valid certificates, specify a value of @TRUST@.
-- For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/trusted-devices.html Restrict WorkSpaces Access to Trusted Devices>.
workspaceAccessProperties_deviceTypeOsx :: Lens.Lens' WorkspaceAccessProperties (Core.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeOsx = Lens.lens (\WorkspaceAccessProperties' {deviceTypeOsx} -> deviceTypeOsx) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeOsx = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use Windows clients to access their
-- WorkSpaces. To restrict WorkSpaces access to trusted devices (also known
-- as managed devices) with valid certificates, specify a value of @TRUST@.
-- For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/trusted-devices.html Restrict WorkSpaces Access to Trusted Devices>.
workspaceAccessProperties_deviceTypeWindows :: Lens.Lens' WorkspaceAccessProperties (Core.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeWindows = Lens.lens (\WorkspaceAccessProperties' {deviceTypeWindows} -> deviceTypeWindows) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeWindows = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use Android devices to access their
-- WorkSpaces.
workspaceAccessProperties_deviceTypeAndroid :: Lens.Lens' WorkspaceAccessProperties (Core.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeAndroid = Lens.lens (\WorkspaceAccessProperties' {deviceTypeAndroid} -> deviceTypeAndroid) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeAndroid = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use zero client devices to access their
-- WorkSpaces.
workspaceAccessProperties_deviceTypeZeroClient :: Lens.Lens' WorkspaceAccessProperties (Core.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeZeroClient = Lens.lens (\WorkspaceAccessProperties' {deviceTypeZeroClient} -> deviceTypeZeroClient) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeZeroClient = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can access their WorkSpaces through a web
-- browser.
workspaceAccessProperties_deviceTypeWeb :: Lens.Lens' WorkspaceAccessProperties (Core.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeWeb = Lens.lens (\WorkspaceAccessProperties' {deviceTypeWeb} -> deviceTypeWeb) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeWeb = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use iOS devices to access their WorkSpaces.
workspaceAccessProperties_deviceTypeIos :: Lens.Lens' WorkspaceAccessProperties (Core.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeIos = Lens.lens (\WorkspaceAccessProperties' {deviceTypeIos} -> deviceTypeIos) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeIos = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use Chromebooks to access their WorkSpaces.
workspaceAccessProperties_deviceTypeChromeOs :: Lens.Lens' WorkspaceAccessProperties (Core.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeChromeOs = Lens.lens (\WorkspaceAccessProperties' {deviceTypeChromeOs} -> deviceTypeChromeOs) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeChromeOs = a} :: WorkspaceAccessProperties)

instance Core.FromJSON WorkspaceAccessProperties where
  parseJSON =
    Core.withObject
      "WorkspaceAccessProperties"
      ( \x ->
          WorkspaceAccessProperties'
            Core.<$> (x Core..:? "DeviceTypeOsx")
            Core.<*> (x Core..:? "DeviceTypeWindows")
            Core.<*> (x Core..:? "DeviceTypeAndroid")
            Core.<*> (x Core..:? "DeviceTypeZeroClient")
            Core.<*> (x Core..:? "DeviceTypeWeb")
            Core.<*> (x Core..:? "DeviceTypeIos")
            Core.<*> (x Core..:? "DeviceTypeChromeOs")
      )

instance Core.Hashable WorkspaceAccessProperties

instance Core.NFData WorkspaceAccessProperties

instance Core.ToJSON WorkspaceAccessProperties where
  toJSON WorkspaceAccessProperties' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeviceTypeOsx" Core..=) Core.<$> deviceTypeOsx,
            ("DeviceTypeWindows" Core..=)
              Core.<$> deviceTypeWindows,
            ("DeviceTypeAndroid" Core..=)
              Core.<$> deviceTypeAndroid,
            ("DeviceTypeZeroClient" Core..=)
              Core.<$> deviceTypeZeroClient,
            ("DeviceTypeWeb" Core..=) Core.<$> deviceTypeWeb,
            ("DeviceTypeIos" Core..=) Core.<$> deviceTypeIos,
            ("DeviceTypeChromeOs" Core..=)
              Core.<$> deviceTypeChromeOs
          ]
      )
