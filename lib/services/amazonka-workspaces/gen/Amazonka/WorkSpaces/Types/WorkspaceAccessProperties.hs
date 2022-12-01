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
-- Module      : Amazonka.WorkSpaces.Types.WorkspaceAccessProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.WorkspaceAccessProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.AccessPropertyValue

-- | The device types and operating systems that can be used to access a
-- WorkSpace. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspaces-network-requirements.html Amazon WorkSpaces Client Network Requirements>.
--
-- /See:/ 'newWorkspaceAccessProperties' smart constructor.
data WorkspaceAccessProperties = WorkspaceAccessProperties'
  { -- | Indicates whether users can use Android and Android-compatible Chrome OS
    -- devices to access their WorkSpaces.
    deviceTypeAndroid :: Prelude.Maybe AccessPropertyValue,
    -- | Indicates whether users can use Linux clients to access their
    -- WorkSpaces.
    deviceTypeLinux :: Prelude.Maybe AccessPropertyValue,
    -- | Indicates whether users can access their WorkSpaces through a web
    -- browser.
    deviceTypeWeb :: Prelude.Maybe AccessPropertyValue,
    -- | Indicates whether users can use macOS clients to access their
    -- WorkSpaces.
    deviceTypeOsx :: Prelude.Maybe AccessPropertyValue,
    -- | Indicates whether users can use Chromebooks to access their WorkSpaces.
    deviceTypeChromeOs :: Prelude.Maybe AccessPropertyValue,
    -- | Indicates whether users can use Windows clients to access their
    -- WorkSpaces.
    deviceTypeWindows :: Prelude.Maybe AccessPropertyValue,
    -- | Indicates whether users can use iOS devices to access their WorkSpaces.
    deviceTypeIos :: Prelude.Maybe AccessPropertyValue,
    -- | Indicates whether users can use zero client devices to access their
    -- WorkSpaces.
    deviceTypeZeroClient :: Prelude.Maybe AccessPropertyValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceAccessProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceTypeAndroid', 'workspaceAccessProperties_deviceTypeAndroid' - Indicates whether users can use Android and Android-compatible Chrome OS
-- devices to access their WorkSpaces.
--
-- 'deviceTypeLinux', 'workspaceAccessProperties_deviceTypeLinux' - Indicates whether users can use Linux clients to access their
-- WorkSpaces.
--
-- 'deviceTypeWeb', 'workspaceAccessProperties_deviceTypeWeb' - Indicates whether users can access their WorkSpaces through a web
-- browser.
--
-- 'deviceTypeOsx', 'workspaceAccessProperties_deviceTypeOsx' - Indicates whether users can use macOS clients to access their
-- WorkSpaces.
--
-- 'deviceTypeChromeOs', 'workspaceAccessProperties_deviceTypeChromeOs' - Indicates whether users can use Chromebooks to access their WorkSpaces.
--
-- 'deviceTypeWindows', 'workspaceAccessProperties_deviceTypeWindows' - Indicates whether users can use Windows clients to access their
-- WorkSpaces.
--
-- 'deviceTypeIos', 'workspaceAccessProperties_deviceTypeIos' - Indicates whether users can use iOS devices to access their WorkSpaces.
--
-- 'deviceTypeZeroClient', 'workspaceAccessProperties_deviceTypeZeroClient' - Indicates whether users can use zero client devices to access their
-- WorkSpaces.
newWorkspaceAccessProperties ::
  WorkspaceAccessProperties
newWorkspaceAccessProperties =
  WorkspaceAccessProperties'
    { deviceTypeAndroid =
        Prelude.Nothing,
      deviceTypeLinux = Prelude.Nothing,
      deviceTypeWeb = Prelude.Nothing,
      deviceTypeOsx = Prelude.Nothing,
      deviceTypeChromeOs = Prelude.Nothing,
      deviceTypeWindows = Prelude.Nothing,
      deviceTypeIos = Prelude.Nothing,
      deviceTypeZeroClient = Prelude.Nothing
    }

-- | Indicates whether users can use Android and Android-compatible Chrome OS
-- devices to access their WorkSpaces.
workspaceAccessProperties_deviceTypeAndroid :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeAndroid = Lens.lens (\WorkspaceAccessProperties' {deviceTypeAndroid} -> deviceTypeAndroid) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeAndroid = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use Linux clients to access their
-- WorkSpaces.
workspaceAccessProperties_deviceTypeLinux :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeLinux = Lens.lens (\WorkspaceAccessProperties' {deviceTypeLinux} -> deviceTypeLinux) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeLinux = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can access their WorkSpaces through a web
-- browser.
workspaceAccessProperties_deviceTypeWeb :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeWeb = Lens.lens (\WorkspaceAccessProperties' {deviceTypeWeb} -> deviceTypeWeb) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeWeb = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use macOS clients to access their
-- WorkSpaces.
workspaceAccessProperties_deviceTypeOsx :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeOsx = Lens.lens (\WorkspaceAccessProperties' {deviceTypeOsx} -> deviceTypeOsx) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeOsx = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use Chromebooks to access their WorkSpaces.
workspaceAccessProperties_deviceTypeChromeOs :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeChromeOs = Lens.lens (\WorkspaceAccessProperties' {deviceTypeChromeOs} -> deviceTypeChromeOs) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeChromeOs = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use Windows clients to access their
-- WorkSpaces.
workspaceAccessProperties_deviceTypeWindows :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeWindows = Lens.lens (\WorkspaceAccessProperties' {deviceTypeWindows} -> deviceTypeWindows) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeWindows = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use iOS devices to access their WorkSpaces.
workspaceAccessProperties_deviceTypeIos :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeIos = Lens.lens (\WorkspaceAccessProperties' {deviceTypeIos} -> deviceTypeIos) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeIos = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use zero client devices to access their
-- WorkSpaces.
workspaceAccessProperties_deviceTypeZeroClient :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeZeroClient = Lens.lens (\WorkspaceAccessProperties' {deviceTypeZeroClient} -> deviceTypeZeroClient) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeZeroClient = a} :: WorkspaceAccessProperties)

instance Core.FromJSON WorkspaceAccessProperties where
  parseJSON =
    Core.withObject
      "WorkspaceAccessProperties"
      ( \x ->
          WorkspaceAccessProperties'
            Prelude.<$> (x Core..:? "DeviceTypeAndroid")
            Prelude.<*> (x Core..:? "DeviceTypeLinux")
            Prelude.<*> (x Core..:? "DeviceTypeWeb")
            Prelude.<*> (x Core..:? "DeviceTypeOsx")
            Prelude.<*> (x Core..:? "DeviceTypeChromeOs")
            Prelude.<*> (x Core..:? "DeviceTypeWindows")
            Prelude.<*> (x Core..:? "DeviceTypeIos")
            Prelude.<*> (x Core..:? "DeviceTypeZeroClient")
      )

instance Prelude.Hashable WorkspaceAccessProperties where
  hashWithSalt _salt WorkspaceAccessProperties' {..} =
    _salt `Prelude.hashWithSalt` deviceTypeAndroid
      `Prelude.hashWithSalt` deviceTypeLinux
      `Prelude.hashWithSalt` deviceTypeWeb
      `Prelude.hashWithSalt` deviceTypeOsx
      `Prelude.hashWithSalt` deviceTypeChromeOs
      `Prelude.hashWithSalt` deviceTypeWindows
      `Prelude.hashWithSalt` deviceTypeIos
      `Prelude.hashWithSalt` deviceTypeZeroClient

instance Prelude.NFData WorkspaceAccessProperties where
  rnf WorkspaceAccessProperties' {..} =
    Prelude.rnf deviceTypeAndroid
      `Prelude.seq` Prelude.rnf deviceTypeLinux
      `Prelude.seq` Prelude.rnf deviceTypeWeb
      `Prelude.seq` Prelude.rnf deviceTypeOsx
      `Prelude.seq` Prelude.rnf deviceTypeChromeOs
      `Prelude.seq` Prelude.rnf deviceTypeWindows
      `Prelude.seq` Prelude.rnf deviceTypeIos
      `Prelude.seq` Prelude.rnf deviceTypeZeroClient

instance Core.ToJSON WorkspaceAccessProperties where
  toJSON WorkspaceAccessProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DeviceTypeAndroid" Core..=)
              Prelude.<$> deviceTypeAndroid,
            ("DeviceTypeLinux" Core..=)
              Prelude.<$> deviceTypeLinux,
            ("DeviceTypeWeb" Core..=) Prelude.<$> deviceTypeWeb,
            ("DeviceTypeOsx" Core..=) Prelude.<$> deviceTypeOsx,
            ("DeviceTypeChromeOs" Core..=)
              Prelude.<$> deviceTypeChromeOs,
            ("DeviceTypeWindows" Core..=)
              Prelude.<$> deviceTypeWindows,
            ("DeviceTypeIos" Core..=) Prelude.<$> deviceTypeIos,
            ("DeviceTypeZeroClient" Core..=)
              Prelude.<$> deviceTypeZeroClient
          ]
      )
