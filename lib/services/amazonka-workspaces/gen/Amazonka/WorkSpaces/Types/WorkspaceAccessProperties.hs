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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.WorkspaceAccessProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    -- | Indicates whether users can use Chromebooks to access their WorkSpaces.
    deviceTypeChromeOs :: Prelude.Maybe AccessPropertyValue,
    -- | Indicates whether users can use iOS devices to access their WorkSpaces.
    deviceTypeIos :: Prelude.Maybe AccessPropertyValue,
    -- | Indicates whether users can use Linux clients to access their
    -- WorkSpaces.
    deviceTypeLinux :: Prelude.Maybe AccessPropertyValue,
    -- | Indicates whether users can use macOS clients to access their
    -- WorkSpaces.
    deviceTypeOsx :: Prelude.Maybe AccessPropertyValue,
    -- | Indicates whether users can access their WorkSpaces through a web
    -- browser.
    deviceTypeWeb :: Prelude.Maybe AccessPropertyValue,
    -- | Indicates whether users can use Windows clients to access their
    -- WorkSpaces.
    deviceTypeWindows :: Prelude.Maybe AccessPropertyValue,
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
-- 'deviceTypeChromeOs', 'workspaceAccessProperties_deviceTypeChromeOs' - Indicates whether users can use Chromebooks to access their WorkSpaces.
--
-- 'deviceTypeIos', 'workspaceAccessProperties_deviceTypeIos' - Indicates whether users can use iOS devices to access their WorkSpaces.
--
-- 'deviceTypeLinux', 'workspaceAccessProperties_deviceTypeLinux' - Indicates whether users can use Linux clients to access their
-- WorkSpaces.
--
-- 'deviceTypeOsx', 'workspaceAccessProperties_deviceTypeOsx' - Indicates whether users can use macOS clients to access their
-- WorkSpaces.
--
-- 'deviceTypeWeb', 'workspaceAccessProperties_deviceTypeWeb' - Indicates whether users can access their WorkSpaces through a web
-- browser.
--
-- 'deviceTypeWindows', 'workspaceAccessProperties_deviceTypeWindows' - Indicates whether users can use Windows clients to access their
-- WorkSpaces.
--
-- 'deviceTypeZeroClient', 'workspaceAccessProperties_deviceTypeZeroClient' - Indicates whether users can use zero client devices to access their
-- WorkSpaces.
newWorkspaceAccessProperties ::
  WorkspaceAccessProperties
newWorkspaceAccessProperties =
  WorkspaceAccessProperties'
    { deviceTypeAndroid =
        Prelude.Nothing,
      deviceTypeChromeOs = Prelude.Nothing,
      deviceTypeIos = Prelude.Nothing,
      deviceTypeLinux = Prelude.Nothing,
      deviceTypeOsx = Prelude.Nothing,
      deviceTypeWeb = Prelude.Nothing,
      deviceTypeWindows = Prelude.Nothing,
      deviceTypeZeroClient = Prelude.Nothing
    }

-- | Indicates whether users can use Android and Android-compatible Chrome OS
-- devices to access their WorkSpaces.
workspaceAccessProperties_deviceTypeAndroid :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeAndroid = Lens.lens (\WorkspaceAccessProperties' {deviceTypeAndroid} -> deviceTypeAndroid) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeAndroid = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use Chromebooks to access their WorkSpaces.
workspaceAccessProperties_deviceTypeChromeOs :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeChromeOs = Lens.lens (\WorkspaceAccessProperties' {deviceTypeChromeOs} -> deviceTypeChromeOs) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeChromeOs = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use iOS devices to access their WorkSpaces.
workspaceAccessProperties_deviceTypeIos :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeIos = Lens.lens (\WorkspaceAccessProperties' {deviceTypeIos} -> deviceTypeIos) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeIos = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use Linux clients to access their
-- WorkSpaces.
workspaceAccessProperties_deviceTypeLinux :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeLinux = Lens.lens (\WorkspaceAccessProperties' {deviceTypeLinux} -> deviceTypeLinux) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeLinux = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use macOS clients to access their
-- WorkSpaces.
workspaceAccessProperties_deviceTypeOsx :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeOsx = Lens.lens (\WorkspaceAccessProperties' {deviceTypeOsx} -> deviceTypeOsx) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeOsx = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can access their WorkSpaces through a web
-- browser.
workspaceAccessProperties_deviceTypeWeb :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeWeb = Lens.lens (\WorkspaceAccessProperties' {deviceTypeWeb} -> deviceTypeWeb) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeWeb = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use Windows clients to access their
-- WorkSpaces.
workspaceAccessProperties_deviceTypeWindows :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeWindows = Lens.lens (\WorkspaceAccessProperties' {deviceTypeWindows} -> deviceTypeWindows) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeWindows = a} :: WorkspaceAccessProperties)

-- | Indicates whether users can use zero client devices to access their
-- WorkSpaces.
workspaceAccessProperties_deviceTypeZeroClient :: Lens.Lens' WorkspaceAccessProperties (Prelude.Maybe AccessPropertyValue)
workspaceAccessProperties_deviceTypeZeroClient = Lens.lens (\WorkspaceAccessProperties' {deviceTypeZeroClient} -> deviceTypeZeroClient) (\s@WorkspaceAccessProperties' {} a -> s {deviceTypeZeroClient = a} :: WorkspaceAccessProperties)

instance Data.FromJSON WorkspaceAccessProperties where
  parseJSON =
    Data.withObject
      "WorkspaceAccessProperties"
      ( \x ->
          WorkspaceAccessProperties'
            Prelude.<$> (x Data..:? "DeviceTypeAndroid")
            Prelude.<*> (x Data..:? "DeviceTypeChromeOs")
            Prelude.<*> (x Data..:? "DeviceTypeIos")
            Prelude.<*> (x Data..:? "DeviceTypeLinux")
            Prelude.<*> (x Data..:? "DeviceTypeOsx")
            Prelude.<*> (x Data..:? "DeviceTypeWeb")
            Prelude.<*> (x Data..:? "DeviceTypeWindows")
            Prelude.<*> (x Data..:? "DeviceTypeZeroClient")
      )

instance Prelude.Hashable WorkspaceAccessProperties where
  hashWithSalt _salt WorkspaceAccessProperties' {..} =
    _salt
      `Prelude.hashWithSalt` deviceTypeAndroid
      `Prelude.hashWithSalt` deviceTypeChromeOs
      `Prelude.hashWithSalt` deviceTypeIos
      `Prelude.hashWithSalt` deviceTypeLinux
      `Prelude.hashWithSalt` deviceTypeOsx
      `Prelude.hashWithSalt` deviceTypeWeb
      `Prelude.hashWithSalt` deviceTypeWindows
      `Prelude.hashWithSalt` deviceTypeZeroClient

instance Prelude.NFData WorkspaceAccessProperties where
  rnf WorkspaceAccessProperties' {..} =
    Prelude.rnf deviceTypeAndroid
      `Prelude.seq` Prelude.rnf deviceTypeChromeOs
      `Prelude.seq` Prelude.rnf deviceTypeIos
      `Prelude.seq` Prelude.rnf deviceTypeLinux
      `Prelude.seq` Prelude.rnf deviceTypeOsx
      `Prelude.seq` Prelude.rnf deviceTypeWeb
      `Prelude.seq` Prelude.rnf deviceTypeWindows
      `Prelude.seq` Prelude.rnf deviceTypeZeroClient

instance Data.ToJSON WorkspaceAccessProperties where
  toJSON WorkspaceAccessProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeviceTypeAndroid" Data..=)
              Prelude.<$> deviceTypeAndroid,
            ("DeviceTypeChromeOs" Data..=)
              Prelude.<$> deviceTypeChromeOs,
            ("DeviceTypeIos" Data..=) Prelude.<$> deviceTypeIos,
            ("DeviceTypeLinux" Data..=)
              Prelude.<$> deviceTypeLinux,
            ("DeviceTypeOsx" Data..=) Prelude.<$> deviceTypeOsx,
            ("DeviceTypeWeb" Data..=) Prelude.<$> deviceTypeWeb,
            ("DeviceTypeWindows" Data..=)
              Prelude.<$> deviceTypeWindows,
            ("DeviceTypeZeroClient" Data..=)
              Prelude.<$> deviceTypeZeroClient
          ]
      )
