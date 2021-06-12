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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DeviceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DeviceType where

import Network.AWS.CognitoIdentityProvider.Types.AttributeType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The device type.
--
-- /See:/ 'newDeviceType' smart constructor.
data DeviceType = DeviceType'
  { -- | The last modified date of the device.
    deviceLastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The device key.
    deviceKey :: Core.Maybe Core.Text,
    -- | The date in which the device was last authenticated.
    deviceLastAuthenticatedDate :: Core.Maybe Core.POSIX,
    -- | The creation date of the device.
    deviceCreateDate :: Core.Maybe Core.POSIX,
    -- | The device attributes.
    deviceAttributes :: Core.Maybe [AttributeType]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceLastModifiedDate', 'deviceType_deviceLastModifiedDate' - The last modified date of the device.
--
-- 'deviceKey', 'deviceType_deviceKey' - The device key.
--
-- 'deviceLastAuthenticatedDate', 'deviceType_deviceLastAuthenticatedDate' - The date in which the device was last authenticated.
--
-- 'deviceCreateDate', 'deviceType_deviceCreateDate' - The creation date of the device.
--
-- 'deviceAttributes', 'deviceType_deviceAttributes' - The device attributes.
newDeviceType ::
  DeviceType
newDeviceType =
  DeviceType'
    { deviceLastModifiedDate = Core.Nothing,
      deviceKey = Core.Nothing,
      deviceLastAuthenticatedDate = Core.Nothing,
      deviceCreateDate = Core.Nothing,
      deviceAttributes = Core.Nothing
    }

-- | The last modified date of the device.
deviceType_deviceLastModifiedDate :: Lens.Lens' DeviceType (Core.Maybe Core.UTCTime)
deviceType_deviceLastModifiedDate = Lens.lens (\DeviceType' {deviceLastModifiedDate} -> deviceLastModifiedDate) (\s@DeviceType' {} a -> s {deviceLastModifiedDate = a} :: DeviceType) Core.. Lens.mapping Core._Time

-- | The device key.
deviceType_deviceKey :: Lens.Lens' DeviceType (Core.Maybe Core.Text)
deviceType_deviceKey = Lens.lens (\DeviceType' {deviceKey} -> deviceKey) (\s@DeviceType' {} a -> s {deviceKey = a} :: DeviceType)

-- | The date in which the device was last authenticated.
deviceType_deviceLastAuthenticatedDate :: Lens.Lens' DeviceType (Core.Maybe Core.UTCTime)
deviceType_deviceLastAuthenticatedDate = Lens.lens (\DeviceType' {deviceLastAuthenticatedDate} -> deviceLastAuthenticatedDate) (\s@DeviceType' {} a -> s {deviceLastAuthenticatedDate = a} :: DeviceType) Core.. Lens.mapping Core._Time

-- | The creation date of the device.
deviceType_deviceCreateDate :: Lens.Lens' DeviceType (Core.Maybe Core.UTCTime)
deviceType_deviceCreateDate = Lens.lens (\DeviceType' {deviceCreateDate} -> deviceCreateDate) (\s@DeviceType' {} a -> s {deviceCreateDate = a} :: DeviceType) Core.. Lens.mapping Core._Time

-- | The device attributes.
deviceType_deviceAttributes :: Lens.Lens' DeviceType (Core.Maybe [AttributeType])
deviceType_deviceAttributes = Lens.lens (\DeviceType' {deviceAttributes} -> deviceAttributes) (\s@DeviceType' {} a -> s {deviceAttributes = a} :: DeviceType) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON DeviceType where
  parseJSON =
    Core.withObject
      "DeviceType"
      ( \x ->
          DeviceType'
            Core.<$> (x Core..:? "DeviceLastModifiedDate")
            Core.<*> (x Core..:? "DeviceKey")
            Core.<*> (x Core..:? "DeviceLastAuthenticatedDate")
            Core.<*> (x Core..:? "DeviceCreateDate")
            Core.<*> (x Core..:? "DeviceAttributes" Core..!= Core.mempty)
      )

instance Core.Hashable DeviceType

instance Core.NFData DeviceType
