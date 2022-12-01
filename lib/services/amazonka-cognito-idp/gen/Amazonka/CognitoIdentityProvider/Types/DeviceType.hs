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
-- Module      : Amazonka.CognitoIdentityProvider.Types.DeviceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.DeviceType where

import Amazonka.CognitoIdentityProvider.Types.AttributeType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The device type.
--
-- /See:/ 'newDeviceType' smart constructor.
data DeviceType = DeviceType'
  { -- | The device key.
    deviceKey :: Prelude.Maybe Prelude.Text,
    -- | The date when the device was last authenticated.
    deviceLastAuthenticatedDate :: Prelude.Maybe Core.POSIX,
    -- | The device attributes.
    deviceAttributes :: Prelude.Maybe [AttributeType],
    -- | The last modified date of the device.
    deviceLastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The creation date of the device.
    deviceCreateDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceKey', 'deviceType_deviceKey' - The device key.
--
-- 'deviceLastAuthenticatedDate', 'deviceType_deviceLastAuthenticatedDate' - The date when the device was last authenticated.
--
-- 'deviceAttributes', 'deviceType_deviceAttributes' - The device attributes.
--
-- 'deviceLastModifiedDate', 'deviceType_deviceLastModifiedDate' - The last modified date of the device.
--
-- 'deviceCreateDate', 'deviceType_deviceCreateDate' - The creation date of the device.
newDeviceType ::
  DeviceType
newDeviceType =
  DeviceType'
    { deviceKey = Prelude.Nothing,
      deviceLastAuthenticatedDate = Prelude.Nothing,
      deviceAttributes = Prelude.Nothing,
      deviceLastModifiedDate = Prelude.Nothing,
      deviceCreateDate = Prelude.Nothing
    }

-- | The device key.
deviceType_deviceKey :: Lens.Lens' DeviceType (Prelude.Maybe Prelude.Text)
deviceType_deviceKey = Lens.lens (\DeviceType' {deviceKey} -> deviceKey) (\s@DeviceType' {} a -> s {deviceKey = a} :: DeviceType)

-- | The date when the device was last authenticated.
deviceType_deviceLastAuthenticatedDate :: Lens.Lens' DeviceType (Prelude.Maybe Prelude.UTCTime)
deviceType_deviceLastAuthenticatedDate = Lens.lens (\DeviceType' {deviceLastAuthenticatedDate} -> deviceLastAuthenticatedDate) (\s@DeviceType' {} a -> s {deviceLastAuthenticatedDate = a} :: DeviceType) Prelude.. Lens.mapping Core._Time

-- | The device attributes.
deviceType_deviceAttributes :: Lens.Lens' DeviceType (Prelude.Maybe [AttributeType])
deviceType_deviceAttributes = Lens.lens (\DeviceType' {deviceAttributes} -> deviceAttributes) (\s@DeviceType' {} a -> s {deviceAttributes = a} :: DeviceType) Prelude.. Lens.mapping Lens.coerced

-- | The last modified date of the device.
deviceType_deviceLastModifiedDate :: Lens.Lens' DeviceType (Prelude.Maybe Prelude.UTCTime)
deviceType_deviceLastModifiedDate = Lens.lens (\DeviceType' {deviceLastModifiedDate} -> deviceLastModifiedDate) (\s@DeviceType' {} a -> s {deviceLastModifiedDate = a} :: DeviceType) Prelude.. Lens.mapping Core._Time

-- | The creation date of the device.
deviceType_deviceCreateDate :: Lens.Lens' DeviceType (Prelude.Maybe Prelude.UTCTime)
deviceType_deviceCreateDate = Lens.lens (\DeviceType' {deviceCreateDate} -> deviceCreateDate) (\s@DeviceType' {} a -> s {deviceCreateDate = a} :: DeviceType) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DeviceType where
  parseJSON =
    Core.withObject
      "DeviceType"
      ( \x ->
          DeviceType'
            Prelude.<$> (x Core..:? "DeviceKey")
            Prelude.<*> (x Core..:? "DeviceLastAuthenticatedDate")
            Prelude.<*> ( x Core..:? "DeviceAttributes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DeviceLastModifiedDate")
            Prelude.<*> (x Core..:? "DeviceCreateDate")
      )

instance Prelude.Hashable DeviceType where
  hashWithSalt _salt DeviceType' {..} =
    _salt `Prelude.hashWithSalt` deviceKey
      `Prelude.hashWithSalt` deviceLastAuthenticatedDate
      `Prelude.hashWithSalt` deviceAttributes
      `Prelude.hashWithSalt` deviceLastModifiedDate
      `Prelude.hashWithSalt` deviceCreateDate

instance Prelude.NFData DeviceType where
  rnf DeviceType' {..} =
    Prelude.rnf deviceKey
      `Prelude.seq` Prelude.rnf deviceLastAuthenticatedDate
      `Prelude.seq` Prelude.rnf deviceAttributes
      `Prelude.seq` Prelude.rnf deviceLastModifiedDate
      `Prelude.seq` Prelude.rnf deviceCreateDate
