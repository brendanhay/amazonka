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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.DeviceType where

import Amazonka.CognitoIdentityProvider.Types.AttributeType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The device type.
--
-- /See:/ 'newDeviceType' smart constructor.
data DeviceType = DeviceType'
  { -- | The device attributes.
    deviceAttributes :: Prelude.Maybe [AttributeType],
    -- | The creation date of the device.
    deviceCreateDate :: Prelude.Maybe Data.POSIX,
    -- | The device key.
    deviceKey :: Prelude.Maybe Prelude.Text,
    -- | The date when the device was last authenticated.
    deviceLastAuthenticatedDate :: Prelude.Maybe Data.POSIX,
    -- | The last modified date of the device.
    deviceLastModifiedDate :: Prelude.Maybe Data.POSIX
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
-- 'deviceAttributes', 'deviceType_deviceAttributes' - The device attributes.
--
-- 'deviceCreateDate', 'deviceType_deviceCreateDate' - The creation date of the device.
--
-- 'deviceKey', 'deviceType_deviceKey' - The device key.
--
-- 'deviceLastAuthenticatedDate', 'deviceType_deviceLastAuthenticatedDate' - The date when the device was last authenticated.
--
-- 'deviceLastModifiedDate', 'deviceType_deviceLastModifiedDate' - The last modified date of the device.
newDeviceType ::
  DeviceType
newDeviceType =
  DeviceType'
    { deviceAttributes = Prelude.Nothing,
      deviceCreateDate = Prelude.Nothing,
      deviceKey = Prelude.Nothing,
      deviceLastAuthenticatedDate = Prelude.Nothing,
      deviceLastModifiedDate = Prelude.Nothing
    }

-- | The device attributes.
deviceType_deviceAttributes :: Lens.Lens' DeviceType (Prelude.Maybe [AttributeType])
deviceType_deviceAttributes = Lens.lens (\DeviceType' {deviceAttributes} -> deviceAttributes) (\s@DeviceType' {} a -> s {deviceAttributes = a} :: DeviceType) Prelude.. Lens.mapping Lens.coerced

-- | The creation date of the device.
deviceType_deviceCreateDate :: Lens.Lens' DeviceType (Prelude.Maybe Prelude.UTCTime)
deviceType_deviceCreateDate = Lens.lens (\DeviceType' {deviceCreateDate} -> deviceCreateDate) (\s@DeviceType' {} a -> s {deviceCreateDate = a} :: DeviceType) Prelude.. Lens.mapping Data._Time

-- | The device key.
deviceType_deviceKey :: Lens.Lens' DeviceType (Prelude.Maybe Prelude.Text)
deviceType_deviceKey = Lens.lens (\DeviceType' {deviceKey} -> deviceKey) (\s@DeviceType' {} a -> s {deviceKey = a} :: DeviceType)

-- | The date when the device was last authenticated.
deviceType_deviceLastAuthenticatedDate :: Lens.Lens' DeviceType (Prelude.Maybe Prelude.UTCTime)
deviceType_deviceLastAuthenticatedDate = Lens.lens (\DeviceType' {deviceLastAuthenticatedDate} -> deviceLastAuthenticatedDate) (\s@DeviceType' {} a -> s {deviceLastAuthenticatedDate = a} :: DeviceType) Prelude.. Lens.mapping Data._Time

-- | The last modified date of the device.
deviceType_deviceLastModifiedDate :: Lens.Lens' DeviceType (Prelude.Maybe Prelude.UTCTime)
deviceType_deviceLastModifiedDate = Lens.lens (\DeviceType' {deviceLastModifiedDate} -> deviceLastModifiedDate) (\s@DeviceType' {} a -> s {deviceLastModifiedDate = a} :: DeviceType) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON DeviceType where
  parseJSON =
    Data.withObject
      "DeviceType"
      ( \x ->
          DeviceType'
            Prelude.<$> ( x
                            Data..:? "DeviceAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DeviceCreateDate")
            Prelude.<*> (x Data..:? "DeviceKey")
            Prelude.<*> (x Data..:? "DeviceLastAuthenticatedDate")
            Prelude.<*> (x Data..:? "DeviceLastModifiedDate")
      )

instance Prelude.Hashable DeviceType where
  hashWithSalt _salt DeviceType' {..} =
    _salt
      `Prelude.hashWithSalt` deviceAttributes
      `Prelude.hashWithSalt` deviceCreateDate
      `Prelude.hashWithSalt` deviceKey
      `Prelude.hashWithSalt` deviceLastAuthenticatedDate
      `Prelude.hashWithSalt` deviceLastModifiedDate

instance Prelude.NFData DeviceType where
  rnf DeviceType' {..} =
    Prelude.rnf deviceAttributes
      `Prelude.seq` Prelude.rnf deviceCreateDate
      `Prelude.seq` Prelude.rnf deviceKey
      `Prelude.seq` Prelude.rnf deviceLastAuthenticatedDate
      `Prelude.seq` Prelude.rnf deviceLastModifiedDate
