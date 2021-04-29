{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.NewDeviceMetadataType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.NewDeviceMetadataType where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The new device metadata type.
--
-- /See:/ 'newNewDeviceMetadataType' smart constructor.
data NewDeviceMetadataType = NewDeviceMetadataType'
  { -- | The device key.
    deviceKey :: Prelude.Maybe Prelude.Text,
    -- | The device group key.
    deviceGroupKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NewDeviceMetadataType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceKey', 'newDeviceMetadataType_deviceKey' - The device key.
--
-- 'deviceGroupKey', 'newDeviceMetadataType_deviceGroupKey' - The device group key.
newNewDeviceMetadataType ::
  NewDeviceMetadataType
newNewDeviceMetadataType =
  NewDeviceMetadataType'
    { deviceKey = Prelude.Nothing,
      deviceGroupKey = Prelude.Nothing
    }

-- | The device key.
newDeviceMetadataType_deviceKey :: Lens.Lens' NewDeviceMetadataType (Prelude.Maybe Prelude.Text)
newDeviceMetadataType_deviceKey = Lens.lens (\NewDeviceMetadataType' {deviceKey} -> deviceKey) (\s@NewDeviceMetadataType' {} a -> s {deviceKey = a} :: NewDeviceMetadataType)

-- | The device group key.
newDeviceMetadataType_deviceGroupKey :: Lens.Lens' NewDeviceMetadataType (Prelude.Maybe Prelude.Text)
newDeviceMetadataType_deviceGroupKey = Lens.lens (\NewDeviceMetadataType' {deviceGroupKey} -> deviceGroupKey) (\s@NewDeviceMetadataType' {} a -> s {deviceGroupKey = a} :: NewDeviceMetadataType)

instance Prelude.FromJSON NewDeviceMetadataType where
  parseJSON =
    Prelude.withObject
      "NewDeviceMetadataType"
      ( \x ->
          NewDeviceMetadataType'
            Prelude.<$> (x Prelude..:? "DeviceKey")
            Prelude.<*> (x Prelude..:? "DeviceGroupKey")
      )

instance Prelude.Hashable NewDeviceMetadataType

instance Prelude.NFData NewDeviceMetadataType
