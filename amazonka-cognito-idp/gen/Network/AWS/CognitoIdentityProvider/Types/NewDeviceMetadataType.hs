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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The new device metadata type.
--
-- /See:/ 'newNewDeviceMetadataType' smart constructor.
data NewDeviceMetadataType = NewDeviceMetadataType'
  { -- | The device key.
    deviceKey :: Core.Maybe Core.Text,
    -- | The device group key.
    deviceGroupKey :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { deviceKey = Core.Nothing,
      deviceGroupKey = Core.Nothing
    }

-- | The device key.
newDeviceMetadataType_deviceKey :: Lens.Lens' NewDeviceMetadataType (Core.Maybe Core.Text)
newDeviceMetadataType_deviceKey = Lens.lens (\NewDeviceMetadataType' {deviceKey} -> deviceKey) (\s@NewDeviceMetadataType' {} a -> s {deviceKey = a} :: NewDeviceMetadataType)

-- | The device group key.
newDeviceMetadataType_deviceGroupKey :: Lens.Lens' NewDeviceMetadataType (Core.Maybe Core.Text)
newDeviceMetadataType_deviceGroupKey = Lens.lens (\NewDeviceMetadataType' {deviceGroupKey} -> deviceGroupKey) (\s@NewDeviceMetadataType' {} a -> s {deviceGroupKey = a} :: NewDeviceMetadataType)

instance Core.FromJSON NewDeviceMetadataType where
  parseJSON =
    Core.withObject
      "NewDeviceMetadataType"
      ( \x ->
          NewDeviceMetadataType'
            Core.<$> (x Core..:? "DeviceKey")
            Core.<*> (x Core..:? "DeviceGroupKey")
      )

instance Core.Hashable NewDeviceMetadataType

instance Core.NFData NewDeviceMetadataType
