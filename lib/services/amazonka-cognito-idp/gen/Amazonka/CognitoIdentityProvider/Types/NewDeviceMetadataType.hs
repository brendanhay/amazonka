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
-- Module      : Amazonka.CognitoIdentityProvider.Types.NewDeviceMetadataType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.NewDeviceMetadataType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The new device metadata type.
--
-- /See:/ 'newNewDeviceMetadataType' smart constructor.
data NewDeviceMetadataType = NewDeviceMetadataType'
  { -- | The device group key.
    deviceGroupKey :: Prelude.Maybe Prelude.Text,
    -- | The device key.
    deviceKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NewDeviceMetadataType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceGroupKey', 'newDeviceMetadataType_deviceGroupKey' - The device group key.
--
-- 'deviceKey', 'newDeviceMetadataType_deviceKey' - The device key.
newNewDeviceMetadataType ::
  NewDeviceMetadataType
newNewDeviceMetadataType =
  NewDeviceMetadataType'
    { deviceGroupKey =
        Prelude.Nothing,
      deviceKey = Prelude.Nothing
    }

-- | The device group key.
newDeviceMetadataType_deviceGroupKey :: Lens.Lens' NewDeviceMetadataType (Prelude.Maybe Prelude.Text)
newDeviceMetadataType_deviceGroupKey = Lens.lens (\NewDeviceMetadataType' {deviceGroupKey} -> deviceGroupKey) (\s@NewDeviceMetadataType' {} a -> s {deviceGroupKey = a} :: NewDeviceMetadataType)

-- | The device key.
newDeviceMetadataType_deviceKey :: Lens.Lens' NewDeviceMetadataType (Prelude.Maybe Prelude.Text)
newDeviceMetadataType_deviceKey = Lens.lens (\NewDeviceMetadataType' {deviceKey} -> deviceKey) (\s@NewDeviceMetadataType' {} a -> s {deviceKey = a} :: NewDeviceMetadataType)

instance Core.FromJSON NewDeviceMetadataType where
  parseJSON =
    Core.withObject
      "NewDeviceMetadataType"
      ( \x ->
          NewDeviceMetadataType'
            Prelude.<$> (x Core..:? "DeviceGroupKey")
            Prelude.<*> (x Core..:? "DeviceKey")
      )

instance Prelude.Hashable NewDeviceMetadataType where
  hashWithSalt _salt NewDeviceMetadataType' {..} =
    _salt `Prelude.hashWithSalt` deviceGroupKey
      `Prelude.hashWithSalt` deviceKey

instance Prelude.NFData NewDeviceMetadataType where
  rnf NewDeviceMetadataType' {..} =
    Prelude.rnf deviceGroupKey
      `Prelude.seq` Prelude.rnf deviceKey
