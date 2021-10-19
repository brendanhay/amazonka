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
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatusDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceStatusDetail where

import Network.AWS.AlexaBusiness.Types.DeviceStatusDetailCode
import Network.AWS.AlexaBusiness.Types.Feature
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details of a device’s status.
--
-- /See:/ 'newDeviceStatusDetail' smart constructor.
data DeviceStatusDetail = DeviceStatusDetail'
  { -- | The list of available features on the device.
    feature :: Prelude.Maybe Feature,
    -- | The device status detail code.
    code :: Prelude.Maybe DeviceStatusDetailCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceStatusDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'feature', 'deviceStatusDetail_feature' - The list of available features on the device.
--
-- 'code', 'deviceStatusDetail_code' - The device status detail code.
newDeviceStatusDetail ::
  DeviceStatusDetail
newDeviceStatusDetail =
  DeviceStatusDetail'
    { feature = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The list of available features on the device.
deviceStatusDetail_feature :: Lens.Lens' DeviceStatusDetail (Prelude.Maybe Feature)
deviceStatusDetail_feature = Lens.lens (\DeviceStatusDetail' {feature} -> feature) (\s@DeviceStatusDetail' {} a -> s {feature = a} :: DeviceStatusDetail)

-- | The device status detail code.
deviceStatusDetail_code :: Lens.Lens' DeviceStatusDetail (Prelude.Maybe DeviceStatusDetailCode)
deviceStatusDetail_code = Lens.lens (\DeviceStatusDetail' {code} -> code) (\s@DeviceStatusDetail' {} a -> s {code = a} :: DeviceStatusDetail)

instance Core.FromJSON DeviceStatusDetail where
  parseJSON =
    Core.withObject
      "DeviceStatusDetail"
      ( \x ->
          DeviceStatusDetail'
            Prelude.<$> (x Core..:? "Feature")
            Prelude.<*> (x Core..:? "Code")
      )

instance Prelude.Hashable DeviceStatusDetail

instance Prelude.NFData DeviceStatusDetail
