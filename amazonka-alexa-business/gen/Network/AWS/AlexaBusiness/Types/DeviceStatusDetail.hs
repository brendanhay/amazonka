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

-- | Details of a device’s status.
--
-- /See:/ 'newDeviceStatusDetail' smart constructor.
data DeviceStatusDetail = DeviceStatusDetail'
  { -- | The device status detail code.
    code :: Core.Maybe DeviceStatusDetailCode,
    -- | The list of available features on the device.
    feature :: Core.Maybe Feature
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceStatusDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'deviceStatusDetail_code' - The device status detail code.
--
-- 'feature', 'deviceStatusDetail_feature' - The list of available features on the device.
newDeviceStatusDetail ::
  DeviceStatusDetail
newDeviceStatusDetail =
  DeviceStatusDetail'
    { code = Core.Nothing,
      feature = Core.Nothing
    }

-- | The device status detail code.
deviceStatusDetail_code :: Lens.Lens' DeviceStatusDetail (Core.Maybe DeviceStatusDetailCode)
deviceStatusDetail_code = Lens.lens (\DeviceStatusDetail' {code} -> code) (\s@DeviceStatusDetail' {} a -> s {code = a} :: DeviceStatusDetail)

-- | The list of available features on the device.
deviceStatusDetail_feature :: Lens.Lens' DeviceStatusDetail (Core.Maybe Feature)
deviceStatusDetail_feature = Lens.lens (\DeviceStatusDetail' {feature} -> feature) (\s@DeviceStatusDetail' {} a -> s {feature = a} :: DeviceStatusDetail)

instance Core.FromJSON DeviceStatusDetail where
  parseJSON =
    Core.withObject
      "DeviceStatusDetail"
      ( \x ->
          DeviceStatusDetail'
            Core.<$> (x Core..:? "Code") Core.<*> (x Core..:? "Feature")
      )

instance Core.Hashable DeviceStatusDetail

instance Core.NFData DeviceStatusDetail
