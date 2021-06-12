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
-- Module      : Network.AWS.SageMaker.Types.Device
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Device where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information of a particular device.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | AWS Internet of Things (IoT) object name.
    iotThingName :: Core.Maybe Core.Text,
    -- | Description of the device.
    description :: Core.Maybe Core.Text,
    -- | The name of the device.
    deviceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Device' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iotThingName', 'device_iotThingName' - AWS Internet of Things (IoT) object name.
--
-- 'description', 'device_description' - Description of the device.
--
-- 'deviceName', 'device_deviceName' - The name of the device.
newDevice ::
  -- | 'deviceName'
  Core.Text ->
  Device
newDevice pDeviceName_ =
  Device'
    { iotThingName = Core.Nothing,
      description = Core.Nothing,
      deviceName = pDeviceName_
    }

-- | AWS Internet of Things (IoT) object name.
device_iotThingName :: Lens.Lens' Device (Core.Maybe Core.Text)
device_iotThingName = Lens.lens (\Device' {iotThingName} -> iotThingName) (\s@Device' {} a -> s {iotThingName = a} :: Device)

-- | Description of the device.
device_description :: Lens.Lens' Device (Core.Maybe Core.Text)
device_description = Lens.lens (\Device' {description} -> description) (\s@Device' {} a -> s {description = a} :: Device)

-- | The name of the device.
device_deviceName :: Lens.Lens' Device Core.Text
device_deviceName = Lens.lens (\Device' {deviceName} -> deviceName) (\s@Device' {} a -> s {deviceName = a} :: Device)

instance Core.Hashable Device

instance Core.NFData Device

instance Core.ToJSON Device where
  toJSON Device' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IotThingName" Core..=) Core.<$> iotThingName,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("DeviceName" Core..= deviceName)
          ]
      )
