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
-- Module      : Amazonka.SageMaker.Types.Device
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Device where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information of a particular device.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | Amazon Web Services Internet of Things (IoT) object name.
    iotThingName :: Prelude.Maybe Prelude.Text,
    -- | Description of the device.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the device.
    deviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Device' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iotThingName', 'device_iotThingName' - Amazon Web Services Internet of Things (IoT) object name.
--
-- 'description', 'device_description' - Description of the device.
--
-- 'deviceName', 'device_deviceName' - The name of the device.
newDevice ::
  -- | 'deviceName'
  Prelude.Text ->
  Device
newDevice pDeviceName_ =
  Device'
    { iotThingName = Prelude.Nothing,
      description = Prelude.Nothing,
      deviceName = pDeviceName_
    }

-- | Amazon Web Services Internet of Things (IoT) object name.
device_iotThingName :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_iotThingName = Lens.lens (\Device' {iotThingName} -> iotThingName) (\s@Device' {} a -> s {iotThingName = a} :: Device)

-- | Description of the device.
device_description :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_description = Lens.lens (\Device' {description} -> description) (\s@Device' {} a -> s {description = a} :: Device)

-- | The name of the device.
device_deviceName :: Lens.Lens' Device Prelude.Text
device_deviceName = Lens.lens (\Device' {deviceName} -> deviceName) (\s@Device' {} a -> s {deviceName = a} :: Device)

instance Prelude.Hashable Device where
  hashWithSalt _salt Device' {..} =
    _salt `Prelude.hashWithSalt` iotThingName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` deviceName

instance Prelude.NFData Device where
  rnf Device' {..} =
    Prelude.rnf iotThingName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf deviceName

instance Core.ToJSON Device where
  toJSON Device' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IotThingName" Core..=) Prelude.<$> iotThingName,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("DeviceName" Core..= deviceName)
          ]
      )
