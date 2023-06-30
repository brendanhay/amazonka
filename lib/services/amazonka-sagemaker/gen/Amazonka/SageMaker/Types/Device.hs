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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Device where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information of a particular device.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | Description of the device.
    description :: Prelude.Maybe Prelude.Text,
    -- | Amazon Web Services Internet of Things (IoT) object name.
    iotThingName :: Prelude.Maybe Prelude.Text,
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
-- 'description', 'device_description' - Description of the device.
--
-- 'iotThingName', 'device_iotThingName' - Amazon Web Services Internet of Things (IoT) object name.
--
-- 'deviceName', 'device_deviceName' - The name of the device.
newDevice ::
  -- | 'deviceName'
  Prelude.Text ->
  Device
newDevice pDeviceName_ =
  Device'
    { description = Prelude.Nothing,
      iotThingName = Prelude.Nothing,
      deviceName = pDeviceName_
    }

-- | Description of the device.
device_description :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_description = Lens.lens (\Device' {description} -> description) (\s@Device' {} a -> s {description = a} :: Device)

-- | Amazon Web Services Internet of Things (IoT) object name.
device_iotThingName :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_iotThingName = Lens.lens (\Device' {iotThingName} -> iotThingName) (\s@Device' {} a -> s {iotThingName = a} :: Device)

-- | The name of the device.
device_deviceName :: Lens.Lens' Device Prelude.Text
device_deviceName = Lens.lens (\Device' {deviceName} -> deviceName) (\s@Device' {} a -> s {deviceName = a} :: Device)

instance Prelude.Hashable Device where
  hashWithSalt _salt Device' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` iotThingName
      `Prelude.hashWithSalt` deviceName

instance Prelude.NFData Device where
  rnf Device' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf iotThingName
      `Prelude.seq` Prelude.rnf deviceName

instance Data.ToJSON Device where
  toJSON Device' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("IotThingName" Data..=) Prelude.<$> iotThingName,
            Prelude.Just ("DeviceName" Data..= deviceName)
          ]
      )
