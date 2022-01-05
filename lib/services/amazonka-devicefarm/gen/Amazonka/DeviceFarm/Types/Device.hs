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
-- Module      : Amazonka.DeviceFarm.Types.Device
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.Device where

import qualified Amazonka.Core as Core
import Amazonka.DeviceFarm.Types.CPU
import Amazonka.DeviceFarm.Types.DeviceAvailability
import Amazonka.DeviceFarm.Types.DeviceFormFactor
import Amazonka.DeviceFarm.Types.DeviceInstance
import Amazonka.DeviceFarm.Types.DevicePlatform
import Amazonka.DeviceFarm.Types.Resolution
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a device type that an app is tested against.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | The device\'s carrier.
    carrier :: Prelude.Maybe Prelude.Text,
    -- | The device\'s image name.
    image :: Prelude.Maybe Prelude.Text,
    -- | The device\'s manufacturer name.
    manufacturer :: Prelude.Maybe Prelude.Text,
    -- | The device\'s platform.
    --
    -- Allowed values include:
    --
    -- -   ANDROID
    --
    -- -   IOS
    platform :: Prelude.Maybe DevicePlatform,
    -- | The device\'s model ID.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether remote access has been enabled for the specified
    -- device.
    remoteAccessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The device\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The device\'s form factor.
    --
    -- Allowed values include:
    --
    -- -   PHONE
    --
    -- -   TABLET
    formFactor :: Prelude.Maybe DeviceFormFactor,
    -- | The type of fleet to which this device belongs. Possible values are
    -- PRIVATE and PUBLIC.
    fleetType :: Prelude.Maybe Prelude.Text,
    -- | The resolution of the device.
    resolution :: Prelude.Maybe Resolution,
    -- | Indicates how likely a device is available for a test run. Currently
    -- available in the ListDevices and GetDevice API methods.
    availability :: Prelude.Maybe DeviceAvailability,
    -- | The device\'s total memory size, expressed in bytes.
    memory :: Prelude.Maybe Prelude.Integer,
    -- | The device\'s radio.
    radio :: Prelude.Maybe Prelude.Text,
    -- | The device\'s operating system type.
    os :: Prelude.Maybe Prelude.Text,
    -- | The device\'s display name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The device\'s model name.
    model :: Prelude.Maybe Prelude.Text,
    -- | The instances that belong to this device.
    instances :: Prelude.Maybe [DeviceInstance],
    -- | This flag is set to @true@ if remote debugging is enabled for the
    -- device.
    --
    -- Remote debugging is
    -- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
    remoteDebugEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Information about the device\'s CPU.
    cpu :: Prelude.Maybe CPU,
    -- | The device\'s heap size, expressed in bytes.
    heapSize :: Prelude.Maybe Prelude.Integer,
    -- | The name of the fleet to which this device belongs.
    fleetName :: Prelude.Maybe Prelude.Text
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
-- 'carrier', 'device_carrier' - The device\'s carrier.
--
-- 'image', 'device_image' - The device\'s image name.
--
-- 'manufacturer', 'device_manufacturer' - The device\'s manufacturer name.
--
-- 'platform', 'device_platform' - The device\'s platform.
--
-- Allowed values include:
--
-- -   ANDROID
--
-- -   IOS
--
-- 'modelId', 'device_modelId' - The device\'s model ID.
--
-- 'remoteAccessEnabled', 'device_remoteAccessEnabled' - Specifies whether remote access has been enabled for the specified
-- device.
--
-- 'arn', 'device_arn' - The device\'s ARN.
--
-- 'formFactor', 'device_formFactor' - The device\'s form factor.
--
-- Allowed values include:
--
-- -   PHONE
--
-- -   TABLET
--
-- 'fleetType', 'device_fleetType' - The type of fleet to which this device belongs. Possible values are
-- PRIVATE and PUBLIC.
--
-- 'resolution', 'device_resolution' - The resolution of the device.
--
-- 'availability', 'device_availability' - Indicates how likely a device is available for a test run. Currently
-- available in the ListDevices and GetDevice API methods.
--
-- 'memory', 'device_memory' - The device\'s total memory size, expressed in bytes.
--
-- 'radio', 'device_radio' - The device\'s radio.
--
-- 'os', 'device_os' - The device\'s operating system type.
--
-- 'name', 'device_name' - The device\'s display name.
--
-- 'model', 'device_model' - The device\'s model name.
--
-- 'instances', 'device_instances' - The instances that belong to this device.
--
-- 'remoteDebugEnabled', 'device_remoteDebugEnabled' - This flag is set to @true@ if remote debugging is enabled for the
-- device.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
--
-- 'cpu', 'device_cpu' - Information about the device\'s CPU.
--
-- 'heapSize', 'device_heapSize' - The device\'s heap size, expressed in bytes.
--
-- 'fleetName', 'device_fleetName' - The name of the fleet to which this device belongs.
newDevice ::
  Device
newDevice =
  Device'
    { carrier = Prelude.Nothing,
      image = Prelude.Nothing,
      manufacturer = Prelude.Nothing,
      platform = Prelude.Nothing,
      modelId = Prelude.Nothing,
      remoteAccessEnabled = Prelude.Nothing,
      arn = Prelude.Nothing,
      formFactor = Prelude.Nothing,
      fleetType = Prelude.Nothing,
      resolution = Prelude.Nothing,
      availability = Prelude.Nothing,
      memory = Prelude.Nothing,
      radio = Prelude.Nothing,
      os = Prelude.Nothing,
      name = Prelude.Nothing,
      model = Prelude.Nothing,
      instances = Prelude.Nothing,
      remoteDebugEnabled = Prelude.Nothing,
      cpu = Prelude.Nothing,
      heapSize = Prelude.Nothing,
      fleetName = Prelude.Nothing
    }

-- | The device\'s carrier.
device_carrier :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_carrier = Lens.lens (\Device' {carrier} -> carrier) (\s@Device' {} a -> s {carrier = a} :: Device)

-- | The device\'s image name.
device_image :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_image = Lens.lens (\Device' {image} -> image) (\s@Device' {} a -> s {image = a} :: Device)

-- | The device\'s manufacturer name.
device_manufacturer :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_manufacturer = Lens.lens (\Device' {manufacturer} -> manufacturer) (\s@Device' {} a -> s {manufacturer = a} :: Device)

-- | The device\'s platform.
--
-- Allowed values include:
--
-- -   ANDROID
--
-- -   IOS
device_platform :: Lens.Lens' Device (Prelude.Maybe DevicePlatform)
device_platform = Lens.lens (\Device' {platform} -> platform) (\s@Device' {} a -> s {platform = a} :: Device)

-- | The device\'s model ID.
device_modelId :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_modelId = Lens.lens (\Device' {modelId} -> modelId) (\s@Device' {} a -> s {modelId = a} :: Device)

-- | Specifies whether remote access has been enabled for the specified
-- device.
device_remoteAccessEnabled :: Lens.Lens' Device (Prelude.Maybe Prelude.Bool)
device_remoteAccessEnabled = Lens.lens (\Device' {remoteAccessEnabled} -> remoteAccessEnabled) (\s@Device' {} a -> s {remoteAccessEnabled = a} :: Device)

-- | The device\'s ARN.
device_arn :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_arn = Lens.lens (\Device' {arn} -> arn) (\s@Device' {} a -> s {arn = a} :: Device)

-- | The device\'s form factor.
--
-- Allowed values include:
--
-- -   PHONE
--
-- -   TABLET
device_formFactor :: Lens.Lens' Device (Prelude.Maybe DeviceFormFactor)
device_formFactor = Lens.lens (\Device' {formFactor} -> formFactor) (\s@Device' {} a -> s {formFactor = a} :: Device)

-- | The type of fleet to which this device belongs. Possible values are
-- PRIVATE and PUBLIC.
device_fleetType :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_fleetType = Lens.lens (\Device' {fleetType} -> fleetType) (\s@Device' {} a -> s {fleetType = a} :: Device)

-- | The resolution of the device.
device_resolution :: Lens.Lens' Device (Prelude.Maybe Resolution)
device_resolution = Lens.lens (\Device' {resolution} -> resolution) (\s@Device' {} a -> s {resolution = a} :: Device)

-- | Indicates how likely a device is available for a test run. Currently
-- available in the ListDevices and GetDevice API methods.
device_availability :: Lens.Lens' Device (Prelude.Maybe DeviceAvailability)
device_availability = Lens.lens (\Device' {availability} -> availability) (\s@Device' {} a -> s {availability = a} :: Device)

-- | The device\'s total memory size, expressed in bytes.
device_memory :: Lens.Lens' Device (Prelude.Maybe Prelude.Integer)
device_memory = Lens.lens (\Device' {memory} -> memory) (\s@Device' {} a -> s {memory = a} :: Device)

-- | The device\'s radio.
device_radio :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_radio = Lens.lens (\Device' {radio} -> radio) (\s@Device' {} a -> s {radio = a} :: Device)

-- | The device\'s operating system type.
device_os :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_os = Lens.lens (\Device' {os} -> os) (\s@Device' {} a -> s {os = a} :: Device)

-- | The device\'s display name.
device_name :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_name = Lens.lens (\Device' {name} -> name) (\s@Device' {} a -> s {name = a} :: Device)

-- | The device\'s model name.
device_model :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_model = Lens.lens (\Device' {model} -> model) (\s@Device' {} a -> s {model = a} :: Device)

-- | The instances that belong to this device.
device_instances :: Lens.Lens' Device (Prelude.Maybe [DeviceInstance])
device_instances = Lens.lens (\Device' {instances} -> instances) (\s@Device' {} a -> s {instances = a} :: Device) Prelude.. Lens.mapping Lens.coerced

-- | This flag is set to @true@ if remote debugging is enabled for the
-- device.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
device_remoteDebugEnabled :: Lens.Lens' Device (Prelude.Maybe Prelude.Bool)
device_remoteDebugEnabled = Lens.lens (\Device' {remoteDebugEnabled} -> remoteDebugEnabled) (\s@Device' {} a -> s {remoteDebugEnabled = a} :: Device)

-- | Information about the device\'s CPU.
device_cpu :: Lens.Lens' Device (Prelude.Maybe CPU)
device_cpu = Lens.lens (\Device' {cpu} -> cpu) (\s@Device' {} a -> s {cpu = a} :: Device)

-- | The device\'s heap size, expressed in bytes.
device_heapSize :: Lens.Lens' Device (Prelude.Maybe Prelude.Integer)
device_heapSize = Lens.lens (\Device' {heapSize} -> heapSize) (\s@Device' {} a -> s {heapSize = a} :: Device)

-- | The name of the fleet to which this device belongs.
device_fleetName :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_fleetName = Lens.lens (\Device' {fleetName} -> fleetName) (\s@Device' {} a -> s {fleetName = a} :: Device)

instance Core.FromJSON Device where
  parseJSON =
    Core.withObject
      "Device"
      ( \x ->
          Device'
            Prelude.<$> (x Core..:? "carrier")
            Prelude.<*> (x Core..:? "image")
            Prelude.<*> (x Core..:? "manufacturer")
            Prelude.<*> (x Core..:? "platform")
            Prelude.<*> (x Core..:? "modelId")
            Prelude.<*> (x Core..:? "remoteAccessEnabled")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "formFactor")
            Prelude.<*> (x Core..:? "fleetType")
            Prelude.<*> (x Core..:? "resolution")
            Prelude.<*> (x Core..:? "availability")
            Prelude.<*> (x Core..:? "memory")
            Prelude.<*> (x Core..:? "radio")
            Prelude.<*> (x Core..:? "os")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "model")
            Prelude.<*> (x Core..:? "instances" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "remoteDebugEnabled")
            Prelude.<*> (x Core..:? "cpu")
            Prelude.<*> (x Core..:? "heapSize")
            Prelude.<*> (x Core..:? "fleetName")
      )

instance Prelude.Hashable Device where
  hashWithSalt _salt Device' {..} =
    _salt `Prelude.hashWithSalt` carrier
      `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` manufacturer
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` remoteAccessEnabled
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` formFactor
      `Prelude.hashWithSalt` fleetType
      `Prelude.hashWithSalt` resolution
      `Prelude.hashWithSalt` availability
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` radio
      `Prelude.hashWithSalt` os
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` model
      `Prelude.hashWithSalt` instances
      `Prelude.hashWithSalt` remoteDebugEnabled
      `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` heapSize
      `Prelude.hashWithSalt` fleetName

instance Prelude.NFData Device where
  rnf Device' {..} =
    Prelude.rnf carrier
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf manufacturer
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf remoteAccessEnabled
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf formFactor
      `Prelude.seq` Prelude.rnf fleetType
      `Prelude.seq` Prelude.rnf resolution
      `Prelude.seq` Prelude.rnf availability
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf radio
      `Prelude.seq` Prelude.rnf os
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf model
      `Prelude.seq` Prelude.rnf instances
      `Prelude.seq` Prelude.rnf remoteDebugEnabled
      `Prelude.seq` Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf heapSize
      `Prelude.seq` Prelude.rnf fleetName
