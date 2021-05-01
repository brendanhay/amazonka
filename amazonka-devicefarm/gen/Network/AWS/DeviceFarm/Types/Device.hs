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
-- Module      : Network.AWS.DeviceFarm.Types.Device
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Device where

import Network.AWS.DeviceFarm.Types.CPU
import Network.AWS.DeviceFarm.Types.DeviceAvailability
import Network.AWS.DeviceFarm.Types.DeviceFormFactor
import Network.AWS.DeviceFarm.Types.DeviceInstance
import Network.AWS.DeviceFarm.Types.DevicePlatform
import Network.AWS.DeviceFarm.Types.Resolution
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a device type that an app is tested against.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | The device\'s manufacturer name.
    manufacturer :: Prelude.Maybe Prelude.Text,
    -- | The device\'s platform.
    --
    -- Allowed values include:
    --
    -- -   ANDROID
    --
    -- -   IOS
    platform :: Prelude.Maybe DevicePlatform,
    -- | The device\'s model name.
    model :: Prelude.Maybe Prelude.Text,
    -- | The name of the fleet to which this device belongs.
    fleetName :: Prelude.Maybe Prelude.Text,
    -- | The device\'s total memory size, expressed in bytes.
    memory :: Prelude.Maybe Prelude.Integer,
    -- | Indicates how likely a device is available for a test run. Currently
    -- available in the ListDevices and GetDevice API methods.
    availability :: Prelude.Maybe DeviceAvailability,
    -- | The type of fleet to which this device belongs. Possible values are
    -- PRIVATE and PUBLIC.
    fleetType :: Prelude.Maybe Prelude.Text,
    -- | The device\'s form factor.
    --
    -- Allowed values include:
    --
    -- -   PHONE
    --
    -- -   TABLET
    formFactor :: Prelude.Maybe DeviceFormFactor,
    -- | Specifies whether remote access has been enabled for the specified
    -- device.
    remoteAccessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The device\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The instances that belong to this device.
    instances :: Prelude.Maybe [DeviceInstance],
    -- | The device\'s display name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The device\'s image name.
    image :: Prelude.Maybe Prelude.Text,
    -- | The device\'s carrier.
    carrier :: Prelude.Maybe Prelude.Text,
    -- | The device\'s operating system type.
    os :: Prelude.Maybe Prelude.Text,
    -- | The device\'s heap size, expressed in bytes.
    heapSize :: Prelude.Maybe Prelude.Integer,
    -- | The device\'s radio.
    radio :: Prelude.Maybe Prelude.Text,
    -- | The resolution of the device.
    resolution :: Prelude.Maybe Resolution,
    -- | Information about the device\'s CPU.
    cpu :: Prelude.Maybe CPU,
    -- | This flag is set to @true@ if remote debugging is enabled for the
    -- device.
    --
    -- Remote debugging is
    -- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
    remoteDebugEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The device\'s model ID.
    modelId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Device' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'model', 'device_model' - The device\'s model name.
--
-- 'fleetName', 'device_fleetName' - The name of the fleet to which this device belongs.
--
-- 'memory', 'device_memory' - The device\'s total memory size, expressed in bytes.
--
-- 'availability', 'device_availability' - Indicates how likely a device is available for a test run. Currently
-- available in the ListDevices and GetDevice API methods.
--
-- 'fleetType', 'device_fleetType' - The type of fleet to which this device belongs. Possible values are
-- PRIVATE and PUBLIC.
--
-- 'formFactor', 'device_formFactor' - The device\'s form factor.
--
-- Allowed values include:
--
-- -   PHONE
--
-- -   TABLET
--
-- 'remoteAccessEnabled', 'device_remoteAccessEnabled' - Specifies whether remote access has been enabled for the specified
-- device.
--
-- 'arn', 'device_arn' - The device\'s ARN.
--
-- 'instances', 'device_instances' - The instances that belong to this device.
--
-- 'name', 'device_name' - The device\'s display name.
--
-- 'image', 'device_image' - The device\'s image name.
--
-- 'carrier', 'device_carrier' - The device\'s carrier.
--
-- 'os', 'device_os' - The device\'s operating system type.
--
-- 'heapSize', 'device_heapSize' - The device\'s heap size, expressed in bytes.
--
-- 'radio', 'device_radio' - The device\'s radio.
--
-- 'resolution', 'device_resolution' - The resolution of the device.
--
-- 'cpu', 'device_cpu' - Information about the device\'s CPU.
--
-- 'remoteDebugEnabled', 'device_remoteDebugEnabled' - This flag is set to @true@ if remote debugging is enabled for the
-- device.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
--
-- 'modelId', 'device_modelId' - The device\'s model ID.
newDevice ::
  Device
newDevice =
  Device'
    { manufacturer = Prelude.Nothing,
      platform = Prelude.Nothing,
      model = Prelude.Nothing,
      fleetName = Prelude.Nothing,
      memory = Prelude.Nothing,
      availability = Prelude.Nothing,
      fleetType = Prelude.Nothing,
      formFactor = Prelude.Nothing,
      remoteAccessEnabled = Prelude.Nothing,
      arn = Prelude.Nothing,
      instances = Prelude.Nothing,
      name = Prelude.Nothing,
      image = Prelude.Nothing,
      carrier = Prelude.Nothing,
      os = Prelude.Nothing,
      heapSize = Prelude.Nothing,
      radio = Prelude.Nothing,
      resolution = Prelude.Nothing,
      cpu = Prelude.Nothing,
      remoteDebugEnabled = Prelude.Nothing,
      modelId = Prelude.Nothing
    }

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

-- | The device\'s model name.
device_model :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_model = Lens.lens (\Device' {model} -> model) (\s@Device' {} a -> s {model = a} :: Device)

-- | The name of the fleet to which this device belongs.
device_fleetName :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_fleetName = Lens.lens (\Device' {fleetName} -> fleetName) (\s@Device' {} a -> s {fleetName = a} :: Device)

-- | The device\'s total memory size, expressed in bytes.
device_memory :: Lens.Lens' Device (Prelude.Maybe Prelude.Integer)
device_memory = Lens.lens (\Device' {memory} -> memory) (\s@Device' {} a -> s {memory = a} :: Device)

-- | Indicates how likely a device is available for a test run. Currently
-- available in the ListDevices and GetDevice API methods.
device_availability :: Lens.Lens' Device (Prelude.Maybe DeviceAvailability)
device_availability = Lens.lens (\Device' {availability} -> availability) (\s@Device' {} a -> s {availability = a} :: Device)

-- | The type of fleet to which this device belongs. Possible values are
-- PRIVATE and PUBLIC.
device_fleetType :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_fleetType = Lens.lens (\Device' {fleetType} -> fleetType) (\s@Device' {} a -> s {fleetType = a} :: Device)

-- | The device\'s form factor.
--
-- Allowed values include:
--
-- -   PHONE
--
-- -   TABLET
device_formFactor :: Lens.Lens' Device (Prelude.Maybe DeviceFormFactor)
device_formFactor = Lens.lens (\Device' {formFactor} -> formFactor) (\s@Device' {} a -> s {formFactor = a} :: Device)

-- | Specifies whether remote access has been enabled for the specified
-- device.
device_remoteAccessEnabled :: Lens.Lens' Device (Prelude.Maybe Prelude.Bool)
device_remoteAccessEnabled = Lens.lens (\Device' {remoteAccessEnabled} -> remoteAccessEnabled) (\s@Device' {} a -> s {remoteAccessEnabled = a} :: Device)

-- | The device\'s ARN.
device_arn :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_arn = Lens.lens (\Device' {arn} -> arn) (\s@Device' {} a -> s {arn = a} :: Device)

-- | The instances that belong to this device.
device_instances :: Lens.Lens' Device (Prelude.Maybe [DeviceInstance])
device_instances = Lens.lens (\Device' {instances} -> instances) (\s@Device' {} a -> s {instances = a} :: Device) Prelude.. Lens.mapping Prelude._Coerce

-- | The device\'s display name.
device_name :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_name = Lens.lens (\Device' {name} -> name) (\s@Device' {} a -> s {name = a} :: Device)

-- | The device\'s image name.
device_image :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_image = Lens.lens (\Device' {image} -> image) (\s@Device' {} a -> s {image = a} :: Device)

-- | The device\'s carrier.
device_carrier :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_carrier = Lens.lens (\Device' {carrier} -> carrier) (\s@Device' {} a -> s {carrier = a} :: Device)

-- | The device\'s operating system type.
device_os :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_os = Lens.lens (\Device' {os} -> os) (\s@Device' {} a -> s {os = a} :: Device)

-- | The device\'s heap size, expressed in bytes.
device_heapSize :: Lens.Lens' Device (Prelude.Maybe Prelude.Integer)
device_heapSize = Lens.lens (\Device' {heapSize} -> heapSize) (\s@Device' {} a -> s {heapSize = a} :: Device)

-- | The device\'s radio.
device_radio :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_radio = Lens.lens (\Device' {radio} -> radio) (\s@Device' {} a -> s {radio = a} :: Device)

-- | The resolution of the device.
device_resolution :: Lens.Lens' Device (Prelude.Maybe Resolution)
device_resolution = Lens.lens (\Device' {resolution} -> resolution) (\s@Device' {} a -> s {resolution = a} :: Device)

-- | Information about the device\'s CPU.
device_cpu :: Lens.Lens' Device (Prelude.Maybe CPU)
device_cpu = Lens.lens (\Device' {cpu} -> cpu) (\s@Device' {} a -> s {cpu = a} :: Device)

-- | This flag is set to @true@ if remote debugging is enabled for the
-- device.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
device_remoteDebugEnabled :: Lens.Lens' Device (Prelude.Maybe Prelude.Bool)
device_remoteDebugEnabled = Lens.lens (\Device' {remoteDebugEnabled} -> remoteDebugEnabled) (\s@Device' {} a -> s {remoteDebugEnabled = a} :: Device)

-- | The device\'s model ID.
device_modelId :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_modelId = Lens.lens (\Device' {modelId} -> modelId) (\s@Device' {} a -> s {modelId = a} :: Device)

instance Prelude.FromJSON Device where
  parseJSON =
    Prelude.withObject
      "Device"
      ( \x ->
          Device'
            Prelude.<$> (x Prelude..:? "manufacturer")
            Prelude.<*> (x Prelude..:? "platform")
            Prelude.<*> (x Prelude..:? "model")
            Prelude.<*> (x Prelude..:? "fleetName")
            Prelude.<*> (x Prelude..:? "memory")
            Prelude.<*> (x Prelude..:? "availability")
            Prelude.<*> (x Prelude..:? "fleetType")
            Prelude.<*> (x Prelude..:? "formFactor")
            Prelude.<*> (x Prelude..:? "remoteAccessEnabled")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> ( x Prelude..:? "instances"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "image")
            Prelude.<*> (x Prelude..:? "carrier")
            Prelude.<*> (x Prelude..:? "os")
            Prelude.<*> (x Prelude..:? "heapSize")
            Prelude.<*> (x Prelude..:? "radio")
            Prelude.<*> (x Prelude..:? "resolution")
            Prelude.<*> (x Prelude..:? "cpu")
            Prelude.<*> (x Prelude..:? "remoteDebugEnabled")
            Prelude.<*> (x Prelude..:? "modelId")
      )

instance Prelude.Hashable Device

instance Prelude.NFData Device
