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

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.CPU
import Network.AWS.DeviceFarm.Types.DeviceAvailability
import Network.AWS.DeviceFarm.Types.DeviceFormFactor
import Network.AWS.DeviceFarm.Types.DeviceInstance
import Network.AWS.DeviceFarm.Types.DevicePlatform
import Network.AWS.DeviceFarm.Types.Resolution
import qualified Network.AWS.Lens as Lens

-- | Represents a device type that an app is tested against.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | The device\'s manufacturer name.
    manufacturer :: Core.Maybe Core.Text,
    -- | The device\'s platform.
    --
    -- Allowed values include:
    --
    -- -   ANDROID
    --
    -- -   IOS
    platform :: Core.Maybe DevicePlatform,
    -- | The device\'s model name.
    model :: Core.Maybe Core.Text,
    -- | The name of the fleet to which this device belongs.
    fleetName :: Core.Maybe Core.Text,
    -- | The device\'s total memory size, expressed in bytes.
    memory :: Core.Maybe Core.Integer,
    -- | Indicates how likely a device is available for a test run. Currently
    -- available in the ListDevices and GetDevice API methods.
    availability :: Core.Maybe DeviceAvailability,
    -- | The type of fleet to which this device belongs. Possible values are
    -- PRIVATE and PUBLIC.
    fleetType :: Core.Maybe Core.Text,
    -- | The device\'s form factor.
    --
    -- Allowed values include:
    --
    -- -   PHONE
    --
    -- -   TABLET
    formFactor :: Core.Maybe DeviceFormFactor,
    -- | Specifies whether remote access has been enabled for the specified
    -- device.
    remoteAccessEnabled :: Core.Maybe Core.Bool,
    -- | The device\'s ARN.
    arn :: Core.Maybe Core.Text,
    -- | The instances that belong to this device.
    instances :: Core.Maybe [DeviceInstance],
    -- | The device\'s display name.
    name :: Core.Maybe Core.Text,
    -- | The device\'s image name.
    image :: Core.Maybe Core.Text,
    -- | The device\'s carrier.
    carrier :: Core.Maybe Core.Text,
    -- | The device\'s operating system type.
    os :: Core.Maybe Core.Text,
    -- | The device\'s heap size, expressed in bytes.
    heapSize :: Core.Maybe Core.Integer,
    -- | The device\'s radio.
    radio :: Core.Maybe Core.Text,
    -- | The resolution of the device.
    resolution :: Core.Maybe Resolution,
    -- | Information about the device\'s CPU.
    cpu :: Core.Maybe CPU,
    -- | This flag is set to @true@ if remote debugging is enabled for the
    -- device.
    --
    -- Remote debugging is
    -- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
    remoteDebugEnabled :: Core.Maybe Core.Bool,
    -- | The device\'s model ID.
    modelId :: Core.Maybe Core.Text
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
    { manufacturer = Core.Nothing,
      platform = Core.Nothing,
      model = Core.Nothing,
      fleetName = Core.Nothing,
      memory = Core.Nothing,
      availability = Core.Nothing,
      fleetType = Core.Nothing,
      formFactor = Core.Nothing,
      remoteAccessEnabled = Core.Nothing,
      arn = Core.Nothing,
      instances = Core.Nothing,
      name = Core.Nothing,
      image = Core.Nothing,
      carrier = Core.Nothing,
      os = Core.Nothing,
      heapSize = Core.Nothing,
      radio = Core.Nothing,
      resolution = Core.Nothing,
      cpu = Core.Nothing,
      remoteDebugEnabled = Core.Nothing,
      modelId = Core.Nothing
    }

-- | The device\'s manufacturer name.
device_manufacturer :: Lens.Lens' Device (Core.Maybe Core.Text)
device_manufacturer = Lens.lens (\Device' {manufacturer} -> manufacturer) (\s@Device' {} a -> s {manufacturer = a} :: Device)

-- | The device\'s platform.
--
-- Allowed values include:
--
-- -   ANDROID
--
-- -   IOS
device_platform :: Lens.Lens' Device (Core.Maybe DevicePlatform)
device_platform = Lens.lens (\Device' {platform} -> platform) (\s@Device' {} a -> s {platform = a} :: Device)

-- | The device\'s model name.
device_model :: Lens.Lens' Device (Core.Maybe Core.Text)
device_model = Lens.lens (\Device' {model} -> model) (\s@Device' {} a -> s {model = a} :: Device)

-- | The name of the fleet to which this device belongs.
device_fleetName :: Lens.Lens' Device (Core.Maybe Core.Text)
device_fleetName = Lens.lens (\Device' {fleetName} -> fleetName) (\s@Device' {} a -> s {fleetName = a} :: Device)

-- | The device\'s total memory size, expressed in bytes.
device_memory :: Lens.Lens' Device (Core.Maybe Core.Integer)
device_memory = Lens.lens (\Device' {memory} -> memory) (\s@Device' {} a -> s {memory = a} :: Device)

-- | Indicates how likely a device is available for a test run. Currently
-- available in the ListDevices and GetDevice API methods.
device_availability :: Lens.Lens' Device (Core.Maybe DeviceAvailability)
device_availability = Lens.lens (\Device' {availability} -> availability) (\s@Device' {} a -> s {availability = a} :: Device)

-- | The type of fleet to which this device belongs. Possible values are
-- PRIVATE and PUBLIC.
device_fleetType :: Lens.Lens' Device (Core.Maybe Core.Text)
device_fleetType = Lens.lens (\Device' {fleetType} -> fleetType) (\s@Device' {} a -> s {fleetType = a} :: Device)

-- | The device\'s form factor.
--
-- Allowed values include:
--
-- -   PHONE
--
-- -   TABLET
device_formFactor :: Lens.Lens' Device (Core.Maybe DeviceFormFactor)
device_formFactor = Lens.lens (\Device' {formFactor} -> formFactor) (\s@Device' {} a -> s {formFactor = a} :: Device)

-- | Specifies whether remote access has been enabled for the specified
-- device.
device_remoteAccessEnabled :: Lens.Lens' Device (Core.Maybe Core.Bool)
device_remoteAccessEnabled = Lens.lens (\Device' {remoteAccessEnabled} -> remoteAccessEnabled) (\s@Device' {} a -> s {remoteAccessEnabled = a} :: Device)

-- | The device\'s ARN.
device_arn :: Lens.Lens' Device (Core.Maybe Core.Text)
device_arn = Lens.lens (\Device' {arn} -> arn) (\s@Device' {} a -> s {arn = a} :: Device)

-- | The instances that belong to this device.
device_instances :: Lens.Lens' Device (Core.Maybe [DeviceInstance])
device_instances = Lens.lens (\Device' {instances} -> instances) (\s@Device' {} a -> s {instances = a} :: Device) Core.. Lens.mapping Lens._Coerce

-- | The device\'s display name.
device_name :: Lens.Lens' Device (Core.Maybe Core.Text)
device_name = Lens.lens (\Device' {name} -> name) (\s@Device' {} a -> s {name = a} :: Device)

-- | The device\'s image name.
device_image :: Lens.Lens' Device (Core.Maybe Core.Text)
device_image = Lens.lens (\Device' {image} -> image) (\s@Device' {} a -> s {image = a} :: Device)

-- | The device\'s carrier.
device_carrier :: Lens.Lens' Device (Core.Maybe Core.Text)
device_carrier = Lens.lens (\Device' {carrier} -> carrier) (\s@Device' {} a -> s {carrier = a} :: Device)

-- | The device\'s operating system type.
device_os :: Lens.Lens' Device (Core.Maybe Core.Text)
device_os = Lens.lens (\Device' {os} -> os) (\s@Device' {} a -> s {os = a} :: Device)

-- | The device\'s heap size, expressed in bytes.
device_heapSize :: Lens.Lens' Device (Core.Maybe Core.Integer)
device_heapSize = Lens.lens (\Device' {heapSize} -> heapSize) (\s@Device' {} a -> s {heapSize = a} :: Device)

-- | The device\'s radio.
device_radio :: Lens.Lens' Device (Core.Maybe Core.Text)
device_radio = Lens.lens (\Device' {radio} -> radio) (\s@Device' {} a -> s {radio = a} :: Device)

-- | The resolution of the device.
device_resolution :: Lens.Lens' Device (Core.Maybe Resolution)
device_resolution = Lens.lens (\Device' {resolution} -> resolution) (\s@Device' {} a -> s {resolution = a} :: Device)

-- | Information about the device\'s CPU.
device_cpu :: Lens.Lens' Device (Core.Maybe CPU)
device_cpu = Lens.lens (\Device' {cpu} -> cpu) (\s@Device' {} a -> s {cpu = a} :: Device)

-- | This flag is set to @true@ if remote debugging is enabled for the
-- device.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
device_remoteDebugEnabled :: Lens.Lens' Device (Core.Maybe Core.Bool)
device_remoteDebugEnabled = Lens.lens (\Device' {remoteDebugEnabled} -> remoteDebugEnabled) (\s@Device' {} a -> s {remoteDebugEnabled = a} :: Device)

-- | The device\'s model ID.
device_modelId :: Lens.Lens' Device (Core.Maybe Core.Text)
device_modelId = Lens.lens (\Device' {modelId} -> modelId) (\s@Device' {} a -> s {modelId = a} :: Device)

instance Core.FromJSON Device where
  parseJSON =
    Core.withObject
      "Device"
      ( \x ->
          Device'
            Core.<$> (x Core..:? "manufacturer")
            Core.<*> (x Core..:? "platform")
            Core.<*> (x Core..:? "model")
            Core.<*> (x Core..:? "fleetName")
            Core.<*> (x Core..:? "memory")
            Core.<*> (x Core..:? "availability")
            Core.<*> (x Core..:? "fleetType")
            Core.<*> (x Core..:? "formFactor")
            Core.<*> (x Core..:? "remoteAccessEnabled")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "instances" Core..!= Core.mempty)
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "image")
            Core.<*> (x Core..:? "carrier")
            Core.<*> (x Core..:? "os")
            Core.<*> (x Core..:? "heapSize")
            Core.<*> (x Core..:? "radio")
            Core.<*> (x Core..:? "resolution")
            Core.<*> (x Core..:? "cpu")
            Core.<*> (x Core..:? "remoteDebugEnabled")
            Core.<*> (x Core..:? "modelId")
      )

instance Core.Hashable Device

instance Core.NFData Device
