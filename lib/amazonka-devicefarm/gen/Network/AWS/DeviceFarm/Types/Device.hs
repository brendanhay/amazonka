{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Device
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Device
  ( Device (..),

    -- * Smart constructor
    mkDevice,

    -- * Lenses
    dArn,
    dAvailability,
    dCarrier,
    dCpu,
    dFleetName,
    dFleetType,
    dFormFactor,
    dHeapSize,
    dImage,
    dInstances,
    dManufacturer,
    dMemory,
    dModel,
    dModelId,
    dName,
    dOs,
    dPlatform,
    dRadio,
    dRemoteAccessEnabled,
    dRemoteDebugEnabled,
    dResolution,
  )
where

import qualified Network.AWS.DeviceFarm.Types.Arn as Types
import qualified Network.AWS.DeviceFarm.Types.CPU as Types
import qualified Network.AWS.DeviceFarm.Types.DeviceAvailability as Types
import qualified Network.AWS.DeviceFarm.Types.DeviceFormFactor as Types
import qualified Network.AWS.DeviceFarm.Types.DeviceInstance as Types
import qualified Network.AWS.DeviceFarm.Types.DevicePlatform as Types
import qualified Network.AWS.DeviceFarm.Types.Name as Types
import qualified Network.AWS.DeviceFarm.Types.Resolution as Types
import qualified Network.AWS.DeviceFarm.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a device type that an app is tested against.
--
-- /See:/ 'mkDevice' smart constructor.
data Device = Device'
  { -- | The device's ARN.
    arn :: Core.Maybe Types.Arn,
    -- | Indicates how likely a device is available for a test run. Currently available in the 'ListDevices' and GetDevice API methods.
    availability :: Core.Maybe Types.DeviceAvailability,
    -- | The device's carrier.
    carrier :: Core.Maybe Types.String,
    -- | Information about the device's CPU.
    cpu :: Core.Maybe Types.CPU,
    -- | The name of the fleet to which this device belongs.
    fleetName :: Core.Maybe Types.String,
    -- | The type of fleet to which this device belongs. Possible values are PRIVATE and PUBLIC.
    fleetType :: Core.Maybe Types.String,
    -- | The device's form factor.
    --
    -- Allowed values include:
    --
    --     * PHONE
    --
    --
    --     * TABLET
    formFactor :: Core.Maybe Types.DeviceFormFactor,
    -- | The device's heap size, expressed in bytes.
    heapSize :: Core.Maybe Core.Integer,
    -- | The device's image name.
    image :: Core.Maybe Types.String,
    -- | The instances that belong to this device.
    instances :: Core.Maybe [Types.DeviceInstance],
    -- | The device's manufacturer name.
    manufacturer :: Core.Maybe Types.String,
    -- | The device's total memory size, expressed in bytes.
    memory :: Core.Maybe Core.Integer,
    -- | The device's model name.
    model :: Core.Maybe Types.String,
    -- | The device's model ID.
    modelId :: Core.Maybe Types.String,
    -- | The device's display name.
    name :: Core.Maybe Types.Name,
    -- | The device's operating system type.
    os :: Core.Maybe Types.String,
    -- | The device's platform.
    --
    -- Allowed values include:
    --
    --     * ANDROID
    --
    --
    --     * IOS
    platform :: Core.Maybe Types.DevicePlatform,
    -- | The device's radio.
    radio :: Core.Maybe Types.String,
    -- | Specifies whether remote access has been enabled for the specified device.
    remoteAccessEnabled :: Core.Maybe Core.Bool,
    -- | This flag is set to @true@ if remote debugging is enabled for the device.
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    remoteDebugEnabled :: Core.Maybe Core.Bool,
    -- | The resolution of the device.
    resolution :: Core.Maybe Types.Resolution
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Device' value with any optional fields omitted.
mkDevice ::
  Device
mkDevice =
  Device'
    { arn = Core.Nothing,
      availability = Core.Nothing,
      carrier = Core.Nothing,
      cpu = Core.Nothing,
      fleetName = Core.Nothing,
      fleetType = Core.Nothing,
      formFactor = Core.Nothing,
      heapSize = Core.Nothing,
      image = Core.Nothing,
      instances = Core.Nothing,
      manufacturer = Core.Nothing,
      memory = Core.Nothing,
      model = Core.Nothing,
      modelId = Core.Nothing,
      name = Core.Nothing,
      os = Core.Nothing,
      platform = Core.Nothing,
      radio = Core.Nothing,
      remoteAccessEnabled = Core.Nothing,
      remoteDebugEnabled = Core.Nothing,
      resolution = Core.Nothing
    }

-- | The device's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArn :: Lens.Lens' Device (Core.Maybe Types.Arn)
dArn = Lens.field @"arn"
{-# DEPRECATED dArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Indicates how likely a device is available for a test run. Currently available in the 'ListDevices' and GetDevice API methods.
--
-- /Note:/ Consider using 'availability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAvailability :: Lens.Lens' Device (Core.Maybe Types.DeviceAvailability)
dAvailability = Lens.field @"availability"
{-# DEPRECATED dAvailability "Use generic-lens or generic-optics with 'availability' instead." #-}

-- | The device's carrier.
--
-- /Note:/ Consider using 'carrier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCarrier :: Lens.Lens' Device (Core.Maybe Types.String)
dCarrier = Lens.field @"carrier"
{-# DEPRECATED dCarrier "Use generic-lens or generic-optics with 'carrier' instead." #-}

-- | Information about the device's CPU.
--
-- /Note:/ Consider using 'cpu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCpu :: Lens.Lens' Device (Core.Maybe Types.CPU)
dCpu = Lens.field @"cpu"
{-# DEPRECATED dCpu "Use generic-lens or generic-optics with 'cpu' instead." #-}

-- | The name of the fleet to which this device belongs.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFleetName :: Lens.Lens' Device (Core.Maybe Types.String)
dFleetName = Lens.field @"fleetName"
{-# DEPRECATED dFleetName "Use generic-lens or generic-optics with 'fleetName' instead." #-}

-- | The type of fleet to which this device belongs. Possible values are PRIVATE and PUBLIC.
--
-- /Note:/ Consider using 'fleetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFleetType :: Lens.Lens' Device (Core.Maybe Types.String)
dFleetType = Lens.field @"fleetType"
{-# DEPRECATED dFleetType "Use generic-lens or generic-optics with 'fleetType' instead." #-}

-- | The device's form factor.
--
-- Allowed values include:
--
--     * PHONE
--
--
--     * TABLET
--
--
--
-- /Note:/ Consider using 'formFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFormFactor :: Lens.Lens' Device (Core.Maybe Types.DeviceFormFactor)
dFormFactor = Lens.field @"formFactor"
{-# DEPRECATED dFormFactor "Use generic-lens or generic-optics with 'formFactor' instead." #-}

-- | The device's heap size, expressed in bytes.
--
-- /Note:/ Consider using 'heapSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHeapSize :: Lens.Lens' Device (Core.Maybe Core.Integer)
dHeapSize = Lens.field @"heapSize"
{-# DEPRECATED dHeapSize "Use generic-lens or generic-optics with 'heapSize' instead." #-}

-- | The device's image name.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dImage :: Lens.Lens' Device (Core.Maybe Types.String)
dImage = Lens.field @"image"
{-# DEPRECATED dImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The instances that belong to this device.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInstances :: Lens.Lens' Device (Core.Maybe [Types.DeviceInstance])
dInstances = Lens.field @"instances"
{-# DEPRECATED dInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The device's manufacturer name.
--
-- /Note:/ Consider using 'manufacturer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dManufacturer :: Lens.Lens' Device (Core.Maybe Types.String)
dManufacturer = Lens.field @"manufacturer"
{-# DEPRECATED dManufacturer "Use generic-lens or generic-optics with 'manufacturer' instead." #-}

-- | The device's total memory size, expressed in bytes.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMemory :: Lens.Lens' Device (Core.Maybe Core.Integer)
dMemory = Lens.field @"memory"
{-# DEPRECATED dMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | The device's model name.
--
-- /Note:/ Consider using 'model' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dModel :: Lens.Lens' Device (Core.Maybe Types.String)
dModel = Lens.field @"model"
{-# DEPRECATED dModel "Use generic-lens or generic-optics with 'model' instead." #-}

-- | The device's model ID.
--
-- /Note:/ Consider using 'modelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dModelId :: Lens.Lens' Device (Core.Maybe Types.String)
dModelId = Lens.field @"modelId"
{-# DEPRECATED dModelId "Use generic-lens or generic-optics with 'modelId' instead." #-}

-- | The device's display name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Device (Core.Maybe Types.Name)
dName = Lens.field @"name"
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The device's operating system type.
--
-- /Note:/ Consider using 'os' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOs :: Lens.Lens' Device (Core.Maybe Types.String)
dOs = Lens.field @"os"
{-# DEPRECATED dOs "Use generic-lens or generic-optics with 'os' instead." #-}

-- | The device's platform.
--
-- Allowed values include:
--
--     * ANDROID
--
--
--     * IOS
--
--
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPlatform :: Lens.Lens' Device (Core.Maybe Types.DevicePlatform)
dPlatform = Lens.field @"platform"
{-# DEPRECATED dPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The device's radio.
--
-- /Note:/ Consider using 'radio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRadio :: Lens.Lens' Device (Core.Maybe Types.String)
dRadio = Lens.field @"radio"
{-# DEPRECATED dRadio "Use generic-lens or generic-optics with 'radio' instead." #-}

-- | Specifies whether remote access has been enabled for the specified device.
--
-- /Note:/ Consider using 'remoteAccessEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRemoteAccessEnabled :: Lens.Lens' Device (Core.Maybe Core.Bool)
dRemoteAccessEnabled = Lens.field @"remoteAccessEnabled"
{-# DEPRECATED dRemoteAccessEnabled "Use generic-lens or generic-optics with 'remoteAccessEnabled' instead." #-}

-- | This flag is set to @true@ if remote debugging is enabled for the device.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'remoteDebugEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRemoteDebugEnabled :: Lens.Lens' Device (Core.Maybe Core.Bool)
dRemoteDebugEnabled = Lens.field @"remoteDebugEnabled"
{-# DEPRECATED dRemoteDebugEnabled "Use generic-lens or generic-optics with 'remoteDebugEnabled' instead." #-}

-- | The resolution of the device.
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResolution :: Lens.Lens' Device (Core.Maybe Types.Resolution)
dResolution = Lens.field @"resolution"
{-# DEPRECATED dResolution "Use generic-lens or generic-optics with 'resolution' instead." #-}

instance Core.FromJSON Device where
  parseJSON =
    Core.withObject "Device" Core.$
      \x ->
        Device'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "availability")
          Core.<*> (x Core..:? "carrier")
          Core.<*> (x Core..:? "cpu")
          Core.<*> (x Core..:? "fleetName")
          Core.<*> (x Core..:? "fleetType")
          Core.<*> (x Core..:? "formFactor")
          Core.<*> (x Core..:? "heapSize")
          Core.<*> (x Core..:? "image")
          Core.<*> (x Core..:? "instances")
          Core.<*> (x Core..:? "manufacturer")
          Core.<*> (x Core..:? "memory")
          Core.<*> (x Core..:? "model")
          Core.<*> (x Core..:? "modelId")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "os")
          Core.<*> (x Core..:? "platform")
          Core.<*> (x Core..:? "radio")
          Core.<*> (x Core..:? "remoteAccessEnabled")
          Core.<*> (x Core..:? "remoteDebugEnabled")
          Core.<*> (x Core..:? "resolution")
