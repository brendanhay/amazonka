{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Device
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Device
  ( Device (..)
  -- * Smart constructor
  , mkDevice
  -- * Lenses
  , dArn
  , dAvailability
  , dCarrier
  , dCpu
  , dFleetName
  , dFleetType
  , dFormFactor
  , dHeapSize
  , dImage
  , dInstances
  , dManufacturer
  , dMemory
  , dModel
  , dModelId
  , dName
  , dOs
  , dPlatform
  , dRadio
  , dRemoteAccessEnabled
  , dRemoteDebugEnabled
  , dResolution
  ) where

import qualified Network.AWS.DeviceFarm.Types.Arn as Types
import qualified Network.AWS.DeviceFarm.Types.CPU as Types
import qualified Network.AWS.DeviceFarm.Types.DeviceAvailability as Types
import qualified Network.AWS.DeviceFarm.Types.DeviceFormFactor as Types
import qualified Network.AWS.DeviceFarm.Types.DeviceInstance as Types
import qualified Network.AWS.DeviceFarm.Types.DevicePlatform as Types
import qualified Network.AWS.DeviceFarm.Types.Name as Types
import qualified Network.AWS.DeviceFarm.Types.Resolution as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a device type that an app is tested against.
--
-- /See:/ 'mkDevice' smart constructor.
data Device = Device'
  { arn :: Core.Maybe Types.Arn
    -- ^ The device's ARN.
  , availability :: Core.Maybe Types.DeviceAvailability
    -- ^ Indicates how likely a device is available for a test run. Currently available in the 'ListDevices' and GetDevice API methods.
  , carrier :: Core.Maybe Core.Text
    -- ^ The device's carrier.
  , cpu :: Core.Maybe Types.CPU
    -- ^ Information about the device's CPU.
  , fleetName :: Core.Maybe Core.Text
    -- ^ The name of the fleet to which this device belongs.
  , fleetType :: Core.Maybe Core.Text
    -- ^ The type of fleet to which this device belongs. Possible values are PRIVATE and PUBLIC.
  , formFactor :: Core.Maybe Types.DeviceFormFactor
    -- ^ The device's form factor.
--
-- Allowed values include:
--
--     * PHONE
--
--
--     * TABLET
--
--
  , heapSize :: Core.Maybe Core.Integer
    -- ^ The device's heap size, expressed in bytes.
  , image :: Core.Maybe Core.Text
    -- ^ The device's image name.
  , instances :: Core.Maybe [Types.DeviceInstance]
    -- ^ The instances that belong to this device.
  , manufacturer :: Core.Maybe Core.Text
    -- ^ The device's manufacturer name.
  , memory :: Core.Maybe Core.Integer
    -- ^ The device's total memory size, expressed in bytes.
  , model :: Core.Maybe Core.Text
    -- ^ The device's model name.
  , modelId :: Core.Maybe Core.Text
    -- ^ The device's model ID.
  , name :: Core.Maybe Types.Name
    -- ^ The device's display name.
  , os :: Core.Maybe Core.Text
    -- ^ The device's operating system type.
  , platform :: Core.Maybe Types.DevicePlatform
    -- ^ The device's platform.
--
-- Allowed values include:
--
--     * ANDROID
--
--
--     * IOS
--
--
  , radio :: Core.Maybe Core.Text
    -- ^ The device's radio.
  , remoteAccessEnabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether remote access has been enabled for the specified device.
  , remoteDebugEnabled :: Core.Maybe Core.Bool
    -- ^ This flag is set to @true@ if remote debugging is enabled for the device.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
  , resolution :: Core.Maybe Types.Resolution
    -- ^ The resolution of the device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Device' value with any optional fields omitted.
mkDevice
    :: Device
mkDevice
  = Device'{arn = Core.Nothing, availability = Core.Nothing,
            carrier = Core.Nothing, cpu = Core.Nothing,
            fleetName = Core.Nothing, fleetType = Core.Nothing,
            formFactor = Core.Nothing, heapSize = Core.Nothing,
            image = Core.Nothing, instances = Core.Nothing,
            manufacturer = Core.Nothing, memory = Core.Nothing,
            model = Core.Nothing, modelId = Core.Nothing, name = Core.Nothing,
            os = Core.Nothing, platform = Core.Nothing, radio = Core.Nothing,
            remoteAccessEnabled = Core.Nothing,
            remoteDebugEnabled = Core.Nothing, resolution = Core.Nothing}

-- | The device's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArn :: Lens.Lens' Device (Core.Maybe Types.Arn)
dArn = Lens.field @"arn"
{-# INLINEABLE dArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Indicates how likely a device is available for a test run. Currently available in the 'ListDevices' and GetDevice API methods.
--
-- /Note:/ Consider using 'availability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAvailability :: Lens.Lens' Device (Core.Maybe Types.DeviceAvailability)
dAvailability = Lens.field @"availability"
{-# INLINEABLE dAvailability #-}
{-# DEPRECATED availability "Use generic-lens or generic-optics with 'availability' instead"  #-}

-- | The device's carrier.
--
-- /Note:/ Consider using 'carrier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCarrier :: Lens.Lens' Device (Core.Maybe Core.Text)
dCarrier = Lens.field @"carrier"
{-# INLINEABLE dCarrier #-}
{-# DEPRECATED carrier "Use generic-lens or generic-optics with 'carrier' instead"  #-}

-- | Information about the device's CPU.
--
-- /Note:/ Consider using 'cpu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCpu :: Lens.Lens' Device (Core.Maybe Types.CPU)
dCpu = Lens.field @"cpu"
{-# INLINEABLE dCpu #-}
{-# DEPRECATED cpu "Use generic-lens or generic-optics with 'cpu' instead"  #-}

-- | The name of the fleet to which this device belongs.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFleetName :: Lens.Lens' Device (Core.Maybe Core.Text)
dFleetName = Lens.field @"fleetName"
{-# INLINEABLE dFleetName #-}
{-# DEPRECATED fleetName "Use generic-lens or generic-optics with 'fleetName' instead"  #-}

-- | The type of fleet to which this device belongs. Possible values are PRIVATE and PUBLIC.
--
-- /Note:/ Consider using 'fleetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFleetType :: Lens.Lens' Device (Core.Maybe Core.Text)
dFleetType = Lens.field @"fleetType"
{-# INLINEABLE dFleetType #-}
{-# DEPRECATED fleetType "Use generic-lens or generic-optics with 'fleetType' instead"  #-}

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
{-# INLINEABLE dFormFactor #-}
{-# DEPRECATED formFactor "Use generic-lens or generic-optics with 'formFactor' instead"  #-}

-- | The device's heap size, expressed in bytes.
--
-- /Note:/ Consider using 'heapSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHeapSize :: Lens.Lens' Device (Core.Maybe Core.Integer)
dHeapSize = Lens.field @"heapSize"
{-# INLINEABLE dHeapSize #-}
{-# DEPRECATED heapSize "Use generic-lens or generic-optics with 'heapSize' instead"  #-}

-- | The device's image name.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dImage :: Lens.Lens' Device (Core.Maybe Core.Text)
dImage = Lens.field @"image"
{-# INLINEABLE dImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | The instances that belong to this device.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInstances :: Lens.Lens' Device (Core.Maybe [Types.DeviceInstance])
dInstances = Lens.field @"instances"
{-# INLINEABLE dInstances #-}
{-# DEPRECATED instances "Use generic-lens or generic-optics with 'instances' instead"  #-}

-- | The device's manufacturer name.
--
-- /Note:/ Consider using 'manufacturer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dManufacturer :: Lens.Lens' Device (Core.Maybe Core.Text)
dManufacturer = Lens.field @"manufacturer"
{-# INLINEABLE dManufacturer #-}
{-# DEPRECATED manufacturer "Use generic-lens or generic-optics with 'manufacturer' instead"  #-}

-- | The device's total memory size, expressed in bytes.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMemory :: Lens.Lens' Device (Core.Maybe Core.Integer)
dMemory = Lens.field @"memory"
{-# INLINEABLE dMemory #-}
{-# DEPRECATED memory "Use generic-lens or generic-optics with 'memory' instead"  #-}

-- | The device's model name.
--
-- /Note:/ Consider using 'model' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dModel :: Lens.Lens' Device (Core.Maybe Core.Text)
dModel = Lens.field @"model"
{-# INLINEABLE dModel #-}
{-# DEPRECATED model "Use generic-lens or generic-optics with 'model' instead"  #-}

-- | The device's model ID.
--
-- /Note:/ Consider using 'modelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dModelId :: Lens.Lens' Device (Core.Maybe Core.Text)
dModelId = Lens.field @"modelId"
{-# INLINEABLE dModelId #-}
{-# DEPRECATED modelId "Use generic-lens or generic-optics with 'modelId' instead"  #-}

-- | The device's display name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Device (Core.Maybe Types.Name)
dName = Lens.field @"name"
{-# INLINEABLE dName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The device's operating system type.
--
-- /Note:/ Consider using 'os' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOs :: Lens.Lens' Device (Core.Maybe Core.Text)
dOs = Lens.field @"os"
{-# INLINEABLE dOs #-}
{-# DEPRECATED os "Use generic-lens or generic-optics with 'os' instead"  #-}

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
{-# INLINEABLE dPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The device's radio.
--
-- /Note:/ Consider using 'radio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRadio :: Lens.Lens' Device (Core.Maybe Core.Text)
dRadio = Lens.field @"radio"
{-# INLINEABLE dRadio #-}
{-# DEPRECATED radio "Use generic-lens or generic-optics with 'radio' instead"  #-}

-- | Specifies whether remote access has been enabled for the specified device.
--
-- /Note:/ Consider using 'remoteAccessEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRemoteAccessEnabled :: Lens.Lens' Device (Core.Maybe Core.Bool)
dRemoteAccessEnabled = Lens.field @"remoteAccessEnabled"
{-# INLINEABLE dRemoteAccessEnabled #-}
{-# DEPRECATED remoteAccessEnabled "Use generic-lens or generic-optics with 'remoteAccessEnabled' instead"  #-}

-- | This flag is set to @true@ if remote debugging is enabled for the device.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'remoteDebugEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRemoteDebugEnabled :: Lens.Lens' Device (Core.Maybe Core.Bool)
dRemoteDebugEnabled = Lens.field @"remoteDebugEnabled"
{-# INLINEABLE dRemoteDebugEnabled #-}
{-# DEPRECATED remoteDebugEnabled "Use generic-lens or generic-optics with 'remoteDebugEnabled' instead"  #-}

-- | The resolution of the device.
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResolution :: Lens.Lens' Device (Core.Maybe Types.Resolution)
dResolution = Lens.field @"resolution"
{-# INLINEABLE dResolution #-}
{-# DEPRECATED resolution "Use generic-lens or generic-optics with 'resolution' instead"  #-}

instance Core.FromJSON Device where
        parseJSON
          = Core.withObject "Device" Core.$
              \ x ->
                Device' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "availability" Core.<*>
                    x Core..:? "carrier"
                    Core.<*> x Core..:? "cpu"
                    Core.<*> x Core..:? "fleetName"
                    Core.<*> x Core..:? "fleetType"
                    Core.<*> x Core..:? "formFactor"
                    Core.<*> x Core..:? "heapSize"
                    Core.<*> x Core..:? "image"
                    Core.<*> x Core..:? "instances"
                    Core.<*> x Core..:? "manufacturer"
                    Core.<*> x Core..:? "memory"
                    Core.<*> x Core..:? "model"
                    Core.<*> x Core..:? "modelId"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "os"
                    Core.<*> x Core..:? "platform"
                    Core.<*> x Core..:? "radio"
                    Core.<*> x Core..:? "remoteAccessEnabled"
                    Core.<*> x Core..:? "remoteDebugEnabled"
                    Core.<*> x Core..:? "resolution"
