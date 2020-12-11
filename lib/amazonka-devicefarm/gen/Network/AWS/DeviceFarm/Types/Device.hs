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
    dCarrier,
    dImage,
    dManufacturer,
    dPlatform,
    dModelId,
    dRemoteAccessEnabled,
    dArn,
    dFormFactor,
    dFleetType,
    dResolution,
    dAvailability,
    dMemory,
    dRadio,
    dOs,
    dName,
    dModel,
    dInstances,
    dRemoteDebugEnabled,
    dCpu,
    dHeapSize,
    dFleetName,
  )
where

import Network.AWS.DeviceFarm.Types.CPU
import Network.AWS.DeviceFarm.Types.DeviceAvailability
import Network.AWS.DeviceFarm.Types.DeviceFormFactor
import Network.AWS.DeviceFarm.Types.DeviceInstance
import Network.AWS.DeviceFarm.Types.DevicePlatform
import Network.AWS.DeviceFarm.Types.Resolution
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a device type that an app is tested against.
--
-- /See:/ 'mkDevice' smart constructor.
data Device = Device'
  { carrier :: Lude.Maybe Lude.Text,
    image :: Lude.Maybe Lude.Text,
    manufacturer :: Lude.Maybe Lude.Text,
    platform :: Lude.Maybe DevicePlatform,
    modelId :: Lude.Maybe Lude.Text,
    remoteAccessEnabled :: Lude.Maybe Lude.Bool,
    arn :: Lude.Maybe Lude.Text,
    formFactor :: Lude.Maybe DeviceFormFactor,
    fleetType :: Lude.Maybe Lude.Text,
    resolution :: Lude.Maybe Resolution,
    availability :: Lude.Maybe DeviceAvailability,
    memory :: Lude.Maybe Lude.Integer,
    radio :: Lude.Maybe Lude.Text,
    os :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    model :: Lude.Maybe Lude.Text,
    instances :: Lude.Maybe [DeviceInstance],
    remoteDebugEnabled :: Lude.Maybe Lude.Bool,
    cpu :: Lude.Maybe CPU,
    heapSize :: Lude.Maybe Lude.Integer,
    fleetName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- * 'arn' - The device's ARN.
-- * 'availability' - Indicates how likely a device is available for a test run. Currently available in the 'ListDevices' and GetDevice API methods.
-- * 'carrier' - The device's carrier.
-- * 'cpu' - Information about the device's CPU.
-- * 'fleetName' - The name of the fleet to which this device belongs.
-- * 'fleetType' - The type of fleet to which this device belongs. Possible values are PRIVATE and PUBLIC.
-- * 'formFactor' - The device's form factor.
--
-- Allowed values include:
--
--     * PHONE
--
--
--     * TABLET
--
--
-- * 'heapSize' - The device's heap size, expressed in bytes.
-- * 'image' - The device's image name.
-- * 'instances' - The instances that belong to this device.
-- * 'manufacturer' - The device's manufacturer name.
-- * 'memory' - The device's total memory size, expressed in bytes.
-- * 'model' - The device's model name.
-- * 'modelId' - The device's model ID.
-- * 'name' - The device's display name.
-- * 'os' - The device's operating system type.
-- * 'platform' - The device's platform.
--
-- Allowed values include:
--
--     * ANDROID
--
--
--     * IOS
--
--
-- * 'radio' - The device's radio.
-- * 'remoteAccessEnabled' - Specifies whether remote access has been enabled for the specified device.
-- * 'remoteDebugEnabled' - This flag is set to @true@ if remote debugging is enabled for the device.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
-- * 'resolution' - The resolution of the device.
mkDevice ::
  Device
mkDevice =
  Device'
    { carrier = Lude.Nothing,
      image = Lude.Nothing,
      manufacturer = Lude.Nothing,
      platform = Lude.Nothing,
      modelId = Lude.Nothing,
      remoteAccessEnabled = Lude.Nothing,
      arn = Lude.Nothing,
      formFactor = Lude.Nothing,
      fleetType = Lude.Nothing,
      resolution = Lude.Nothing,
      availability = Lude.Nothing,
      memory = Lude.Nothing,
      radio = Lude.Nothing,
      os = Lude.Nothing,
      name = Lude.Nothing,
      model = Lude.Nothing,
      instances = Lude.Nothing,
      remoteDebugEnabled = Lude.Nothing,
      cpu = Lude.Nothing,
      heapSize = Lude.Nothing,
      fleetName = Lude.Nothing
    }

-- | The device's carrier.
--
-- /Note:/ Consider using 'carrier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCarrier :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dCarrier = Lens.lens (carrier :: Device -> Lude.Maybe Lude.Text) (\s a -> s {carrier = a} :: Device)
{-# DEPRECATED dCarrier "Use generic-lens or generic-optics with 'carrier' instead." #-}

-- | The device's image name.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dImage :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dImage = Lens.lens (image :: Device -> Lude.Maybe Lude.Text) (\s a -> s {image = a} :: Device)
{-# DEPRECATED dImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The device's manufacturer name.
--
-- /Note:/ Consider using 'manufacturer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dManufacturer :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dManufacturer = Lens.lens (manufacturer :: Device -> Lude.Maybe Lude.Text) (\s a -> s {manufacturer = a} :: Device)
{-# DEPRECATED dManufacturer "Use generic-lens or generic-optics with 'manufacturer' instead." #-}

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
dPlatform :: Lens.Lens' Device (Lude.Maybe DevicePlatform)
dPlatform = Lens.lens (platform :: Device -> Lude.Maybe DevicePlatform) (\s a -> s {platform = a} :: Device)
{-# DEPRECATED dPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The device's model ID.
--
-- /Note:/ Consider using 'modelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dModelId :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dModelId = Lens.lens (modelId :: Device -> Lude.Maybe Lude.Text) (\s a -> s {modelId = a} :: Device)
{-# DEPRECATED dModelId "Use generic-lens or generic-optics with 'modelId' instead." #-}

-- | Specifies whether remote access has been enabled for the specified device.
--
-- /Note:/ Consider using 'remoteAccessEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRemoteAccessEnabled :: Lens.Lens' Device (Lude.Maybe Lude.Bool)
dRemoteAccessEnabled = Lens.lens (remoteAccessEnabled :: Device -> Lude.Maybe Lude.Bool) (\s a -> s {remoteAccessEnabled = a} :: Device)
{-# DEPRECATED dRemoteAccessEnabled "Use generic-lens or generic-optics with 'remoteAccessEnabled' instead." #-}

-- | The device's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArn :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dArn = Lens.lens (arn :: Device -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Device)
{-# DEPRECATED dArn "Use generic-lens or generic-optics with 'arn' instead." #-}

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
dFormFactor :: Lens.Lens' Device (Lude.Maybe DeviceFormFactor)
dFormFactor = Lens.lens (formFactor :: Device -> Lude.Maybe DeviceFormFactor) (\s a -> s {formFactor = a} :: Device)
{-# DEPRECATED dFormFactor "Use generic-lens or generic-optics with 'formFactor' instead." #-}

-- | The type of fleet to which this device belongs. Possible values are PRIVATE and PUBLIC.
--
-- /Note:/ Consider using 'fleetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFleetType :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dFleetType = Lens.lens (fleetType :: Device -> Lude.Maybe Lude.Text) (\s a -> s {fleetType = a} :: Device)
{-# DEPRECATED dFleetType "Use generic-lens or generic-optics with 'fleetType' instead." #-}

-- | The resolution of the device.
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResolution :: Lens.Lens' Device (Lude.Maybe Resolution)
dResolution = Lens.lens (resolution :: Device -> Lude.Maybe Resolution) (\s a -> s {resolution = a} :: Device)
{-# DEPRECATED dResolution "Use generic-lens or generic-optics with 'resolution' instead." #-}

-- | Indicates how likely a device is available for a test run. Currently available in the 'ListDevices' and GetDevice API methods.
--
-- /Note:/ Consider using 'availability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAvailability :: Lens.Lens' Device (Lude.Maybe DeviceAvailability)
dAvailability = Lens.lens (availability :: Device -> Lude.Maybe DeviceAvailability) (\s a -> s {availability = a} :: Device)
{-# DEPRECATED dAvailability "Use generic-lens or generic-optics with 'availability' instead." #-}

-- | The device's total memory size, expressed in bytes.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMemory :: Lens.Lens' Device (Lude.Maybe Lude.Integer)
dMemory = Lens.lens (memory :: Device -> Lude.Maybe Lude.Integer) (\s a -> s {memory = a} :: Device)
{-# DEPRECATED dMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | The device's radio.
--
-- /Note:/ Consider using 'radio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRadio :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dRadio = Lens.lens (radio :: Device -> Lude.Maybe Lude.Text) (\s a -> s {radio = a} :: Device)
{-# DEPRECATED dRadio "Use generic-lens or generic-optics with 'radio' instead." #-}

-- | The device's operating system type.
--
-- /Note:/ Consider using 'os' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOs :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dOs = Lens.lens (os :: Device -> Lude.Maybe Lude.Text) (\s a -> s {os = a} :: Device)
{-# DEPRECATED dOs "Use generic-lens or generic-optics with 'os' instead." #-}

-- | The device's display name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dName = Lens.lens (name :: Device -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Device)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The device's model name.
--
-- /Note:/ Consider using 'model' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dModel :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dModel = Lens.lens (model :: Device -> Lude.Maybe Lude.Text) (\s a -> s {model = a} :: Device)
{-# DEPRECATED dModel "Use generic-lens or generic-optics with 'model' instead." #-}

-- | The instances that belong to this device.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInstances :: Lens.Lens' Device (Lude.Maybe [DeviceInstance])
dInstances = Lens.lens (instances :: Device -> Lude.Maybe [DeviceInstance]) (\s a -> s {instances = a} :: Device)
{-# DEPRECATED dInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | This flag is set to @true@ if remote debugging is enabled for the device.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'remoteDebugEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRemoteDebugEnabled :: Lens.Lens' Device (Lude.Maybe Lude.Bool)
dRemoteDebugEnabled = Lens.lens (remoteDebugEnabled :: Device -> Lude.Maybe Lude.Bool) (\s a -> s {remoteDebugEnabled = a} :: Device)
{-# DEPRECATED dRemoteDebugEnabled "Use generic-lens or generic-optics with 'remoteDebugEnabled' instead." #-}

-- | Information about the device's CPU.
--
-- /Note:/ Consider using 'cpu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCpu :: Lens.Lens' Device (Lude.Maybe CPU)
dCpu = Lens.lens (cpu :: Device -> Lude.Maybe CPU) (\s a -> s {cpu = a} :: Device)
{-# DEPRECATED dCpu "Use generic-lens or generic-optics with 'cpu' instead." #-}

-- | The device's heap size, expressed in bytes.
--
-- /Note:/ Consider using 'heapSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHeapSize :: Lens.Lens' Device (Lude.Maybe Lude.Integer)
dHeapSize = Lens.lens (heapSize :: Device -> Lude.Maybe Lude.Integer) (\s a -> s {heapSize = a} :: Device)
{-# DEPRECATED dHeapSize "Use generic-lens or generic-optics with 'heapSize' instead." #-}

-- | The name of the fleet to which this device belongs.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFleetName :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dFleetName = Lens.lens (fleetName :: Device -> Lude.Maybe Lude.Text) (\s a -> s {fleetName = a} :: Device)
{-# DEPRECATED dFleetName "Use generic-lens or generic-optics with 'fleetName' instead." #-}

instance Lude.FromJSON Device where
  parseJSON =
    Lude.withObject
      "Device"
      ( \x ->
          Device'
            Lude.<$> (x Lude..:? "carrier")
            Lude.<*> (x Lude..:? "image")
            Lude.<*> (x Lude..:? "manufacturer")
            Lude.<*> (x Lude..:? "platform")
            Lude.<*> (x Lude..:? "modelId")
            Lude.<*> (x Lude..:? "remoteAccessEnabled")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "formFactor")
            Lude.<*> (x Lude..:? "fleetType")
            Lude.<*> (x Lude..:? "resolution")
            Lude.<*> (x Lude..:? "availability")
            Lude.<*> (x Lude..:? "memory")
            Lude.<*> (x Lude..:? "radio")
            Lude.<*> (x Lude..:? "os")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "model")
            Lude.<*> (x Lude..:? "instances" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "remoteDebugEnabled")
            Lude.<*> (x Lude..:? "cpu")
            Lude.<*> (x Lude..:? "heapSize")
            Lude.<*> (x Lude..:? "fleetName")
      )
