{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Device
-- Copyright   : (c) 2013-2020 Brendan Hay
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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a device type that an app is tested against.
--
--
--
-- /See:/ 'device' smart constructor.
data Device = Device'
  { _dCarrier :: !(Maybe Text),
    _dImage :: !(Maybe Text),
    _dManufacturer :: !(Maybe Text),
    _dPlatform :: !(Maybe DevicePlatform),
    _dModelId :: !(Maybe Text),
    _dRemoteAccessEnabled :: !(Maybe Bool),
    _dArn :: !(Maybe Text),
    _dFormFactor :: !(Maybe DeviceFormFactor),
    _dFleetType :: !(Maybe Text),
    _dResolution :: !(Maybe Resolution),
    _dAvailability :: !(Maybe DeviceAvailability),
    _dMemory :: !(Maybe Integer),
    _dRadio :: !(Maybe Text),
    _dOs :: !(Maybe Text),
    _dName :: !(Maybe Text),
    _dModel :: !(Maybe Text),
    _dInstances :: !(Maybe [DeviceInstance]),
    _dRemoteDebugEnabled :: !(Maybe Bool),
    _dCpu :: !(Maybe CPU),
    _dHeapSize :: !(Maybe Integer),
    _dFleetName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCarrier' - The device's carrier.
--
-- * 'dImage' - The device's image name.
--
-- * 'dManufacturer' - The device's manufacturer name.
--
-- * 'dPlatform' - The device's platform. Allowed values include:     * ANDROID     * IOS
--
-- * 'dModelId' - The device's model ID.
--
-- * 'dRemoteAccessEnabled' - Specifies whether remote access has been enabled for the specified device.
--
-- * 'dArn' - The device's ARN.
--
-- * 'dFormFactor' - The device's form factor. Allowed values include:     * PHONE     * TABLET
--
-- * 'dFleetType' - The type of fleet to which this device belongs. Possible values are PRIVATE and PUBLIC.
--
-- * 'dResolution' - The resolution of the device.
--
-- * 'dAvailability' - Indicates how likely a device is available for a test run. Currently available in the 'ListDevices' and GetDevice API methods.
--
-- * 'dMemory' - The device's total memory size, expressed in bytes.
--
-- * 'dRadio' - The device's radio.
--
-- * 'dOs' - The device's operating system type.
--
-- * 'dName' - The device's display name.
--
-- * 'dModel' - The device's model name.
--
-- * 'dInstances' - The instances that belong to this device.
--
-- * 'dRemoteDebugEnabled' - This flag is set to @true@ if remote debugging is enabled for the device. Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- * 'dCpu' - Information about the device's CPU.
--
-- * 'dHeapSize' - The device's heap size, expressed in bytes.
--
-- * 'dFleetName' - The name of the fleet to which this device belongs.
device ::
  Device
device =
  Device'
    { _dCarrier = Nothing,
      _dImage = Nothing,
      _dManufacturer = Nothing,
      _dPlatform = Nothing,
      _dModelId = Nothing,
      _dRemoteAccessEnabled = Nothing,
      _dArn = Nothing,
      _dFormFactor = Nothing,
      _dFleetType = Nothing,
      _dResolution = Nothing,
      _dAvailability = Nothing,
      _dMemory = Nothing,
      _dRadio = Nothing,
      _dOs = Nothing,
      _dName = Nothing,
      _dModel = Nothing,
      _dInstances = Nothing,
      _dRemoteDebugEnabled = Nothing,
      _dCpu = Nothing,
      _dHeapSize = Nothing,
      _dFleetName = Nothing
    }

-- | The device's carrier.
dCarrier :: Lens' Device (Maybe Text)
dCarrier = lens _dCarrier (\s a -> s {_dCarrier = a})

-- | The device's image name.
dImage :: Lens' Device (Maybe Text)
dImage = lens _dImage (\s a -> s {_dImage = a})

-- | The device's manufacturer name.
dManufacturer :: Lens' Device (Maybe Text)
dManufacturer = lens _dManufacturer (\s a -> s {_dManufacturer = a})

-- | The device's platform. Allowed values include:     * ANDROID     * IOS
dPlatform :: Lens' Device (Maybe DevicePlatform)
dPlatform = lens _dPlatform (\s a -> s {_dPlatform = a})

-- | The device's model ID.
dModelId :: Lens' Device (Maybe Text)
dModelId = lens _dModelId (\s a -> s {_dModelId = a})

-- | Specifies whether remote access has been enabled for the specified device.
dRemoteAccessEnabled :: Lens' Device (Maybe Bool)
dRemoteAccessEnabled = lens _dRemoteAccessEnabled (\s a -> s {_dRemoteAccessEnabled = a})

-- | The device's ARN.
dArn :: Lens' Device (Maybe Text)
dArn = lens _dArn (\s a -> s {_dArn = a})

-- | The device's form factor. Allowed values include:     * PHONE     * TABLET
dFormFactor :: Lens' Device (Maybe DeviceFormFactor)
dFormFactor = lens _dFormFactor (\s a -> s {_dFormFactor = a})

-- | The type of fleet to which this device belongs. Possible values are PRIVATE and PUBLIC.
dFleetType :: Lens' Device (Maybe Text)
dFleetType = lens _dFleetType (\s a -> s {_dFleetType = a})

-- | The resolution of the device.
dResolution :: Lens' Device (Maybe Resolution)
dResolution = lens _dResolution (\s a -> s {_dResolution = a})

-- | Indicates how likely a device is available for a test run. Currently available in the 'ListDevices' and GetDevice API methods.
dAvailability :: Lens' Device (Maybe DeviceAvailability)
dAvailability = lens _dAvailability (\s a -> s {_dAvailability = a})

-- | The device's total memory size, expressed in bytes.
dMemory :: Lens' Device (Maybe Integer)
dMemory = lens _dMemory (\s a -> s {_dMemory = a})

-- | The device's radio.
dRadio :: Lens' Device (Maybe Text)
dRadio = lens _dRadio (\s a -> s {_dRadio = a})

-- | The device's operating system type.
dOs :: Lens' Device (Maybe Text)
dOs = lens _dOs (\s a -> s {_dOs = a})

-- | The device's display name.
dName :: Lens' Device (Maybe Text)
dName = lens _dName (\s a -> s {_dName = a})

-- | The device's model name.
dModel :: Lens' Device (Maybe Text)
dModel = lens _dModel (\s a -> s {_dModel = a})

-- | The instances that belong to this device.
dInstances :: Lens' Device [DeviceInstance]
dInstances = lens _dInstances (\s a -> s {_dInstances = a}) . _Default . _Coerce

-- | This flag is set to @true@ if remote debugging is enabled for the device. Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
dRemoteDebugEnabled :: Lens' Device (Maybe Bool)
dRemoteDebugEnabled = lens _dRemoteDebugEnabled (\s a -> s {_dRemoteDebugEnabled = a})

-- | Information about the device's CPU.
dCpu :: Lens' Device (Maybe CPU)
dCpu = lens _dCpu (\s a -> s {_dCpu = a})

-- | The device's heap size, expressed in bytes.
dHeapSize :: Lens' Device (Maybe Integer)
dHeapSize = lens _dHeapSize (\s a -> s {_dHeapSize = a})

-- | The name of the fleet to which this device belongs.
dFleetName :: Lens' Device (Maybe Text)
dFleetName = lens _dFleetName (\s a -> s {_dFleetName = a})

instance FromJSON Device where
  parseJSON =
    withObject
      "Device"
      ( \x ->
          Device'
            <$> (x .:? "carrier")
            <*> (x .:? "image")
            <*> (x .:? "manufacturer")
            <*> (x .:? "platform")
            <*> (x .:? "modelId")
            <*> (x .:? "remoteAccessEnabled")
            <*> (x .:? "arn")
            <*> (x .:? "formFactor")
            <*> (x .:? "fleetType")
            <*> (x .:? "resolution")
            <*> (x .:? "availability")
            <*> (x .:? "memory")
            <*> (x .:? "radio")
            <*> (x .:? "os")
            <*> (x .:? "name")
            <*> (x .:? "model")
            <*> (x .:? "instances" .!= mempty)
            <*> (x .:? "remoteDebugEnabled")
            <*> (x .:? "cpu")
            <*> (x .:? "heapSize")
            <*> (x .:? "fleetName")
      )

instance Hashable Device

instance NFData Device
