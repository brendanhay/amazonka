{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Device
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Device where

import Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo
import Network.AWS.AlexaBusiness.Types.DeviceStatus
import Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A device with attributes.
--
--
--
-- /See:/ 'device' smart constructor.
data Device = Device'
  { _dDeviceStatus :: !(Maybe DeviceStatus),
    _dDeviceStatusInfo :: !(Maybe DeviceStatusInfo),
    _dDeviceARN :: !(Maybe Text),
    _dMACAddress :: !(Maybe Text),
    _dDeviceName :: !(Maybe Text),
    _dRoomARN :: !(Maybe Text),
    _dSoftwareVersion :: !(Maybe Text),
    _dDeviceType :: !(Maybe Text),
    _dNetworkProfileInfo :: !(Maybe DeviceNetworkProfileInfo),
    _dDeviceSerialNumber :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDeviceStatus' - The status of a device. If the status is not READY, check the DeviceStatusInfo value for details.
--
-- * 'dDeviceStatusInfo' - Detailed information about a device's status.
--
-- * 'dDeviceARN' - The ARN of a device.
--
-- * 'dMACAddress' - The MAC address of a device.
--
-- * 'dDeviceName' - The name of a device.
--
-- * 'dRoomARN' - The room ARN of a device.
--
-- * 'dSoftwareVersion' - The software version of a device.
--
-- * 'dDeviceType' - The type of a device.
--
-- * 'dNetworkProfileInfo' - Detailed information about a device's network profile.
--
-- * 'dDeviceSerialNumber' - The serial number of a device.
device ::
  Device
device =
  Device'
    { _dDeviceStatus = Nothing,
      _dDeviceStatusInfo = Nothing,
      _dDeviceARN = Nothing,
      _dMACAddress = Nothing,
      _dDeviceName = Nothing,
      _dRoomARN = Nothing,
      _dSoftwareVersion = Nothing,
      _dDeviceType = Nothing,
      _dNetworkProfileInfo = Nothing,
      _dDeviceSerialNumber = Nothing
    }

-- | The status of a device. If the status is not READY, check the DeviceStatusInfo value for details.
dDeviceStatus :: Lens' Device (Maybe DeviceStatus)
dDeviceStatus = lens _dDeviceStatus (\s a -> s {_dDeviceStatus = a})

-- | Detailed information about a device's status.
dDeviceStatusInfo :: Lens' Device (Maybe DeviceStatusInfo)
dDeviceStatusInfo = lens _dDeviceStatusInfo (\s a -> s {_dDeviceStatusInfo = a})

-- | The ARN of a device.
dDeviceARN :: Lens' Device (Maybe Text)
dDeviceARN = lens _dDeviceARN (\s a -> s {_dDeviceARN = a})

-- | The MAC address of a device.
dMACAddress :: Lens' Device (Maybe Text)
dMACAddress = lens _dMACAddress (\s a -> s {_dMACAddress = a})

-- | The name of a device.
dDeviceName :: Lens' Device (Maybe Text)
dDeviceName = lens _dDeviceName (\s a -> s {_dDeviceName = a})

-- | The room ARN of a device.
dRoomARN :: Lens' Device (Maybe Text)
dRoomARN = lens _dRoomARN (\s a -> s {_dRoomARN = a})

-- | The software version of a device.
dSoftwareVersion :: Lens' Device (Maybe Text)
dSoftwareVersion = lens _dSoftwareVersion (\s a -> s {_dSoftwareVersion = a})

-- | The type of a device.
dDeviceType :: Lens' Device (Maybe Text)
dDeviceType = lens _dDeviceType (\s a -> s {_dDeviceType = a})

-- | Detailed information about a device's network profile.
dNetworkProfileInfo :: Lens' Device (Maybe DeviceNetworkProfileInfo)
dNetworkProfileInfo = lens _dNetworkProfileInfo (\s a -> s {_dNetworkProfileInfo = a})

-- | The serial number of a device.
dDeviceSerialNumber :: Lens' Device (Maybe Text)
dDeviceSerialNumber = lens _dDeviceSerialNumber (\s a -> s {_dDeviceSerialNumber = a})

instance FromJSON Device where
  parseJSON =
    withObject
      "Device"
      ( \x ->
          Device'
            <$> (x .:? "DeviceStatus")
            <*> (x .:? "DeviceStatusInfo")
            <*> (x .:? "DeviceArn")
            <*> (x .:? "MacAddress")
            <*> (x .:? "DeviceName")
            <*> (x .:? "RoomArn")
            <*> (x .:? "SoftwareVersion")
            <*> (x .:? "DeviceType")
            <*> (x .:? "NetworkProfileInfo")
            <*> (x .:? "DeviceSerialNumber")
      )

instance Hashable Device

instance NFData Device
