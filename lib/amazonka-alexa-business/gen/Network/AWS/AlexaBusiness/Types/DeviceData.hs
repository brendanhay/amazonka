{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceData where

import Network.AWS.AlexaBusiness.Types.DeviceStatus
import Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Device attributes.
--
--
--
-- /See:/ 'deviceData' smart constructor.
data DeviceData = DeviceData'
  { _ddDeviceStatus ::
      !(Maybe DeviceStatus),
    _ddNetworkProfileName :: !(Maybe Text),
    _ddDeviceStatusInfo :: !(Maybe DeviceStatusInfo),
    _ddCreatedTime :: !(Maybe POSIX),
    _ddDeviceARN :: !(Maybe Text),
    _ddNetworkProfileARN :: !(Maybe Text),
    _ddMACAddress :: !(Maybe Text),
    _ddDeviceName :: !(Maybe Text),
    _ddRoomARN :: !(Maybe Text),
    _ddSoftwareVersion :: !(Maybe Text),
    _ddDeviceType :: !(Maybe Text),
    _ddRoomName :: !(Maybe Text),
    _ddDeviceSerialNumber :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDeviceStatus' - The status of a device.
--
-- * 'ddNetworkProfileName' - The name of the network profile associated with a device.
--
-- * 'ddDeviceStatusInfo' - Detailed information about a device's status.
--
-- * 'ddCreatedTime' - The time (in epoch) when the device data was created.
--
-- * 'ddDeviceARN' - The ARN of a device.
--
-- * 'ddNetworkProfileARN' - The ARN of the network profile associated with a device.
--
-- * 'ddMACAddress' - The MAC address of a device.
--
-- * 'ddDeviceName' - The name of a device.
--
-- * 'ddRoomARN' - The room ARN associated with a device.
--
-- * 'ddSoftwareVersion' - The software version of a device.
--
-- * 'ddDeviceType' - The type of a device.
--
-- * 'ddRoomName' - The name of the room associated with a device.
--
-- * 'ddDeviceSerialNumber' - The serial number of a device.
deviceData ::
  DeviceData
deviceData =
  DeviceData'
    { _ddDeviceStatus = Nothing,
      _ddNetworkProfileName = Nothing,
      _ddDeviceStatusInfo = Nothing,
      _ddCreatedTime = Nothing,
      _ddDeviceARN = Nothing,
      _ddNetworkProfileARN = Nothing,
      _ddMACAddress = Nothing,
      _ddDeviceName = Nothing,
      _ddRoomARN = Nothing,
      _ddSoftwareVersion = Nothing,
      _ddDeviceType = Nothing,
      _ddRoomName = Nothing,
      _ddDeviceSerialNumber = Nothing
    }

-- | The status of a device.
ddDeviceStatus :: Lens' DeviceData (Maybe DeviceStatus)
ddDeviceStatus = lens _ddDeviceStatus (\s a -> s {_ddDeviceStatus = a})

-- | The name of the network profile associated with a device.
ddNetworkProfileName :: Lens' DeviceData (Maybe Text)
ddNetworkProfileName = lens _ddNetworkProfileName (\s a -> s {_ddNetworkProfileName = a})

-- | Detailed information about a device's status.
ddDeviceStatusInfo :: Lens' DeviceData (Maybe DeviceStatusInfo)
ddDeviceStatusInfo = lens _ddDeviceStatusInfo (\s a -> s {_ddDeviceStatusInfo = a})

-- | The time (in epoch) when the device data was created.
ddCreatedTime :: Lens' DeviceData (Maybe UTCTime)
ddCreatedTime = lens _ddCreatedTime (\s a -> s {_ddCreatedTime = a}) . mapping _Time

-- | The ARN of a device.
ddDeviceARN :: Lens' DeviceData (Maybe Text)
ddDeviceARN = lens _ddDeviceARN (\s a -> s {_ddDeviceARN = a})

-- | The ARN of the network profile associated with a device.
ddNetworkProfileARN :: Lens' DeviceData (Maybe Text)
ddNetworkProfileARN = lens _ddNetworkProfileARN (\s a -> s {_ddNetworkProfileARN = a})

-- | The MAC address of a device.
ddMACAddress :: Lens' DeviceData (Maybe Text)
ddMACAddress = lens _ddMACAddress (\s a -> s {_ddMACAddress = a})

-- | The name of a device.
ddDeviceName :: Lens' DeviceData (Maybe Text)
ddDeviceName = lens _ddDeviceName (\s a -> s {_ddDeviceName = a})

-- | The room ARN associated with a device.
ddRoomARN :: Lens' DeviceData (Maybe Text)
ddRoomARN = lens _ddRoomARN (\s a -> s {_ddRoomARN = a})

-- | The software version of a device.
ddSoftwareVersion :: Lens' DeviceData (Maybe Text)
ddSoftwareVersion = lens _ddSoftwareVersion (\s a -> s {_ddSoftwareVersion = a})

-- | The type of a device.
ddDeviceType :: Lens' DeviceData (Maybe Text)
ddDeviceType = lens _ddDeviceType (\s a -> s {_ddDeviceType = a})

-- | The name of the room associated with a device.
ddRoomName :: Lens' DeviceData (Maybe Text)
ddRoomName = lens _ddRoomName (\s a -> s {_ddRoomName = a})

-- | The serial number of a device.
ddDeviceSerialNumber :: Lens' DeviceData (Maybe Text)
ddDeviceSerialNumber = lens _ddDeviceSerialNumber (\s a -> s {_ddDeviceSerialNumber = a})

instance FromJSON DeviceData where
  parseJSON =
    withObject
      "DeviceData"
      ( \x ->
          DeviceData'
            <$> (x .:? "DeviceStatus")
            <*> (x .:? "NetworkProfileName")
            <*> (x .:? "DeviceStatusInfo")
            <*> (x .:? "CreatedTime")
            <*> (x .:? "DeviceArn")
            <*> (x .:? "NetworkProfileArn")
            <*> (x .:? "MacAddress")
            <*> (x .:? "DeviceName")
            <*> (x .:? "RoomArn")
            <*> (x .:? "SoftwareVersion")
            <*> (x .:? "DeviceType")
            <*> (x .:? "RoomName")
            <*> (x .:? "DeviceSerialNumber")
      )

instance Hashable DeviceData

instance NFData DeviceData
