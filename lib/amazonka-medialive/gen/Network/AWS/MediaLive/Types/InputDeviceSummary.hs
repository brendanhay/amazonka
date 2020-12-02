{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceSummary where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.DeviceSettingsSyncState
import Network.AWS.MediaLive.Types.DeviceUpdateStatus
import Network.AWS.MediaLive.Types.InputDeviceConnectionState
import Network.AWS.MediaLive.Types.InputDeviceHdSettings
import Network.AWS.MediaLive.Types.InputDeviceNetworkSettings
import Network.AWS.MediaLive.Types.InputDeviceType
import Network.AWS.Prelude

-- | Details of the input device.
--
-- /See:/ 'inputDeviceSummary' smart constructor.
data InputDeviceSummary = InputDeviceSummary'
  { _idsARN ::
      !(Maybe Text),
    _idsMACAddress :: !(Maybe Text),
    _idsHdDeviceSettings ::
      !(Maybe InputDeviceHdSettings),
    _idsName :: !(Maybe Text),
    _idsId :: !(Maybe Text),
    _idsDeviceUpdateStatus :: !(Maybe DeviceUpdateStatus),
    _idsDeviceSettingsSyncState ::
      !(Maybe DeviceSettingsSyncState),
    _idsType :: !(Maybe InputDeviceType),
    _idsSerialNumber :: !(Maybe Text),
    _idsNetworkSettings ::
      !(Maybe InputDeviceNetworkSettings),
    _idsConnectionState ::
      !(Maybe InputDeviceConnectionState)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDeviceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idsARN' - The unique ARN of the input device.
--
-- * 'idsMACAddress' - The network MAC address of the input device.
--
-- * 'idsHdDeviceSettings' - Settings that describe an input device that is type HD.
--
-- * 'idsName' - A name that you specify for the input device.
--
-- * 'idsId' - The unique ID of the input device.
--
-- * 'idsDeviceUpdateStatus' - The status of software on the input device.
--
-- * 'idsDeviceSettingsSyncState' - The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
--
-- * 'idsType' - The type of the input device.
--
-- * 'idsSerialNumber' - The unique serial number of the input device.
--
-- * 'idsNetworkSettings' - Network settings for the input device.
--
-- * 'idsConnectionState' - The state of the connection between the input device and AWS.
inputDeviceSummary ::
  InputDeviceSummary
inputDeviceSummary =
  InputDeviceSummary'
    { _idsARN = Nothing,
      _idsMACAddress = Nothing,
      _idsHdDeviceSettings = Nothing,
      _idsName = Nothing,
      _idsId = Nothing,
      _idsDeviceUpdateStatus = Nothing,
      _idsDeviceSettingsSyncState = Nothing,
      _idsType = Nothing,
      _idsSerialNumber = Nothing,
      _idsNetworkSettings = Nothing,
      _idsConnectionState = Nothing
    }

-- | The unique ARN of the input device.
idsARN :: Lens' InputDeviceSummary (Maybe Text)
idsARN = lens _idsARN (\s a -> s {_idsARN = a})

-- | The network MAC address of the input device.
idsMACAddress :: Lens' InputDeviceSummary (Maybe Text)
idsMACAddress = lens _idsMACAddress (\s a -> s {_idsMACAddress = a})

-- | Settings that describe an input device that is type HD.
idsHdDeviceSettings :: Lens' InputDeviceSummary (Maybe InputDeviceHdSettings)
idsHdDeviceSettings = lens _idsHdDeviceSettings (\s a -> s {_idsHdDeviceSettings = a})

-- | A name that you specify for the input device.
idsName :: Lens' InputDeviceSummary (Maybe Text)
idsName = lens _idsName (\s a -> s {_idsName = a})

-- | The unique ID of the input device.
idsId :: Lens' InputDeviceSummary (Maybe Text)
idsId = lens _idsId (\s a -> s {_idsId = a})

-- | The status of software on the input device.
idsDeviceUpdateStatus :: Lens' InputDeviceSummary (Maybe DeviceUpdateStatus)
idsDeviceUpdateStatus = lens _idsDeviceUpdateStatus (\s a -> s {_idsDeviceUpdateStatus = a})

-- | The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
idsDeviceSettingsSyncState :: Lens' InputDeviceSummary (Maybe DeviceSettingsSyncState)
idsDeviceSettingsSyncState = lens _idsDeviceSettingsSyncState (\s a -> s {_idsDeviceSettingsSyncState = a})

-- | The type of the input device.
idsType :: Lens' InputDeviceSummary (Maybe InputDeviceType)
idsType = lens _idsType (\s a -> s {_idsType = a})

-- | The unique serial number of the input device.
idsSerialNumber :: Lens' InputDeviceSummary (Maybe Text)
idsSerialNumber = lens _idsSerialNumber (\s a -> s {_idsSerialNumber = a})

-- | Network settings for the input device.
idsNetworkSettings :: Lens' InputDeviceSummary (Maybe InputDeviceNetworkSettings)
idsNetworkSettings = lens _idsNetworkSettings (\s a -> s {_idsNetworkSettings = a})

-- | The state of the connection between the input device and AWS.
idsConnectionState :: Lens' InputDeviceSummary (Maybe InputDeviceConnectionState)
idsConnectionState = lens _idsConnectionState (\s a -> s {_idsConnectionState = a})

instance FromJSON InputDeviceSummary where
  parseJSON =
    withObject
      "InputDeviceSummary"
      ( \x ->
          InputDeviceSummary'
            <$> (x .:? "arn")
            <*> (x .:? "macAddress")
            <*> (x .:? "hdDeviceSettings")
            <*> (x .:? "name")
            <*> (x .:? "id")
            <*> (x .:? "deviceUpdateStatus")
            <*> (x .:? "deviceSettingsSyncState")
            <*> (x .:? "type")
            <*> (x .:? "serialNumber")
            <*> (x .:? "networkSettings")
            <*> (x .:? "connectionState")
      )

instance Hashable InputDeviceSummary

instance NFData InputDeviceSummary
