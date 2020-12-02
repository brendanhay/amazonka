{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeInputDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details for the input device
module Network.AWS.MediaLive.DescribeInputDevice
  ( -- * Creating a Request
    describeInputDevice,
    DescribeInputDevice,

    -- * Request Lenses
    didInputDeviceId,

    -- * Destructuring the Response
    describeInputDeviceResponse,
    DescribeInputDeviceResponse,

    -- * Response Lenses
    didrsARN,
    didrsMACAddress,
    didrsHdDeviceSettings,
    didrsName,
    didrsId,
    didrsDeviceUpdateStatus,
    didrsDeviceSettingsSyncState,
    didrsType,
    didrsSerialNumber,
    didrsNetworkSettings,
    didrsConnectionState,
    didrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeInputDeviceRequest
--
-- /See:/ 'describeInputDevice' smart constructor.
newtype DescribeInputDevice = DescribeInputDevice'
  { _didInputDeviceId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInputDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'didInputDeviceId' - The unique ID of this input device. For example, hd-123456789abcdef.
describeInputDevice ::
  -- | 'didInputDeviceId'
  Text ->
  DescribeInputDevice
describeInputDevice pInputDeviceId_ =
  DescribeInputDevice' {_didInputDeviceId = pInputDeviceId_}

-- | The unique ID of this input device. For example, hd-123456789abcdef.
didInputDeviceId :: Lens' DescribeInputDevice Text
didInputDeviceId = lens _didInputDeviceId (\s a -> s {_didInputDeviceId = a})

instance AWSRequest DescribeInputDevice where
  type Rs DescribeInputDevice = DescribeInputDeviceResponse
  request = get mediaLive
  response =
    receiveJSON
      ( \s h x ->
          DescribeInputDeviceResponse'
            <$> (x .?> "arn")
            <*> (x .?> "macAddress")
            <*> (x .?> "hdDeviceSettings")
            <*> (x .?> "name")
            <*> (x .?> "id")
            <*> (x .?> "deviceUpdateStatus")
            <*> (x .?> "deviceSettingsSyncState")
            <*> (x .?> "type")
            <*> (x .?> "serialNumber")
            <*> (x .?> "networkSettings")
            <*> (x .?> "connectionState")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeInputDevice

instance NFData DescribeInputDevice

instance ToHeaders DescribeInputDevice where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeInputDevice where
  toPath DescribeInputDevice' {..} =
    mconcat ["/prod/inputDevices/", toBS _didInputDeviceId]

instance ToQuery DescribeInputDevice where
  toQuery = const mempty

-- | Placeholder documentation for DescribeInputDeviceResponse
--
-- /See:/ 'describeInputDeviceResponse' smart constructor.
data DescribeInputDeviceResponse = DescribeInputDeviceResponse'
  { _didrsARN ::
      !(Maybe Text),
    _didrsMACAddress :: !(Maybe Text),
    _didrsHdDeviceSettings ::
      !(Maybe InputDeviceHdSettings),
    _didrsName :: !(Maybe Text),
    _didrsId :: !(Maybe Text),
    _didrsDeviceUpdateStatus ::
      !(Maybe DeviceUpdateStatus),
    _didrsDeviceSettingsSyncState ::
      !(Maybe DeviceSettingsSyncState),
    _didrsType ::
      !(Maybe InputDeviceType),
    _didrsSerialNumber :: !(Maybe Text),
    _didrsNetworkSettings ::
      !(Maybe InputDeviceNetworkSettings),
    _didrsConnectionState ::
      !(Maybe InputDeviceConnectionState),
    _didrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInputDeviceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'didrsARN' - The unique ARN of the input device.
--
-- * 'didrsMACAddress' - The network MAC address of the input device.
--
-- * 'didrsHdDeviceSettings' - Settings that describe an input device that is type HD.
--
-- * 'didrsName' - A name that you specify for the input device.
--
-- * 'didrsId' - The unique ID of the input device.
--
-- * 'didrsDeviceUpdateStatus' - The status of software on the input device.
--
-- * 'didrsDeviceSettingsSyncState' - The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
--
-- * 'didrsType' - The type of the input device.
--
-- * 'didrsSerialNumber' - The unique serial number of the input device.
--
-- * 'didrsNetworkSettings' - The network settings for the input device.
--
-- * 'didrsConnectionState' - The state of the connection between the input device and AWS.
--
-- * 'didrsResponseStatus' - -- | The response status code.
describeInputDeviceResponse ::
  -- | 'didrsResponseStatus'
  Int ->
  DescribeInputDeviceResponse
describeInputDeviceResponse pResponseStatus_ =
  DescribeInputDeviceResponse'
    { _didrsARN = Nothing,
      _didrsMACAddress = Nothing,
      _didrsHdDeviceSettings = Nothing,
      _didrsName = Nothing,
      _didrsId = Nothing,
      _didrsDeviceUpdateStatus = Nothing,
      _didrsDeviceSettingsSyncState = Nothing,
      _didrsType = Nothing,
      _didrsSerialNumber = Nothing,
      _didrsNetworkSettings = Nothing,
      _didrsConnectionState = Nothing,
      _didrsResponseStatus = pResponseStatus_
    }

-- | The unique ARN of the input device.
didrsARN :: Lens' DescribeInputDeviceResponse (Maybe Text)
didrsARN = lens _didrsARN (\s a -> s {_didrsARN = a})

-- | The network MAC address of the input device.
didrsMACAddress :: Lens' DescribeInputDeviceResponse (Maybe Text)
didrsMACAddress = lens _didrsMACAddress (\s a -> s {_didrsMACAddress = a})

-- | Settings that describe an input device that is type HD.
didrsHdDeviceSettings :: Lens' DescribeInputDeviceResponse (Maybe InputDeviceHdSettings)
didrsHdDeviceSettings = lens _didrsHdDeviceSettings (\s a -> s {_didrsHdDeviceSettings = a})

-- | A name that you specify for the input device.
didrsName :: Lens' DescribeInputDeviceResponse (Maybe Text)
didrsName = lens _didrsName (\s a -> s {_didrsName = a})

-- | The unique ID of the input device.
didrsId :: Lens' DescribeInputDeviceResponse (Maybe Text)
didrsId = lens _didrsId (\s a -> s {_didrsId = a})

-- | The status of software on the input device.
didrsDeviceUpdateStatus :: Lens' DescribeInputDeviceResponse (Maybe DeviceUpdateStatus)
didrsDeviceUpdateStatus = lens _didrsDeviceUpdateStatus (\s a -> s {_didrsDeviceUpdateStatus = a})

-- | The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
didrsDeviceSettingsSyncState :: Lens' DescribeInputDeviceResponse (Maybe DeviceSettingsSyncState)
didrsDeviceSettingsSyncState = lens _didrsDeviceSettingsSyncState (\s a -> s {_didrsDeviceSettingsSyncState = a})

-- | The type of the input device.
didrsType :: Lens' DescribeInputDeviceResponse (Maybe InputDeviceType)
didrsType = lens _didrsType (\s a -> s {_didrsType = a})

-- | The unique serial number of the input device.
didrsSerialNumber :: Lens' DescribeInputDeviceResponse (Maybe Text)
didrsSerialNumber = lens _didrsSerialNumber (\s a -> s {_didrsSerialNumber = a})

-- | The network settings for the input device.
didrsNetworkSettings :: Lens' DescribeInputDeviceResponse (Maybe InputDeviceNetworkSettings)
didrsNetworkSettings = lens _didrsNetworkSettings (\s a -> s {_didrsNetworkSettings = a})

-- | The state of the connection between the input device and AWS.
didrsConnectionState :: Lens' DescribeInputDeviceResponse (Maybe InputDeviceConnectionState)
didrsConnectionState = lens _didrsConnectionState (\s a -> s {_didrsConnectionState = a})

-- | -- | The response status code.
didrsResponseStatus :: Lens' DescribeInputDeviceResponse Int
didrsResponseStatus = lens _didrsResponseStatus (\s a -> s {_didrsResponseStatus = a})

instance NFData DescribeInputDeviceResponse
