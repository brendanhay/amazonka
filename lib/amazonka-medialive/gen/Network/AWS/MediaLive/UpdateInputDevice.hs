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
-- Module      : Network.AWS.MediaLive.UpdateInputDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the parameters for the input device.
module Network.AWS.MediaLive.UpdateInputDevice
  ( -- * Creating a Request
    updateInputDevice,
    UpdateInputDevice,

    -- * Request Lenses
    uidHdDeviceSettings,
    uidName,
    uidInputDeviceId,

    -- * Destructuring the Response
    updateInputDeviceResponse,
    UpdateInputDeviceResponse,

    -- * Response Lenses
    uidrsARN,
    uidrsMACAddress,
    uidrsHdDeviceSettings,
    uidrsName,
    uidrsId,
    uidrsDeviceUpdateStatus,
    uidrsDeviceSettingsSyncState,
    uidrsType,
    uidrsSerialNumber,
    uidrsNetworkSettings,
    uidrsConnectionState,
    uidrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to update an input device.
--
-- /See:/ 'updateInputDevice' smart constructor.
data UpdateInputDevice = UpdateInputDevice'
  { _uidHdDeviceSettings ::
      !(Maybe InputDeviceConfigurableSettings),
    _uidName :: !(Maybe Text),
    _uidInputDeviceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateInputDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uidHdDeviceSettings' - The settings that you want to apply to the input device.
--
-- * 'uidName' - The name that you assigned to this input device (not the unique ID).
--
-- * 'uidInputDeviceId' - The unique ID of the input device. For example, hd-123456789abcdef.
updateInputDevice ::
  -- | 'uidInputDeviceId'
  Text ->
  UpdateInputDevice
updateInputDevice pInputDeviceId_ =
  UpdateInputDevice'
    { _uidHdDeviceSettings = Nothing,
      _uidName = Nothing,
      _uidInputDeviceId = pInputDeviceId_
    }

-- | The settings that you want to apply to the input device.
uidHdDeviceSettings :: Lens' UpdateInputDevice (Maybe InputDeviceConfigurableSettings)
uidHdDeviceSettings = lens _uidHdDeviceSettings (\s a -> s {_uidHdDeviceSettings = a})

-- | The name that you assigned to this input device (not the unique ID).
uidName :: Lens' UpdateInputDevice (Maybe Text)
uidName = lens _uidName (\s a -> s {_uidName = a})

-- | The unique ID of the input device. For example, hd-123456789abcdef.
uidInputDeviceId :: Lens' UpdateInputDevice Text
uidInputDeviceId = lens _uidInputDeviceId (\s a -> s {_uidInputDeviceId = a})

instance AWSRequest UpdateInputDevice where
  type Rs UpdateInputDevice = UpdateInputDeviceResponse
  request = putJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          UpdateInputDeviceResponse'
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

instance Hashable UpdateInputDevice

instance NFData UpdateInputDevice

instance ToHeaders UpdateInputDevice where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateInputDevice where
  toJSON UpdateInputDevice' {..} =
    object
      ( catMaybes
          [ ("hdDeviceSettings" .=) <$> _uidHdDeviceSettings,
            ("name" .=) <$> _uidName
          ]
      )

instance ToPath UpdateInputDevice where
  toPath UpdateInputDevice' {..} =
    mconcat ["/prod/inputDevices/", toBS _uidInputDeviceId]

instance ToQuery UpdateInputDevice where
  toQuery = const mempty

-- | Placeholder documentation for UpdateInputDeviceResponse
--
-- /See:/ 'updateInputDeviceResponse' smart constructor.
data UpdateInputDeviceResponse = UpdateInputDeviceResponse'
  { _uidrsARN ::
      !(Maybe Text),
    _uidrsMACAddress :: !(Maybe Text),
    _uidrsHdDeviceSettings ::
      !(Maybe InputDeviceHdSettings),
    _uidrsName :: !(Maybe Text),
    _uidrsId :: !(Maybe Text),
    _uidrsDeviceUpdateStatus ::
      !(Maybe DeviceUpdateStatus),
    _uidrsDeviceSettingsSyncState ::
      !(Maybe DeviceSettingsSyncState),
    _uidrsType :: !(Maybe InputDeviceType),
    _uidrsSerialNumber :: !(Maybe Text),
    _uidrsNetworkSettings ::
      !(Maybe InputDeviceNetworkSettings),
    _uidrsConnectionState ::
      !(Maybe InputDeviceConnectionState),
    _uidrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateInputDeviceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uidrsARN' - The unique ARN of the input device.
--
-- * 'uidrsMACAddress' - The network MAC address of the input device.
--
-- * 'uidrsHdDeviceSettings' - Settings that describe an input device that is type HD.
--
-- * 'uidrsName' - A name that you specify for the input device.
--
-- * 'uidrsId' - The unique ID of the input device.
--
-- * 'uidrsDeviceUpdateStatus' - The status of software on the input device.
--
-- * 'uidrsDeviceSettingsSyncState' - The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
--
-- * 'uidrsType' - The type of the input device.
--
-- * 'uidrsSerialNumber' - The unique serial number of the input device.
--
-- * 'uidrsNetworkSettings' - The network settings for the input device.
--
-- * 'uidrsConnectionState' - The state of the connection between the input device and AWS.
--
-- * 'uidrsResponseStatus' - -- | The response status code.
updateInputDeviceResponse ::
  -- | 'uidrsResponseStatus'
  Int ->
  UpdateInputDeviceResponse
updateInputDeviceResponse pResponseStatus_ =
  UpdateInputDeviceResponse'
    { _uidrsARN = Nothing,
      _uidrsMACAddress = Nothing,
      _uidrsHdDeviceSettings = Nothing,
      _uidrsName = Nothing,
      _uidrsId = Nothing,
      _uidrsDeviceUpdateStatus = Nothing,
      _uidrsDeviceSettingsSyncState = Nothing,
      _uidrsType = Nothing,
      _uidrsSerialNumber = Nothing,
      _uidrsNetworkSettings = Nothing,
      _uidrsConnectionState = Nothing,
      _uidrsResponseStatus = pResponseStatus_
    }

-- | The unique ARN of the input device.
uidrsARN :: Lens' UpdateInputDeviceResponse (Maybe Text)
uidrsARN = lens _uidrsARN (\s a -> s {_uidrsARN = a})

-- | The network MAC address of the input device.
uidrsMACAddress :: Lens' UpdateInputDeviceResponse (Maybe Text)
uidrsMACAddress = lens _uidrsMACAddress (\s a -> s {_uidrsMACAddress = a})

-- | Settings that describe an input device that is type HD.
uidrsHdDeviceSettings :: Lens' UpdateInputDeviceResponse (Maybe InputDeviceHdSettings)
uidrsHdDeviceSettings = lens _uidrsHdDeviceSettings (\s a -> s {_uidrsHdDeviceSettings = a})

-- | A name that you specify for the input device.
uidrsName :: Lens' UpdateInputDeviceResponse (Maybe Text)
uidrsName = lens _uidrsName (\s a -> s {_uidrsName = a})

-- | The unique ID of the input device.
uidrsId :: Lens' UpdateInputDeviceResponse (Maybe Text)
uidrsId = lens _uidrsId (\s a -> s {_uidrsId = a})

-- | The status of software on the input device.
uidrsDeviceUpdateStatus :: Lens' UpdateInputDeviceResponse (Maybe DeviceUpdateStatus)
uidrsDeviceUpdateStatus = lens _uidrsDeviceUpdateStatus (\s a -> s {_uidrsDeviceUpdateStatus = a})

-- | The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
uidrsDeviceSettingsSyncState :: Lens' UpdateInputDeviceResponse (Maybe DeviceSettingsSyncState)
uidrsDeviceSettingsSyncState = lens _uidrsDeviceSettingsSyncState (\s a -> s {_uidrsDeviceSettingsSyncState = a})

-- | The type of the input device.
uidrsType :: Lens' UpdateInputDeviceResponse (Maybe InputDeviceType)
uidrsType = lens _uidrsType (\s a -> s {_uidrsType = a})

-- | The unique serial number of the input device.
uidrsSerialNumber :: Lens' UpdateInputDeviceResponse (Maybe Text)
uidrsSerialNumber = lens _uidrsSerialNumber (\s a -> s {_uidrsSerialNumber = a})

-- | The network settings for the input device.
uidrsNetworkSettings :: Lens' UpdateInputDeviceResponse (Maybe InputDeviceNetworkSettings)
uidrsNetworkSettings = lens _uidrsNetworkSettings (\s a -> s {_uidrsNetworkSettings = a})

-- | The state of the connection between the input device and AWS.
uidrsConnectionState :: Lens' UpdateInputDeviceResponse (Maybe InputDeviceConnectionState)
uidrsConnectionState = lens _uidrsConnectionState (\s a -> s {_uidrsConnectionState = a})

-- | -- | The response status code.
uidrsResponseStatus :: Lens' UpdateInputDeviceResponse Int
uidrsResponseStatus = lens _uidrsResponseStatus (\s a -> s {_uidrsResponseStatus = a})

instance NFData UpdateInputDeviceResponse
