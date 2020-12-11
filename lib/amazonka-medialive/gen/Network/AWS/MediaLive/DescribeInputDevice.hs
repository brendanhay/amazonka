{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    DescribeInputDevice (..),
    mkDescribeInputDevice,

    -- ** Request lenses
    didInputDeviceId,

    -- * Destructuring the response
    DescribeInputDeviceResponse (..),
    mkDescribeInputDeviceResponse,

    -- ** Response lenses
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DescribeInputDeviceRequest
--
-- /See:/ 'mkDescribeInputDevice' smart constructor.
newtype DescribeInputDevice = DescribeInputDevice'
  { inputDeviceId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInputDevice' with the minimum fields required to make a request.
--
-- * 'inputDeviceId' - The unique ID of this input device. For example, hd-123456789abcdef.
mkDescribeInputDevice ::
  -- | 'inputDeviceId'
  Lude.Text ->
  DescribeInputDevice
mkDescribeInputDevice pInputDeviceId_ =
  DescribeInputDevice' {inputDeviceId = pInputDeviceId_}

-- | The unique ID of this input device. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didInputDeviceId :: Lens.Lens' DescribeInputDevice Lude.Text
didInputDeviceId = Lens.lens (inputDeviceId :: DescribeInputDevice -> Lude.Text) (\s a -> s {inputDeviceId = a} :: DescribeInputDevice)
{-# DEPRECATED didInputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead." #-}

instance Lude.AWSRequest DescribeInputDevice where
  type Rs DescribeInputDevice = DescribeInputDeviceResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInputDeviceResponse'
            Lude.<$> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "macAddress")
            Lude.<*> (x Lude..?> "hdDeviceSettings")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (x Lude..?> "deviceUpdateStatus")
            Lude.<*> (x Lude..?> "deviceSettingsSyncState")
            Lude.<*> (x Lude..?> "type")
            Lude.<*> (x Lude..?> "serialNumber")
            Lude.<*> (x Lude..?> "networkSettings")
            Lude.<*> (x Lude..?> "connectionState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInputDevice where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeInputDevice where
  toPath DescribeInputDevice' {..} =
    Lude.mconcat ["/prod/inputDevices/", Lude.toBS inputDeviceId]

instance Lude.ToQuery DescribeInputDevice where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DescribeInputDeviceResponse
--
-- /See:/ 'mkDescribeInputDeviceResponse' smart constructor.
data DescribeInputDeviceResponse = DescribeInputDeviceResponse'
  { arn ::
      Lude.Maybe Lude.Text,
    mACAddress :: Lude.Maybe Lude.Text,
    hdDeviceSettings ::
      Lude.Maybe InputDeviceHdSettings,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    deviceUpdateStatus ::
      Lude.Maybe DeviceUpdateStatus,
    deviceSettingsSyncState ::
      Lude.Maybe DeviceSettingsSyncState,
    type' :: Lude.Maybe InputDeviceType,
    serialNumber ::
      Lude.Maybe Lude.Text,
    networkSettings ::
      Lude.Maybe
        InputDeviceNetworkSettings,
    connectionState ::
      Lude.Maybe
        InputDeviceConnectionState,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInputDeviceResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The unique ARN of the input device.
-- * 'connectionState' - The state of the connection between the input device and AWS.
-- * 'deviceSettingsSyncState' - The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
-- * 'deviceUpdateStatus' - The status of software on the input device.
-- * 'hdDeviceSettings' - Settings that describe an input device that is type HD.
-- * 'id' - The unique ID of the input device.
-- * 'mACAddress' - The network MAC address of the input device.
-- * 'name' - A name that you specify for the input device.
-- * 'networkSettings' - The network settings for the input device.
-- * 'responseStatus' - The response status code.
-- * 'serialNumber' - The unique serial number of the input device.
-- * 'type'' - The type of the input device.
mkDescribeInputDeviceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInputDeviceResponse
mkDescribeInputDeviceResponse pResponseStatus_ =
  DescribeInputDeviceResponse'
    { arn = Lude.Nothing,
      mACAddress = Lude.Nothing,
      hdDeviceSettings = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      deviceUpdateStatus = Lude.Nothing,
      deviceSettingsSyncState = Lude.Nothing,
      type' = Lude.Nothing,
      serialNumber = Lude.Nothing,
      networkSettings = Lude.Nothing,
      connectionState = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique ARN of the input device.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsARN :: Lens.Lens' DescribeInputDeviceResponse (Lude.Maybe Lude.Text)
didrsARN = Lens.lens (arn :: DescribeInputDeviceResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeInputDeviceResponse)
{-# DEPRECATED didrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The network MAC address of the input device.
--
-- /Note:/ Consider using 'mACAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsMACAddress :: Lens.Lens' DescribeInputDeviceResponse (Lude.Maybe Lude.Text)
didrsMACAddress = Lens.lens (mACAddress :: DescribeInputDeviceResponse -> Lude.Maybe Lude.Text) (\s a -> s {mACAddress = a} :: DescribeInputDeviceResponse)
{-# DEPRECATED didrsMACAddress "Use generic-lens or generic-optics with 'mACAddress' instead." #-}

-- | Settings that describe an input device that is type HD.
--
-- /Note:/ Consider using 'hdDeviceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsHdDeviceSettings :: Lens.Lens' DescribeInputDeviceResponse (Lude.Maybe InputDeviceHdSettings)
didrsHdDeviceSettings = Lens.lens (hdDeviceSettings :: DescribeInputDeviceResponse -> Lude.Maybe InputDeviceHdSettings) (\s a -> s {hdDeviceSettings = a} :: DescribeInputDeviceResponse)
{-# DEPRECATED didrsHdDeviceSettings "Use generic-lens or generic-optics with 'hdDeviceSettings' instead." #-}

-- | A name that you specify for the input device.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsName :: Lens.Lens' DescribeInputDeviceResponse (Lude.Maybe Lude.Text)
didrsName = Lens.lens (name :: DescribeInputDeviceResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeInputDeviceResponse)
{-# DEPRECATED didrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique ID of the input device.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsId :: Lens.Lens' DescribeInputDeviceResponse (Lude.Maybe Lude.Text)
didrsId = Lens.lens (id :: DescribeInputDeviceResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeInputDeviceResponse)
{-# DEPRECATED didrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The status of software on the input device.
--
-- /Note:/ Consider using 'deviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsDeviceUpdateStatus :: Lens.Lens' DescribeInputDeviceResponse (Lude.Maybe DeviceUpdateStatus)
didrsDeviceUpdateStatus = Lens.lens (deviceUpdateStatus :: DescribeInputDeviceResponse -> Lude.Maybe DeviceUpdateStatus) (\s a -> s {deviceUpdateStatus = a} :: DescribeInputDeviceResponse)
{-# DEPRECATED didrsDeviceUpdateStatus "Use generic-lens or generic-optics with 'deviceUpdateStatus' instead." #-}

-- | The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
--
-- /Note:/ Consider using 'deviceSettingsSyncState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsDeviceSettingsSyncState :: Lens.Lens' DescribeInputDeviceResponse (Lude.Maybe DeviceSettingsSyncState)
didrsDeviceSettingsSyncState = Lens.lens (deviceSettingsSyncState :: DescribeInputDeviceResponse -> Lude.Maybe DeviceSettingsSyncState) (\s a -> s {deviceSettingsSyncState = a} :: DescribeInputDeviceResponse)
{-# DEPRECATED didrsDeviceSettingsSyncState "Use generic-lens or generic-optics with 'deviceSettingsSyncState' instead." #-}

-- | The type of the input device.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsType :: Lens.Lens' DescribeInputDeviceResponse (Lude.Maybe InputDeviceType)
didrsType = Lens.lens (type' :: DescribeInputDeviceResponse -> Lude.Maybe InputDeviceType) (\s a -> s {type' = a} :: DescribeInputDeviceResponse)
{-# DEPRECATED didrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The unique serial number of the input device.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsSerialNumber :: Lens.Lens' DescribeInputDeviceResponse (Lude.Maybe Lude.Text)
didrsSerialNumber = Lens.lens (serialNumber :: DescribeInputDeviceResponse -> Lude.Maybe Lude.Text) (\s a -> s {serialNumber = a} :: DescribeInputDeviceResponse)
{-# DEPRECATED didrsSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | The network settings for the input device.
--
-- /Note:/ Consider using 'networkSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsNetworkSettings :: Lens.Lens' DescribeInputDeviceResponse (Lude.Maybe InputDeviceNetworkSettings)
didrsNetworkSettings = Lens.lens (networkSettings :: DescribeInputDeviceResponse -> Lude.Maybe InputDeviceNetworkSettings) (\s a -> s {networkSettings = a} :: DescribeInputDeviceResponse)
{-# DEPRECATED didrsNetworkSettings "Use generic-lens or generic-optics with 'networkSettings' instead." #-}

-- | The state of the connection between the input device and AWS.
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsConnectionState :: Lens.Lens' DescribeInputDeviceResponse (Lude.Maybe InputDeviceConnectionState)
didrsConnectionState = Lens.lens (connectionState :: DescribeInputDeviceResponse -> Lude.Maybe InputDeviceConnectionState) (\s a -> s {connectionState = a} :: DescribeInputDeviceResponse)
{-# DEPRECATED didrsConnectionState "Use generic-lens or generic-optics with 'connectionState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsResponseStatus :: Lens.Lens' DescribeInputDeviceResponse Lude.Int
didrsResponseStatus = Lens.lens (responseStatus :: DescribeInputDeviceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInputDeviceResponse)
{-# DEPRECATED didrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
