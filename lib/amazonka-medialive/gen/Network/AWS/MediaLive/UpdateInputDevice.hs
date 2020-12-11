{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    UpdateInputDevice (..),
    mkUpdateInputDevice,

    -- ** Request lenses
    uidHdDeviceSettings,
    uidName,
    uidInputDeviceId,

    -- * Destructuring the response
    UpdateInputDeviceResponse (..),
    mkUpdateInputDeviceResponse,

    -- ** Response lenses
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to update an input device.
--
-- /See:/ 'mkUpdateInputDevice' smart constructor.
data UpdateInputDevice = UpdateInputDevice'
  { hdDeviceSettings ::
      Lude.Maybe InputDeviceConfigurableSettings,
    name :: Lude.Maybe Lude.Text,
    inputDeviceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateInputDevice' with the minimum fields required to make a request.
--
-- * 'hdDeviceSettings' - The settings that you want to apply to the input device.
-- * 'inputDeviceId' - The unique ID of the input device. For example, hd-123456789abcdef.
-- * 'name' - The name that you assigned to this input device (not the unique ID).
mkUpdateInputDevice ::
  -- | 'inputDeviceId'
  Lude.Text ->
  UpdateInputDevice
mkUpdateInputDevice pInputDeviceId_ =
  UpdateInputDevice'
    { hdDeviceSettings = Lude.Nothing,
      name = Lude.Nothing,
      inputDeviceId = pInputDeviceId_
    }

-- | The settings that you want to apply to the input device.
--
-- /Note:/ Consider using 'hdDeviceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidHdDeviceSettings :: Lens.Lens' UpdateInputDevice (Lude.Maybe InputDeviceConfigurableSettings)
uidHdDeviceSettings = Lens.lens (hdDeviceSettings :: UpdateInputDevice -> Lude.Maybe InputDeviceConfigurableSettings) (\s a -> s {hdDeviceSettings = a} :: UpdateInputDevice)
{-# DEPRECATED uidHdDeviceSettings "Use generic-lens or generic-optics with 'hdDeviceSettings' instead." #-}

-- | The name that you assigned to this input device (not the unique ID).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidName :: Lens.Lens' UpdateInputDevice (Lude.Maybe Lude.Text)
uidName = Lens.lens (name :: UpdateInputDevice -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateInputDevice)
{-# DEPRECATED uidName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique ID of the input device. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidInputDeviceId :: Lens.Lens' UpdateInputDevice Lude.Text
uidInputDeviceId = Lens.lens (inputDeviceId :: UpdateInputDevice -> Lude.Text) (\s a -> s {inputDeviceId = a} :: UpdateInputDevice)
{-# DEPRECATED uidInputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead." #-}

instance Lude.AWSRequest UpdateInputDevice where
  type Rs UpdateInputDevice = UpdateInputDeviceResponse
  request = Req.putJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateInputDeviceResponse'
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

instance Lude.ToHeaders UpdateInputDevice where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateInputDevice where
  toJSON UpdateInputDevice' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("hdDeviceSettings" Lude..=) Lude.<$> hdDeviceSettings,
            ("name" Lude..=) Lude.<$> name
          ]
      )

instance Lude.ToPath UpdateInputDevice where
  toPath UpdateInputDevice' {..} =
    Lude.mconcat ["/prod/inputDevices/", Lude.toBS inputDeviceId]

instance Lude.ToQuery UpdateInputDevice where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for UpdateInputDeviceResponse
--
-- /See:/ 'mkUpdateInputDeviceResponse' smart constructor.
data UpdateInputDeviceResponse = UpdateInputDeviceResponse'
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
    serialNumber :: Lude.Maybe Lude.Text,
    networkSettings ::
      Lude.Maybe InputDeviceNetworkSettings,
    connectionState ::
      Lude.Maybe InputDeviceConnectionState,
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

-- | Creates a value of 'UpdateInputDeviceResponse' with the minimum fields required to make a request.
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
mkUpdateInputDeviceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateInputDeviceResponse
mkUpdateInputDeviceResponse pResponseStatus_ =
  UpdateInputDeviceResponse'
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
uidrsARN :: Lens.Lens' UpdateInputDeviceResponse (Lude.Maybe Lude.Text)
uidrsARN = Lens.lens (arn :: UpdateInputDeviceResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: UpdateInputDeviceResponse)
{-# DEPRECATED uidrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The network MAC address of the input device.
--
-- /Note:/ Consider using 'mACAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrsMACAddress :: Lens.Lens' UpdateInputDeviceResponse (Lude.Maybe Lude.Text)
uidrsMACAddress = Lens.lens (mACAddress :: UpdateInputDeviceResponse -> Lude.Maybe Lude.Text) (\s a -> s {mACAddress = a} :: UpdateInputDeviceResponse)
{-# DEPRECATED uidrsMACAddress "Use generic-lens or generic-optics with 'mACAddress' instead." #-}

-- | Settings that describe an input device that is type HD.
--
-- /Note:/ Consider using 'hdDeviceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrsHdDeviceSettings :: Lens.Lens' UpdateInputDeviceResponse (Lude.Maybe InputDeviceHdSettings)
uidrsHdDeviceSettings = Lens.lens (hdDeviceSettings :: UpdateInputDeviceResponse -> Lude.Maybe InputDeviceHdSettings) (\s a -> s {hdDeviceSettings = a} :: UpdateInputDeviceResponse)
{-# DEPRECATED uidrsHdDeviceSettings "Use generic-lens or generic-optics with 'hdDeviceSettings' instead." #-}

-- | A name that you specify for the input device.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrsName :: Lens.Lens' UpdateInputDeviceResponse (Lude.Maybe Lude.Text)
uidrsName = Lens.lens (name :: UpdateInputDeviceResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateInputDeviceResponse)
{-# DEPRECATED uidrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique ID of the input device.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrsId :: Lens.Lens' UpdateInputDeviceResponse (Lude.Maybe Lude.Text)
uidrsId = Lens.lens (id :: UpdateInputDeviceResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: UpdateInputDeviceResponse)
{-# DEPRECATED uidrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The status of software on the input device.
--
-- /Note:/ Consider using 'deviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrsDeviceUpdateStatus :: Lens.Lens' UpdateInputDeviceResponse (Lude.Maybe DeviceUpdateStatus)
uidrsDeviceUpdateStatus = Lens.lens (deviceUpdateStatus :: UpdateInputDeviceResponse -> Lude.Maybe DeviceUpdateStatus) (\s a -> s {deviceUpdateStatus = a} :: UpdateInputDeviceResponse)
{-# DEPRECATED uidrsDeviceUpdateStatus "Use generic-lens or generic-optics with 'deviceUpdateStatus' instead." #-}

-- | The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
--
-- /Note:/ Consider using 'deviceSettingsSyncState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrsDeviceSettingsSyncState :: Lens.Lens' UpdateInputDeviceResponse (Lude.Maybe DeviceSettingsSyncState)
uidrsDeviceSettingsSyncState = Lens.lens (deviceSettingsSyncState :: UpdateInputDeviceResponse -> Lude.Maybe DeviceSettingsSyncState) (\s a -> s {deviceSettingsSyncState = a} :: UpdateInputDeviceResponse)
{-# DEPRECATED uidrsDeviceSettingsSyncState "Use generic-lens or generic-optics with 'deviceSettingsSyncState' instead." #-}

-- | The type of the input device.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrsType :: Lens.Lens' UpdateInputDeviceResponse (Lude.Maybe InputDeviceType)
uidrsType = Lens.lens (type' :: UpdateInputDeviceResponse -> Lude.Maybe InputDeviceType) (\s a -> s {type' = a} :: UpdateInputDeviceResponse)
{-# DEPRECATED uidrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The unique serial number of the input device.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrsSerialNumber :: Lens.Lens' UpdateInputDeviceResponse (Lude.Maybe Lude.Text)
uidrsSerialNumber = Lens.lens (serialNumber :: UpdateInputDeviceResponse -> Lude.Maybe Lude.Text) (\s a -> s {serialNumber = a} :: UpdateInputDeviceResponse)
{-# DEPRECATED uidrsSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | The network settings for the input device.
--
-- /Note:/ Consider using 'networkSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrsNetworkSettings :: Lens.Lens' UpdateInputDeviceResponse (Lude.Maybe InputDeviceNetworkSettings)
uidrsNetworkSettings = Lens.lens (networkSettings :: UpdateInputDeviceResponse -> Lude.Maybe InputDeviceNetworkSettings) (\s a -> s {networkSettings = a} :: UpdateInputDeviceResponse)
{-# DEPRECATED uidrsNetworkSettings "Use generic-lens or generic-optics with 'networkSettings' instead." #-}

-- | The state of the connection between the input device and AWS.
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrsConnectionState :: Lens.Lens' UpdateInputDeviceResponse (Lude.Maybe InputDeviceConnectionState)
uidrsConnectionState = Lens.lens (connectionState :: UpdateInputDeviceResponse -> Lude.Maybe InputDeviceConnectionState) (\s a -> s {connectionState = a} :: UpdateInputDeviceResponse)
{-# DEPRECATED uidrsConnectionState "Use generic-lens or generic-optics with 'connectionState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrsResponseStatus :: Lens.Lens' UpdateInputDeviceResponse Lude.Int
uidrsResponseStatus = Lens.lens (responseStatus :: UpdateInputDeviceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateInputDeviceResponse)
{-# DEPRECATED uidrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
