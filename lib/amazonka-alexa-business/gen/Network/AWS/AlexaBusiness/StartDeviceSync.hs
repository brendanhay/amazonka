{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.StartDeviceSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a device and its account to the known default settings. This clears all information and settings set by previous users in the following ways:
--
--
--     * Bluetooth - This unpairs all bluetooth devices paired with your echo device.
--
--
--     * Volume - This resets the echo device's volume to the default value.
--
--
--     * Notifications - This clears all notifications from your echo device.
--
--
--     * Lists - This clears all to-do items from your echo device.
--
--
--     * Settings - This internally syncs the room's profile (if the device is assigned to a room), contacts, address books, delegation access for account linking, and communications (if enabled on the room profile).
module Network.AWS.AlexaBusiness.StartDeviceSync
  ( -- * Creating a request
    StartDeviceSync (..),
    mkStartDeviceSync,

    -- ** Request lenses
    sdsDeviceARN,
    sdsFeatures,
    sdsRoomARN,

    -- * Destructuring the response
    StartDeviceSyncResponse (..),
    mkStartDeviceSyncResponse,

    -- ** Response lenses
    sdsrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartDeviceSync' smart constructor.
data StartDeviceSync = StartDeviceSync'
  { -- | The ARN of the device to sync. Required.
    deviceARN :: Lude.Maybe Lude.Text,
    -- | Request structure to start the device sync. Required.
    features :: [Feature],
    -- | The ARN of the room with which the device to sync is associated. Required.
    roomARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartDeviceSync' with the minimum fields required to make a request.
--
-- * 'deviceARN' - The ARN of the device to sync. Required.
-- * 'features' - Request structure to start the device sync. Required.
-- * 'roomARN' - The ARN of the room with which the device to sync is associated. Required.
mkStartDeviceSync ::
  StartDeviceSync
mkStartDeviceSync =
  StartDeviceSync'
    { deviceARN = Lude.Nothing,
      features = Lude.mempty,
      roomARN = Lude.Nothing
    }

-- | The ARN of the device to sync. Required.
--
-- /Note:/ Consider using 'deviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsDeviceARN :: Lens.Lens' StartDeviceSync (Lude.Maybe Lude.Text)
sdsDeviceARN = Lens.lens (deviceARN :: StartDeviceSync -> Lude.Maybe Lude.Text) (\s a -> s {deviceARN = a} :: StartDeviceSync)
{-# DEPRECATED sdsDeviceARN "Use generic-lens or generic-optics with 'deviceARN' instead." #-}

-- | Request structure to start the device sync. Required.
--
-- /Note:/ Consider using 'features' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsFeatures :: Lens.Lens' StartDeviceSync [Feature]
sdsFeatures = Lens.lens (features :: StartDeviceSync -> [Feature]) (\s a -> s {features = a} :: StartDeviceSync)
{-# DEPRECATED sdsFeatures "Use generic-lens or generic-optics with 'features' instead." #-}

-- | The ARN of the room with which the device to sync is associated. Required.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsRoomARN :: Lens.Lens' StartDeviceSync (Lude.Maybe Lude.Text)
sdsRoomARN = Lens.lens (roomARN :: StartDeviceSync -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: StartDeviceSync)
{-# DEPRECATED sdsRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

instance Lude.AWSRequest StartDeviceSync where
  type Rs StartDeviceSync = StartDeviceSyncResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartDeviceSyncResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartDeviceSync where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.StartDeviceSync" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartDeviceSync where
  toJSON StartDeviceSync' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeviceArn" Lude..=) Lude.<$> deviceARN,
            Lude.Just ("Features" Lude..= features),
            ("RoomArn" Lude..=) Lude.<$> roomARN
          ]
      )

instance Lude.ToPath StartDeviceSync where
  toPath = Lude.const "/"

instance Lude.ToQuery StartDeviceSync where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartDeviceSyncResponse' smart constructor.
newtype StartDeviceSyncResponse = StartDeviceSyncResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartDeviceSyncResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartDeviceSyncResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartDeviceSyncResponse
mkStartDeviceSyncResponse pResponseStatus_ =
  StartDeviceSyncResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsrsResponseStatus :: Lens.Lens' StartDeviceSyncResponse Lude.Int
sdsrsResponseStatus = Lens.lens (responseStatus :: StartDeviceSyncResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartDeviceSyncResponse)
{-# DEPRECATED sdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
