{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.AssociateDeviceWithRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a device with a given room. This applies all the settings from the room profile to the device, and all the skills in any skill groups added to that room. This operation requires the device to be online, or else a manual sync is required.
module Network.AWS.AlexaBusiness.AssociateDeviceWithRoom
  ( -- * Creating a request
    AssociateDeviceWithRoom (..),
    mkAssociateDeviceWithRoom,

    -- ** Request lenses
    adwrDeviceARN,
    adwrRoomARN,

    -- * Destructuring the response
    AssociateDeviceWithRoomResponse (..),
    mkAssociateDeviceWithRoomResponse,

    -- ** Response lenses
    adwrrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateDeviceWithRoom' smart constructor.
data AssociateDeviceWithRoom = AssociateDeviceWithRoom'
  { -- | The ARN of the device to associate to a room. Required.
    deviceARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the room with which to associate the device. Required.
    roomARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateDeviceWithRoom' with the minimum fields required to make a request.
--
-- * 'deviceARN' - The ARN of the device to associate to a room. Required.
-- * 'roomARN' - The ARN of the room with which to associate the device. Required.
mkAssociateDeviceWithRoom ::
  AssociateDeviceWithRoom
mkAssociateDeviceWithRoom =
  AssociateDeviceWithRoom'
    { deviceARN = Lude.Nothing,
      roomARN = Lude.Nothing
    }

-- | The ARN of the device to associate to a room. Required.
--
-- /Note:/ Consider using 'deviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwrDeviceARN :: Lens.Lens' AssociateDeviceWithRoom (Lude.Maybe Lude.Text)
adwrDeviceARN = Lens.lens (deviceARN :: AssociateDeviceWithRoom -> Lude.Maybe Lude.Text) (\s a -> s {deviceARN = a} :: AssociateDeviceWithRoom)
{-# DEPRECATED adwrDeviceARN "Use generic-lens or generic-optics with 'deviceARN' instead." #-}

-- | The ARN of the room with which to associate the device. Required.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwrRoomARN :: Lens.Lens' AssociateDeviceWithRoom (Lude.Maybe Lude.Text)
adwrRoomARN = Lens.lens (roomARN :: AssociateDeviceWithRoom -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: AssociateDeviceWithRoom)
{-# DEPRECATED adwrRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

instance Lude.AWSRequest AssociateDeviceWithRoom where
  type Rs AssociateDeviceWithRoom = AssociateDeviceWithRoomResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateDeviceWithRoomResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateDeviceWithRoom where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.AssociateDeviceWithRoom" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateDeviceWithRoom where
  toJSON AssociateDeviceWithRoom' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeviceArn" Lude..=) Lude.<$> deviceARN,
            ("RoomArn" Lude..=) Lude.<$> roomARN
          ]
      )

instance Lude.ToPath AssociateDeviceWithRoom where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateDeviceWithRoom where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateDeviceWithRoomResponse' smart constructor.
newtype AssociateDeviceWithRoomResponse = AssociateDeviceWithRoomResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateDeviceWithRoomResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateDeviceWithRoomResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateDeviceWithRoomResponse
mkAssociateDeviceWithRoomResponse pResponseStatus_ =
  AssociateDeviceWithRoomResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwrrsResponseStatus :: Lens.Lens' AssociateDeviceWithRoomResponse Lude.Int
adwrrsResponseStatus = Lens.lens (responseStatus :: AssociateDeviceWithRoomResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateDeviceWithRoomResponse)
{-# DEPRECATED adwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
