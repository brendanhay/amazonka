{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DisassociateDeviceFromRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a device from its current room. The device continues to be connected to the Wi-Fi network and is still registered to the account. The device settings and skills are removed from the room.
module Network.AWS.AlexaBusiness.DisassociateDeviceFromRoom
  ( -- * Creating a request
    DisassociateDeviceFromRoom (..),
    mkDisassociateDeviceFromRoom,

    -- ** Request lenses
    ddfrDeviceARN,

    -- * Destructuring the response
    DisassociateDeviceFromRoomResponse (..),
    mkDisassociateDeviceFromRoomResponse,

    -- ** Response lenses
    ddfrrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateDeviceFromRoom' smart constructor.
newtype DisassociateDeviceFromRoom = DisassociateDeviceFromRoom'
  { -- | The ARN of the device to disassociate from a room. Required.
    deviceARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateDeviceFromRoom' with the minimum fields required to make a request.
--
-- * 'deviceARN' - The ARN of the device to disassociate from a room. Required.
mkDisassociateDeviceFromRoom ::
  DisassociateDeviceFromRoom
mkDisassociateDeviceFromRoom =
  DisassociateDeviceFromRoom' {deviceARN = Lude.Nothing}

-- | The ARN of the device to disassociate from a room. Required.
--
-- /Note:/ Consider using 'deviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfrDeviceARN :: Lens.Lens' DisassociateDeviceFromRoom (Lude.Maybe Lude.Text)
ddfrDeviceARN = Lens.lens (deviceARN :: DisassociateDeviceFromRoom -> Lude.Maybe Lude.Text) (\s a -> s {deviceARN = a} :: DisassociateDeviceFromRoom)
{-# DEPRECATED ddfrDeviceARN "Use generic-lens or generic-optics with 'deviceARN' instead." #-}

instance Lude.AWSRequest DisassociateDeviceFromRoom where
  type
    Rs DisassociateDeviceFromRoom =
      DisassociateDeviceFromRoomResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateDeviceFromRoomResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateDeviceFromRoom where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.DisassociateDeviceFromRoom" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateDeviceFromRoom where
  toJSON DisassociateDeviceFromRoom' {..} =
    Lude.object
      (Lude.catMaybes [("DeviceArn" Lude..=) Lude.<$> deviceARN])

instance Lude.ToPath DisassociateDeviceFromRoom where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateDeviceFromRoom where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateDeviceFromRoomResponse' smart constructor.
newtype DisassociateDeviceFromRoomResponse = DisassociateDeviceFromRoomResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateDeviceFromRoomResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateDeviceFromRoomResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateDeviceFromRoomResponse
mkDisassociateDeviceFromRoomResponse pResponseStatus_ =
  DisassociateDeviceFromRoomResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfrrsResponseStatus :: Lens.Lens' DisassociateDeviceFromRoomResponse Lude.Int
ddfrrsResponseStatus = Lens.lens (responseStatus :: DisassociateDeviceFromRoomResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateDeviceFromRoomResponse)
{-# DEPRECATED ddfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
