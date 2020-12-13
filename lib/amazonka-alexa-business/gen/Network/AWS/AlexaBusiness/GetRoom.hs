{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets room details by room ARN.
module Network.AWS.AlexaBusiness.GetRoom
  ( -- * Creating a request
    GetRoom (..),
    mkGetRoom,

    -- ** Request lenses
    grRoomARN,

    -- * Destructuring the response
    GetRoomResponse (..),
    mkGetRoomResponse,

    -- ** Response lenses
    grrsRoom,
    grrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRoom' smart constructor.
newtype GetRoom = GetRoom'
  { -- | The ARN of the room for which to request details. Required.
    roomARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRoom' with the minimum fields required to make a request.
--
-- * 'roomARN' - The ARN of the room for which to request details. Required.
mkGetRoom ::
  GetRoom
mkGetRoom = GetRoom' {roomARN = Lude.Nothing}

-- | The ARN of the room for which to request details. Required.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRoomARN :: Lens.Lens' GetRoom (Lude.Maybe Lude.Text)
grRoomARN = Lens.lens (roomARN :: GetRoom -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: GetRoom)
{-# DEPRECATED grRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

instance Lude.AWSRequest GetRoom where
  type Rs GetRoom = GetRoomResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRoomResponse'
            Lude.<$> (x Lude..?> "Room") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRoom where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.GetRoom" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRoom where
  toJSON GetRoom' {..} =
    Lude.object
      (Lude.catMaybes [("RoomArn" Lude..=) Lude.<$> roomARN])

instance Lude.ToPath GetRoom where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRoom where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRoomResponse' smart constructor.
data GetRoomResponse = GetRoomResponse'
  { -- | The details of the room requested.
    room :: Lude.Maybe Room,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRoomResponse' with the minimum fields required to make a request.
--
-- * 'room' - The details of the room requested.
-- * 'responseStatus' - The response status code.
mkGetRoomResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRoomResponse
mkGetRoomResponse pResponseStatus_ =
  GetRoomResponse'
    { room = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details of the room requested.
--
-- /Note:/ Consider using 'room' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsRoom :: Lens.Lens' GetRoomResponse (Lude.Maybe Room)
grrsRoom = Lens.lens (room :: GetRoomResponse -> Lude.Maybe Room) (\s a -> s {room = a} :: GetRoomResponse)
{-# DEPRECATED grrsRoom "Use generic-lens or generic-optics with 'room' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResponseStatus :: Lens.Lens' GetRoomResponse Lude.Int
grrsResponseStatus = Lens.lens (responseStatus :: GetRoomResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRoomResponse)
{-# DEPRECATED grrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
