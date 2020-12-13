{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a room by the room ARN.
module Network.AWS.AlexaBusiness.DeleteRoom
  ( -- * Creating a request
    DeleteRoom (..),
    mkDeleteRoom,

    -- ** Request lenses
    drRoomARN,

    -- * Destructuring the response
    DeleteRoomResponse (..),
    mkDeleteRoomResponse,

    -- ** Response lenses
    drrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRoom' smart constructor.
newtype DeleteRoom = DeleteRoom'
  { -- | The ARN of the room to delete. Required.
    roomARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRoom' with the minimum fields required to make a request.
--
-- * 'roomARN' - The ARN of the room to delete. Required.
mkDeleteRoom ::
  DeleteRoom
mkDeleteRoom = DeleteRoom' {roomARN = Lude.Nothing}

-- | The ARN of the room to delete. Required.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRoomARN :: Lens.Lens' DeleteRoom (Lude.Maybe Lude.Text)
drRoomARN = Lens.lens (roomARN :: DeleteRoom -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: DeleteRoom)
{-# DEPRECATED drRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

instance Lude.AWSRequest DeleteRoom where
  type Rs DeleteRoom = DeleteRoomResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteRoomResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRoom where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.DeleteRoom" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRoom where
  toJSON DeleteRoom' {..} =
    Lude.object
      (Lude.catMaybes [("RoomArn" Lude..=) Lude.<$> roomARN])

instance Lude.ToPath DeleteRoom where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRoom where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRoomResponse' smart constructor.
newtype DeleteRoomResponse = DeleteRoomResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRoomResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteRoomResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRoomResponse
mkDeleteRoomResponse pResponseStatus_ =
  DeleteRoomResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DeleteRoomResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DeleteRoomResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRoomResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
