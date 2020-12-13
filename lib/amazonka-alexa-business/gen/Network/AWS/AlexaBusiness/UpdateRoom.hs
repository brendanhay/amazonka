{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates room details by room ARN.
module Network.AWS.AlexaBusiness.UpdateRoom
  ( -- * Creating a request
    UpdateRoom (..),
    mkUpdateRoom,

    -- ** Request lenses
    urProfileARN,
    urProviderCalendarId,
    urRoomARN,
    urRoomName,
    urDescription,

    -- * Destructuring the response
    UpdateRoomResponse (..),
    mkUpdateRoomResponse,

    -- ** Response lenses
    urrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateRoom' smart constructor.
data UpdateRoom = UpdateRoom'
  { -- | The updated profile ARN for the room.
    profileARN :: Lude.Maybe Lude.Text,
    -- | The updated provider calendar ARN for the room.
    providerCalendarId :: Lude.Maybe Lude.Text,
    -- | The ARN of the room to update.
    roomARN :: Lude.Maybe Lude.Text,
    -- | The updated name for the room.
    roomName :: Lude.Maybe Lude.Text,
    -- | The updated description for the room.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRoom' with the minimum fields required to make a request.
--
-- * 'profileARN' - The updated profile ARN for the room.
-- * 'providerCalendarId' - The updated provider calendar ARN for the room.
-- * 'roomARN' - The ARN of the room to update.
-- * 'roomName' - The updated name for the room.
-- * 'description' - The updated description for the room.
mkUpdateRoom ::
  UpdateRoom
mkUpdateRoom =
  UpdateRoom'
    { profileARN = Lude.Nothing,
      providerCalendarId = Lude.Nothing,
      roomARN = Lude.Nothing,
      roomName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The updated profile ARN for the room.
--
-- /Note:/ Consider using 'profileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urProfileARN :: Lens.Lens' UpdateRoom (Lude.Maybe Lude.Text)
urProfileARN = Lens.lens (profileARN :: UpdateRoom -> Lude.Maybe Lude.Text) (\s a -> s {profileARN = a} :: UpdateRoom)
{-# DEPRECATED urProfileARN "Use generic-lens or generic-optics with 'profileARN' instead." #-}

-- | The updated provider calendar ARN for the room.
--
-- /Note:/ Consider using 'providerCalendarId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urProviderCalendarId :: Lens.Lens' UpdateRoom (Lude.Maybe Lude.Text)
urProviderCalendarId = Lens.lens (providerCalendarId :: UpdateRoom -> Lude.Maybe Lude.Text) (\s a -> s {providerCalendarId = a} :: UpdateRoom)
{-# DEPRECATED urProviderCalendarId "Use generic-lens or generic-optics with 'providerCalendarId' instead." #-}

-- | The ARN of the room to update.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRoomARN :: Lens.Lens' UpdateRoom (Lude.Maybe Lude.Text)
urRoomARN = Lens.lens (roomARN :: UpdateRoom -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: UpdateRoom)
{-# DEPRECATED urRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

-- | The updated name for the room.
--
-- /Note:/ Consider using 'roomName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRoomName :: Lens.Lens' UpdateRoom (Lude.Maybe Lude.Text)
urRoomName = Lens.lens (roomName :: UpdateRoom -> Lude.Maybe Lude.Text) (\s a -> s {roomName = a} :: UpdateRoom)
{-# DEPRECATED urRoomName "Use generic-lens or generic-optics with 'roomName' instead." #-}

-- | The updated description for the room.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDescription :: Lens.Lens' UpdateRoom (Lude.Maybe Lude.Text)
urDescription = Lens.lens (description :: UpdateRoom -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateRoom)
{-# DEPRECATED urDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateRoom where
  type Rs UpdateRoom = UpdateRoomResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateRoomResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRoom where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.UpdateRoom" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRoom where
  toJSON UpdateRoom' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProfileArn" Lude..=) Lude.<$> profileARN,
            ("ProviderCalendarId" Lude..=) Lude.<$> providerCalendarId,
            ("RoomArn" Lude..=) Lude.<$> roomARN,
            ("RoomName" Lude..=) Lude.<$> roomName,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateRoom where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRoom where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRoomResponse' smart constructor.
newtype UpdateRoomResponse = UpdateRoomResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRoomResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateRoomResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRoomResponse
mkUpdateRoomResponse pResponseStatus_ =
  UpdateRoomResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UpdateRoomResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UpdateRoomResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRoomResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
