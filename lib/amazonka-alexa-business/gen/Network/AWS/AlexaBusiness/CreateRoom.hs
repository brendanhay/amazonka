{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a room with the specified details.
module Network.AWS.AlexaBusiness.CreateRoom
  ( -- * Creating a request
    CreateRoom (..),
    mkCreateRoom,

    -- ** Request lenses
    crProfileARN,
    crProviderCalendarId,
    crClientRequestToken,
    crDescription,
    crTags,
    crRoomName,

    -- * Destructuring the response
    CreateRoomResponse (..),
    mkCreateRoomResponse,

    -- ** Response lenses
    crrsRoomARN,
    crrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateRoom' smart constructor.
data CreateRoom = CreateRoom'
  { profileARN :: Lude.Maybe Lude.Text,
    providerCalendarId :: Lude.Maybe Lude.Text,
    clientRequestToken :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    roomName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRoom' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - A unique, user-specified identifier for this request that ensures idempotency.
-- * 'description' - The description for the room.
-- * 'profileARN' - The profile ARN for the room. This is required.
-- * 'providerCalendarId' - The calendar ARN for the room.
-- * 'roomName' - The name for the room.
-- * 'tags' - The tags for the room.
mkCreateRoom ::
  -- | 'roomName'
  Lude.Text ->
  CreateRoom
mkCreateRoom pRoomName_ =
  CreateRoom'
    { profileARN = Lude.Nothing,
      providerCalendarId = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      roomName = pRoomName_
    }

-- | The profile ARN for the room. This is required.
--
-- /Note:/ Consider using 'profileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crProfileARN :: Lens.Lens' CreateRoom (Lude.Maybe Lude.Text)
crProfileARN = Lens.lens (profileARN :: CreateRoom -> Lude.Maybe Lude.Text) (\s a -> s {profileARN = a} :: CreateRoom)
{-# DEPRECATED crProfileARN "Use generic-lens or generic-optics with 'profileARN' instead." #-}

-- | The calendar ARN for the room.
--
-- /Note:/ Consider using 'providerCalendarId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crProviderCalendarId :: Lens.Lens' CreateRoom (Lude.Maybe Lude.Text)
crProviderCalendarId = Lens.lens (providerCalendarId :: CreateRoom -> Lude.Maybe Lude.Text) (\s a -> s {providerCalendarId = a} :: CreateRoom)
{-# DEPRECATED crProviderCalendarId "Use generic-lens or generic-optics with 'providerCalendarId' instead." #-}

-- | A unique, user-specified identifier for this request that ensures idempotency.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crClientRequestToken :: Lens.Lens' CreateRoom (Lude.Maybe Lude.Text)
crClientRequestToken = Lens.lens (clientRequestToken :: CreateRoom -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateRoom)
{-# DEPRECATED crClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The description for the room.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDescription :: Lens.Lens' CreateRoom (Lude.Maybe Lude.Text)
crDescription = Lens.lens (description :: CreateRoom -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateRoom)
{-# DEPRECATED crDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags for the room.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRoom (Lude.Maybe [Tag])
crTags = Lens.lens (tags :: CreateRoom -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateRoom)
{-# DEPRECATED crTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name for the room.
--
-- /Note:/ Consider using 'roomName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRoomName :: Lens.Lens' CreateRoom Lude.Text
crRoomName = Lens.lens (roomName :: CreateRoom -> Lude.Text) (\s a -> s {roomName = a} :: CreateRoom)
{-# DEPRECATED crRoomName "Use generic-lens or generic-optics with 'roomName' instead." #-}

instance Lude.AWSRequest CreateRoom where
  type Rs CreateRoom = CreateRoomResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRoomResponse'
            Lude.<$> (x Lude..?> "RoomArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRoom where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.CreateRoom" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRoom where
  toJSON CreateRoom' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProfileArn" Lude..=) Lude.<$> profileARN,
            ("ProviderCalendarId" Lude..=) Lude.<$> providerCalendarId,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("RoomName" Lude..= roomName)
          ]
      )

instance Lude.ToPath CreateRoom where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRoom where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateRoomResponse' smart constructor.
data CreateRoomResponse = CreateRoomResponse'
  { roomARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateRoomResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'roomARN' - The ARN of the newly created room in the response.
mkCreateRoomResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRoomResponse
mkCreateRoomResponse pResponseStatus_ =
  CreateRoomResponse'
    { roomARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the newly created room in the response.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsRoomARN :: Lens.Lens' CreateRoomResponse (Lude.Maybe Lude.Text)
crrsRoomARN = Lens.lens (roomARN :: CreateRoomResponse -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: CreateRoomResponse)
{-# DEPRECATED crrsRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsResponseStatus :: Lens.Lens' CreateRoomResponse Lude.Int
crrsResponseStatus = Lens.lens (responseStatus :: CreateRoomResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRoomResponse)
{-# DEPRECATED crrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
