{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ResolveRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines the details for the room from which a skill request was invoked. This operation is used by skill developers.
module Network.AWS.AlexaBusiness.ResolveRoom
  ( -- * Creating a request
    ResolveRoom (..),
    mkResolveRoom,

    -- ** Request lenses
    rrUserId,
    rrSkillId,

    -- * Destructuring the response
    ResolveRoomResponse (..),
    mkResolveRoomResponse,

    -- ** Response lenses
    rrrsRoomSkillParameters,
    rrrsRoomARN,
    rrrsRoomName,
    rrrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkResolveRoom' smart constructor.
data ResolveRoom = ResolveRoom'
  { userId :: Lude.Text,
    skillId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResolveRoom' with the minimum fields required to make a request.
--
-- * 'skillId' - The ARN of the skill that was requested. Required.
-- * 'userId' - The ARN of the user. Required.
mkResolveRoom ::
  -- | 'userId'
  Lude.Text ->
  -- | 'skillId'
  Lude.Text ->
  ResolveRoom
mkResolveRoom pUserId_ pSkillId_ =
  ResolveRoom' {userId = pUserId_, skillId = pSkillId_}

-- | The ARN of the user. Required.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrUserId :: Lens.Lens' ResolveRoom Lude.Text
rrUserId = Lens.lens (userId :: ResolveRoom -> Lude.Text) (\s a -> s {userId = a} :: ResolveRoom)
{-# DEPRECATED rrUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The ARN of the skill that was requested. Required.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrSkillId :: Lens.Lens' ResolveRoom Lude.Text
rrSkillId = Lens.lens (skillId :: ResolveRoom -> Lude.Text) (\s a -> s {skillId = a} :: ResolveRoom)
{-# DEPRECATED rrSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

instance Lude.AWSRequest ResolveRoom where
  type Rs ResolveRoom = ResolveRoomResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          ResolveRoomResponse'
            Lude.<$> (x Lude..?> "RoomSkillParameters" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "RoomArn")
            Lude.<*> (x Lude..?> "RoomName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResolveRoom where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.ResolveRoom" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResolveRoom where
  toJSON ResolveRoom' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserId" Lude..= userId),
            Lude.Just ("SkillId" Lude..= skillId)
          ]
      )

instance Lude.ToPath ResolveRoom where
  toPath = Lude.const "/"

instance Lude.ToQuery ResolveRoom where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkResolveRoomResponse' smart constructor.
data ResolveRoomResponse = ResolveRoomResponse'
  { roomSkillParameters ::
      Lude.Maybe [RoomSkillParameter],
    roomARN :: Lude.Maybe Lude.Text,
    roomName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ResolveRoomResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'roomARN' - The ARN of the room from which the skill request was invoked.
-- * 'roomName' - The name of the room from which the skill request was invoked.
-- * 'roomSkillParameters' - Response to get the room profile request. Required.
mkResolveRoomResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResolveRoomResponse
mkResolveRoomResponse pResponseStatus_ =
  ResolveRoomResponse'
    { roomSkillParameters = Lude.Nothing,
      roomARN = Lude.Nothing,
      roomName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Response to get the room profile request. Required.
--
-- /Note:/ Consider using 'roomSkillParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrsRoomSkillParameters :: Lens.Lens' ResolveRoomResponse (Lude.Maybe [RoomSkillParameter])
rrrsRoomSkillParameters = Lens.lens (roomSkillParameters :: ResolveRoomResponse -> Lude.Maybe [RoomSkillParameter]) (\s a -> s {roomSkillParameters = a} :: ResolveRoomResponse)
{-# DEPRECATED rrrsRoomSkillParameters "Use generic-lens or generic-optics with 'roomSkillParameters' instead." #-}

-- | The ARN of the room from which the skill request was invoked.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrsRoomARN :: Lens.Lens' ResolveRoomResponse (Lude.Maybe Lude.Text)
rrrsRoomARN = Lens.lens (roomARN :: ResolveRoomResponse -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: ResolveRoomResponse)
{-# DEPRECATED rrrsRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

-- | The name of the room from which the skill request was invoked.
--
-- /Note:/ Consider using 'roomName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrsRoomName :: Lens.Lens' ResolveRoomResponse (Lude.Maybe Lude.Text)
rrrsRoomName = Lens.lens (roomName :: ResolveRoomResponse -> Lude.Maybe Lude.Text) (\s a -> s {roomName = a} :: ResolveRoomResponse)
{-# DEPRECATED rrrsRoomName "Use generic-lens or generic-optics with 'roomName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrsResponseStatus :: Lens.Lens' ResolveRoomResponse Lude.Int
rrrsResponseStatus = Lens.lens (responseStatus :: ResolveRoomResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResolveRoomResponse)
{-# DEPRECATED rrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
