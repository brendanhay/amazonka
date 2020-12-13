{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DisassociateSkillGroupFromRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a skill group from a specified room. This disables all skills in the skill group on all devices in the room.
module Network.AWS.AlexaBusiness.DisassociateSkillGroupFromRoom
  ( -- * Creating a request
    DisassociateSkillGroupFromRoom (..),
    mkDisassociateSkillGroupFromRoom,

    -- ** Request lenses
    dsgfrSkillGroupARN,
    dsgfrRoomARN,

    -- * Destructuring the response
    DisassociateSkillGroupFromRoomResponse (..),
    mkDisassociateSkillGroupFromRoomResponse,

    -- ** Response lenses
    dsgfrrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateSkillGroupFromRoom' smart constructor.
data DisassociateSkillGroupFromRoom = DisassociateSkillGroupFromRoom'
  { -- | The ARN of the skill group to disassociate from a room. Required.
    skillGroupARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the room from which the skill group is to be disassociated. Required.
    roomARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateSkillGroupFromRoom' with the minimum fields required to make a request.
--
-- * 'skillGroupARN' - The ARN of the skill group to disassociate from a room. Required.
-- * 'roomARN' - The ARN of the room from which the skill group is to be disassociated. Required.
mkDisassociateSkillGroupFromRoom ::
  DisassociateSkillGroupFromRoom
mkDisassociateSkillGroupFromRoom =
  DisassociateSkillGroupFromRoom'
    { skillGroupARN = Lude.Nothing,
      roomARN = Lude.Nothing
    }

-- | The ARN of the skill group to disassociate from a room. Required.
--
-- /Note:/ Consider using 'skillGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgfrSkillGroupARN :: Lens.Lens' DisassociateSkillGroupFromRoom (Lude.Maybe Lude.Text)
dsgfrSkillGroupARN = Lens.lens (skillGroupARN :: DisassociateSkillGroupFromRoom -> Lude.Maybe Lude.Text) (\s a -> s {skillGroupARN = a} :: DisassociateSkillGroupFromRoom)
{-# DEPRECATED dsgfrSkillGroupARN "Use generic-lens or generic-optics with 'skillGroupARN' instead." #-}

-- | The ARN of the room from which the skill group is to be disassociated. Required.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgfrRoomARN :: Lens.Lens' DisassociateSkillGroupFromRoom (Lude.Maybe Lude.Text)
dsgfrRoomARN = Lens.lens (roomARN :: DisassociateSkillGroupFromRoom -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: DisassociateSkillGroupFromRoom)
{-# DEPRECATED dsgfrRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

instance Lude.AWSRequest DisassociateSkillGroupFromRoom where
  type
    Rs DisassociateSkillGroupFromRoom =
      DisassociateSkillGroupFromRoomResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateSkillGroupFromRoomResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateSkillGroupFromRoom where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AlexaForBusiness.DisassociateSkillGroupFromRoom" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateSkillGroupFromRoom where
  toJSON DisassociateSkillGroupFromRoom' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SkillGroupArn" Lude..=) Lude.<$> skillGroupARN,
            ("RoomArn" Lude..=) Lude.<$> roomARN
          ]
      )

instance Lude.ToPath DisassociateSkillGroupFromRoom where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateSkillGroupFromRoom where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateSkillGroupFromRoomResponse' smart constructor.
newtype DisassociateSkillGroupFromRoomResponse = DisassociateSkillGroupFromRoomResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateSkillGroupFromRoomResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateSkillGroupFromRoomResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateSkillGroupFromRoomResponse
mkDisassociateSkillGroupFromRoomResponse pResponseStatus_ =
  DisassociateSkillGroupFromRoomResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgfrrsResponseStatus :: Lens.Lens' DisassociateSkillGroupFromRoomResponse Lude.Int
dsgfrrsResponseStatus = Lens.lens (responseStatus :: DisassociateSkillGroupFromRoomResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateSkillGroupFromRoomResponse)
{-# DEPRECATED dsgfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
