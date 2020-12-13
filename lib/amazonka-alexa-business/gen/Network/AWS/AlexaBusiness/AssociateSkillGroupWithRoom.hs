{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill group with a given room. This enables all skills in the associated skill group on all devices in the room.
module Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom
  ( -- * Creating a request
    AssociateSkillGroupWithRoom (..),
    mkAssociateSkillGroupWithRoom,

    -- ** Request lenses
    asgwrSkillGroupARN,
    asgwrRoomARN,

    -- * Destructuring the response
    AssociateSkillGroupWithRoomResponse (..),
    mkAssociateSkillGroupWithRoomResponse,

    -- ** Response lenses
    asgwrrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateSkillGroupWithRoom' smart constructor.
data AssociateSkillGroupWithRoom = AssociateSkillGroupWithRoom'
  { -- | The ARN of the skill group to associate with a room. Required.
    skillGroupARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the room with which to associate the skill group. Required.
    roomARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateSkillGroupWithRoom' with the minimum fields required to make a request.
--
-- * 'skillGroupARN' - The ARN of the skill group to associate with a room. Required.
-- * 'roomARN' - The ARN of the room with which to associate the skill group. Required.
mkAssociateSkillGroupWithRoom ::
  AssociateSkillGroupWithRoom
mkAssociateSkillGroupWithRoom =
  AssociateSkillGroupWithRoom'
    { skillGroupARN = Lude.Nothing,
      roomARN = Lude.Nothing
    }

-- | The ARN of the skill group to associate with a room. Required.
--
-- /Note:/ Consider using 'skillGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgwrSkillGroupARN :: Lens.Lens' AssociateSkillGroupWithRoom (Lude.Maybe Lude.Text)
asgwrSkillGroupARN = Lens.lens (skillGroupARN :: AssociateSkillGroupWithRoom -> Lude.Maybe Lude.Text) (\s a -> s {skillGroupARN = a} :: AssociateSkillGroupWithRoom)
{-# DEPRECATED asgwrSkillGroupARN "Use generic-lens or generic-optics with 'skillGroupARN' instead." #-}

-- | The ARN of the room with which to associate the skill group. Required.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgwrRoomARN :: Lens.Lens' AssociateSkillGroupWithRoom (Lude.Maybe Lude.Text)
asgwrRoomARN = Lens.lens (roomARN :: AssociateSkillGroupWithRoom -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: AssociateSkillGroupWithRoom)
{-# DEPRECATED asgwrRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

instance Lude.AWSRequest AssociateSkillGroupWithRoom where
  type
    Rs AssociateSkillGroupWithRoom =
      AssociateSkillGroupWithRoomResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateSkillGroupWithRoomResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateSkillGroupWithRoom where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AlexaForBusiness.AssociateSkillGroupWithRoom" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateSkillGroupWithRoom where
  toJSON AssociateSkillGroupWithRoom' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SkillGroupArn" Lude..=) Lude.<$> skillGroupARN,
            ("RoomArn" Lude..=) Lude.<$> roomARN
          ]
      )

instance Lude.ToPath AssociateSkillGroupWithRoom where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateSkillGroupWithRoom where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateSkillGroupWithRoomResponse' smart constructor.
newtype AssociateSkillGroupWithRoomResponse = AssociateSkillGroupWithRoomResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateSkillGroupWithRoomResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateSkillGroupWithRoomResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateSkillGroupWithRoomResponse
mkAssociateSkillGroupWithRoomResponse pResponseStatus_ =
  AssociateSkillGroupWithRoomResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgwrrsResponseStatus :: Lens.Lens' AssociateSkillGroupWithRoomResponse Lude.Int
asgwrrsResponseStatus = Lens.lens (responseStatus :: AssociateSkillGroupWithRoomResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateSkillGroupWithRoomResponse)
{-# DEPRECATED asgwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
