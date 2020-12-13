{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteRoomSkillParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes room skill parameter details by room, skill, and parameter key ID.
module Network.AWS.AlexaBusiness.DeleteRoomSkillParameter
  ( -- * Creating a request
    DeleteRoomSkillParameter (..),
    mkDeleteRoomSkillParameter,

    -- ** Request lenses
    drspSkillId,
    drspParameterKey,
    drspRoomARN,

    -- * Destructuring the response
    DeleteRoomSkillParameterResponse (..),
    mkDeleteRoomSkillParameterResponse,

    -- ** Response lenses
    drsprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRoomSkillParameter' smart constructor.
data DeleteRoomSkillParameter = DeleteRoomSkillParameter'
  { -- | The ID of the skill from which to remove the room skill parameter details.
    skillId :: Lude.Text,
    -- | The room skill parameter key for which to remove details.
    parameterKey :: Lude.Text,
    -- | The ARN of the room from which to remove the room skill parameter details.
    roomARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRoomSkillParameter' with the minimum fields required to make a request.
--
-- * 'skillId' - The ID of the skill from which to remove the room skill parameter details.
-- * 'parameterKey' - The room skill parameter key for which to remove details.
-- * 'roomARN' - The ARN of the room from which to remove the room skill parameter details.
mkDeleteRoomSkillParameter ::
  -- | 'skillId'
  Lude.Text ->
  -- | 'parameterKey'
  Lude.Text ->
  DeleteRoomSkillParameter
mkDeleteRoomSkillParameter pSkillId_ pParameterKey_ =
  DeleteRoomSkillParameter'
    { skillId = pSkillId_,
      parameterKey = pParameterKey_,
      roomARN = Lude.Nothing
    }

-- | The ID of the skill from which to remove the room skill parameter details.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drspSkillId :: Lens.Lens' DeleteRoomSkillParameter Lude.Text
drspSkillId = Lens.lens (skillId :: DeleteRoomSkillParameter -> Lude.Text) (\s a -> s {skillId = a} :: DeleteRoomSkillParameter)
{-# DEPRECATED drspSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

-- | The room skill parameter key for which to remove details.
--
-- /Note:/ Consider using 'parameterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drspParameterKey :: Lens.Lens' DeleteRoomSkillParameter Lude.Text
drspParameterKey = Lens.lens (parameterKey :: DeleteRoomSkillParameter -> Lude.Text) (\s a -> s {parameterKey = a} :: DeleteRoomSkillParameter)
{-# DEPRECATED drspParameterKey "Use generic-lens or generic-optics with 'parameterKey' instead." #-}

-- | The ARN of the room from which to remove the room skill parameter details.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drspRoomARN :: Lens.Lens' DeleteRoomSkillParameter (Lude.Maybe Lude.Text)
drspRoomARN = Lens.lens (roomARN :: DeleteRoomSkillParameter -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: DeleteRoomSkillParameter)
{-# DEPRECATED drspRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

instance Lude.AWSRequest DeleteRoomSkillParameter where
  type Rs DeleteRoomSkillParameter = DeleteRoomSkillParameterResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteRoomSkillParameterResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRoomSkillParameter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.DeleteRoomSkillParameter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRoomSkillParameter where
  toJSON DeleteRoomSkillParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SkillId" Lude..= skillId),
            Lude.Just ("ParameterKey" Lude..= parameterKey),
            ("RoomArn" Lude..=) Lude.<$> roomARN
          ]
      )

instance Lude.ToPath DeleteRoomSkillParameter where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRoomSkillParameter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRoomSkillParameterResponse' smart constructor.
newtype DeleteRoomSkillParameterResponse = DeleteRoomSkillParameterResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRoomSkillParameterResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteRoomSkillParameterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRoomSkillParameterResponse
mkDeleteRoomSkillParameterResponse pResponseStatus_ =
  DeleteRoomSkillParameterResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsprsResponseStatus :: Lens.Lens' DeleteRoomSkillParameterResponse Lude.Int
drsprsResponseStatus = Lens.lens (responseStatus :: DeleteRoomSkillParameterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRoomSkillParameterResponse)
{-# DEPRECATED drsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
