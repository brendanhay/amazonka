{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetRoomSkillParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets room skill parameter details by room, skill, and parameter key ARN.
module Network.AWS.AlexaBusiness.GetRoomSkillParameter
  ( -- * Creating a request
    GetRoomSkillParameter (..),
    mkGetRoomSkillParameter,

    -- ** Request lenses
    grspRoomARN,
    grspSkillId,
    grspParameterKey,

    -- * Destructuring the response
    GetRoomSkillParameterResponse (..),
    mkGetRoomSkillParameterResponse,

    -- ** Response lenses
    grsprsRoomSkillParameter,
    grsprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRoomSkillParameter' smart constructor.
data GetRoomSkillParameter = GetRoomSkillParameter'
  { roomARN ::
      Lude.Maybe Lude.Text,
    skillId :: Lude.Text,
    parameterKey :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRoomSkillParameter' with the minimum fields required to make a request.
--
-- * 'parameterKey' - The room skill parameter key for which to get details. Required.
-- * 'roomARN' - The ARN of the room from which to get the room skill parameter details.
-- * 'skillId' - The ARN of the skill from which to get the room skill parameter details. Required.
mkGetRoomSkillParameter ::
  -- | 'skillId'
  Lude.Text ->
  -- | 'parameterKey'
  Lude.Text ->
  GetRoomSkillParameter
mkGetRoomSkillParameter pSkillId_ pParameterKey_ =
  GetRoomSkillParameter'
    { roomARN = Lude.Nothing,
      skillId = pSkillId_,
      parameterKey = pParameterKey_
    }

-- | The ARN of the room from which to get the room skill parameter details.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grspRoomARN :: Lens.Lens' GetRoomSkillParameter (Lude.Maybe Lude.Text)
grspRoomARN = Lens.lens (roomARN :: GetRoomSkillParameter -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: GetRoomSkillParameter)
{-# DEPRECATED grspRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

-- | The ARN of the skill from which to get the room skill parameter details. Required.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grspSkillId :: Lens.Lens' GetRoomSkillParameter Lude.Text
grspSkillId = Lens.lens (skillId :: GetRoomSkillParameter -> Lude.Text) (\s a -> s {skillId = a} :: GetRoomSkillParameter)
{-# DEPRECATED grspSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

-- | The room skill parameter key for which to get details. Required.
--
-- /Note:/ Consider using 'parameterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grspParameterKey :: Lens.Lens' GetRoomSkillParameter Lude.Text
grspParameterKey = Lens.lens (parameterKey :: GetRoomSkillParameter -> Lude.Text) (\s a -> s {parameterKey = a} :: GetRoomSkillParameter)
{-# DEPRECATED grspParameterKey "Use generic-lens or generic-optics with 'parameterKey' instead." #-}

instance Lude.AWSRequest GetRoomSkillParameter where
  type Rs GetRoomSkillParameter = GetRoomSkillParameterResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRoomSkillParameterResponse'
            Lude.<$> (x Lude..?> "RoomSkillParameter")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRoomSkillParameter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.GetRoomSkillParameter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRoomSkillParameter where
  toJSON GetRoomSkillParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoomArn" Lude..=) Lude.<$> roomARN,
            Lude.Just ("SkillId" Lude..= skillId),
            Lude.Just ("ParameterKey" Lude..= parameterKey)
          ]
      )

instance Lude.ToPath GetRoomSkillParameter where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRoomSkillParameter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRoomSkillParameterResponse' smart constructor.
data GetRoomSkillParameterResponse = GetRoomSkillParameterResponse'
  { roomSkillParameter ::
      Lude.Maybe RoomSkillParameter,
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

-- | Creates a value of 'GetRoomSkillParameterResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'roomSkillParameter' - The details of the room skill parameter requested. Required.
mkGetRoomSkillParameterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRoomSkillParameterResponse
mkGetRoomSkillParameterResponse pResponseStatus_ =
  GetRoomSkillParameterResponse'
    { roomSkillParameter = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details of the room skill parameter requested. Required.
--
-- /Note:/ Consider using 'roomSkillParameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsprsRoomSkillParameter :: Lens.Lens' GetRoomSkillParameterResponse (Lude.Maybe RoomSkillParameter)
grsprsRoomSkillParameter = Lens.lens (roomSkillParameter :: GetRoomSkillParameterResponse -> Lude.Maybe RoomSkillParameter) (\s a -> s {roomSkillParameter = a} :: GetRoomSkillParameterResponse)
{-# DEPRECATED grsprsRoomSkillParameter "Use generic-lens or generic-optics with 'roomSkillParameter' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsprsResponseStatus :: Lens.Lens' GetRoomSkillParameterResponse Lude.Int
grsprsResponseStatus = Lens.lens (responseStatus :: GetRoomSkillParameterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRoomSkillParameterResponse)
{-# DEPRECATED grsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
