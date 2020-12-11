{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.PutRoomSkillParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates room skill parameter details by room, skill, and parameter key ID. Not all skills have a room skill parameter.
module Network.AWS.AlexaBusiness.PutRoomSkillParameter
  ( -- * Creating a request
    PutRoomSkillParameter (..),
    mkPutRoomSkillParameter,

    -- ** Request lenses
    prspRoomARN,
    prspSkillId,
    prspRoomSkillParameter,

    -- * Destructuring the response
    PutRoomSkillParameterResponse (..),
    mkPutRoomSkillParameterResponse,

    -- ** Response lenses
    prsprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutRoomSkillParameter' smart constructor.
data PutRoomSkillParameter = PutRoomSkillParameter'
  { roomARN ::
      Lude.Maybe Lude.Text,
    skillId :: Lude.Text,
    roomSkillParameter :: RoomSkillParameter
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRoomSkillParameter' with the minimum fields required to make a request.
--
-- * 'roomARN' - The ARN of the room associated with the room skill parameter. Required.
-- * 'roomSkillParameter' - The updated room skill parameter. Required.
-- * 'skillId' - The ARN of the skill associated with the room skill parameter. Required.
mkPutRoomSkillParameter ::
  -- | 'skillId'
  Lude.Text ->
  -- | 'roomSkillParameter'
  RoomSkillParameter ->
  PutRoomSkillParameter
mkPutRoomSkillParameter pSkillId_ pRoomSkillParameter_ =
  PutRoomSkillParameter'
    { roomARN = Lude.Nothing,
      skillId = pSkillId_,
      roomSkillParameter = pRoomSkillParameter_
    }

-- | The ARN of the room associated with the room skill parameter. Required.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prspRoomARN :: Lens.Lens' PutRoomSkillParameter (Lude.Maybe Lude.Text)
prspRoomARN = Lens.lens (roomARN :: PutRoomSkillParameter -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: PutRoomSkillParameter)
{-# DEPRECATED prspRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

-- | The ARN of the skill associated with the room skill parameter. Required.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prspSkillId :: Lens.Lens' PutRoomSkillParameter Lude.Text
prspSkillId = Lens.lens (skillId :: PutRoomSkillParameter -> Lude.Text) (\s a -> s {skillId = a} :: PutRoomSkillParameter)
{-# DEPRECATED prspSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

-- | The updated room skill parameter. Required.
--
-- /Note:/ Consider using 'roomSkillParameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prspRoomSkillParameter :: Lens.Lens' PutRoomSkillParameter RoomSkillParameter
prspRoomSkillParameter = Lens.lens (roomSkillParameter :: PutRoomSkillParameter -> RoomSkillParameter) (\s a -> s {roomSkillParameter = a} :: PutRoomSkillParameter)
{-# DEPRECATED prspRoomSkillParameter "Use generic-lens or generic-optics with 'roomSkillParameter' instead." #-}

instance Lude.AWSRequest PutRoomSkillParameter where
  type Rs PutRoomSkillParameter = PutRoomSkillParameterResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutRoomSkillParameterResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutRoomSkillParameter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.PutRoomSkillParameter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutRoomSkillParameter where
  toJSON PutRoomSkillParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoomArn" Lude..=) Lude.<$> roomARN,
            Lude.Just ("SkillId" Lude..= skillId),
            Lude.Just ("RoomSkillParameter" Lude..= roomSkillParameter)
          ]
      )

instance Lude.ToPath PutRoomSkillParameter where
  toPath = Lude.const "/"

instance Lude.ToQuery PutRoomSkillParameter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutRoomSkillParameterResponse' smart constructor.
newtype PutRoomSkillParameterResponse = PutRoomSkillParameterResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRoomSkillParameterResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutRoomSkillParameterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutRoomSkillParameterResponse
mkPutRoomSkillParameterResponse pResponseStatus_ =
  PutRoomSkillParameterResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsprsResponseStatus :: Lens.Lens' PutRoomSkillParameterResponse Lude.Int
prsprsResponseStatus = Lens.lens (responseStatus :: PutRoomSkillParameterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutRoomSkillParameterResponse)
{-# DEPRECATED prsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
