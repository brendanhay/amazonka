{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteSkillAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a third-party account from a skill.
module Network.AWS.AlexaBusiness.DeleteSkillAuthorization
  ( -- * Creating a request
    DeleteSkillAuthorization (..),
    mkDeleteSkillAuthorization,

    -- ** Request lenses
    dsaRoomARN,
    dsaSkillId,

    -- * Destructuring the response
    DeleteSkillAuthorizationResponse (..),
    mkDeleteSkillAuthorizationResponse,

    -- ** Response lenses
    dsarsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSkillAuthorization' smart constructor.
data DeleteSkillAuthorization = DeleteSkillAuthorization'
  { roomARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DeleteSkillAuthorization' with the minimum fields required to make a request.
--
-- * 'roomARN' - The room that the skill is authorized for.
-- * 'skillId' - The unique identifier of a skill.
mkDeleteSkillAuthorization ::
  -- | 'skillId'
  Lude.Text ->
  DeleteSkillAuthorization
mkDeleteSkillAuthorization pSkillId_ =
  DeleteSkillAuthorization'
    { roomARN = Lude.Nothing,
      skillId = pSkillId_
    }

-- | The room that the skill is authorized for.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaRoomARN :: Lens.Lens' DeleteSkillAuthorization (Lude.Maybe Lude.Text)
dsaRoomARN = Lens.lens (roomARN :: DeleteSkillAuthorization -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: DeleteSkillAuthorization)
{-# DEPRECATED dsaRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

-- | The unique identifier of a skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaSkillId :: Lens.Lens' DeleteSkillAuthorization Lude.Text
dsaSkillId = Lens.lens (skillId :: DeleteSkillAuthorization -> Lude.Text) (\s a -> s {skillId = a} :: DeleteSkillAuthorization)
{-# DEPRECATED dsaSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

instance Lude.AWSRequest DeleteSkillAuthorization where
  type Rs DeleteSkillAuthorization = DeleteSkillAuthorizationResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteSkillAuthorizationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSkillAuthorization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.DeleteSkillAuthorization" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteSkillAuthorization where
  toJSON DeleteSkillAuthorization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoomArn" Lude..=) Lude.<$> roomARN,
            Lude.Just ("SkillId" Lude..= skillId)
          ]
      )

instance Lude.ToPath DeleteSkillAuthorization where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSkillAuthorization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSkillAuthorizationResponse' smart constructor.
newtype DeleteSkillAuthorizationResponse = DeleteSkillAuthorizationResponse'
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

-- | Creates a value of 'DeleteSkillAuthorizationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteSkillAuthorizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSkillAuthorizationResponse
mkDeleteSkillAuthorizationResponse pResponseStatus_ =
  DeleteSkillAuthorizationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsResponseStatus :: Lens.Lens' DeleteSkillAuthorizationResponse Lude.Int
dsarsResponseStatus = Lens.lens (responseStatus :: DeleteSkillAuthorizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSkillAuthorizationResponse)
{-# DEPRECATED dsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
