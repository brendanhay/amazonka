{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.PutSkillAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Links a user's account to a third-party skill provider. If this API operation is called by an assumed IAM role, the skill being linked must be a private skill. Also, the skill must be owned by the AWS account that assumed the IAM role.
module Network.AWS.AlexaBusiness.PutSkillAuthorization
  ( -- * Creating a request
    PutSkillAuthorization (..),
    mkPutSkillAuthorization,

    -- ** Request lenses
    psaRoomARN,
    psaAuthorizationResult,
    psaSkillId,

    -- * Destructuring the response
    PutSkillAuthorizationResponse (..),
    mkPutSkillAuthorizationResponse,

    -- ** Response lenses
    psarsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutSkillAuthorization' smart constructor.
data PutSkillAuthorization = PutSkillAuthorization'
  { roomARN ::
      Lude.Maybe Lude.Text,
    authorizationResult ::
      Lude.HashMap Lude.Text (Lude.Text),
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

-- | Creates a value of 'PutSkillAuthorization' with the minimum fields required to make a request.
--
-- * 'authorizationResult' - The authorization result specific to OAUTH code grant output. "Code” must be populated in the AuthorizationResult map to establish the authorization.
-- * 'roomARN' - The room that the skill is authorized for.
-- * 'skillId' - The unique identifier of a skill.
mkPutSkillAuthorization ::
  -- | 'skillId'
  Lude.Text ->
  PutSkillAuthorization
mkPutSkillAuthorization pSkillId_ =
  PutSkillAuthorization'
    { roomARN = Lude.Nothing,
      authorizationResult = Lude.mempty,
      skillId = pSkillId_
    }

-- | The room that the skill is authorized for.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psaRoomARN :: Lens.Lens' PutSkillAuthorization (Lude.Maybe Lude.Text)
psaRoomARN = Lens.lens (roomARN :: PutSkillAuthorization -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: PutSkillAuthorization)
{-# DEPRECATED psaRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

-- | The authorization result specific to OAUTH code grant output. "Code” must be populated in the AuthorizationResult map to establish the authorization.
--
-- /Note:/ Consider using 'authorizationResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psaAuthorizationResult :: Lens.Lens' PutSkillAuthorization (Lude.HashMap Lude.Text (Lude.Text))
psaAuthorizationResult = Lens.lens (authorizationResult :: PutSkillAuthorization -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {authorizationResult = a} :: PutSkillAuthorization)
{-# DEPRECATED psaAuthorizationResult "Use generic-lens or generic-optics with 'authorizationResult' instead." #-}

-- | The unique identifier of a skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psaSkillId :: Lens.Lens' PutSkillAuthorization Lude.Text
psaSkillId = Lens.lens (skillId :: PutSkillAuthorization -> Lude.Text) (\s a -> s {skillId = a} :: PutSkillAuthorization)
{-# DEPRECATED psaSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

instance Lude.AWSRequest PutSkillAuthorization where
  type Rs PutSkillAuthorization = PutSkillAuthorizationResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutSkillAuthorizationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutSkillAuthorization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.PutSkillAuthorization" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutSkillAuthorization where
  toJSON PutSkillAuthorization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoomArn" Lude..=) Lude.<$> roomARN,
            Lude.Just ("AuthorizationResult" Lude..= authorizationResult),
            Lude.Just ("SkillId" Lude..= skillId)
          ]
      )

instance Lude.ToPath PutSkillAuthorization where
  toPath = Lude.const "/"

instance Lude.ToQuery PutSkillAuthorization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutSkillAuthorizationResponse' smart constructor.
newtype PutSkillAuthorizationResponse = PutSkillAuthorizationResponse'
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

-- | Creates a value of 'PutSkillAuthorizationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutSkillAuthorizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutSkillAuthorizationResponse
mkPutSkillAuthorizationResponse pResponseStatus_ =
  PutSkillAuthorizationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psarsResponseStatus :: Lens.Lens' PutSkillAuthorizationResponse Lude.Int
psarsResponseStatus = Lens.lens (responseStatus :: PutSkillAuthorizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutSkillAuthorizationResponse)
{-# DEPRECATED psarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
