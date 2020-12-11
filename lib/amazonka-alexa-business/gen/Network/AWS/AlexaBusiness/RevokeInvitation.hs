{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.RevokeInvitation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes an invitation and invalidates the enrollment URL.
module Network.AWS.AlexaBusiness.RevokeInvitation
  ( -- * Creating a request
    RevokeInvitation (..),
    mkRevokeInvitation,

    -- ** Request lenses
    riEnrollmentId,
    riUserARN,

    -- * Destructuring the response
    RevokeInvitationResponse (..),
    mkRevokeInvitationResponse,

    -- ** Response lenses
    rirsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRevokeInvitation' smart constructor.
data RevokeInvitation = RevokeInvitation'
  { enrollmentId ::
      Lude.Maybe Lude.Text,
    userARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeInvitation' with the minimum fields required to make a request.
--
-- * 'enrollmentId' - The ARN of the enrollment invitation to revoke. Required.
-- * 'userARN' - The ARN of the user for whom to revoke an enrollment invitation. Required.
mkRevokeInvitation ::
  RevokeInvitation
mkRevokeInvitation =
  RevokeInvitation'
    { enrollmentId = Lude.Nothing,
      userARN = Lude.Nothing
    }

-- | The ARN of the enrollment invitation to revoke. Required.
--
-- /Note:/ Consider using 'enrollmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riEnrollmentId :: Lens.Lens' RevokeInvitation (Lude.Maybe Lude.Text)
riEnrollmentId = Lens.lens (enrollmentId :: RevokeInvitation -> Lude.Maybe Lude.Text) (\s a -> s {enrollmentId = a} :: RevokeInvitation)
{-# DEPRECATED riEnrollmentId "Use generic-lens or generic-optics with 'enrollmentId' instead." #-}

-- | The ARN of the user for whom to revoke an enrollment invitation. Required.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riUserARN :: Lens.Lens' RevokeInvitation (Lude.Maybe Lude.Text)
riUserARN = Lens.lens (userARN :: RevokeInvitation -> Lude.Maybe Lude.Text) (\s a -> s {userARN = a} :: RevokeInvitation)
{-# DEPRECATED riUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

instance Lude.AWSRequest RevokeInvitation where
  type Rs RevokeInvitation = RevokeInvitationResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RevokeInvitationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RevokeInvitation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.RevokeInvitation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RevokeInvitation where
  toJSON RevokeInvitation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EnrollmentId" Lude..=) Lude.<$> enrollmentId,
            ("UserArn" Lude..=) Lude.<$> userARN
          ]
      )

instance Lude.ToPath RevokeInvitation where
  toPath = Lude.const "/"

instance Lude.ToQuery RevokeInvitation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRevokeInvitationResponse' smart constructor.
newtype RevokeInvitationResponse = RevokeInvitationResponse'
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

-- | Creates a value of 'RevokeInvitationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRevokeInvitationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RevokeInvitationResponse
mkRevokeInvitationResponse pResponseStatus_ =
  RevokeInvitationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirsResponseStatus :: Lens.Lens' RevokeInvitationResponse Lude.Int
rirsResponseStatus = Lens.lens (responseStatus :: RevokeInvitationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RevokeInvitationResponse)
{-# DEPRECATED rirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
