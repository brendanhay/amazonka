{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ResetPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the administrator to reset the password for a user.
module Network.AWS.WorkMail.ResetPassword
  ( -- * Creating a request
    ResetPassword (..),
    mkResetPassword,

    -- ** Request lenses
    rpOrganizationId,
    rpUserId,
    rpPassword,

    -- * Destructuring the response
    ResetPasswordResponse (..),
    mkResetPasswordResponse,

    -- ** Response lenses
    rprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkResetPassword' smart constructor.
data ResetPassword = ResetPassword'
  { organizationId :: Lude.Text,
    userId :: Lude.Text,
    password :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetPassword' with the minimum fields required to make a request.
--
-- * 'organizationId' - The identifier of the organization that contains the user for which the password is reset.
-- * 'password' - The new password for the user.
-- * 'userId' - The identifier of the user for whom the password is reset.
mkResetPassword ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  -- | 'password'
  Lude.Sensitive Lude.Text ->
  ResetPassword
mkResetPassword pOrganizationId_ pUserId_ pPassword_ =
  ResetPassword'
    { organizationId = pOrganizationId_,
      userId = pUserId_,
      password = pPassword_
    }

-- | The identifier of the organization that contains the user for which the password is reset.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpOrganizationId :: Lens.Lens' ResetPassword Lude.Text
rpOrganizationId = Lens.lens (organizationId :: ResetPassword -> Lude.Text) (\s a -> s {organizationId = a} :: ResetPassword)
{-# DEPRECATED rpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier of the user for whom the password is reset.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpUserId :: Lens.Lens' ResetPassword Lude.Text
rpUserId = Lens.lens (userId :: ResetPassword -> Lude.Text) (\s a -> s {userId = a} :: ResetPassword)
{-# DEPRECATED rpUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The new password for the user.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpPassword :: Lens.Lens' ResetPassword (Lude.Sensitive Lude.Text)
rpPassword = Lens.lens (password :: ResetPassword -> Lude.Sensitive Lude.Text) (\s a -> s {password = a} :: ResetPassword)
{-# DEPRECATED rpPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Lude.AWSRequest ResetPassword where
  type Rs ResetPassword = ResetPasswordResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ResetPasswordResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResetPassword where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.ResetPassword" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResetPassword where
  toJSON ResetPassword' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("UserId" Lude..= userId),
            Lude.Just ("Password" Lude..= password)
          ]
      )

instance Lude.ToPath ResetPassword where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetPassword where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkResetPasswordResponse' smart constructor.
newtype ResetPasswordResponse = ResetPasswordResponse'
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

-- | Creates a value of 'ResetPasswordResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkResetPasswordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResetPasswordResponse
mkResetPasswordResponse pResponseStatus_ =
  ResetPasswordResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rprsResponseStatus :: Lens.Lens' ResetPasswordResponse Lude.Int
rprsResponseStatus = Lens.lens (responseStatus :: ResetPasswordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResetPasswordResponse)
{-# DEPRECATED rprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
