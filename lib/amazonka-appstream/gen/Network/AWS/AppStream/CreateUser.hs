{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user in the user pool.
module Network.AWS.AppStream.CreateUser
  ( -- * Creating a request
    CreateUser (..),
    mkCreateUser,

    -- ** Request lenses
    cuLastName,
    cuMessageAction,
    cuFirstName,
    cuUserName,
    cuAuthenticationType,

    -- * Destructuring the response
    CreateUserResponse (..),
    mkCreateUserResponse,

    -- ** Response lenses
    cursResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { lastName ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    messageAction :: Lude.Maybe MessageAction,
    firstName :: Lude.Maybe (Lude.Sensitive Lude.Text),
    userName :: Lude.Sensitive Lude.Text,
    authenticationType :: AuthenticationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUser' with the minimum fields required to make a request.
--
-- * 'authenticationType' - The authentication type for the user. You must specify USERPOOL.
-- * 'firstName' - The first name, or given name, of the user.
-- * 'lastName' - The last name, or surname, of the user.
-- * 'messageAction' - The action to take for the welcome email that is sent to a user after the user is created in the user pool. If you specify SUPPRESS, no email is sent. If you specify RESEND, do not specify the first name or last name of the user. If the value is null, the email is sent.
-- * 'userName' - The email address of the user.
mkCreateUser ::
  -- | 'userName'
  Lude.Sensitive Lude.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  CreateUser
mkCreateUser pUserName_ pAuthenticationType_ =
  CreateUser'
    { lastName = Lude.Nothing,
      messageAction = Lude.Nothing,
      firstName = Lude.Nothing,
      userName = pUserName_,
      authenticationType = pAuthenticationType_
    }

-- | The last name, or surname, of the user.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuLastName :: Lens.Lens' CreateUser (Lude.Maybe (Lude.Sensitive Lude.Text))
cuLastName = Lens.lens (lastName :: CreateUser -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {lastName = a} :: CreateUser)
{-# DEPRECATED cuLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | The action to take for the welcome email that is sent to a user after the user is created in the user pool. If you specify SUPPRESS, no email is sent. If you specify RESEND, do not specify the first name or last name of the user. If the value is null, the email is sent.
--
-- /Note:/ Consider using 'messageAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuMessageAction :: Lens.Lens' CreateUser (Lude.Maybe MessageAction)
cuMessageAction = Lens.lens (messageAction :: CreateUser -> Lude.Maybe MessageAction) (\s a -> s {messageAction = a} :: CreateUser)
{-# DEPRECATED cuMessageAction "Use generic-lens or generic-optics with 'messageAction' instead." #-}

-- | The first name, or given name, of the user.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuFirstName :: Lens.Lens' CreateUser (Lude.Maybe (Lude.Sensitive Lude.Text))
cuFirstName = Lens.lens (firstName :: CreateUser -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {firstName = a} :: CreateUser)
{-# DEPRECATED cuFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

-- | The email address of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUserName :: Lens.Lens' CreateUser (Lude.Sensitive Lude.Text)
cuUserName = Lens.lens (userName :: CreateUser -> Lude.Sensitive Lude.Text) (\s a -> s {userName = a} :: CreateUser)
{-# DEPRECATED cuUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The authentication type for the user. You must specify USERPOOL.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuAuthenticationType :: Lens.Lens' CreateUser AuthenticationType
cuAuthenticationType = Lens.lens (authenticationType :: CreateUser -> AuthenticationType) (\s a -> s {authenticationType = a} :: CreateUser)
{-# DEPRECATED cuAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

instance Lude.AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateUserResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.CreateUser" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LastName" Lude..=) Lude.<$> lastName,
            ("MessageAction" Lude..=) Lude.<$> messageAction,
            ("FirstName" Lude..=) Lude.<$> firstName,
            Lude.Just ("UserName" Lude..= userName),
            Lude.Just ("AuthenticationType" Lude..= authenticationType)
          ]
      )

instance Lude.ToPath CreateUser where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateUserResponse' smart constructor.
newtype CreateUserResponse = CreateUserResponse'
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

-- | Creates a value of 'CreateUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserResponse
mkCreateUserResponse pResponseStatus_ =
  CreateUserResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursResponseStatus :: Lens.Lens' CreateUserResponse Lude.Int
cursResponseStatus = Lens.lens (responseStatus :: CreateUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserResponse)
{-# DEPRECATED cursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
