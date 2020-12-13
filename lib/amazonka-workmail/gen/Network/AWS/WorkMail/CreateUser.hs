{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.CreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user who can be used in Amazon WorkMail by calling the 'RegisterToWorkMail' operation.
module Network.AWS.WorkMail.CreateUser
  ( -- * Creating a request
    CreateUser (..),
    mkCreateUser,

    -- ** Request lenses
    cuName,
    cuPassword,
    cuDisplayName,
    cuOrganizationId,

    -- * Destructuring the response
    CreateUserResponse (..),
    mkCreateUserResponse,

    -- ** Response lenses
    cursUserId,
    cursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The name for the new user. WorkMail directory user names have a maximum length of 64. All others have a maximum length of 20.
    name :: Lude.Text,
    -- | The password for the new user.
    password :: Lude.Sensitive Lude.Text,
    -- | The display name for the new user.
    displayName :: Lude.Text,
    -- | The identifier of the organization for which the user is created.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUser' with the minimum fields required to make a request.
--
-- * 'name' - The name for the new user. WorkMail directory user names have a maximum length of 64. All others have a maximum length of 20.
-- * 'password' - The password for the new user.
-- * 'displayName' - The display name for the new user.
-- * 'organizationId' - The identifier of the organization for which the user is created.
mkCreateUser ::
  -- | 'name'
  Lude.Text ->
  -- | 'password'
  Lude.Sensitive Lude.Text ->
  -- | 'displayName'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  CreateUser
mkCreateUser pName_ pPassword_ pDisplayName_ pOrganizationId_ =
  CreateUser'
    { name = pName_,
      password = pPassword_,
      displayName = pDisplayName_,
      organizationId = pOrganizationId_
    }

-- | The name for the new user. WorkMail directory user names have a maximum length of 64. All others have a maximum length of 20.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuName :: Lens.Lens' CreateUser Lude.Text
cuName = Lens.lens (name :: CreateUser -> Lude.Text) (\s a -> s {name = a} :: CreateUser)
{-# DEPRECATED cuName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The password for the new user.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPassword :: Lens.Lens' CreateUser (Lude.Sensitive Lude.Text)
cuPassword = Lens.lens (password :: CreateUser -> Lude.Sensitive Lude.Text) (\s a -> s {password = a} :: CreateUser)
{-# DEPRECATED cuPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The display name for the new user.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuDisplayName :: Lens.Lens' CreateUser Lude.Text
cuDisplayName = Lens.lens (displayName :: CreateUser -> Lude.Text) (\s a -> s {displayName = a} :: CreateUser)
{-# DEPRECATED cuDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The identifier of the organization for which the user is created.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuOrganizationId :: Lens.Lens' CreateUser Lude.Text
cuOrganizationId = Lens.lens (organizationId :: CreateUser -> Lude.Text) (\s a -> s {organizationId = a} :: CreateUser)
{-# DEPRECATED cuOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Lude.<$> (x Lude..?> "UserId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.CreateUser" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("Password" Lude..= password),
            Lude.Just ("DisplayName" Lude..= displayName),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath CreateUser where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The identifier for the new user.
    userId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserResponse' with the minimum fields required to make a request.
--
-- * 'userId' - The identifier for the new user.
-- * 'responseStatus' - The response status code.
mkCreateUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserResponse
mkCreateUserResponse pResponseStatus_ =
  CreateUserResponse'
    { userId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for the new user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursUserId :: Lens.Lens' CreateUserResponse (Lude.Maybe Lude.Text)
cursUserId = Lens.lens (userId :: CreateUserResponse -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: CreateUserResponse)
{-# DEPRECATED cursUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursResponseStatus :: Lens.Lens' CreateUserResponse Lude.Int
cursResponseStatus = Lens.lens (responseStatus :: CreateUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserResponse)
{-# DEPRECATED cursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
