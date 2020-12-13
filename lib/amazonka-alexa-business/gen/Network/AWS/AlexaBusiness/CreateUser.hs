{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user.
module Network.AWS.AlexaBusiness.CreateUser
  ( -- * Creating a request
    CreateUser (..),
    mkCreateUser,

    -- ** Request lenses
    cuEmail,
    cuLastName,
    cuUserId,
    cuFirstName,
    cuClientRequestToken,
    cuTags,

    -- * Destructuring the response
    CreateUserResponse (..),
    mkCreateUserResponse,

    -- ** Response lenses
    cursUserARN,
    cursResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The email address for the user.
    email :: Lude.Maybe Lude.Text,
    -- | The last name for the user.
    lastName :: Lude.Maybe Lude.Text,
    -- | The ARN for the user.
    userId :: Lude.Text,
    -- | The first name for the user.
    firstName :: Lude.Maybe Lude.Text,
    -- | A unique, user-specified identifier for this request that ensures idempotency.
    clientRequestToken :: Lude.Maybe Lude.Text,
    -- | The tags for the user.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUser' with the minimum fields required to make a request.
--
-- * 'email' - The email address for the user.
-- * 'lastName' - The last name for the user.
-- * 'userId' - The ARN for the user.
-- * 'firstName' - The first name for the user.
-- * 'clientRequestToken' - A unique, user-specified identifier for this request that ensures idempotency.
-- * 'tags' - The tags for the user.
mkCreateUser ::
  -- | 'userId'
  Lude.Text ->
  CreateUser
mkCreateUser pUserId_ =
  CreateUser'
    { email = Lude.Nothing,
      lastName = Lude.Nothing,
      userId = pUserId_,
      firstName = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The email address for the user.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuEmail :: Lens.Lens' CreateUser (Lude.Maybe Lude.Text)
cuEmail = Lens.lens (email :: CreateUser -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: CreateUser)
{-# DEPRECATED cuEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The last name for the user.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuLastName :: Lens.Lens' CreateUser (Lude.Maybe Lude.Text)
cuLastName = Lens.lens (lastName :: CreateUser -> Lude.Maybe Lude.Text) (\s a -> s {lastName = a} :: CreateUser)
{-# DEPRECATED cuLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | The ARN for the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUserId :: Lens.Lens' CreateUser Lude.Text
cuUserId = Lens.lens (userId :: CreateUser -> Lude.Text) (\s a -> s {userId = a} :: CreateUser)
{-# DEPRECATED cuUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The first name for the user.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuFirstName :: Lens.Lens' CreateUser (Lude.Maybe Lude.Text)
cuFirstName = Lens.lens (firstName :: CreateUser -> Lude.Maybe Lude.Text) (\s a -> s {firstName = a} :: CreateUser)
{-# DEPRECATED cuFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

-- | A unique, user-specified identifier for this request that ensures idempotency.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuClientRequestToken :: Lens.Lens' CreateUser (Lude.Maybe Lude.Text)
cuClientRequestToken = Lens.lens (clientRequestToken :: CreateUser -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateUser)
{-# DEPRECATED cuClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The tags for the user.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuTags :: Lens.Lens' CreateUser (Lude.Maybe [Tag])
cuTags = Lens.lens (tags :: CreateUser -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateUser)
{-# DEPRECATED cuTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Lude.<$> (x Lude..?> "UserArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.CreateUser" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Email" Lude..=) Lude.<$> email,
            ("LastName" Lude..=) Lude.<$> lastName,
            Lude.Just ("UserId" Lude..= userId),
            ("FirstName" Lude..=) Lude.<$> firstName,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateUser where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The ARN of the newly created user in the response.
    userARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserResponse' with the minimum fields required to make a request.
--
-- * 'userARN' - The ARN of the newly created user in the response.
-- * 'responseStatus' - The response status code.
mkCreateUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserResponse
mkCreateUserResponse pResponseStatus_ =
  CreateUserResponse'
    { userARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the newly created user in the response.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursUserARN :: Lens.Lens' CreateUserResponse (Lude.Maybe Lude.Text)
cursUserARN = Lens.lens (userARN :: CreateUserResponse -> Lude.Maybe Lude.Text) (\s a -> s {userARN = a} :: CreateUserResponse)
{-# DEPRECATED cursUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursResponseStatus :: Lens.Lens' CreateUserResponse Lude.Int
cursResponseStatus = Lens.lens (responseStatus :: CreateUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserResponse)
{-# DEPRECATED cursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
