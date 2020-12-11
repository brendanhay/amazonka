{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.ActivateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the specified user. Only active users can access Amazon WorkDocs.
module Network.AWS.WorkDocs.ActivateUser
  ( -- * Creating a request
    ActivateUser (..),
    mkActivateUser,

    -- ** Request lenses
    auAuthenticationToken,
    auUserId,

    -- * Destructuring the response
    ActivateUserResponse (..),
    mkActivateUserResponse,

    -- ** Response lenses
    aursUser,
    aursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkActivateUser' smart constructor.
data ActivateUser = ActivateUser'
  { authenticationToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    userId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivateUser' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'userId' - The ID of the user.
mkActivateUser ::
  -- | 'userId'
  Lude.Text ->
  ActivateUser
mkActivateUser pUserId_ =
  ActivateUser'
    { authenticationToken = Lude.Nothing,
      userId = pUserId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auAuthenticationToken :: Lens.Lens' ActivateUser (Lude.Maybe (Lude.Sensitive Lude.Text))
auAuthenticationToken = Lens.lens (authenticationToken :: ActivateUser -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: ActivateUser)
{-# DEPRECATED auAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auUserId :: Lens.Lens' ActivateUser Lude.Text
auUserId = Lens.lens (userId :: ActivateUser -> Lude.Text) (\s a -> s {userId = a} :: ActivateUser)
{-# DEPRECATED auUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.AWSRequest ActivateUser where
  type Rs ActivateUser = ActivateUserResponse
  request = Req.postJSON workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ActivateUserResponse'
            Lude.<$> (x Lude..?> "User") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ActivateUser where
  toHeaders ActivateUser' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON ActivateUser where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath ActivateUser where
  toPath ActivateUser' {..} =
    Lude.mconcat ["/api/v1/users/", Lude.toBS userId, "/activation"]

instance Lude.ToQuery ActivateUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkActivateUserResponse' smart constructor.
data ActivateUserResponse = ActivateUserResponse'
  { user ::
      Lude.Maybe User,
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

-- | Creates a value of 'ActivateUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'user' - The user information.
mkActivateUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ActivateUserResponse
mkActivateUserResponse pResponseStatus_ =
  ActivateUserResponse'
    { user = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The user information.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aursUser :: Lens.Lens' ActivateUserResponse (Lude.Maybe User)
aursUser = Lens.lens (user :: ActivateUserResponse -> Lude.Maybe User) (\s a -> s {user = a} :: ActivateUserResponse)
{-# DEPRECATED aursUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aursResponseStatus :: Lens.Lens' ActivateUserResponse Lude.Int
aursResponseStatus = Lens.lens (responseStatus :: ActivateUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ActivateUserResponse)
{-# DEPRECATED aursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
