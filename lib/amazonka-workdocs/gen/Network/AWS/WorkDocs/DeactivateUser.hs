{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeactivateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified user, which revokes the user's access to Amazon WorkDocs.
module Network.AWS.WorkDocs.DeactivateUser
  ( -- * Creating a request
    DeactivateUser (..),
    mkDeactivateUser,

    -- ** Request lenses
    dAuthenticationToken,
    dUserId,

    -- * Destructuring the response
    DeactivateUserResponse (..),
    mkDeactivateUserResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDeactivateUser' smart constructor.
data DeactivateUser = DeactivateUser'
  { authenticationToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    userId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeactivateUser' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'userId' - The ID of the user.
mkDeactivateUser ::
  -- | 'userId'
  Lude.Text ->
  DeactivateUser
mkDeactivateUser pUserId_ =
  DeactivateUser'
    { authenticationToken = Lude.Nothing,
      userId = pUserId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAuthenticationToken :: Lens.Lens' DeactivateUser (Lude.Maybe (Lude.Sensitive Lude.Text))
dAuthenticationToken = Lens.lens (authenticationToken :: DeactivateUser -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: DeactivateUser)
{-# DEPRECATED dAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserId :: Lens.Lens' DeactivateUser Lude.Text
dUserId = Lens.lens (userId :: DeactivateUser -> Lude.Text) (\s a -> s {userId = a} :: DeactivateUser)
{-# DEPRECATED dUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.AWSRequest DeactivateUser where
  type Rs DeactivateUser = DeactivateUserResponse
  request = Req.delete workDocsService
  response = Res.receiveNull DeactivateUserResponse'

instance Lude.ToHeaders DeactivateUser where
  toHeaders DeactivateUser' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DeactivateUser where
  toPath DeactivateUser' {..} =
    Lude.mconcat ["/api/v1/users/", Lude.toBS userId, "/activation"]

instance Lude.ToQuery DeactivateUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeactivateUserResponse' smart constructor.
data DeactivateUserResponse = DeactivateUserResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeactivateUserResponse' with the minimum fields required to make a request.
mkDeactivateUserResponse ::
  DeactivateUserResponse
mkDeactivateUserResponse = DeactivateUserResponse'
