{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetCurrentUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details of the current user for whom the authentication token was generated. This is not a valid action for SigV4 (administrative API) clients.
--
-- This action requires an authentication token. To get an authentication token, register an application with Amazon WorkDocs. For more information, see <https://docs.aws.amazon.com/workdocs/latest/developerguide/wd-auth-user.html Authentication and Access Control for User Applications> in the /Amazon WorkDocs Developer Guide/ .
module Network.AWS.WorkDocs.GetCurrentUser
  ( -- * Creating a request
    GetCurrentUser (..),
    mkGetCurrentUser,

    -- ** Request lenses
    gcuAuthenticationToken,

    -- * Destructuring the response
    GetCurrentUserResponse (..),
    mkGetCurrentUserResponse,

    -- ** Response lenses
    gcursUser,
    gcursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkGetCurrentUser' smart constructor.
newtype GetCurrentUser = GetCurrentUser'
  { -- | Amazon WorkDocs authentication token.
    authenticationToken :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCurrentUser' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token.
mkGetCurrentUser ::
  -- | 'authenticationToken'
  Lude.Sensitive Lude.Text ->
  GetCurrentUser
mkGetCurrentUser pAuthenticationToken_ =
  GetCurrentUser' {authenticationToken = pAuthenticationToken_}

-- | Amazon WorkDocs authentication token.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcuAuthenticationToken :: Lens.Lens' GetCurrentUser (Lude.Sensitive Lude.Text)
gcuAuthenticationToken = Lens.lens (authenticationToken :: GetCurrentUser -> Lude.Sensitive Lude.Text) (\s a -> s {authenticationToken = a} :: GetCurrentUser)
{-# DEPRECATED gcuAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

instance Lude.AWSRequest GetCurrentUser where
  type Rs GetCurrentUser = GetCurrentUserResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCurrentUserResponse'
            Lude.<$> (x Lude..?> "User") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCurrentUser where
  toHeaders GetCurrentUser' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath GetCurrentUser where
  toPath = Lude.const "/api/v1/me"

instance Lude.ToQuery GetCurrentUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCurrentUserResponse' smart constructor.
data GetCurrentUserResponse = GetCurrentUserResponse'
  { -- | Metadata of the user.
    user :: Lude.Maybe User,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCurrentUserResponse' with the minimum fields required to make a request.
--
-- * 'user' - Metadata of the user.
-- * 'responseStatus' - The response status code.
mkGetCurrentUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCurrentUserResponse
mkGetCurrentUserResponse pResponseStatus_ =
  GetCurrentUserResponse'
    { user = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Metadata of the user.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcursUser :: Lens.Lens' GetCurrentUserResponse (Lude.Maybe User)
gcursUser = Lens.lens (user :: GetCurrentUserResponse -> Lude.Maybe User) (\s a -> s {user = a} :: GetCurrentUserResponse)
{-# DEPRECATED gcursUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcursResponseStatus :: Lens.Lens' GetCurrentUserResponse Lude.Int
gcursResponseStatus = Lens.lens (responseStatus :: GetCurrentUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCurrentUserResponse)
{-# DEPRECATED gcursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
