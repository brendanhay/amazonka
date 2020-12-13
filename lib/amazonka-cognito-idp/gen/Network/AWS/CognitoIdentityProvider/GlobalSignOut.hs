{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GlobalSignOut
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Signs out users from all devices. It also invalidates all refresh tokens issued to a user. The user's current access and Id tokens remain valid until their expiry. Access and Id tokens expire one hour after they are issued.
module Network.AWS.CognitoIdentityProvider.GlobalSignOut
  ( -- * Creating a request
    GlobalSignOut (..),
    mkGlobalSignOut,

    -- ** Request lenses
    gsoAccessToken,

    -- * Destructuring the response
    GlobalSignOutResponse (..),
    mkGlobalSignOutResponse,

    -- ** Response lenses
    gsorsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to sign out all devices.
--
-- /See:/ 'mkGlobalSignOut' smart constructor.
newtype GlobalSignOut = GlobalSignOut'
  { -- | The access token.
    accessToken :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlobalSignOut' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token.
mkGlobalSignOut ::
  -- | 'accessToken'
  Lude.Sensitive Lude.Text ->
  GlobalSignOut
mkGlobalSignOut pAccessToken_ =
  GlobalSignOut' {accessToken = pAccessToken_}

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsoAccessToken :: Lens.Lens' GlobalSignOut (Lude.Sensitive Lude.Text)
gsoAccessToken = Lens.lens (accessToken :: GlobalSignOut -> Lude.Sensitive Lude.Text) (\s a -> s {accessToken = a} :: GlobalSignOut)
{-# DEPRECATED gsoAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

instance Lude.AWSRequest GlobalSignOut where
  type Rs GlobalSignOut = GlobalSignOutResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          GlobalSignOutResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GlobalSignOut where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.GlobalSignOut" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GlobalSignOut where
  toJSON GlobalSignOut' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AccessToken" Lude..= accessToken)])

instance Lude.ToPath GlobalSignOut where
  toPath = Lude.const "/"

instance Lude.ToQuery GlobalSignOut where
  toQuery = Lude.const Lude.mempty

-- | The response to the request to sign out all devices.
--
-- /See:/ 'mkGlobalSignOutResponse' smart constructor.
newtype GlobalSignOutResponse = GlobalSignOutResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlobalSignOutResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkGlobalSignOutResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GlobalSignOutResponse
mkGlobalSignOutResponse pResponseStatus_ =
  GlobalSignOutResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsorsResponseStatus :: Lens.Lens' GlobalSignOutResponse Lude.Int
gsorsResponseStatus = Lens.lens (responseStatus :: GlobalSignOutResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GlobalSignOutResponse)
{-# DEPRECATED gsorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
