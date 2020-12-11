{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetUICustomization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the UI Customization information for a particular app client's app UI, if there is something set. If nothing is set for the particular client, but there is an existing pool level customization (app @clientId@ will be @ALL@ ), then that is returned. If nothing is present, then an empty shape is returned.
module Network.AWS.CognitoIdentityProvider.GetUICustomization
  ( -- * Creating a request
    GetUICustomization (..),
    mkGetUICustomization,

    -- ** Request lenses
    guicClientId,
    guicUserPoolId,

    -- * Destructuring the response
    GetUICustomizationResponse (..),
    mkGetUICustomizationResponse,

    -- ** Response lenses
    guicrsResponseStatus,
    guicrsUICustomization,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetUICustomization' smart constructor.
data GetUICustomization = GetUICustomization'
  { clientId ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    userPoolId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUICustomization' with the minimum fields required to make a request.
--
-- * 'clientId' - The client ID for the client app.
-- * 'userPoolId' - The user pool ID for the user pool.
mkGetUICustomization ::
  -- | 'userPoolId'
  Lude.Text ->
  GetUICustomization
mkGetUICustomization pUserPoolId_ =
  GetUICustomization'
    { clientId = Lude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | The client ID for the client app.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guicClientId :: Lens.Lens' GetUICustomization (Lude.Maybe (Lude.Sensitive Lude.Text))
guicClientId = Lens.lens (clientId :: GetUICustomization -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {clientId = a} :: GetUICustomization)
{-# DEPRECATED guicClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guicUserPoolId :: Lens.Lens' GetUICustomization Lude.Text
guicUserPoolId = Lens.lens (userPoolId :: GetUICustomization -> Lude.Text) (\s a -> s {userPoolId = a} :: GetUICustomization)
{-# DEPRECATED guicUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest GetUICustomization where
  type Rs GetUICustomization = GetUICustomizationResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUICustomizationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "UICustomization")
      )

instance Lude.ToHeaders GetUICustomization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.GetUICustomization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetUICustomization where
  toJSON GetUICustomization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientId" Lude..=) Lude.<$> clientId,
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath GetUICustomization where
  toPath = Lude.const "/"

instance Lude.ToQuery GetUICustomization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetUICustomizationResponse' smart constructor.
data GetUICustomizationResponse = GetUICustomizationResponse'
  { responseStatus ::
      Lude.Int,
    uICustomization ::
      UICustomizationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUICustomizationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'uICustomization' - The UI customization information.
mkGetUICustomizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'uICustomization'
  UICustomizationType ->
  GetUICustomizationResponse
mkGetUICustomizationResponse pResponseStatus_ pUICustomization_ =
  GetUICustomizationResponse'
    { responseStatus = pResponseStatus_,
      uICustomization = pUICustomization_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guicrsResponseStatus :: Lens.Lens' GetUICustomizationResponse Lude.Int
guicrsResponseStatus = Lens.lens (responseStatus :: GetUICustomizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUICustomizationResponse)
{-# DEPRECATED guicrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The UI customization information.
--
-- /Note:/ Consider using 'uICustomization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guicrsUICustomization :: Lens.Lens' GetUICustomizationResponse UICustomizationType
guicrsUICustomization = Lens.lens (uICustomization :: GetUICustomizationResponse -> UICustomizationType) (\s a -> s {uICustomization = a} :: GetUICustomizationResponse)
{-# DEPRECATED guicrsUICustomization "Use generic-lens or generic-optics with 'uICustomization' instead." #-}
