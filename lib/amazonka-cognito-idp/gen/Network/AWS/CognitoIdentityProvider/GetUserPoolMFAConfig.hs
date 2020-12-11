{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetUserPoolMFAConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user pool multi-factor authentication (MFA) configuration.
module Network.AWS.CognitoIdentityProvider.GetUserPoolMFAConfig
  ( -- * Creating a request
    GetUserPoolMFAConfig (..),
    mkGetUserPoolMFAConfig,

    -- ** Request lenses
    gupmcUserPoolId,

    -- * Destructuring the response
    GetUserPoolMFAConfigResponse (..),
    mkGetUserPoolMFAConfigResponse,

    -- ** Response lenses
    gupmcrsSmsMFAConfiguration,
    gupmcrsSoftwareTokenMFAConfiguration,
    gupmcrsMFAConfiguration,
    gupmcrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetUserPoolMFAConfig' smart constructor.
newtype GetUserPoolMFAConfig = GetUserPoolMFAConfig'
  { userPoolId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUserPoolMFAConfig' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID.
mkGetUserPoolMFAConfig ::
  -- | 'userPoolId'
  Lude.Text ->
  GetUserPoolMFAConfig
mkGetUserPoolMFAConfig pUserPoolId_ =
  GetUserPoolMFAConfig' {userPoolId = pUserPoolId_}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupmcUserPoolId :: Lens.Lens' GetUserPoolMFAConfig Lude.Text
gupmcUserPoolId = Lens.lens (userPoolId :: GetUserPoolMFAConfig -> Lude.Text) (\s a -> s {userPoolId = a} :: GetUserPoolMFAConfig)
{-# DEPRECATED gupmcUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest GetUserPoolMFAConfig where
  type Rs GetUserPoolMFAConfig = GetUserPoolMFAConfigResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUserPoolMFAConfigResponse'
            Lude.<$> (x Lude..?> "SmsMfaConfiguration")
            Lude.<*> (x Lude..?> "SoftwareTokenMfaConfiguration")
            Lude.<*> (x Lude..?> "MfaConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUserPoolMFAConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.GetUserPoolMfaConfig" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetUserPoolMFAConfig where
  toJSON GetUserPoolMFAConfig' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("UserPoolId" Lude..= userPoolId)])

instance Lude.ToPath GetUserPoolMFAConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery GetUserPoolMFAConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetUserPoolMFAConfigResponse' smart constructor.
data GetUserPoolMFAConfigResponse = GetUserPoolMFAConfigResponse'
  { smsMFAConfiguration ::
      Lude.Maybe SmsMFAConfigType,
    softwareTokenMFAConfiguration ::
      Lude.Maybe
        SoftwareTokenMFAConfigType,
    mfaConfiguration ::
      Lude.Maybe UserPoolMFAType,
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

-- | Creates a value of 'GetUserPoolMFAConfigResponse' with the minimum fields required to make a request.
--
-- * 'mfaConfiguration' - The multi-factor (MFA) configuration. Valid values include:
--
--
--     * @OFF@ MFA will not be used for any users.
--
--
--     * @ON@ MFA is required for all users to sign in.
--
--
--     * @OPTIONAL@ MFA will be required only for individual users who have an MFA factor enabled.
--
--
-- * 'responseStatus' - The response status code.
-- * 'smsMFAConfiguration' - The SMS text message multi-factor (MFA) configuration.
-- * 'softwareTokenMFAConfiguration' - The software token multi-factor (MFA) configuration.
mkGetUserPoolMFAConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetUserPoolMFAConfigResponse
mkGetUserPoolMFAConfigResponse pResponseStatus_ =
  GetUserPoolMFAConfigResponse'
    { smsMFAConfiguration = Lude.Nothing,
      softwareTokenMFAConfiguration = Lude.Nothing,
      mfaConfiguration = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The SMS text message multi-factor (MFA) configuration.
--
-- /Note:/ Consider using 'smsMFAConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupmcrsSmsMFAConfiguration :: Lens.Lens' GetUserPoolMFAConfigResponse (Lude.Maybe SmsMFAConfigType)
gupmcrsSmsMFAConfiguration = Lens.lens (smsMFAConfiguration :: GetUserPoolMFAConfigResponse -> Lude.Maybe SmsMFAConfigType) (\s a -> s {smsMFAConfiguration = a} :: GetUserPoolMFAConfigResponse)
{-# DEPRECATED gupmcrsSmsMFAConfiguration "Use generic-lens or generic-optics with 'smsMFAConfiguration' instead." #-}

-- | The software token multi-factor (MFA) configuration.
--
-- /Note:/ Consider using 'softwareTokenMFAConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupmcrsSoftwareTokenMFAConfiguration :: Lens.Lens' GetUserPoolMFAConfigResponse (Lude.Maybe SoftwareTokenMFAConfigType)
gupmcrsSoftwareTokenMFAConfiguration = Lens.lens (softwareTokenMFAConfiguration :: GetUserPoolMFAConfigResponse -> Lude.Maybe SoftwareTokenMFAConfigType) (\s a -> s {softwareTokenMFAConfiguration = a} :: GetUserPoolMFAConfigResponse)
{-# DEPRECATED gupmcrsSoftwareTokenMFAConfiguration "Use generic-lens or generic-optics with 'softwareTokenMFAConfiguration' instead." #-}

-- | The multi-factor (MFA) configuration. Valid values include:
--
--
--     * @OFF@ MFA will not be used for any users.
--
--
--     * @ON@ MFA is required for all users to sign in.
--
--
--     * @OPTIONAL@ MFA will be required only for individual users who have an MFA factor enabled.
--
--
--
-- /Note:/ Consider using 'mfaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupmcrsMFAConfiguration :: Lens.Lens' GetUserPoolMFAConfigResponse (Lude.Maybe UserPoolMFAType)
gupmcrsMFAConfiguration = Lens.lens (mfaConfiguration :: GetUserPoolMFAConfigResponse -> Lude.Maybe UserPoolMFAType) (\s a -> s {mfaConfiguration = a} :: GetUserPoolMFAConfigResponse)
{-# DEPRECATED gupmcrsMFAConfiguration "Use generic-lens or generic-optics with 'mfaConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupmcrsResponseStatus :: Lens.Lens' GetUserPoolMFAConfigResponse Lude.Int
gupmcrsResponseStatus = Lens.lens (responseStatus :: GetUserPoolMFAConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUserPoolMFAConfigResponse)
{-# DEPRECATED gupmcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
