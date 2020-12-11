{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.SetUserPoolMFAConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the user pool multi-factor authentication (MFA) configuration.
module Network.AWS.CognitoIdentityProvider.SetUserPoolMFAConfig
  ( -- * Creating a request
    SetUserPoolMFAConfig (..),
    mkSetUserPoolMFAConfig,

    -- ** Request lenses
    supmcSmsMFAConfiguration,
    supmcSoftwareTokenMFAConfiguration,
    supmcMFAConfiguration,
    supmcUserPoolId,

    -- * Destructuring the response
    SetUserPoolMFAConfigResponse (..),
    mkSetUserPoolMFAConfigResponse,

    -- ** Response lenses
    supmcrsSmsMFAConfiguration,
    supmcrsSoftwareTokenMFAConfiguration,
    supmcrsMFAConfiguration,
    supmcrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetUserPoolMFAConfig' smart constructor.
data SetUserPoolMFAConfig = SetUserPoolMFAConfig'
  { smsMFAConfiguration ::
      Lude.Maybe SmsMFAConfigType,
    softwareTokenMFAConfiguration ::
      Lude.Maybe SoftwareTokenMFAConfigType,
    mfaConfiguration :: Lude.Maybe UserPoolMFAType,
    userPoolId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetUserPoolMFAConfig' with the minimum fields required to make a request.
--
-- * 'mfaConfiguration' - The MFA configuration. Valid values include:
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
-- * 'smsMFAConfiguration' - The SMS text message MFA configuration.
-- * 'softwareTokenMFAConfiguration' - The software token MFA configuration.
-- * 'userPoolId' - The user pool ID.
mkSetUserPoolMFAConfig ::
  -- | 'userPoolId'
  Lude.Text ->
  SetUserPoolMFAConfig
mkSetUserPoolMFAConfig pUserPoolId_ =
  SetUserPoolMFAConfig'
    { smsMFAConfiguration = Lude.Nothing,
      softwareTokenMFAConfiguration = Lude.Nothing,
      mfaConfiguration = Lude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | The SMS text message MFA configuration.
--
-- /Note:/ Consider using 'smsMFAConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supmcSmsMFAConfiguration :: Lens.Lens' SetUserPoolMFAConfig (Lude.Maybe SmsMFAConfigType)
supmcSmsMFAConfiguration = Lens.lens (smsMFAConfiguration :: SetUserPoolMFAConfig -> Lude.Maybe SmsMFAConfigType) (\s a -> s {smsMFAConfiguration = a} :: SetUserPoolMFAConfig)
{-# DEPRECATED supmcSmsMFAConfiguration "Use generic-lens or generic-optics with 'smsMFAConfiguration' instead." #-}

-- | The software token MFA configuration.
--
-- /Note:/ Consider using 'softwareTokenMFAConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supmcSoftwareTokenMFAConfiguration :: Lens.Lens' SetUserPoolMFAConfig (Lude.Maybe SoftwareTokenMFAConfigType)
supmcSoftwareTokenMFAConfiguration = Lens.lens (softwareTokenMFAConfiguration :: SetUserPoolMFAConfig -> Lude.Maybe SoftwareTokenMFAConfigType) (\s a -> s {softwareTokenMFAConfiguration = a} :: SetUserPoolMFAConfig)
{-# DEPRECATED supmcSoftwareTokenMFAConfiguration "Use generic-lens or generic-optics with 'softwareTokenMFAConfiguration' instead." #-}

-- | The MFA configuration. Valid values include:
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
supmcMFAConfiguration :: Lens.Lens' SetUserPoolMFAConfig (Lude.Maybe UserPoolMFAType)
supmcMFAConfiguration = Lens.lens (mfaConfiguration :: SetUserPoolMFAConfig -> Lude.Maybe UserPoolMFAType) (\s a -> s {mfaConfiguration = a} :: SetUserPoolMFAConfig)
{-# DEPRECATED supmcMFAConfiguration "Use generic-lens or generic-optics with 'mfaConfiguration' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supmcUserPoolId :: Lens.Lens' SetUserPoolMFAConfig Lude.Text
supmcUserPoolId = Lens.lens (userPoolId :: SetUserPoolMFAConfig -> Lude.Text) (\s a -> s {userPoolId = a} :: SetUserPoolMFAConfig)
{-# DEPRECATED supmcUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest SetUserPoolMFAConfig where
  type Rs SetUserPoolMFAConfig = SetUserPoolMFAConfigResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          SetUserPoolMFAConfigResponse'
            Lude.<$> (x Lude..?> "SmsMfaConfiguration")
            Lude.<*> (x Lude..?> "SoftwareTokenMfaConfiguration")
            Lude.<*> (x Lude..?> "MfaConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetUserPoolMFAConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.SetUserPoolMfaConfig" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetUserPoolMFAConfig where
  toJSON SetUserPoolMFAConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SmsMfaConfiguration" Lude..=) Lude.<$> smsMFAConfiguration,
            ("SoftwareTokenMfaConfiguration" Lude..=)
              Lude.<$> softwareTokenMFAConfiguration,
            ("MfaConfiguration" Lude..=) Lude.<$> mfaConfiguration,
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath SetUserPoolMFAConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery SetUserPoolMFAConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetUserPoolMFAConfigResponse' smart constructor.
data SetUserPoolMFAConfigResponse = SetUserPoolMFAConfigResponse'
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

-- | Creates a value of 'SetUserPoolMFAConfigResponse' with the minimum fields required to make a request.
--
-- * 'mfaConfiguration' - The MFA configuration. Valid values include:
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
-- * 'smsMFAConfiguration' - The SMS text message MFA configuration.
-- * 'softwareTokenMFAConfiguration' - The software token MFA configuration.
mkSetUserPoolMFAConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetUserPoolMFAConfigResponse
mkSetUserPoolMFAConfigResponse pResponseStatus_ =
  SetUserPoolMFAConfigResponse'
    { smsMFAConfiguration = Lude.Nothing,
      softwareTokenMFAConfiguration = Lude.Nothing,
      mfaConfiguration = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The SMS text message MFA configuration.
--
-- /Note:/ Consider using 'smsMFAConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supmcrsSmsMFAConfiguration :: Lens.Lens' SetUserPoolMFAConfigResponse (Lude.Maybe SmsMFAConfigType)
supmcrsSmsMFAConfiguration = Lens.lens (smsMFAConfiguration :: SetUserPoolMFAConfigResponse -> Lude.Maybe SmsMFAConfigType) (\s a -> s {smsMFAConfiguration = a} :: SetUserPoolMFAConfigResponse)
{-# DEPRECATED supmcrsSmsMFAConfiguration "Use generic-lens or generic-optics with 'smsMFAConfiguration' instead." #-}

-- | The software token MFA configuration.
--
-- /Note:/ Consider using 'softwareTokenMFAConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supmcrsSoftwareTokenMFAConfiguration :: Lens.Lens' SetUserPoolMFAConfigResponse (Lude.Maybe SoftwareTokenMFAConfigType)
supmcrsSoftwareTokenMFAConfiguration = Lens.lens (softwareTokenMFAConfiguration :: SetUserPoolMFAConfigResponse -> Lude.Maybe SoftwareTokenMFAConfigType) (\s a -> s {softwareTokenMFAConfiguration = a} :: SetUserPoolMFAConfigResponse)
{-# DEPRECATED supmcrsSoftwareTokenMFAConfiguration "Use generic-lens or generic-optics with 'softwareTokenMFAConfiguration' instead." #-}

-- | The MFA configuration. Valid values include:
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
supmcrsMFAConfiguration :: Lens.Lens' SetUserPoolMFAConfigResponse (Lude.Maybe UserPoolMFAType)
supmcrsMFAConfiguration = Lens.lens (mfaConfiguration :: SetUserPoolMFAConfigResponse -> Lude.Maybe UserPoolMFAType) (\s a -> s {mfaConfiguration = a} :: SetUserPoolMFAConfigResponse)
{-# DEPRECATED supmcrsMFAConfiguration "Use generic-lens or generic-optics with 'mfaConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supmcrsResponseStatus :: Lens.Lens' SetUserPoolMFAConfigResponse Lude.Int
supmcrsResponseStatus = Lens.lens (responseStatus :: SetUserPoolMFAConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetUserPoolMFAConfigResponse)
{-# DEPRECATED supmcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
