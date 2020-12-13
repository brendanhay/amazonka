{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.SetUserSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /This action is no longer supported./ You can use it to configure only SMS MFA. You can't use it to configure TOTP software token MFA. To configure either type of MFA, use <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_SetUserMFAPreference.html SetUserMFAPreference> instead.
module Network.AWS.CognitoIdentityProvider.SetUserSettings
  ( -- * Creating a request
    SetUserSettings (..),
    mkSetUserSettings,

    -- ** Request lenses
    susAccessToken,
    susMFAOptions,

    -- * Destructuring the response
    SetUserSettingsResponse (..),
    mkSetUserSettingsResponse,

    -- ** Response lenses
    susrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to set user settings.
--
-- /See:/ 'mkSetUserSettings' smart constructor.
data SetUserSettings = SetUserSettings'
  { -- | The access token for the set user settings request.
    accessToken :: Lude.Sensitive Lude.Text,
    -- | You can use this parameter only to set an SMS configuration that uses SMS for delivery.
    mfaOptions :: [MFAOptionType]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetUserSettings' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token for the set user settings request.
-- * 'mfaOptions' - You can use this parameter only to set an SMS configuration that uses SMS for delivery.
mkSetUserSettings ::
  -- | 'accessToken'
  Lude.Sensitive Lude.Text ->
  SetUserSettings
mkSetUserSettings pAccessToken_ =
  SetUserSettings'
    { accessToken = pAccessToken_,
      mfaOptions = Lude.mempty
    }

-- | The access token for the set user settings request.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
susAccessToken :: Lens.Lens' SetUserSettings (Lude.Sensitive Lude.Text)
susAccessToken = Lens.lens (accessToken :: SetUserSettings -> Lude.Sensitive Lude.Text) (\s a -> s {accessToken = a} :: SetUserSettings)
{-# DEPRECATED susAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | You can use this parameter only to set an SMS configuration that uses SMS for delivery.
--
-- /Note:/ Consider using 'mfaOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
susMFAOptions :: Lens.Lens' SetUserSettings [MFAOptionType]
susMFAOptions = Lens.lens (mfaOptions :: SetUserSettings -> [MFAOptionType]) (\s a -> s {mfaOptions = a} :: SetUserSettings)
{-# DEPRECATED susMFAOptions "Use generic-lens or generic-optics with 'mfaOptions' instead." #-}

instance Lude.AWSRequest SetUserSettings where
  type Rs SetUserSettings = SetUserSettingsResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          SetUserSettingsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetUserSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.SetUserSettings" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetUserSettings where
  toJSON SetUserSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccessToken" Lude..= accessToken),
            Lude.Just ("MFAOptions" Lude..= mfaOptions)
          ]
      )

instance Lude.ToPath SetUserSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery SetUserSettings where
  toQuery = Lude.const Lude.mempty

-- | The response from the server for a set user settings request.
--
-- /See:/ 'mkSetUserSettingsResponse' smart constructor.
newtype SetUserSettingsResponse = SetUserSettingsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetUserSettingsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetUserSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetUserSettingsResponse
mkSetUserSettingsResponse pResponseStatus_ =
  SetUserSettingsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
susrsResponseStatus :: Lens.Lens' SetUserSettingsResponse Lude.Int
susrsResponseStatus = Lens.lens (responseStatus :: SetUserSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetUserSettingsResponse)
{-# DEPRECATED susrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
