{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.SetUserMFAPreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the user's multi-factor authentication (MFA) method preference, including which MFA factors are enabled and if any are preferred. Only one factor can be set as preferred. The preferred MFA factor will be used to authenticate a user if multiple factors are enabled. If multiple options are enabled and no preference is set, a challenge to choose an MFA option will be returned during sign in. If an MFA type is enabled for a user, the user will be prompted for MFA during all sign in attempts, unless device tracking is turned on and the device has been trusted. If you would like MFA to be applied selectively based on the assessed risk level of sign in attempts, disable MFA for users and turn on Adaptive Authentication for the user pool.
module Network.AWS.CognitoIdentityProvider.SetUserMFAPreference
  ( -- * Creating a request
    SetUserMFAPreference (..),
    mkSetUserMFAPreference,

    -- ** Request lenses
    sumpAccessToken,
    sumpSMSMFASettings,
    sumpSoftwareTokenMFASettings,

    -- * Destructuring the response
    SetUserMFAPreferenceResponse (..),
    mkSetUserMFAPreferenceResponse,

    -- ** Response lenses
    sumprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetUserMFAPreference' smart constructor.
data SetUserMFAPreference = SetUserMFAPreference'
  { -- | The access token for the user.
    accessToken :: Lude.Sensitive Lude.Text,
    -- | The SMS text message multi-factor authentication (MFA) settings.
    sMSMFASettings :: Lude.Maybe SMSMFASettingsType,
    -- | The time-based one-time password software token MFA settings.
    softwareTokenMFASettings :: Lude.Maybe SoftwareTokenMFASettingsType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetUserMFAPreference' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token for the user.
-- * 'sMSMFASettings' - The SMS text message multi-factor authentication (MFA) settings.
-- * 'softwareTokenMFASettings' - The time-based one-time password software token MFA settings.
mkSetUserMFAPreference ::
  -- | 'accessToken'
  Lude.Sensitive Lude.Text ->
  SetUserMFAPreference
mkSetUserMFAPreference pAccessToken_ =
  SetUserMFAPreference'
    { accessToken = pAccessToken_,
      sMSMFASettings = Lude.Nothing,
      softwareTokenMFASettings = Lude.Nothing
    }

-- | The access token for the user.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumpAccessToken :: Lens.Lens' SetUserMFAPreference (Lude.Sensitive Lude.Text)
sumpAccessToken = Lens.lens (accessToken :: SetUserMFAPreference -> Lude.Sensitive Lude.Text) (\s a -> s {accessToken = a} :: SetUserMFAPreference)
{-# DEPRECATED sumpAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | The SMS text message multi-factor authentication (MFA) settings.
--
-- /Note:/ Consider using 'sMSMFASettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumpSMSMFASettings :: Lens.Lens' SetUserMFAPreference (Lude.Maybe SMSMFASettingsType)
sumpSMSMFASettings = Lens.lens (sMSMFASettings :: SetUserMFAPreference -> Lude.Maybe SMSMFASettingsType) (\s a -> s {sMSMFASettings = a} :: SetUserMFAPreference)
{-# DEPRECATED sumpSMSMFASettings "Use generic-lens or generic-optics with 'sMSMFASettings' instead." #-}

-- | The time-based one-time password software token MFA settings.
--
-- /Note:/ Consider using 'softwareTokenMFASettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumpSoftwareTokenMFASettings :: Lens.Lens' SetUserMFAPreference (Lude.Maybe SoftwareTokenMFASettingsType)
sumpSoftwareTokenMFASettings = Lens.lens (softwareTokenMFASettings :: SetUserMFAPreference -> Lude.Maybe SoftwareTokenMFASettingsType) (\s a -> s {softwareTokenMFASettings = a} :: SetUserMFAPreference)
{-# DEPRECATED sumpSoftwareTokenMFASettings "Use generic-lens or generic-optics with 'softwareTokenMFASettings' instead." #-}

instance Lude.AWSRequest SetUserMFAPreference where
  type Rs SetUserMFAPreference = SetUserMFAPreferenceResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          SetUserMFAPreferenceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetUserMFAPreference where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.SetUserMFAPreference" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetUserMFAPreference where
  toJSON SetUserMFAPreference' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccessToken" Lude..= accessToken),
            ("SMSMfaSettings" Lude..=) Lude.<$> sMSMFASettings,
            ("SoftwareTokenMfaSettings" Lude..=)
              Lude.<$> softwareTokenMFASettings
          ]
      )

instance Lude.ToPath SetUserMFAPreference where
  toPath = Lude.const "/"

instance Lude.ToQuery SetUserMFAPreference where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetUserMFAPreferenceResponse' smart constructor.
newtype SetUserMFAPreferenceResponse = SetUserMFAPreferenceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetUserMFAPreferenceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetUserMFAPreferenceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetUserMFAPreferenceResponse
mkSetUserMFAPreferenceResponse pResponseStatus_ =
  SetUserMFAPreferenceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumprsResponseStatus :: Lens.Lens' SetUserMFAPreferenceResponse Lude.Int
sumprsResponseStatus = Lens.lens (responseStatus :: SetUserMFAPreferenceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetUserMFAPreferenceResponse)
{-# DEPRECATED sumprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
