{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminSetUserMFAPreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the user's multi-factor authentication (MFA) preference, including which MFA options are enabled and if any are preferred. Only one factor can be set as preferred. The preferred MFA factor will be used to authenticate a user if multiple factors are enabled. If multiple options are enabled and no preference is set, a challenge to choose an MFA option will be returned during sign in.
module Network.AWS.CognitoIdentityProvider.AdminSetUserMFAPreference
  ( -- * Creating a request
    AdminSetUserMFAPreference (..),
    mkAdminSetUserMFAPreference,

    -- ** Request lenses
    asumpSMSMFASettings,
    asumpSoftwareTokenMFASettings,
    asumpUsername,
    asumpUserPoolId,

    -- * Destructuring the response
    AdminSetUserMFAPreferenceResponse (..),
    mkAdminSetUserMFAPreferenceResponse,

    -- ** Response lenses
    asumprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAdminSetUserMFAPreference' smart constructor.
data AdminSetUserMFAPreference = AdminSetUserMFAPreference'
  { sMSMFASettings ::
      Lude.Maybe SMSMFASettingsType,
    softwareTokenMFASettings ::
      Lude.Maybe SoftwareTokenMFASettingsType,
    username :: Lude.Sensitive Lude.Text,
    userPoolId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminSetUserMFAPreference' with the minimum fields required to make a request.
--
-- * 'sMSMFASettings' - The SMS text message MFA settings.
-- * 'softwareTokenMFASettings' - The time-based one-time password software token MFA settings.
-- * 'userPoolId' - The user pool ID.
-- * 'username' - The user pool username or alias.
mkAdminSetUserMFAPreference ::
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  -- | 'userPoolId'
  Lude.Text ->
  AdminSetUserMFAPreference
mkAdminSetUserMFAPreference pUsername_ pUserPoolId_ =
  AdminSetUserMFAPreference'
    { sMSMFASettings = Lude.Nothing,
      softwareTokenMFASettings = Lude.Nothing,
      username = pUsername_,
      userPoolId = pUserPoolId_
    }

-- | The SMS text message MFA settings.
--
-- /Note:/ Consider using 'sMSMFASettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asumpSMSMFASettings :: Lens.Lens' AdminSetUserMFAPreference (Lude.Maybe SMSMFASettingsType)
asumpSMSMFASettings = Lens.lens (sMSMFASettings :: AdminSetUserMFAPreference -> Lude.Maybe SMSMFASettingsType) (\s a -> s {sMSMFASettings = a} :: AdminSetUserMFAPreference)
{-# DEPRECATED asumpSMSMFASettings "Use generic-lens or generic-optics with 'sMSMFASettings' instead." #-}

-- | The time-based one-time password software token MFA settings.
--
-- /Note:/ Consider using 'softwareTokenMFASettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asumpSoftwareTokenMFASettings :: Lens.Lens' AdminSetUserMFAPreference (Lude.Maybe SoftwareTokenMFASettingsType)
asumpSoftwareTokenMFASettings = Lens.lens (softwareTokenMFASettings :: AdminSetUserMFAPreference -> Lude.Maybe SoftwareTokenMFASettingsType) (\s a -> s {softwareTokenMFASettings = a} :: AdminSetUserMFAPreference)
{-# DEPRECATED asumpSoftwareTokenMFASettings "Use generic-lens or generic-optics with 'softwareTokenMFASettings' instead." #-}

-- | The user pool username or alias.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asumpUsername :: Lens.Lens' AdminSetUserMFAPreference (Lude.Sensitive Lude.Text)
asumpUsername = Lens.lens (username :: AdminSetUserMFAPreference -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminSetUserMFAPreference)
{-# DEPRECATED asumpUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asumpUserPoolId :: Lens.Lens' AdminSetUserMFAPreference Lude.Text
asumpUserPoolId = Lens.lens (userPoolId :: AdminSetUserMFAPreference -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminSetUserMFAPreference)
{-# DEPRECATED asumpUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest AdminSetUserMFAPreference where
  type
    Rs AdminSetUserMFAPreference =
      AdminSetUserMFAPreferenceResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AdminSetUserMFAPreferenceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminSetUserMFAPreference where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminSetUserMFAPreference" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminSetUserMFAPreference where
  toJSON AdminSetUserMFAPreference' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SMSMfaSettings" Lude..=) Lude.<$> sMSMFASettings,
            ("SoftwareTokenMfaSettings" Lude..=)
              Lude.<$> softwareTokenMFASettings,
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath AdminSetUserMFAPreference where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminSetUserMFAPreference where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAdminSetUserMFAPreferenceResponse' smart constructor.
newtype AdminSetUserMFAPreferenceResponse = AdminSetUserMFAPreferenceResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminSetUserMFAPreferenceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAdminSetUserMFAPreferenceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminSetUserMFAPreferenceResponse
mkAdminSetUserMFAPreferenceResponse pResponseStatus_ =
  AdminSetUserMFAPreferenceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asumprsResponseStatus :: Lens.Lens' AdminSetUserMFAPreferenceResponse Lude.Int
asumprsResponseStatus = Lens.lens (responseStatus :: AdminSetUserMFAPreferenceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminSetUserMFAPreferenceResponse)
{-# DEPRECATED asumprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
