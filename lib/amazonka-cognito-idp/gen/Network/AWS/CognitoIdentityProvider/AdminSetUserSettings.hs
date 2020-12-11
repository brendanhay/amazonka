{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminSetUserSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /This action is no longer supported./ You can use it to configure only SMS MFA. You can't use it to configure TOTP software token MFA. To configure either type of MFA, use <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminSetUserMFAPreference.html AdminSetUserMFAPreference> instead.
module Network.AWS.CognitoIdentityProvider.AdminSetUserSettings
  ( -- * Creating a request
    AdminSetUserSettings (..),
    mkAdminSetUserSettings,

    -- ** Request lenses
    asusUserPoolId,
    asusUsername,
    asusMFAOptions,

    -- * Destructuring the response
    AdminSetUserSettingsResponse (..),
    mkAdminSetUserSettingsResponse,

    -- ** Response lenses
    asusrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | You can use this parameter to set an MFA configuration that uses the SMS delivery medium.
--
-- /See:/ 'mkAdminSetUserSettings' smart constructor.
data AdminSetUserSettings = AdminSetUserSettings'
  { userPoolId ::
      Lude.Text,
    username :: Lude.Sensitive Lude.Text,
    mfaOptions :: [MFAOptionType]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminSetUserSettings' with the minimum fields required to make a request.
--
-- * 'mfaOptions' - You can use this parameter only to set an SMS configuration that uses SMS for delivery.
-- * 'userPoolId' - The ID of the user pool that contains the user that you are setting options for.
-- * 'username' - The user name of the user that you are setting options for.
mkAdminSetUserSettings ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminSetUserSettings
mkAdminSetUserSettings pUserPoolId_ pUsername_ =
  AdminSetUserSettings'
    { userPoolId = pUserPoolId_,
      username = pUsername_,
      mfaOptions = Lude.mempty
    }

-- | The ID of the user pool that contains the user that you are setting options for.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asusUserPoolId :: Lens.Lens' AdminSetUserSettings Lude.Text
asusUserPoolId = Lens.lens (userPoolId :: AdminSetUserSettings -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminSetUserSettings)
{-# DEPRECATED asusUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name of the user that you are setting options for.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asusUsername :: Lens.Lens' AdminSetUserSettings (Lude.Sensitive Lude.Text)
asusUsername = Lens.lens (username :: AdminSetUserSettings -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminSetUserSettings)
{-# DEPRECATED asusUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | You can use this parameter only to set an SMS configuration that uses SMS for delivery.
--
-- /Note:/ Consider using 'mfaOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asusMFAOptions :: Lens.Lens' AdminSetUserSettings [MFAOptionType]
asusMFAOptions = Lens.lens (mfaOptions :: AdminSetUserSettings -> [MFAOptionType]) (\s a -> s {mfaOptions = a} :: AdminSetUserSettings)
{-# DEPRECATED asusMFAOptions "Use generic-lens or generic-optics with 'mfaOptions' instead." #-}

instance Lude.AWSRequest AdminSetUserSettings where
  type Rs AdminSetUserSettings = AdminSetUserSettingsResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AdminSetUserSettingsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminSetUserSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminSetUserSettings" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminSetUserSettings where
  toJSON AdminSetUserSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("MFAOptions" Lude..= mfaOptions)
          ]
      )

instance Lude.ToPath AdminSetUserSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminSetUserSettings where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server to set user settings as an administrator.
--
-- /See:/ 'mkAdminSetUserSettingsResponse' smart constructor.
newtype AdminSetUserSettingsResponse = AdminSetUserSettingsResponse'
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

-- | Creates a value of 'AdminSetUserSettingsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAdminSetUserSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminSetUserSettingsResponse
mkAdminSetUserSettingsResponse pResponseStatus_ =
  AdminSetUserSettingsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asusrsResponseStatus :: Lens.Lens' AdminSetUserSettingsResponse Lude.Int
asusrsResponseStatus = Lens.lens (responseStatus :: AdminSetUserSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminSetUserSettingsResponse)
{-# DEPRECATED asusrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
