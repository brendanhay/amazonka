{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminGetUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified user by user name in a user pool as an administrator. Works on any user.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminGetUser
  ( -- * Creating a request
    AdminGetUser (..),
    mkAdminGetUser,

    -- ** Request lenses
    aguUserPoolId,
    aguUsername,

    -- * Destructuring the response
    AdminGetUserResponse (..),
    mkAdminGetUserResponse,

    -- ** Response lenses
    agursEnabled,
    agursUserStatus,
    agursUserAttributes,
    agursUserCreateDate,
    agursUserMFASettingList,
    agursMFAOptions,
    agursUserLastModifiedDate,
    agursPreferredMFASetting,
    agursResponseStatus,
    agursUsername,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to get the specified user as an administrator.
--
-- /See:/ 'mkAdminGetUser' smart constructor.
data AdminGetUser = AdminGetUser'
  { userPoolId :: Lude.Text,
    username :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminGetUser' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool where you want to get information about the user.
-- * 'username' - The user name of the user you wish to retrieve.
mkAdminGetUser ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminGetUser
mkAdminGetUser pUserPoolId_ pUsername_ =
  AdminGetUser' {userPoolId = pUserPoolId_, username = pUsername_}

-- | The user pool ID for the user pool where you want to get information about the user.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aguUserPoolId :: Lens.Lens' AdminGetUser Lude.Text
aguUserPoolId = Lens.lens (userPoolId :: AdminGetUser -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminGetUser)
{-# DEPRECATED aguUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name of the user you wish to retrieve.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aguUsername :: Lens.Lens' AdminGetUser (Lude.Sensitive Lude.Text)
aguUsername = Lens.lens (username :: AdminGetUser -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminGetUser)
{-# DEPRECATED aguUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Lude.AWSRequest AdminGetUser where
  type Rs AdminGetUser = AdminGetUserResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          AdminGetUserResponse'
            Lude.<$> (x Lude..?> "Enabled")
            Lude.<*> (x Lude..?> "UserStatus")
            Lude.<*> (x Lude..?> "UserAttributes" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "UserCreateDate")
            Lude.<*> (x Lude..?> "UserMFASettingList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "MFAOptions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "UserLastModifiedDate")
            Lude.<*> (x Lude..?> "PreferredMfaSetting")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "Username")
      )

instance Lude.ToHeaders AdminGetUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminGetUser" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminGetUser where
  toJSON AdminGetUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username)
          ]
      )

instance Lude.ToPath AdminGetUser where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminGetUser where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server from the request to get the specified user as an administrator.
--
-- /See:/ 'mkAdminGetUserResponse' smart constructor.
data AdminGetUserResponse = AdminGetUserResponse'
  { enabled ::
      Lude.Maybe Lude.Bool,
    userStatus :: Lude.Maybe UserStatusType,
    userAttributes :: Lude.Maybe [AttributeType],
    userCreateDate :: Lude.Maybe Lude.Timestamp,
    userMFASettingList :: Lude.Maybe [Lude.Text],
    mfaOptions :: Lude.Maybe [MFAOptionType],
    userLastModifiedDate :: Lude.Maybe Lude.Timestamp,
    preferredMFASetting :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    username :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminGetUserResponse' with the minimum fields required to make a request.
--
-- * 'enabled' - Indicates that the status is enabled.
-- * 'mfaOptions' - /This response parameter is no longer supported./ It provides information only about SMS MFA configurations. It doesn't provide information about TOTP software token MFA configurations. To look up information about either type of MFA configuration, use UserMFASettingList instead.
-- * 'preferredMFASetting' - The user's preferred MFA setting.
-- * 'responseStatus' - The response status code.
-- * 'userAttributes' - An array of name-value pairs representing user attributes.
-- * 'userCreateDate' - The date the user was created.
-- * 'userLastModifiedDate' - The date the user was last modified.
-- * 'userMFASettingList' - The MFA options that are enabled for the user. The possible values in this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@ .
-- * 'userStatus' - The user status. Can be one of the following:
--
--
--     * UNCONFIRMED - User has been created but not confirmed.
--
--
--     * CONFIRMED - User has been confirmed.
--
--
--     * ARCHIVED - User is no longer active.
--
--
--     * COMPROMISED - User is disabled due to a potential security threat.
--
--
--     * UNKNOWN - User status is not known.
--
--
--     * RESET_REQUIRED - User is confirmed, but the user must request a code and reset his or her password before he or she can sign in.
--
--
--     * FORCE_CHANGE_PASSWORD - The user is confirmed and the user can sign in using a temporary password, but on first sign-in, the user must change his or her password to a new value before doing anything else.
--
--
-- * 'username' - The user name of the user about whom you are receiving information.
mkAdminGetUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminGetUserResponse
mkAdminGetUserResponse pResponseStatus_ pUsername_ =
  AdminGetUserResponse'
    { enabled = Lude.Nothing,
      userStatus = Lude.Nothing,
      userAttributes = Lude.Nothing,
      userCreateDate = Lude.Nothing,
      userMFASettingList = Lude.Nothing,
      mfaOptions = Lude.Nothing,
      userLastModifiedDate = Lude.Nothing,
      preferredMFASetting = Lude.Nothing,
      responseStatus = pResponseStatus_,
      username = pUsername_
    }

-- | Indicates that the status is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agursEnabled :: Lens.Lens' AdminGetUserResponse (Lude.Maybe Lude.Bool)
agursEnabled = Lens.lens (enabled :: AdminGetUserResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: AdminGetUserResponse)
{-# DEPRECATED agursEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The user status. Can be one of the following:
--
--
--     * UNCONFIRMED - User has been created but not confirmed.
--
--
--     * CONFIRMED - User has been confirmed.
--
--
--     * ARCHIVED - User is no longer active.
--
--
--     * COMPROMISED - User is disabled due to a potential security threat.
--
--
--     * UNKNOWN - User status is not known.
--
--
--     * RESET_REQUIRED - User is confirmed, but the user must request a code and reset his or her password before he or she can sign in.
--
--
--     * FORCE_CHANGE_PASSWORD - The user is confirmed and the user can sign in using a temporary password, but on first sign-in, the user must change his or her password to a new value before doing anything else.
--
--
--
-- /Note:/ Consider using 'userStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agursUserStatus :: Lens.Lens' AdminGetUserResponse (Lude.Maybe UserStatusType)
agursUserStatus = Lens.lens (userStatus :: AdminGetUserResponse -> Lude.Maybe UserStatusType) (\s a -> s {userStatus = a} :: AdminGetUserResponse)
{-# DEPRECATED agursUserStatus "Use generic-lens or generic-optics with 'userStatus' instead." #-}

-- | An array of name-value pairs representing user attributes.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agursUserAttributes :: Lens.Lens' AdminGetUserResponse (Lude.Maybe [AttributeType])
agursUserAttributes = Lens.lens (userAttributes :: AdminGetUserResponse -> Lude.Maybe [AttributeType]) (\s a -> s {userAttributes = a} :: AdminGetUserResponse)
{-# DEPRECATED agursUserAttributes "Use generic-lens or generic-optics with 'userAttributes' instead." #-}

-- | The date the user was created.
--
-- /Note:/ Consider using 'userCreateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agursUserCreateDate :: Lens.Lens' AdminGetUserResponse (Lude.Maybe Lude.Timestamp)
agursUserCreateDate = Lens.lens (userCreateDate :: AdminGetUserResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {userCreateDate = a} :: AdminGetUserResponse)
{-# DEPRECATED agursUserCreateDate "Use generic-lens or generic-optics with 'userCreateDate' instead." #-}

-- | The MFA options that are enabled for the user. The possible values in this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@ .
--
-- /Note:/ Consider using 'userMFASettingList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agursUserMFASettingList :: Lens.Lens' AdminGetUserResponse (Lude.Maybe [Lude.Text])
agursUserMFASettingList = Lens.lens (userMFASettingList :: AdminGetUserResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {userMFASettingList = a} :: AdminGetUserResponse)
{-# DEPRECATED agursUserMFASettingList "Use generic-lens or generic-optics with 'userMFASettingList' instead." #-}

-- | /This response parameter is no longer supported./ It provides information only about SMS MFA configurations. It doesn't provide information about TOTP software token MFA configurations. To look up information about either type of MFA configuration, use UserMFASettingList instead.
--
-- /Note:/ Consider using 'mfaOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agursMFAOptions :: Lens.Lens' AdminGetUserResponse (Lude.Maybe [MFAOptionType])
agursMFAOptions = Lens.lens (mfaOptions :: AdminGetUserResponse -> Lude.Maybe [MFAOptionType]) (\s a -> s {mfaOptions = a} :: AdminGetUserResponse)
{-# DEPRECATED agursMFAOptions "Use generic-lens or generic-optics with 'mfaOptions' instead." #-}

-- | The date the user was last modified.
--
-- /Note:/ Consider using 'userLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agursUserLastModifiedDate :: Lens.Lens' AdminGetUserResponse (Lude.Maybe Lude.Timestamp)
agursUserLastModifiedDate = Lens.lens (userLastModifiedDate :: AdminGetUserResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {userLastModifiedDate = a} :: AdminGetUserResponse)
{-# DEPRECATED agursUserLastModifiedDate "Use generic-lens or generic-optics with 'userLastModifiedDate' instead." #-}

-- | The user's preferred MFA setting.
--
-- /Note:/ Consider using 'preferredMFASetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agursPreferredMFASetting :: Lens.Lens' AdminGetUserResponse (Lude.Maybe Lude.Text)
agursPreferredMFASetting = Lens.lens (preferredMFASetting :: AdminGetUserResponse -> Lude.Maybe Lude.Text) (\s a -> s {preferredMFASetting = a} :: AdminGetUserResponse)
{-# DEPRECATED agursPreferredMFASetting "Use generic-lens or generic-optics with 'preferredMFASetting' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agursResponseStatus :: Lens.Lens' AdminGetUserResponse Lude.Int
agursResponseStatus = Lens.lens (responseStatus :: AdminGetUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminGetUserResponse)
{-# DEPRECATED agursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The user name of the user about whom you are receiving information.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agursUsername :: Lens.Lens' AdminGetUserResponse (Lude.Sensitive Lude.Text)
agursUsername = Lens.lens (username :: AdminGetUserResponse -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminGetUserResponse)
{-# DEPRECATED agursUsername "Use generic-lens or generic-optics with 'username' instead." #-}
