{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user attributes and metadata for a user.
module Network.AWS.CognitoIdentityProvider.GetUser
  ( -- * Creating a request
    GetUser (..),
    mkGetUser,

    -- ** Request lenses
    guAccessToken,

    -- * Destructuring the response
    GetUserResponse (..),
    mkGetUserResponse,

    -- ** Response lenses
    gursUserAttributes,
    gursUsername,
    gursUserMFASettingList,
    gursMFAOptions,
    gursPreferredMFASetting,
    gursResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to get information about the user.
--
-- /See:/ 'mkGetUser' smart constructor.
newtype GetUser = GetUser'
  { -- | The access token returned by the server response to get information about the user.
    accessToken :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUser' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token returned by the server response to get information about the user.
mkGetUser ::
  -- | 'accessToken'
  Lude.Sensitive Lude.Text ->
  GetUser
mkGetUser pAccessToken_ = GetUser' {accessToken = pAccessToken_}

-- | The access token returned by the server response to get information about the user.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guAccessToken :: Lens.Lens' GetUser (Lude.Sensitive Lude.Text)
guAccessToken = Lens.lens (accessToken :: GetUser -> Lude.Sensitive Lude.Text) (\s a -> s {accessToken = a} :: GetUser)
{-# DEPRECATED guAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

instance Lude.AWSRequest GetUser where
  type Rs GetUser = GetUserResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUserResponse'
            Lude.<$> (x Lude..?> "UserAttributes" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "Username")
            Lude.<*> (x Lude..?> "UserMFASettingList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "MFAOptions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "PreferredMfaSetting")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSCognitoIdentityProviderService.GetUser" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetUser where
  toJSON GetUser' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AccessToken" Lude..= accessToken)])

instance Lude.ToPath GetUser where
  toPath = Lude.const "/"

instance Lude.ToQuery GetUser where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server from the request to get information about the user.
--
-- /See:/ 'mkGetUserResponse' smart constructor.
data GetUserResponse = GetUserResponse'
  { -- | An array of name-value pairs representing user attributes.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
    userAttributes :: [AttributeType],
    -- | The user name of the user you wish to retrieve from the get user request.
    username :: Lude.Sensitive Lude.Text,
    -- | The MFA options that are enabled for the user. The possible values in this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@ .
    userMFASettingList :: Lude.Maybe [Lude.Text],
    -- | /This response parameter is no longer supported./ It provides information only about SMS MFA configurations. It doesn't provide information about TOTP software token MFA configurations. To look up information about either type of MFA configuration, use UserMFASettingList instead.
    mfaOptions :: Lude.Maybe [MFAOptionType],
    -- | The user's preferred MFA setting.
    preferredMFASetting :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUserResponse' with the minimum fields required to make a request.
--
-- * 'userAttributes' - An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
-- * 'username' - The user name of the user you wish to retrieve from the get user request.
-- * 'userMFASettingList' - The MFA options that are enabled for the user. The possible values in this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@ .
-- * 'mfaOptions' - /This response parameter is no longer supported./ It provides information only about SMS MFA configurations. It doesn't provide information about TOTP software token MFA configurations. To look up information about either type of MFA configuration, use UserMFASettingList instead.
-- * 'preferredMFASetting' - The user's preferred MFA setting.
-- * 'responseStatus' - The response status code.
mkGetUserResponse ::
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  GetUserResponse
mkGetUserResponse pUsername_ pResponseStatus_ =
  GetUserResponse'
    { userAttributes = Lude.mempty,
      username = pUsername_,
      userMFASettingList = Lude.Nothing,
      mfaOptions = Lude.Nothing,
      preferredMFASetting = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gursUserAttributes :: Lens.Lens' GetUserResponse [AttributeType]
gursUserAttributes = Lens.lens (userAttributes :: GetUserResponse -> [AttributeType]) (\s a -> s {userAttributes = a} :: GetUserResponse)
{-# DEPRECATED gursUserAttributes "Use generic-lens or generic-optics with 'userAttributes' instead." #-}

-- | The user name of the user you wish to retrieve from the get user request.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gursUsername :: Lens.Lens' GetUserResponse (Lude.Sensitive Lude.Text)
gursUsername = Lens.lens (username :: GetUserResponse -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: GetUserResponse)
{-# DEPRECATED gursUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The MFA options that are enabled for the user. The possible values in this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@ .
--
-- /Note:/ Consider using 'userMFASettingList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gursUserMFASettingList :: Lens.Lens' GetUserResponse (Lude.Maybe [Lude.Text])
gursUserMFASettingList = Lens.lens (userMFASettingList :: GetUserResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {userMFASettingList = a} :: GetUserResponse)
{-# DEPRECATED gursUserMFASettingList "Use generic-lens or generic-optics with 'userMFASettingList' instead." #-}

-- | /This response parameter is no longer supported./ It provides information only about SMS MFA configurations. It doesn't provide information about TOTP software token MFA configurations. To look up information about either type of MFA configuration, use UserMFASettingList instead.
--
-- /Note:/ Consider using 'mfaOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gursMFAOptions :: Lens.Lens' GetUserResponse (Lude.Maybe [MFAOptionType])
gursMFAOptions = Lens.lens (mfaOptions :: GetUserResponse -> Lude.Maybe [MFAOptionType]) (\s a -> s {mfaOptions = a} :: GetUserResponse)
{-# DEPRECATED gursMFAOptions "Use generic-lens or generic-optics with 'mfaOptions' instead." #-}

-- | The user's preferred MFA setting.
--
-- /Note:/ Consider using 'preferredMFASetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gursPreferredMFASetting :: Lens.Lens' GetUserResponse (Lude.Maybe Lude.Text)
gursPreferredMFASetting = Lens.lens (preferredMFASetting :: GetUserResponse -> Lude.Maybe Lude.Text) (\s a -> s {preferredMFASetting = a} :: GetUserResponse)
{-# DEPRECATED gursPreferredMFASetting "Use generic-lens or generic-optics with 'preferredMFASetting' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gursResponseStatus :: Lens.Lens' GetUserResponse Lude.Int
gursResponseStatus = Lens.lens (responseStatus :: GetUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUserResponse)
{-# DEPRECATED gursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
