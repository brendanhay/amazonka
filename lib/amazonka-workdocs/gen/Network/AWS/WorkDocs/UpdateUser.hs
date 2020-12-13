{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.UpdateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the specified user, and grants or revokes administrative privileges to the Amazon WorkDocs site.
module Network.AWS.WorkDocs.UpdateUser
  ( -- * Creating a request
    UpdateUser (..),
    mkUpdateUser,

    -- ** Request lenses
    uuGivenName,
    uuGrantPoweruserPrivileges,
    uuLocale,
    uuAuthenticationToken,
    uuUserId,
    uuStorageRule,
    uuType,
    uuSurname,
    uuTimeZoneId,

    -- * Destructuring the response
    UpdateUserResponse (..),
    mkUpdateUserResponse,

    -- ** Response lenses
    uursUser,
    uursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { -- | The given name of the user.
    givenName :: Lude.Maybe Lude.Text,
    -- | Boolean value to determine whether the user is granted Poweruser privileges.
    grantPoweruserPrivileges :: Lude.Maybe BooleanEnumType,
    -- | The locale of the user.
    locale :: Lude.Maybe LocaleType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The ID of the user.
    userId :: Lude.Text,
    -- | The amount of storage for the user.
    storageRule :: Lude.Maybe StorageRuleType,
    -- | The type of the user.
    type' :: Lude.Maybe UserType,
    -- | The surname of the user.
    surname :: Lude.Maybe Lude.Text,
    -- | The time zone ID of the user.
    timeZoneId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUser' with the minimum fields required to make a request.
--
-- * 'givenName' - The given name of the user.
-- * 'grantPoweruserPrivileges' - Boolean value to determine whether the user is granted Poweruser privileges.
-- * 'locale' - The locale of the user.
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'userId' - The ID of the user.
-- * 'storageRule' - The amount of storage for the user.
-- * 'type'' - The type of the user.
-- * 'surname' - The surname of the user.
-- * 'timeZoneId' - The time zone ID of the user.
mkUpdateUser ::
  -- | 'userId'
  Lude.Text ->
  UpdateUser
mkUpdateUser pUserId_ =
  UpdateUser'
    { givenName = Lude.Nothing,
      grantPoweruserPrivileges = Lude.Nothing,
      locale = Lude.Nothing,
      authenticationToken = Lude.Nothing,
      userId = pUserId_,
      storageRule = Lude.Nothing,
      type' = Lude.Nothing,
      surname = Lude.Nothing,
      timeZoneId = Lude.Nothing
    }

-- | The given name of the user.
--
-- /Note:/ Consider using 'givenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuGivenName :: Lens.Lens' UpdateUser (Lude.Maybe Lude.Text)
uuGivenName = Lens.lens (givenName :: UpdateUser -> Lude.Maybe Lude.Text) (\s a -> s {givenName = a} :: UpdateUser)
{-# DEPRECATED uuGivenName "Use generic-lens or generic-optics with 'givenName' instead." #-}

-- | Boolean value to determine whether the user is granted Poweruser privileges.
--
-- /Note:/ Consider using 'grantPoweruserPrivileges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuGrantPoweruserPrivileges :: Lens.Lens' UpdateUser (Lude.Maybe BooleanEnumType)
uuGrantPoweruserPrivileges = Lens.lens (grantPoweruserPrivileges :: UpdateUser -> Lude.Maybe BooleanEnumType) (\s a -> s {grantPoweruserPrivileges = a} :: UpdateUser)
{-# DEPRECATED uuGrantPoweruserPrivileges "Use generic-lens or generic-optics with 'grantPoweruserPrivileges' instead." #-}

-- | The locale of the user.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuLocale :: Lens.Lens' UpdateUser (Lude.Maybe LocaleType)
uuLocale = Lens.lens (locale :: UpdateUser -> Lude.Maybe LocaleType) (\s a -> s {locale = a} :: UpdateUser)
{-# DEPRECATED uuLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuAuthenticationToken :: Lens.Lens' UpdateUser (Lude.Maybe (Lude.Sensitive Lude.Text))
uuAuthenticationToken = Lens.lens (authenticationToken :: UpdateUser -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: UpdateUser)
{-# DEPRECATED uuAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuUserId :: Lens.Lens' UpdateUser Lude.Text
uuUserId = Lens.lens (userId :: UpdateUser -> Lude.Text) (\s a -> s {userId = a} :: UpdateUser)
{-# DEPRECATED uuUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The amount of storage for the user.
--
-- /Note:/ Consider using 'storageRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuStorageRule :: Lens.Lens' UpdateUser (Lude.Maybe StorageRuleType)
uuStorageRule = Lens.lens (storageRule :: UpdateUser -> Lude.Maybe StorageRuleType) (\s a -> s {storageRule = a} :: UpdateUser)
{-# DEPRECATED uuStorageRule "Use generic-lens or generic-optics with 'storageRule' instead." #-}

-- | The type of the user.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuType :: Lens.Lens' UpdateUser (Lude.Maybe UserType)
uuType = Lens.lens (type' :: UpdateUser -> Lude.Maybe UserType) (\s a -> s {type' = a} :: UpdateUser)
{-# DEPRECATED uuType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The surname of the user.
--
-- /Note:/ Consider using 'surname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuSurname :: Lens.Lens' UpdateUser (Lude.Maybe Lude.Text)
uuSurname = Lens.lens (surname :: UpdateUser -> Lude.Maybe Lude.Text) (\s a -> s {surname = a} :: UpdateUser)
{-# DEPRECATED uuSurname "Use generic-lens or generic-optics with 'surname' instead." #-}

-- | The time zone ID of the user.
--
-- /Note:/ Consider using 'timeZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuTimeZoneId :: Lens.Lens' UpdateUser (Lude.Maybe Lude.Text)
uuTimeZoneId = Lens.lens (timeZoneId :: UpdateUser -> Lude.Maybe Lude.Text) (\s a -> s {timeZoneId = a} :: UpdateUser)
{-# DEPRECATED uuTimeZoneId "Use generic-lens or generic-optics with 'timeZoneId' instead." #-}

instance Lude.AWSRequest UpdateUser where
  type Rs UpdateUser = UpdateUserResponse
  request = Req.patchJSON workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateUserResponse'
            Lude.<$> (x Lude..?> "User") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateUser where
  toHeaders UpdateUser' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON UpdateUser where
  toJSON UpdateUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GivenName" Lude..=) Lude.<$> givenName,
            ("GrantPoweruserPrivileges" Lude..=)
              Lude.<$> grantPoweruserPrivileges,
            ("Locale" Lude..=) Lude.<$> locale,
            ("StorageRule" Lude..=) Lude.<$> storageRule,
            ("Type" Lude..=) Lude.<$> type',
            ("Surname" Lude..=) Lude.<$> surname,
            ("TimeZoneId" Lude..=) Lude.<$> timeZoneId
          ]
      )

instance Lude.ToPath UpdateUser where
  toPath UpdateUser' {..} =
    Lude.mconcat ["/api/v1/users/", Lude.toBS userId]

instance Lude.ToQuery UpdateUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  { -- | The user information.
    user :: Lude.Maybe User,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserResponse' with the minimum fields required to make a request.
--
-- * 'user' - The user information.
-- * 'responseStatus' - The response status code.
mkUpdateUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateUserResponse
mkUpdateUserResponse pResponseStatus_ =
  UpdateUserResponse'
    { user = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The user information.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uursUser :: Lens.Lens' UpdateUserResponse (Lude.Maybe User)
uursUser = Lens.lens (user :: UpdateUserResponse -> Lude.Maybe User) (\s a -> s {user = a} :: UpdateUserResponse)
{-# DEPRECATED uursUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uursResponseStatus :: Lens.Lens' UpdateUserResponse Lude.Int
uursResponseStatus = Lens.lens (responseStatus :: UpdateUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateUserResponse)
{-# DEPRECATED uursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
