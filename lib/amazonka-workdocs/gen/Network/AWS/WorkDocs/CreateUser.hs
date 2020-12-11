{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.CreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user in a Simple AD or Microsoft AD directory. The status of a newly created user is "ACTIVE". New users can access Amazon WorkDocs.
module Network.AWS.WorkDocs.CreateUser
  ( -- * Creating a request
    CreateUser (..),
    mkCreateUser,

    -- ** Request lenses
    cuAuthenticationToken,
    cuStorageRule,
    cuEmailAddress,
    cuTimeZoneId,
    cuOrganizationId,
    cuUsername,
    cuGivenName,
    cuSurname,
    cuPassword,

    -- * Destructuring the response
    CreateUserResponse (..),
    mkCreateUserResponse,

    -- ** Response lenses
    cursUser,
    cursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { authenticationToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    storageRule :: Lude.Maybe StorageRuleType,
    emailAddress :: Lude.Maybe Lude.Text,
    timeZoneId :: Lude.Maybe Lude.Text,
    organizationId :: Lude.Maybe Lude.Text,
    username :: Lude.Text,
    givenName :: Lude.Text,
    surname :: Lude.Text,
    password :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUser' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'emailAddress' - The email address of the user.
-- * 'givenName' - The given name of the user.
-- * 'organizationId' - The ID of the organization.
-- * 'password' - The password of the user.
-- * 'storageRule' - The amount of storage for the user.
-- * 'surname' - The surname of the user.
-- * 'timeZoneId' - The time zone ID of the user.
-- * 'username' - The login name of the user.
mkCreateUser ::
  -- | 'username'
  Lude.Text ->
  -- | 'givenName'
  Lude.Text ->
  -- | 'surname'
  Lude.Text ->
  -- | 'password'
  Lude.Sensitive Lude.Text ->
  CreateUser
mkCreateUser pUsername_ pGivenName_ pSurname_ pPassword_ =
  CreateUser'
    { authenticationToken = Lude.Nothing,
      storageRule = Lude.Nothing,
      emailAddress = Lude.Nothing,
      timeZoneId = Lude.Nothing,
      organizationId = Lude.Nothing,
      username = pUsername_,
      givenName = pGivenName_,
      surname = pSurname_,
      password = pPassword_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuAuthenticationToken :: Lens.Lens' CreateUser (Lude.Maybe (Lude.Sensitive Lude.Text))
cuAuthenticationToken = Lens.lens (authenticationToken :: CreateUser -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: CreateUser)
{-# DEPRECATED cuAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The amount of storage for the user.
--
-- /Note:/ Consider using 'storageRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuStorageRule :: Lens.Lens' CreateUser (Lude.Maybe StorageRuleType)
cuStorageRule = Lens.lens (storageRule :: CreateUser -> Lude.Maybe StorageRuleType) (\s a -> s {storageRule = a} :: CreateUser)
{-# DEPRECATED cuStorageRule "Use generic-lens or generic-optics with 'storageRule' instead." #-}

-- | The email address of the user.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuEmailAddress :: Lens.Lens' CreateUser (Lude.Maybe Lude.Text)
cuEmailAddress = Lens.lens (emailAddress :: CreateUser -> Lude.Maybe Lude.Text) (\s a -> s {emailAddress = a} :: CreateUser)
{-# DEPRECATED cuEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The time zone ID of the user.
--
-- /Note:/ Consider using 'timeZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuTimeZoneId :: Lens.Lens' CreateUser (Lude.Maybe Lude.Text)
cuTimeZoneId = Lens.lens (timeZoneId :: CreateUser -> Lude.Maybe Lude.Text) (\s a -> s {timeZoneId = a} :: CreateUser)
{-# DEPRECATED cuTimeZoneId "Use generic-lens or generic-optics with 'timeZoneId' instead." #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuOrganizationId :: Lens.Lens' CreateUser (Lude.Maybe Lude.Text)
cuOrganizationId = Lens.lens (organizationId :: CreateUser -> Lude.Maybe Lude.Text) (\s a -> s {organizationId = a} :: CreateUser)
{-# DEPRECATED cuOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The login name of the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUsername :: Lens.Lens' CreateUser Lude.Text
cuUsername = Lens.lens (username :: CreateUser -> Lude.Text) (\s a -> s {username = a} :: CreateUser)
{-# DEPRECATED cuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The given name of the user.
--
-- /Note:/ Consider using 'givenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuGivenName :: Lens.Lens' CreateUser Lude.Text
cuGivenName = Lens.lens (givenName :: CreateUser -> Lude.Text) (\s a -> s {givenName = a} :: CreateUser)
{-# DEPRECATED cuGivenName "Use generic-lens or generic-optics with 'givenName' instead." #-}

-- | The surname of the user.
--
-- /Note:/ Consider using 'surname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuSurname :: Lens.Lens' CreateUser Lude.Text
cuSurname = Lens.lens (surname :: CreateUser -> Lude.Text) (\s a -> s {surname = a} :: CreateUser)
{-# DEPRECATED cuSurname "Use generic-lens or generic-optics with 'surname' instead." #-}

-- | The password of the user.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPassword :: Lens.Lens' CreateUser (Lude.Sensitive Lude.Text)
cuPassword = Lens.lens (password :: CreateUser -> Lude.Sensitive Lude.Text) (\s a -> s {password = a} :: CreateUser)
{-# DEPRECATED cuPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Lude.AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request = Req.postJSON workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Lude.<$> (x Lude..?> "User") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUser where
  toHeaders CreateUser' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StorageRule" Lude..=) Lude.<$> storageRule,
            ("EmailAddress" Lude..=) Lude.<$> emailAddress,
            ("TimeZoneId" Lude..=) Lude.<$> timeZoneId,
            ("OrganizationId" Lude..=) Lude.<$> organizationId,
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("GivenName" Lude..= givenName),
            Lude.Just ("Surname" Lude..= surname),
            Lude.Just ("Password" Lude..= password)
          ]
      )

instance Lude.ToPath CreateUser where
  toPath = Lude.const "/api/v1/users"

instance Lude.ToQuery CreateUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { user ::
      Lude.Maybe User,
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

-- | Creates a value of 'CreateUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'user' - The user information.
mkCreateUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserResponse
mkCreateUserResponse pResponseStatus_ =
  CreateUserResponse'
    { user = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The user information.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursUser :: Lens.Lens' CreateUserResponse (Lude.Maybe User)
cursUser = Lens.lens (user :: CreateUserResponse -> Lude.Maybe User) (\s a -> s {user = a} :: CreateUserResponse)
{-# DEPRECATED cursUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursResponseStatus :: Lens.Lens' CreateUserResponse Lude.Int
cursResponseStatus = Lens.lens (responseStatus :: CreateUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserResponse)
{-# DEPRECATED cursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
