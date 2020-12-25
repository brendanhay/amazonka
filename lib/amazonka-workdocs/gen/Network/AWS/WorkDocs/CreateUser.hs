{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cuUsername,
    cuGivenName,
    cuSurname,
    cuPassword,
    cuAuthenticationToken,
    cuEmailAddress,
    cuOrganizationId,
    cuStorageRule,
    cuTimeZoneId,

    -- * Destructuring the response
    CreateUserResponse (..),
    mkCreateUserResponse,

    -- ** Response lenses
    currsUser,
    currsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The login name of the user.
    username :: Types.Username,
    -- | The given name of the user.
    givenName :: Types.UserAttributeValueType,
    -- | The surname of the user.
    surname :: Types.UserAttributeValueType,
    -- | The password of the user.
    password :: Types.PasswordType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | The email address of the user.
    emailAddress :: Core.Maybe Types.EmailAddressType,
    -- | The ID of the organization.
    organizationId :: Core.Maybe Types.IdType,
    -- | The amount of storage for the user.
    storageRule :: Core.Maybe Types.StorageRuleType,
    -- | The time zone ID of the user.
    timeZoneId :: Core.Maybe Types.TimeZoneIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUser' value with any optional fields omitted.
mkCreateUser ::
  -- | 'username'
  Types.Username ->
  -- | 'givenName'
  Types.UserAttributeValueType ->
  -- | 'surname'
  Types.UserAttributeValueType ->
  -- | 'password'
  Types.PasswordType ->
  CreateUser
mkCreateUser username givenName surname password =
  CreateUser'
    { username,
      givenName,
      surname,
      password,
      authenticationToken = Core.Nothing,
      emailAddress = Core.Nothing,
      organizationId = Core.Nothing,
      storageRule = Core.Nothing,
      timeZoneId = Core.Nothing
    }

-- | The login name of the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUsername :: Lens.Lens' CreateUser Types.Username
cuUsername = Lens.field @"username"
{-# DEPRECATED cuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The given name of the user.
--
-- /Note:/ Consider using 'givenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuGivenName :: Lens.Lens' CreateUser Types.UserAttributeValueType
cuGivenName = Lens.field @"givenName"
{-# DEPRECATED cuGivenName "Use generic-lens or generic-optics with 'givenName' instead." #-}

-- | The surname of the user.
--
-- /Note:/ Consider using 'surname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuSurname :: Lens.Lens' CreateUser Types.UserAttributeValueType
cuSurname = Lens.field @"surname"
{-# DEPRECATED cuSurname "Use generic-lens or generic-optics with 'surname' instead." #-}

-- | The password of the user.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPassword :: Lens.Lens' CreateUser Types.PasswordType
cuPassword = Lens.field @"password"
{-# DEPRECATED cuPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuAuthenticationToken :: Lens.Lens' CreateUser (Core.Maybe Types.AuthenticationHeaderType)
cuAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED cuAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The email address of the user.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuEmailAddress :: Lens.Lens' CreateUser (Core.Maybe Types.EmailAddressType)
cuEmailAddress = Lens.field @"emailAddress"
{-# DEPRECATED cuEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuOrganizationId :: Lens.Lens' CreateUser (Core.Maybe Types.IdType)
cuOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED cuOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The amount of storage for the user.
--
-- /Note:/ Consider using 'storageRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuStorageRule :: Lens.Lens' CreateUser (Core.Maybe Types.StorageRuleType)
cuStorageRule = Lens.field @"storageRule"
{-# DEPRECATED cuStorageRule "Use generic-lens or generic-optics with 'storageRule' instead." #-}

-- | The time zone ID of the user.
--
-- /Note:/ Consider using 'timeZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuTimeZoneId :: Lens.Lens' CreateUser (Core.Maybe Types.TimeZoneIdType)
cuTimeZoneId = Lens.field @"timeZoneId"
{-# DEPRECATED cuTimeZoneId "Use generic-lens or generic-optics with 'timeZoneId' instead." #-}

instance Core.FromJSON CreateUser where
  toJSON CreateUser {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Username" Core..= username),
            Core.Just ("GivenName" Core..= givenName),
            Core.Just ("Surname" Core..= surname),
            Core.Just ("Password" Core..= password),
            ("EmailAddress" Core..=) Core.<$> emailAddress,
            ("OrganizationId" Core..=) Core.<$> organizationId,
            ("StorageRule" Core..=) Core.<$> storageRule,
            ("TimeZoneId" Core..=) Core.<$> timeZoneId
          ]
      )

instance Core.AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/api/v1/users",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Core.<$> (x Core..:? "User") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The user information.
    user :: Core.Maybe Types.User,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateUserResponse' value with any optional fields omitted.
mkCreateUserResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateUserResponse
mkCreateUserResponse responseStatus =
  CreateUserResponse' {user = Core.Nothing, responseStatus}

-- | The user information.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsUser :: Lens.Lens' CreateUserResponse (Core.Maybe Types.User)
currsUser = Lens.field @"user"
{-# DEPRECATED currsUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsResponseStatus :: Lens.Lens' CreateUserResponse Core.Int
currsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED currsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
