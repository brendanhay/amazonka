{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateUser (..)
    , mkCreateUser
    -- ** Request lenses
    , cuUsername
    , cuGivenName
    , cuSurname
    , cuPassword
    , cuAuthenticationToken
    , cuEmailAddress
    , cuOrganizationId
    , cuStorageRule
    , cuTimeZoneId

    -- * Destructuring the response
    , CreateUserResponse (..)
    , mkCreateUserResponse
    -- ** Response lenses
    , currsUser
    , currsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { username :: Types.Username
    -- ^ The login name of the user.
  , givenName :: Types.UserAttributeValueType
    -- ^ The given name of the user.
  , surname :: Types.UserAttributeValueType
    -- ^ The surname of the user.
  , password :: Types.PasswordType
    -- ^ The password of the user.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , emailAddress :: Core.Maybe Types.EmailAddressType
    -- ^ The email address of the user.
  , organizationId :: Core.Maybe Types.IdType
    -- ^ The ID of the organization.
  , storageRule :: Core.Maybe Types.StorageRuleType
    -- ^ The amount of storage for the user.
  , timeZoneId :: Core.Maybe Types.TimeZoneIdType
    -- ^ The time zone ID of the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUser' value with any optional fields omitted.
mkCreateUser
    :: Types.Username -- ^ 'username'
    -> Types.UserAttributeValueType -- ^ 'givenName'
    -> Types.UserAttributeValueType -- ^ 'surname'
    -> Types.PasswordType -- ^ 'password'
    -> CreateUser
mkCreateUser username givenName surname password
  = CreateUser'{username, givenName, surname, password,
                authenticationToken = Core.Nothing, emailAddress = Core.Nothing,
                organizationId = Core.Nothing, storageRule = Core.Nothing,
                timeZoneId = Core.Nothing}

-- | The login name of the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUsername :: Lens.Lens' CreateUser Types.Username
cuUsername = Lens.field @"username"
{-# INLINEABLE cuUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | The given name of the user.
--
-- /Note:/ Consider using 'givenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuGivenName :: Lens.Lens' CreateUser Types.UserAttributeValueType
cuGivenName = Lens.field @"givenName"
{-# INLINEABLE cuGivenName #-}
{-# DEPRECATED givenName "Use generic-lens or generic-optics with 'givenName' instead"  #-}

-- | The surname of the user.
--
-- /Note:/ Consider using 'surname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuSurname :: Lens.Lens' CreateUser Types.UserAttributeValueType
cuSurname = Lens.field @"surname"
{-# INLINEABLE cuSurname #-}
{-# DEPRECATED surname "Use generic-lens or generic-optics with 'surname' instead"  #-}

-- | The password of the user.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPassword :: Lens.Lens' CreateUser Types.PasswordType
cuPassword = Lens.field @"password"
{-# INLINEABLE cuPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuAuthenticationToken :: Lens.Lens' CreateUser (Core.Maybe Types.AuthenticationHeaderType)
cuAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE cuAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | The email address of the user.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuEmailAddress :: Lens.Lens' CreateUser (Core.Maybe Types.EmailAddressType)
cuEmailAddress = Lens.field @"emailAddress"
{-# INLINEABLE cuEmailAddress #-}
{-# DEPRECATED emailAddress "Use generic-lens or generic-optics with 'emailAddress' instead"  #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuOrganizationId :: Lens.Lens' CreateUser (Core.Maybe Types.IdType)
cuOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE cuOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The amount of storage for the user.
--
-- /Note:/ Consider using 'storageRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuStorageRule :: Lens.Lens' CreateUser (Core.Maybe Types.StorageRuleType)
cuStorageRule = Lens.field @"storageRule"
{-# INLINEABLE cuStorageRule #-}
{-# DEPRECATED storageRule "Use generic-lens or generic-optics with 'storageRule' instead"  #-}

-- | The time zone ID of the user.
--
-- /Note:/ Consider using 'timeZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuTimeZoneId :: Lens.Lens' CreateUser (Core.Maybe Types.TimeZoneIdType)
cuTimeZoneId = Lens.field @"timeZoneId"
{-# INLINEABLE cuTimeZoneId #-}
{-# DEPRECATED timeZoneId "Use generic-lens or generic-optics with 'timeZoneId' instead"  #-}

instance Core.ToQuery CreateUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateUser where
        toHeaders CreateUser{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateUser where
        toJSON CreateUser{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Username" Core..= username),
                  Core.Just ("GivenName" Core..= givenName),
                  Core.Just ("Surname" Core..= surname),
                  Core.Just ("Password" Core..= password),
                  ("EmailAddress" Core..=) Core.<$> emailAddress,
                  ("OrganizationId" Core..=) Core.<$> organizationId,
                  ("StorageRule" Core..=) Core.<$> storageRule,
                  ("TimeZoneId" Core..=) Core.<$> timeZoneId])

instance Core.AWSRequest CreateUser where
        type Rs CreateUser = CreateUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/api/v1/users",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateUserResponse' Core.<$>
                   (x Core..:? "User") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { user :: Core.Maybe Types.User
    -- ^ The user information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateUserResponse' value with any optional fields omitted.
mkCreateUserResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateUserResponse
mkCreateUserResponse responseStatus
  = CreateUserResponse'{user = Core.Nothing, responseStatus}

-- | The user information.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsUser :: Lens.Lens' CreateUserResponse (Core.Maybe Types.User)
currsUser = Lens.field @"user"
{-# INLINEABLE currsUser #-}
{-# DEPRECATED user "Use generic-lens or generic-optics with 'user' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
currsResponseStatus :: Lens.Lens' CreateUserResponse Core.Int
currsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE currsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
