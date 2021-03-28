{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateUser (..)
    , mkUpdateUser
    -- ** Request lenses
    , uuUserId
    , uuAuthenticationToken
    , uuGivenName
    , uuGrantPoweruserPrivileges
    , uuLocale
    , uuStorageRule
    , uuSurname
    , uuTimeZoneId
    , uuType

    -- * Destructuring the response
    , UpdateUserResponse (..)
    , mkUpdateUserResponse
    -- ** Response lenses
    , uurrsUser
    , uurrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { userId :: Types.IdType
    -- ^ The ID of the user.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , givenName :: Core.Maybe Types.UserAttributeValueType
    -- ^ The given name of the user.
  , grantPoweruserPrivileges :: Core.Maybe Types.BooleanEnumType
    -- ^ Boolean value to determine whether the user is granted Poweruser privileges.
  , locale :: Core.Maybe Types.LocaleType
    -- ^ The locale of the user.
  , storageRule :: Core.Maybe Types.StorageRuleType
    -- ^ The amount of storage for the user.
  , surname :: Core.Maybe Types.UserAttributeValueType
    -- ^ The surname of the user.
  , timeZoneId :: Core.Maybe Types.TimeZoneIdType
    -- ^ The time zone ID of the user.
  , type' :: Core.Maybe Types.UserType
    -- ^ The type of the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUser' value with any optional fields omitted.
mkUpdateUser
    :: Types.IdType -- ^ 'userId'
    -> UpdateUser
mkUpdateUser userId
  = UpdateUser'{userId, authenticationToken = Core.Nothing,
                givenName = Core.Nothing, grantPoweruserPrivileges = Core.Nothing,
                locale = Core.Nothing, storageRule = Core.Nothing,
                surname = Core.Nothing, timeZoneId = Core.Nothing,
                type' = Core.Nothing}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuUserId :: Lens.Lens' UpdateUser Types.IdType
uuUserId = Lens.field @"userId"
{-# INLINEABLE uuUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuAuthenticationToken :: Lens.Lens' UpdateUser (Core.Maybe Types.AuthenticationHeaderType)
uuAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE uuAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | The given name of the user.
--
-- /Note:/ Consider using 'givenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuGivenName :: Lens.Lens' UpdateUser (Core.Maybe Types.UserAttributeValueType)
uuGivenName = Lens.field @"givenName"
{-# INLINEABLE uuGivenName #-}
{-# DEPRECATED givenName "Use generic-lens or generic-optics with 'givenName' instead"  #-}

-- | Boolean value to determine whether the user is granted Poweruser privileges.
--
-- /Note:/ Consider using 'grantPoweruserPrivileges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuGrantPoweruserPrivileges :: Lens.Lens' UpdateUser (Core.Maybe Types.BooleanEnumType)
uuGrantPoweruserPrivileges = Lens.field @"grantPoweruserPrivileges"
{-# INLINEABLE uuGrantPoweruserPrivileges #-}
{-# DEPRECATED grantPoweruserPrivileges "Use generic-lens or generic-optics with 'grantPoweruserPrivileges' instead"  #-}

-- | The locale of the user.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuLocale :: Lens.Lens' UpdateUser (Core.Maybe Types.LocaleType)
uuLocale = Lens.field @"locale"
{-# INLINEABLE uuLocale #-}
{-# DEPRECATED locale "Use generic-lens or generic-optics with 'locale' instead"  #-}

-- | The amount of storage for the user.
--
-- /Note:/ Consider using 'storageRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuStorageRule :: Lens.Lens' UpdateUser (Core.Maybe Types.StorageRuleType)
uuStorageRule = Lens.field @"storageRule"
{-# INLINEABLE uuStorageRule #-}
{-# DEPRECATED storageRule "Use generic-lens or generic-optics with 'storageRule' instead"  #-}

-- | The surname of the user.
--
-- /Note:/ Consider using 'surname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuSurname :: Lens.Lens' UpdateUser (Core.Maybe Types.UserAttributeValueType)
uuSurname = Lens.field @"surname"
{-# INLINEABLE uuSurname #-}
{-# DEPRECATED surname "Use generic-lens or generic-optics with 'surname' instead"  #-}

-- | The time zone ID of the user.
--
-- /Note:/ Consider using 'timeZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuTimeZoneId :: Lens.Lens' UpdateUser (Core.Maybe Types.TimeZoneIdType)
uuTimeZoneId = Lens.field @"timeZoneId"
{-# INLINEABLE uuTimeZoneId #-}
{-# DEPRECATED timeZoneId "Use generic-lens or generic-optics with 'timeZoneId' instead"  #-}

-- | The type of the user.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuType :: Lens.Lens' UpdateUser (Core.Maybe Types.UserType)
uuType = Lens.field @"type'"
{-# INLINEABLE uuType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery UpdateUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateUser where
        toHeaders UpdateUser{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateUser where
        toJSON UpdateUser{..}
          = Core.object
              (Core.catMaybes
                 [("GivenName" Core..=) Core.<$> givenName,
                  ("GrantPoweruserPrivileges" Core..=) Core.<$>
                    grantPoweruserPrivileges,
                  ("Locale" Core..=) Core.<$> locale,
                  ("StorageRule" Core..=) Core.<$> storageRule,
                  ("Surname" Core..=) Core.<$> surname,
                  ("TimeZoneId" Core..=) Core.<$> timeZoneId,
                  ("Type" Core..=) Core.<$> type'])

instance Core.AWSRequest UpdateUser where
        type Rs UpdateUser = UpdateUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath = "/api/v1/users/" Core.<> Core.toText userId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateUserResponse' Core.<$>
                   (x Core..:? "User") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  { user :: Core.Maybe Types.User
    -- ^ The user information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateUserResponse' value with any optional fields omitted.
mkUpdateUserResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateUserResponse
mkUpdateUserResponse responseStatus
  = UpdateUserResponse'{user = Core.Nothing, responseStatus}

-- | The user information.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uurrsUser :: Lens.Lens' UpdateUserResponse (Core.Maybe Types.User)
uurrsUser = Lens.field @"user"
{-# INLINEABLE uurrsUser #-}
{-# DEPRECATED user "Use generic-lens or generic-optics with 'user' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uurrsResponseStatus :: Lens.Lens' UpdateUserResponse Core.Int
uurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
