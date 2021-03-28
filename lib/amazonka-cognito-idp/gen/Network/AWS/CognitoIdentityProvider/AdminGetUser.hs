{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AdminGetUser (..)
    , mkAdminGetUser
    -- ** Request lenses
    , aguUserPoolId
    , aguUsername

    -- * Destructuring the response
    , AdminGetUserResponse (..)
    , mkAdminGetUserResponse
    -- ** Response lenses
    , agurrsUsername
    , agurrsEnabled
    , agurrsMFAOptions
    , agurrsPreferredMfaSetting
    , agurrsUserAttributes
    , agurrsUserCreateDate
    , agurrsUserLastModifiedDate
    , agurrsUserMFASettingList
    , agurrsUserStatus
    , agurrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to get the specified user as an administrator.
--
-- /See:/ 'mkAdminGetUser' smart constructor.
data AdminGetUser = AdminGetUser'
  { userPoolId :: Types.UserPoolIdType
    -- ^ The user pool ID for the user pool where you want to get information about the user.
  , username :: Types.UsernameType
    -- ^ The user name of the user you wish to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminGetUser' value with any optional fields omitted.
mkAdminGetUser
    :: Types.UserPoolIdType -- ^ 'userPoolId'
    -> Types.UsernameType -- ^ 'username'
    -> AdminGetUser
mkAdminGetUser userPoolId username
  = AdminGetUser'{userPoolId, username}

-- | The user pool ID for the user pool where you want to get information about the user.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aguUserPoolId :: Lens.Lens' AdminGetUser Types.UserPoolIdType
aguUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE aguUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The user name of the user you wish to retrieve.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aguUsername :: Lens.Lens' AdminGetUser Types.UsernameType
aguUsername = Lens.field @"username"
{-# INLINEABLE aguUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.ToQuery AdminGetUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminGetUser where
        toHeaders AdminGetUser{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityProviderService.AdminGetUser")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminGetUser where
        toJSON AdminGetUser{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Username" Core..= username)])

instance Core.AWSRequest AdminGetUser where
        type Rs AdminGetUser = AdminGetUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AdminGetUserResponse' Core.<$>
                   (x Core..: "Username") Core.<*> x Core..:? "Enabled" Core.<*>
                     x Core..:? "MFAOptions"
                     Core.<*> x Core..:? "PreferredMfaSetting"
                     Core.<*> x Core..:? "UserAttributes"
                     Core.<*> x Core..:? "UserCreateDate"
                     Core.<*> x Core..:? "UserLastModifiedDate"
                     Core.<*> x Core..:? "UserMFASettingList"
                     Core.<*> x Core..:? "UserStatus"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the response from the server from the request to get the specified user as an administrator.
--
-- /See:/ 'mkAdminGetUserResponse' smart constructor.
data AdminGetUserResponse = AdminGetUserResponse'
  { username :: Types.Username
    -- ^ The user name of the user about whom you are receiving information.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Indicates that the status is enabled.
  , mFAOptions :: Core.Maybe [Types.MFAOptionType]
    -- ^ /This response parameter is no longer supported./ It provides information only about SMS MFA configurations. It doesn't provide information about TOTP software token MFA configurations. To look up information about either type of MFA configuration, use UserMFASettingList instead.
  , preferredMfaSetting :: Core.Maybe Types.PreferredMfaSetting
    -- ^ The user's preferred MFA setting.
  , userAttributes :: Core.Maybe [Types.AttributeType]
    -- ^ An array of name-value pairs representing user attributes.
  , userCreateDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the user was created.
  , userLastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the user was last modified.
  , userMFASettingList :: Core.Maybe [Types.StringType]
    -- ^ The MFA options that are enabled for the user. The possible values in this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@ .
  , userStatus :: Core.Maybe Types.UserStatusType
    -- ^ The user status. Can be one of the following:
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
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AdminGetUserResponse' value with any optional fields omitted.
mkAdminGetUserResponse
    :: Types.Username -- ^ 'username'
    -> Core.Int -- ^ 'responseStatus'
    -> AdminGetUserResponse
mkAdminGetUserResponse username responseStatus
  = AdminGetUserResponse'{username, enabled = Core.Nothing,
                          mFAOptions = Core.Nothing, preferredMfaSetting = Core.Nothing,
                          userAttributes = Core.Nothing, userCreateDate = Core.Nothing,
                          userLastModifiedDate = Core.Nothing,
                          userMFASettingList = Core.Nothing, userStatus = Core.Nothing,
                          responseStatus}

-- | The user name of the user about whom you are receiving information.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agurrsUsername :: Lens.Lens' AdminGetUserResponse Types.Username
agurrsUsername = Lens.field @"username"
{-# INLINEABLE agurrsUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | Indicates that the status is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agurrsEnabled :: Lens.Lens' AdminGetUserResponse (Core.Maybe Core.Bool)
agurrsEnabled = Lens.field @"enabled"
{-# INLINEABLE agurrsEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | /This response parameter is no longer supported./ It provides information only about SMS MFA configurations. It doesn't provide information about TOTP software token MFA configurations. To look up information about either type of MFA configuration, use UserMFASettingList instead.
--
-- /Note:/ Consider using 'mFAOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agurrsMFAOptions :: Lens.Lens' AdminGetUserResponse (Core.Maybe [Types.MFAOptionType])
agurrsMFAOptions = Lens.field @"mFAOptions"
{-# INLINEABLE agurrsMFAOptions #-}
{-# DEPRECATED mFAOptions "Use generic-lens or generic-optics with 'mFAOptions' instead"  #-}

-- | The user's preferred MFA setting.
--
-- /Note:/ Consider using 'preferredMfaSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agurrsPreferredMfaSetting :: Lens.Lens' AdminGetUserResponse (Core.Maybe Types.PreferredMfaSetting)
agurrsPreferredMfaSetting = Lens.field @"preferredMfaSetting"
{-# INLINEABLE agurrsPreferredMfaSetting #-}
{-# DEPRECATED preferredMfaSetting "Use generic-lens or generic-optics with 'preferredMfaSetting' instead"  #-}

-- | An array of name-value pairs representing user attributes.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agurrsUserAttributes :: Lens.Lens' AdminGetUserResponse (Core.Maybe [Types.AttributeType])
agurrsUserAttributes = Lens.field @"userAttributes"
{-# INLINEABLE agurrsUserAttributes #-}
{-# DEPRECATED userAttributes "Use generic-lens or generic-optics with 'userAttributes' instead"  #-}

-- | The date the user was created.
--
-- /Note:/ Consider using 'userCreateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agurrsUserCreateDate :: Lens.Lens' AdminGetUserResponse (Core.Maybe Core.NominalDiffTime)
agurrsUserCreateDate = Lens.field @"userCreateDate"
{-# INLINEABLE agurrsUserCreateDate #-}
{-# DEPRECATED userCreateDate "Use generic-lens or generic-optics with 'userCreateDate' instead"  #-}

-- | The date the user was last modified.
--
-- /Note:/ Consider using 'userLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agurrsUserLastModifiedDate :: Lens.Lens' AdminGetUserResponse (Core.Maybe Core.NominalDiffTime)
agurrsUserLastModifiedDate = Lens.field @"userLastModifiedDate"
{-# INLINEABLE agurrsUserLastModifiedDate #-}
{-# DEPRECATED userLastModifiedDate "Use generic-lens or generic-optics with 'userLastModifiedDate' instead"  #-}

-- | The MFA options that are enabled for the user. The possible values in this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@ .
--
-- /Note:/ Consider using 'userMFASettingList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agurrsUserMFASettingList :: Lens.Lens' AdminGetUserResponse (Core.Maybe [Types.StringType])
agurrsUserMFASettingList = Lens.field @"userMFASettingList"
{-# INLINEABLE agurrsUserMFASettingList #-}
{-# DEPRECATED userMFASettingList "Use generic-lens or generic-optics with 'userMFASettingList' instead"  #-}

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
agurrsUserStatus :: Lens.Lens' AdminGetUserResponse (Core.Maybe Types.UserStatusType)
agurrsUserStatus = Lens.field @"userStatus"
{-# INLINEABLE agurrsUserStatus #-}
{-# DEPRECATED userStatus "Use generic-lens or generic-optics with 'userStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agurrsResponseStatus :: Lens.Lens' AdminGetUserResponse Core.Int
agurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE agurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
