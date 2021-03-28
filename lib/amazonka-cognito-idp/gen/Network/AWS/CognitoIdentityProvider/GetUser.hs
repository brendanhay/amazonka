{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetUser (..)
    , mkGetUser
    -- ** Request lenses
    , guAccessToken

    -- * Destructuring the response
    , GetUserResponse (..)
    , mkGetUserResponse
    -- ** Response lenses
    , gurrsUsername
    , gurrsUserAttributes
    , gurrsMFAOptions
    , gurrsPreferredMfaSetting
    , gurrsUserMFASettingList
    , gurrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to get information about the user.
--
-- /See:/ 'mkGetUser' smart constructor.
newtype GetUser = GetUser'
  { accessToken :: Types.AccessToken
    -- ^ The access token returned by the server response to get information about the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetUser' value with any optional fields omitted.
mkGetUser
    :: Types.AccessToken -- ^ 'accessToken'
    -> GetUser
mkGetUser accessToken = GetUser'{accessToken}

-- | The access token returned by the server response to get information about the user.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guAccessToken :: Lens.Lens' GetUser Types.AccessToken
guAccessToken = Lens.field @"accessToken"
{-# INLINEABLE guAccessToken #-}
{-# DEPRECATED accessToken "Use generic-lens or generic-optics with 'accessToken' instead"  #-}

instance Core.ToQuery GetUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetUser where
        toHeaders GetUser{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityProviderService.GetUser")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetUser where
        toJSON GetUser{..}
          = Core.object
              (Core.catMaybes [Core.Just ("AccessToken" Core..= accessToken)])

instance Core.AWSRequest GetUser where
        type Rs GetUser = GetUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetUserResponse' Core.<$>
                   (x Core..: "Username") Core.<*>
                     x Core..:? "UserAttributes" Core..!= Core.mempty
                     Core.<*> x Core..:? "MFAOptions"
                     Core.<*> x Core..:? "PreferredMfaSetting"
                     Core.<*> x Core..:? "UserMFASettingList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the response from the server from the request to get information about the user.
--
-- /See:/ 'mkGetUserResponse' smart constructor.
data GetUserResponse = GetUserResponse'
  { username :: Types.Username
    -- ^ The user name of the user you wish to retrieve from the get user request.
  , userAttributes :: [Types.AttributeType]
    -- ^ An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
  , mFAOptions :: Core.Maybe [Types.MFAOptionType]
    -- ^ /This response parameter is no longer supported./ It provides information only about SMS MFA configurations. It doesn't provide information about TOTP software token MFA configurations. To look up information about either type of MFA configuration, use UserMFASettingList instead.
  , preferredMfaSetting :: Core.Maybe Types.PreferredMfaSetting
    -- ^ The user's preferred MFA setting.
  , userMFASettingList :: Core.Maybe [Types.StringType]
    -- ^ The MFA options that are enabled for the user. The possible values in this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUserResponse' value with any optional fields omitted.
mkGetUserResponse
    :: Types.Username -- ^ 'username'
    -> Core.Int -- ^ 'responseStatus'
    -> GetUserResponse
mkGetUserResponse username responseStatus
  = GetUserResponse'{username, userAttributes = Core.mempty,
                     mFAOptions = Core.Nothing, preferredMfaSetting = Core.Nothing,
                     userMFASettingList = Core.Nothing, responseStatus}

-- | The user name of the user you wish to retrieve from the get user request.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gurrsUsername :: Lens.Lens' GetUserResponse Types.Username
gurrsUsername = Lens.field @"username"
{-# INLINEABLE gurrsUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gurrsUserAttributes :: Lens.Lens' GetUserResponse [Types.AttributeType]
gurrsUserAttributes = Lens.field @"userAttributes"
{-# INLINEABLE gurrsUserAttributes #-}
{-# DEPRECATED userAttributes "Use generic-lens or generic-optics with 'userAttributes' instead"  #-}

-- | /This response parameter is no longer supported./ It provides information only about SMS MFA configurations. It doesn't provide information about TOTP software token MFA configurations. To look up information about either type of MFA configuration, use UserMFASettingList instead.
--
-- /Note:/ Consider using 'mFAOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gurrsMFAOptions :: Lens.Lens' GetUserResponse (Core.Maybe [Types.MFAOptionType])
gurrsMFAOptions = Lens.field @"mFAOptions"
{-# INLINEABLE gurrsMFAOptions #-}
{-# DEPRECATED mFAOptions "Use generic-lens or generic-optics with 'mFAOptions' instead"  #-}

-- | The user's preferred MFA setting.
--
-- /Note:/ Consider using 'preferredMfaSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gurrsPreferredMfaSetting :: Lens.Lens' GetUserResponse (Core.Maybe Types.PreferredMfaSetting)
gurrsPreferredMfaSetting = Lens.field @"preferredMfaSetting"
{-# INLINEABLE gurrsPreferredMfaSetting #-}
{-# DEPRECATED preferredMfaSetting "Use generic-lens or generic-optics with 'preferredMfaSetting' instead"  #-}

-- | The MFA options that are enabled for the user. The possible values in this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@ .
--
-- /Note:/ Consider using 'userMFASettingList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gurrsUserMFASettingList :: Lens.Lens' GetUserResponse (Core.Maybe [Types.StringType])
gurrsUserMFASettingList = Lens.field @"userMFASettingList"
{-# INLINEABLE gurrsUserMFASettingList #-}
{-# DEPRECATED userMFASettingList "Use generic-lens or generic-optics with 'userMFASettingList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gurrsResponseStatus :: Lens.Lens' GetUserResponse Core.Int
gurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
