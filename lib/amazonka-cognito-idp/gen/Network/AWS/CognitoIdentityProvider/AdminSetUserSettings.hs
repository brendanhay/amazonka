{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AdminSetUserSettings (..)
    , mkAdminSetUserSettings
    -- ** Request lenses
    , asusUserPoolId
    , asusUsername
    , asusMFAOptions

    -- * Destructuring the response
    , AdminSetUserSettingsResponse (..)
    , mkAdminSetUserSettingsResponse
    -- ** Response lenses
    , asusrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | You can use this parameter to set an MFA configuration that uses the SMS delivery medium.
--
-- /See:/ 'mkAdminSetUserSettings' smart constructor.
data AdminSetUserSettings = AdminSetUserSettings'
  { userPoolId :: Types.UserPoolIdType
    -- ^ The ID of the user pool that contains the user that you are setting options for.
  , username :: Types.Username
    -- ^ The user name of the user that you are setting options for.
  , mFAOptions :: [Types.MFAOptionType]
    -- ^ You can use this parameter only to set an SMS configuration that uses SMS for delivery.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminSetUserSettings' value with any optional fields omitted.
mkAdminSetUserSettings
    :: Types.UserPoolIdType -- ^ 'userPoolId'
    -> Types.Username -- ^ 'username'
    -> AdminSetUserSettings
mkAdminSetUserSettings userPoolId username
  = AdminSetUserSettings'{userPoolId, username,
                          mFAOptions = Core.mempty}

-- | The ID of the user pool that contains the user that you are setting options for.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asusUserPoolId :: Lens.Lens' AdminSetUserSettings Types.UserPoolIdType
asusUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE asusUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The user name of the user that you are setting options for.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asusUsername :: Lens.Lens' AdminSetUserSettings Types.Username
asusUsername = Lens.field @"username"
{-# INLINEABLE asusUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | You can use this parameter only to set an SMS configuration that uses SMS for delivery.
--
-- /Note:/ Consider using 'mFAOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asusMFAOptions :: Lens.Lens' AdminSetUserSettings [Types.MFAOptionType]
asusMFAOptions = Lens.field @"mFAOptions"
{-# INLINEABLE asusMFAOptions #-}
{-# DEPRECATED mFAOptions "Use generic-lens or generic-optics with 'mFAOptions' instead"  #-}

instance Core.ToQuery AdminSetUserSettings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminSetUserSettings where
        toHeaders AdminSetUserSettings{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminSetUserSettings")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminSetUserSettings where
        toJSON AdminSetUserSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Username" Core..= username),
                  Core.Just ("MFAOptions" Core..= mFAOptions)])

instance Core.AWSRequest AdminSetUserSettings where
        type Rs AdminSetUserSettings = AdminSetUserSettingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AdminSetUserSettingsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Represents the response from the server to set user settings as an administrator.
--
-- /See:/ 'mkAdminSetUserSettingsResponse' smart constructor.
newtype AdminSetUserSettingsResponse = AdminSetUserSettingsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminSetUserSettingsResponse' value with any optional fields omitted.
mkAdminSetUserSettingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AdminSetUserSettingsResponse
mkAdminSetUserSettingsResponse responseStatus
  = AdminSetUserSettingsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asusrrsResponseStatus :: Lens.Lens' AdminSetUserSettingsResponse Core.Int
asusrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE asusrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
