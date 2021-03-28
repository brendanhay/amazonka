{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUserGlobalSignOut
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Signs out users from all devices, as an administrator. It also invalidates all refresh tokens issued to a user. The user's current access and Id tokens remain valid until their expiry. Access and Id tokens expire one hour after they are issued.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminUserGlobalSignOut
    (
    -- * Creating a request
      AdminUserGlobalSignOut (..)
    , mkAdminUserGlobalSignOut
    -- ** Request lenses
    , augsoUserPoolId
    , augsoUsername

    -- * Destructuring the response
    , AdminUserGlobalSignOutResponse (..)
    , mkAdminUserGlobalSignOutResponse
    -- ** Response lenses
    , augsorrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to sign out of all devices, as an administrator.
--
-- /See:/ 'mkAdminUserGlobalSignOut' smart constructor.
data AdminUserGlobalSignOut = AdminUserGlobalSignOut'
  { userPoolId :: Types.UserPoolIdType
    -- ^ The user pool ID.
  , username :: Types.UsernameType
    -- ^ The user name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminUserGlobalSignOut' value with any optional fields omitted.
mkAdminUserGlobalSignOut
    :: Types.UserPoolIdType -- ^ 'userPoolId'
    -> Types.UsernameType -- ^ 'username'
    -> AdminUserGlobalSignOut
mkAdminUserGlobalSignOut userPoolId username
  = AdminUserGlobalSignOut'{userPoolId, username}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
augsoUserPoolId :: Lens.Lens' AdminUserGlobalSignOut Types.UserPoolIdType
augsoUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE augsoUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
augsoUsername :: Lens.Lens' AdminUserGlobalSignOut Types.UsernameType
augsoUsername = Lens.field @"username"
{-# INLINEABLE augsoUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.ToQuery AdminUserGlobalSignOut where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminUserGlobalSignOut where
        toHeaders AdminUserGlobalSignOut{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminUserGlobalSignOut")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminUserGlobalSignOut where
        toJSON AdminUserGlobalSignOut{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Username" Core..= username)])

instance Core.AWSRequest AdminUserGlobalSignOut where
        type Rs AdminUserGlobalSignOut = AdminUserGlobalSignOutResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AdminUserGlobalSignOutResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The global sign-out response, as an administrator.
--
-- /See:/ 'mkAdminUserGlobalSignOutResponse' smart constructor.
newtype AdminUserGlobalSignOutResponse = AdminUserGlobalSignOutResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminUserGlobalSignOutResponse' value with any optional fields omitted.
mkAdminUserGlobalSignOutResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AdminUserGlobalSignOutResponse
mkAdminUserGlobalSignOutResponse responseStatus
  = AdminUserGlobalSignOutResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
augsorrsResponseStatus :: Lens.Lens' AdminUserGlobalSignOutResponse Core.Int
augsorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE augsorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
