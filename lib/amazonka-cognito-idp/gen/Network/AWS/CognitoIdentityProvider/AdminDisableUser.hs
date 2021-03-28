{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminDisableUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified user.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminDisableUser
    (
    -- * Creating a request
      AdminDisableUser (..)
    , mkAdminDisableUser
    -- ** Request lenses
    , aduUserPoolId
    , aduUsername

    -- * Destructuring the response
    , AdminDisableUserResponse (..)
    , mkAdminDisableUserResponse
    -- ** Response lenses
    , adurrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to disable any user as an administrator.
--
-- /See:/ 'mkAdminDisableUser' smart constructor.
data AdminDisableUser = AdminDisableUser'
  { userPoolId :: Types.UserPoolIdType
    -- ^ The user pool ID for the user pool where you want to disable the user.
  , username :: Types.Username
    -- ^ The user name of the user you wish to disable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminDisableUser' value with any optional fields omitted.
mkAdminDisableUser
    :: Types.UserPoolIdType -- ^ 'userPoolId'
    -> Types.Username -- ^ 'username'
    -> AdminDisableUser
mkAdminDisableUser userPoolId username
  = AdminDisableUser'{userPoolId, username}

-- | The user pool ID for the user pool where you want to disable the user.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aduUserPoolId :: Lens.Lens' AdminDisableUser Types.UserPoolIdType
aduUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE aduUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The user name of the user you wish to disable.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aduUsername :: Lens.Lens' AdminDisableUser Types.Username
aduUsername = Lens.field @"username"
{-# INLINEABLE aduUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.ToQuery AdminDisableUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminDisableUser where
        toHeaders AdminDisableUser{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminDisableUser")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminDisableUser where
        toJSON AdminDisableUser{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Username" Core..= username)])

instance Core.AWSRequest AdminDisableUser where
        type Rs AdminDisableUser = AdminDisableUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AdminDisableUserResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Represents the response received from the server to disable the user as an administrator.
--
-- /See:/ 'mkAdminDisableUserResponse' smart constructor.
newtype AdminDisableUserResponse = AdminDisableUserResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminDisableUserResponse' value with any optional fields omitted.
mkAdminDisableUserResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AdminDisableUserResponse
mkAdminDisableUserResponse responseStatus
  = AdminDisableUserResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adurrsResponseStatus :: Lens.Lens' AdminDisableUserResponse Core.Int
adurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE adurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
