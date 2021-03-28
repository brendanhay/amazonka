{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminDeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user as an administrator. Works on any user.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminDeleteUser
    (
    -- * Creating a request
      AdminDeleteUser (..)
    , mkAdminDeleteUser
    -- ** Request lenses
    , aUserPoolId
    , aUsername

    -- * Destructuring the response
    , AdminDeleteUserResponse (..)
    , mkAdminDeleteUserResponse
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to delete a user as an administrator.
--
-- /See:/ 'mkAdminDeleteUser' smart constructor.
data AdminDeleteUser = AdminDeleteUser'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool where you want to delete the user.
  , username :: Types.Username
    -- ^ The user name of the user you wish to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminDeleteUser' value with any optional fields omitted.
mkAdminDeleteUser
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.Username -- ^ 'username'
    -> AdminDeleteUser
mkAdminDeleteUser userPoolId username
  = AdminDeleteUser'{userPoolId, username}

-- | The user pool ID for the user pool where you want to delete the user.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aUserPoolId :: Lens.Lens' AdminDeleteUser Types.UserPoolId
aUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE aUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The user name of the user you wish to delete.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aUsername :: Lens.Lens' AdminDeleteUser Types.Username
aUsername = Lens.field @"username"
{-# INLINEABLE aUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.ToQuery AdminDeleteUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminDeleteUser where
        toHeaders AdminDeleteUser{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminDeleteUser")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminDeleteUser where
        toJSON AdminDeleteUser{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Username" Core..= username)])

instance Core.AWSRequest AdminDeleteUser where
        type Rs AdminDeleteUser = AdminDeleteUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull AdminDeleteUserResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAdminDeleteUserResponse' smart constructor.
data AdminDeleteUserResponse = AdminDeleteUserResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminDeleteUserResponse' value with any optional fields omitted.
mkAdminDeleteUserResponse
    :: AdminDeleteUserResponse
mkAdminDeleteUserResponse = AdminDeleteUserResponse'
