{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminSetUserPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified user's password in a user pool as an administrator. Works on any user. 
--
-- The password can be temporary or permanent. If it is temporary, the user status will be placed into the @FORCE_CHANGE_PASSWORD@ state. When the user next tries to sign in, the InitiateAuth/AdminInitiateAuth response will contain the @NEW_PASSWORD_REQUIRED@ challenge. If the user does not sign in before it expires, the user will not be able to sign in and their password will need to be reset by an administrator. 
-- Once the user has set a new password, or the password is permanent, the user status will be set to @Confirmed@ .
module Network.AWS.CognitoIdentityProvider.AdminSetUserPassword
    (
    -- * Creating a request
      AdminSetUserPassword (..)
    , mkAdminSetUserPassword
    -- ** Request lenses
    , asupUserPoolId
    , asupUsername
    , asupPassword
    , asupPermanent

    -- * Destructuring the response
    , AdminSetUserPasswordResponse (..)
    , mkAdminSetUserPasswordResponse
    -- ** Response lenses
    , asuprrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAdminSetUserPassword' smart constructor.
data AdminSetUserPassword = AdminSetUserPassword'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool where you want to set the user's password.
  , username :: Types.Username
    -- ^ The user name of the user whose password you wish to set.
  , password :: Types.PasswordType
    -- ^ The password for the user.
  , permanent :: Core.Maybe Core.Bool
    -- ^ @True@ if the password is permanent, @False@ if it is temporary.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminSetUserPassword' value with any optional fields omitted.
mkAdminSetUserPassword
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.Username -- ^ 'username'
    -> Types.PasswordType -- ^ 'password'
    -> AdminSetUserPassword
mkAdminSetUserPassword userPoolId username password
  = AdminSetUserPassword'{userPoolId, username, password,
                          permanent = Core.Nothing}

-- | The user pool ID for the user pool where you want to set the user's password.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asupUserPoolId :: Lens.Lens' AdminSetUserPassword Types.UserPoolId
asupUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE asupUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The user name of the user whose password you wish to set.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asupUsername :: Lens.Lens' AdminSetUserPassword Types.Username
asupUsername = Lens.field @"username"
{-# INLINEABLE asupUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | The password for the user.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asupPassword :: Lens.Lens' AdminSetUserPassword Types.PasswordType
asupPassword = Lens.field @"password"
{-# INLINEABLE asupPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | @True@ if the password is permanent, @False@ if it is temporary.
--
-- /Note:/ Consider using 'permanent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asupPermanent :: Lens.Lens' AdminSetUserPassword (Core.Maybe Core.Bool)
asupPermanent = Lens.field @"permanent"
{-# INLINEABLE asupPermanent #-}
{-# DEPRECATED permanent "Use generic-lens or generic-optics with 'permanent' instead"  #-}

instance Core.ToQuery AdminSetUserPassword where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminSetUserPassword where
        toHeaders AdminSetUserPassword{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminSetUserPassword")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminSetUserPassword where
        toJSON AdminSetUserPassword{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Username" Core..= username),
                  Core.Just ("Password" Core..= password),
                  ("Permanent" Core..=) Core.<$> permanent])

instance Core.AWSRequest AdminSetUserPassword where
        type Rs AdminSetUserPassword = AdminSetUserPasswordResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AdminSetUserPasswordResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAdminSetUserPasswordResponse' smart constructor.
newtype AdminSetUserPasswordResponse = AdminSetUserPasswordResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminSetUserPasswordResponse' value with any optional fields omitted.
mkAdminSetUserPasswordResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AdminSetUserPasswordResponse
mkAdminSetUserPasswordResponse responseStatus
  = AdminSetUserPasswordResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asuprrsResponseStatus :: Lens.Lens' AdminSetUserPasswordResponse Core.Int
asuprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE asuprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
