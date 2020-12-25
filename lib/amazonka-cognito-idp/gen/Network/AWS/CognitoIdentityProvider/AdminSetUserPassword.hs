{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AdminSetUserPassword (..),
    mkAdminSetUserPassword,

    -- ** Request lenses
    asupUserPoolId,
    asupUsername,
    asupPassword,
    asupPermanent,

    -- * Destructuring the response
    AdminSetUserPasswordResponse (..),
    mkAdminSetUserPasswordResponse,

    -- ** Response lenses
    asuprrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAdminSetUserPassword' smart constructor.
data AdminSetUserPassword = AdminSetUserPassword'
  { -- | The user pool ID for the user pool where you want to set the user's password.
    userPoolId :: Types.UserPoolId,
    -- | The user name of the user whose password you wish to set.
    username :: Types.Username,
    -- | The password for the user.
    password :: Types.PasswordType,
    -- | @True@ if the password is permanent, @False@ if it is temporary.
    permanent :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminSetUserPassword' value with any optional fields omitted.
mkAdminSetUserPassword ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'username'
  Types.Username ->
  -- | 'password'
  Types.PasswordType ->
  AdminSetUserPassword
mkAdminSetUserPassword userPoolId username password =
  AdminSetUserPassword'
    { userPoolId,
      username,
      password,
      permanent = Core.Nothing
    }

-- | The user pool ID for the user pool where you want to set the user's password.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asupUserPoolId :: Lens.Lens' AdminSetUserPassword Types.UserPoolId
asupUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED asupUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name of the user whose password you wish to set.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asupUsername :: Lens.Lens' AdminSetUserPassword Types.Username
asupUsername = Lens.field @"username"
{-# DEPRECATED asupUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The password for the user.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asupPassword :: Lens.Lens' AdminSetUserPassword Types.PasswordType
asupPassword = Lens.field @"password"
{-# DEPRECATED asupPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | @True@ if the password is permanent, @False@ if it is temporary.
--
-- /Note:/ Consider using 'permanent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asupPermanent :: Lens.Lens' AdminSetUserPassword (Core.Maybe Core.Bool)
asupPermanent = Lens.field @"permanent"
{-# DEPRECATED asupPermanent "Use generic-lens or generic-optics with 'permanent' instead." #-}

instance Core.FromJSON AdminSetUserPassword where
  toJSON AdminSetUserPassword {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username),
            Core.Just ("Password" Core..= password),
            ("Permanent" Core..=) Core.<$> permanent
          ]
      )

instance Core.AWSRequest AdminSetUserPassword where
  type Rs AdminSetUserPassword = AdminSetUserPasswordResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.AdminSetUserPassword"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminSetUserPasswordResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAdminSetUserPasswordResponse' smart constructor.
newtype AdminSetUserPasswordResponse = AdminSetUserPasswordResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminSetUserPasswordResponse' value with any optional fields omitted.
mkAdminSetUserPasswordResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AdminSetUserPasswordResponse
mkAdminSetUserPasswordResponse responseStatus =
  AdminSetUserPasswordResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asuprrsResponseStatus :: Lens.Lens' AdminSetUserPasswordResponse Core.Int
asuprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED asuprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
