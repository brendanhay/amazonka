{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminEnableUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified user as an administrator. Works on any user.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminEnableUser
  ( -- * Creating a request
    AdminEnableUser (..),
    mkAdminEnableUser,

    -- ** Request lenses
    aeuUserPoolId,
    aeuUsername,

    -- * Destructuring the response
    AdminEnableUserResponse (..),
    mkAdminEnableUserResponse,

    -- ** Response lenses
    aeurrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request that enables the user as an administrator.
--
-- /See:/ 'mkAdminEnableUser' smart constructor.
data AdminEnableUser = AdminEnableUser'
  { -- | The user pool ID for the user pool where you want to enable the user.
    userPoolId :: Types.UserPoolIdType,
    -- | The user name of the user you wish to enable.
    username :: Types.UsernameType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminEnableUser' value with any optional fields omitted.
mkAdminEnableUser ::
  -- | 'userPoolId'
  Types.UserPoolIdType ->
  -- | 'username'
  Types.UsernameType ->
  AdminEnableUser
mkAdminEnableUser userPoolId username =
  AdminEnableUser' {userPoolId, username}

-- | The user pool ID for the user pool where you want to enable the user.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeuUserPoolId :: Lens.Lens' AdminEnableUser Types.UserPoolIdType
aeuUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED aeuUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name of the user you wish to enable.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeuUsername :: Lens.Lens' AdminEnableUser Types.UsernameType
aeuUsername = Lens.field @"username"
{-# DEPRECATED aeuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Core.FromJSON AdminEnableUser where
  toJSON AdminEnableUser {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username)
          ]
      )

instance Core.AWSRequest AdminEnableUser where
  type Rs AdminEnableUser = AdminEnableUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.AdminEnableUser"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminEnableUserResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Represents the response from the server for the request to enable a user as an administrator.
--
-- /See:/ 'mkAdminEnableUserResponse' smart constructor.
newtype AdminEnableUserResponse = AdminEnableUserResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminEnableUserResponse' value with any optional fields omitted.
mkAdminEnableUserResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AdminEnableUserResponse
mkAdminEnableUserResponse responseStatus =
  AdminEnableUserResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeurrsResponseStatus :: Lens.Lens' AdminEnableUserResponse Core.Int
aeurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aeurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
