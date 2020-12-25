{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminAddUserToGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified user to the specified group.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminAddUserToGroup
  ( -- * Creating a request
    AdminAddUserToGroup (..),
    mkAdminAddUserToGroup,

    -- ** Request lenses
    aautgUserPoolId,
    aautgUsername,
    aautgGroupName,

    -- * Destructuring the response
    AdminAddUserToGroupResponse (..),
    mkAdminAddUserToGroupResponse,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAdminAddUserToGroup' smart constructor.
data AdminAddUserToGroup = AdminAddUserToGroup'
  { -- | The user pool ID for the user pool.
    userPoolId :: Types.UserPoolId,
    -- | The username for the user.
    username :: Types.Username,
    -- | The group name.
    groupName :: Types.GroupNameType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminAddUserToGroup' value with any optional fields omitted.
mkAdminAddUserToGroup ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'username'
  Types.Username ->
  -- | 'groupName'
  Types.GroupNameType ->
  AdminAddUserToGroup
mkAdminAddUserToGroup userPoolId username groupName =
  AdminAddUserToGroup' {userPoolId, username, groupName}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aautgUserPoolId :: Lens.Lens' AdminAddUserToGroup Types.UserPoolId
aautgUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED aautgUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The username for the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aautgUsername :: Lens.Lens' AdminAddUserToGroup Types.Username
aautgUsername = Lens.field @"username"
{-# DEPRECATED aautgUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The group name.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aautgGroupName :: Lens.Lens' AdminAddUserToGroup Types.GroupNameType
aautgGroupName = Lens.field @"groupName"
{-# DEPRECATED aautgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Core.FromJSON AdminAddUserToGroup where
  toJSON AdminAddUserToGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username),
            Core.Just ("GroupName" Core..= groupName)
          ]
      )

instance Core.AWSRequest AdminAddUserToGroup where
  type Rs AdminAddUserToGroup = AdminAddUserToGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.AdminAddUserToGroup"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull AdminAddUserToGroupResponse'

-- | /See:/ 'mkAdminAddUserToGroupResponse' smart constructor.
data AdminAddUserToGroupResponse = AdminAddUserToGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminAddUserToGroupResponse' value with any optional fields omitted.
mkAdminAddUserToGroupResponse ::
  AdminAddUserToGroupResponse
mkAdminAddUserToGroupResponse = AdminAddUserToGroupResponse'
