{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group. Currently only groups with no members can be deleted.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.DeleteGroup
  ( -- * Creating a request
    DeleteGroup (..),
    mkDeleteGroup,

    -- ** Request lenses
    dgGroupName,
    dgUserPoolId,

    -- * Destructuring the response
    DeleteGroupResponse (..),
    mkDeleteGroupResponse,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { -- | The name of the group.
    groupName :: Types.GroupNameType,
    -- | The user pool ID for the user pool.
    userPoolId :: Types.UserPoolId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGroup' value with any optional fields omitted.
mkDeleteGroup ::
  -- | 'groupName'
  Types.GroupNameType ->
  -- | 'userPoolId'
  Types.UserPoolId ->
  DeleteGroup
mkDeleteGroup groupName userPoolId =
  DeleteGroup' {groupName, userPoolId}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroupName :: Lens.Lens' DeleteGroup Types.GroupNameType
dgGroupName = Lens.field @"groupName"
{-# DEPRECATED dgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgUserPoolId :: Lens.Lens' DeleteGroup Types.UserPoolId
dgUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED dgUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Core.FromJSON DeleteGroup where
  toJSON DeleteGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GroupName" Core..= groupName),
            Core.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.AWSRequest DeleteGroup where
  type Rs DeleteGroup = DeleteGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSCognitoIdentityProviderService.DeleteGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteGroupResponse'

-- | /See:/ 'mkDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGroupResponse' value with any optional fields omitted.
mkDeleteGroupResponse ::
  DeleteGroupResponse
mkDeleteGroupResponse = DeleteGroupResponse'
