{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.AssociateRoleToGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a role with a group. Your Greengrass core will use the role to access AWS cloud services. The role's permissions should allow Greengrass core Lambda functions to perform actions against the cloud.
module Network.AWS.Greengrass.AssociateRoleToGroup
  ( -- * Creating a request
    AssociateRoleToGroup (..),
    mkAssociateRoleToGroup,

    -- ** Request lenses
    artgGroupId,
    artgRoleArn,

    -- * Destructuring the response
    AssociateRoleToGroupResponse (..),
    mkAssociateRoleToGroupResponse,

    -- ** Response lenses
    artgrrsAssociatedAt,
    artgrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateRoleToGroup' smart constructor.
data AssociateRoleToGroup = AssociateRoleToGroup'
  { -- | The ID of the Greengrass group.
    groupId :: Core.Text,
    -- | The ARN of the role you wish to associate with this group. The existence of the role is not validated.
    roleArn :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateRoleToGroup' value with any optional fields omitted.
mkAssociateRoleToGroup ::
  -- | 'groupId'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  AssociateRoleToGroup
mkAssociateRoleToGroup groupId roleArn =
  AssociateRoleToGroup' {groupId, roleArn}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artgGroupId :: Lens.Lens' AssociateRoleToGroup Core.Text
artgGroupId = Lens.field @"groupId"
{-# DEPRECATED artgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The ARN of the role you wish to associate with this group. The existence of the role is not validated.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artgRoleArn :: Lens.Lens' AssociateRoleToGroup Core.Text
artgRoleArn = Lens.field @"roleArn"
{-# DEPRECATED artgRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

instance Core.FromJSON AssociateRoleToGroup where
  toJSON AssociateRoleToGroup {..} =
    Core.object
      (Core.catMaybes [Core.Just ("RoleArn" Core..= roleArn)])

instance Core.AWSRequest AssociateRoleToGroup where
  type Rs AssociateRoleToGroup = AssociateRoleToGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/groups/" Core.<> (Core.toText groupId)
                Core.<> ("/role")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateRoleToGroupResponse'
            Core.<$> (x Core..:? "AssociatedAt") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateRoleToGroupResponse' smart constructor.
data AssociateRoleToGroupResponse = AssociateRoleToGroupResponse'
  { -- | The time, in milliseconds since the epoch, when the role ARN was associated with the group.
    associatedAt :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateRoleToGroupResponse' value with any optional fields omitted.
mkAssociateRoleToGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateRoleToGroupResponse
mkAssociateRoleToGroupResponse responseStatus =
  AssociateRoleToGroupResponse'
    { associatedAt = Core.Nothing,
      responseStatus
    }

-- | The time, in milliseconds since the epoch, when the role ARN was associated with the group.
--
-- /Note:/ Consider using 'associatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artgrrsAssociatedAt :: Lens.Lens' AssociateRoleToGroupResponse (Core.Maybe Core.Text)
artgrrsAssociatedAt = Lens.field @"associatedAt"
{-# DEPRECATED artgrrsAssociatedAt "Use generic-lens or generic-optics with 'associatedAt' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artgrrsResponseStatus :: Lens.Lens' AssociateRoleToGroupResponse Core.Int
artgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED artgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
