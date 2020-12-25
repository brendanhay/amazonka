{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.AssociateMemberToGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a member (user or group) to the group's set.
module Network.AWS.WorkMail.AssociateMemberToGroup
  ( -- * Creating a request
    AssociateMemberToGroup (..),
    mkAssociateMemberToGroup,

    -- ** Request lenses
    amtgOrganizationId,
    amtgGroupId,
    amtgMemberId,

    -- * Destructuring the response
    AssociateMemberToGroupResponse (..),
    mkAssociateMemberToGroupResponse,

    -- ** Response lenses
    amtgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkAssociateMemberToGroup' smart constructor.
data AssociateMemberToGroup = AssociateMemberToGroup'
  { -- | The organization under which the group exists.
    organizationId :: Types.OrganizationId,
    -- | The group to which the member (user or group) is associated.
    groupId :: Types.WorkMailIdentifier,
    -- | The member (user or group) to associate to the group.
    memberId :: Types.WorkMailIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateMemberToGroup' value with any optional fields omitted.
mkAssociateMemberToGroup ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'groupId'
  Types.WorkMailIdentifier ->
  -- | 'memberId'
  Types.WorkMailIdentifier ->
  AssociateMemberToGroup
mkAssociateMemberToGroup organizationId groupId memberId =
  AssociateMemberToGroup' {organizationId, groupId, memberId}

-- | The organization under which the group exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amtgOrganizationId :: Lens.Lens' AssociateMemberToGroup Types.OrganizationId
amtgOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED amtgOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The group to which the member (user or group) is associated.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amtgGroupId :: Lens.Lens' AssociateMemberToGroup Types.WorkMailIdentifier
amtgGroupId = Lens.field @"groupId"
{-# DEPRECATED amtgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The member (user or group) to associate to the group.
--
-- /Note:/ Consider using 'memberId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amtgMemberId :: Lens.Lens' AssociateMemberToGroup Types.WorkMailIdentifier
amtgMemberId = Lens.field @"memberId"
{-# DEPRECATED amtgMemberId "Use generic-lens or generic-optics with 'memberId' instead." #-}

instance Core.FromJSON AssociateMemberToGroup where
  toJSON AssociateMemberToGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("GroupId" Core..= groupId),
            Core.Just ("MemberId" Core..= memberId)
          ]
      )

instance Core.AWSRequest AssociateMemberToGroup where
  type Rs AssociateMemberToGroup = AssociateMemberToGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkMailService.AssociateMemberToGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateMemberToGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateMemberToGroupResponse' smart constructor.
newtype AssociateMemberToGroupResponse = AssociateMemberToGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateMemberToGroupResponse' value with any optional fields omitted.
mkAssociateMemberToGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateMemberToGroupResponse
mkAssociateMemberToGroupResponse responseStatus =
  AssociateMemberToGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amtgrrsResponseStatus :: Lens.Lens' AssociateMemberToGroupResponse Core.Int
amtgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED amtgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
